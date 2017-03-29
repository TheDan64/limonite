extern crate llvm_sys;

use self::llvm_sys::core::{LLVMContextCreate, LLVMCreateBuilderInContext, LLVMModuleCreateWithNameInContext, LLVMContextDispose, LLVMDisposeBuilder, LLVMVoidTypeInContext, LLVMDumpModule, LLVMInt1TypeInContext, LLVMInt8TypeInContext, LLVMInt16TypeInContext, LLVMInt32TypeInContext, LLVMInt64TypeInContext, LLVMBuildRet, LLVMBuildRetVoid, LLVMPositionBuilderAtEnd, LLVMBuildCall, LLVMBuildStore, LLVMPointerType, LLVMStructTypeInContext, LLVMAddFunction, LLVMFunctionType, LLVMSetValueName, LLVMCreatePassManager, LLVMBuildExtractValue, LLVMAppendBasicBlockInContext, LLVMBuildLoad, LLVMBuildGEP, LLVMBuildCondBr, LLVMBuildICmp, LLVMBuildCast, LLVMGetNamedFunction, LLVMBuildAdd, LLVMConstInt, LLVMGetFirstParam, LLVMGetNextParam, LLVMCountParams, LLVMDisposePassManager, LLVMCreateFunctionPassManagerForModule, LLVMInitializeFunctionPassManager};
use self::llvm_sys::execution_engine::{LLVMGetExecutionEngineTargetData, LLVMCreateExecutionEngineForModule, LLVMExecutionEngineRef, LLVMRunFunction, LLVMRunFunctionAsMain};
use self::llvm_sys::prelude::{LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMTypeRef, LLVMValueRef, LLVMBasicBlockRef, LLVMPassManagerRef};
use self::llvm_sys::target::{LLVMOpaqueTargetData, LLVMTargetDataRef};
use self::llvm_sys::{LLVMOpcode, LLVMIntPredicate};

use std::ffi::CString;
use std::iter;
use std::mem::{transmute, uninitialized, zeroed};

pub struct Context {
    context: LLVMContextRef,
}

impl Context {
    pub fn new() -> Self {
        Context {
            context: unsafe {
                LLVMContextCreate()
            }
        }
    }

    pub fn create_builder(&self) -> Builder {
        Builder {
            builder: unsafe {
                LLVMCreateBuilderInContext(self.context)
            }
        }
    }

    pub fn create_module(&self, name: &str) -> Module {
        let c_string = CString::new(name).unwrap().as_ptr();

        Module {
            module: unsafe {
                LLVMModuleCreateWithNameInContext(c_string, self.context)
            }
        }
    }

    fn void_type(&self) -> Type {
        unsafe {
            Type {
                type_: LLVMVoidTypeInContext(self.context)
            }
        }
    }

    fn bool_type(&self) -> Type {
        unsafe {
            Type {
                type_: LLVMInt1TypeInContext(self.context)
            }
        }
    }

    fn i8_type(&self) -> Type {
        unsafe {
            Type {
                type_: LLVMInt8TypeInContext(self.context)
            }
        }
    }

    fn i16_type(&self) -> Type {
        unsafe {
            Type {
                type_: LLVMInt16TypeInContext(self.context)
            }
        }
    }

    fn i32_type(&self) -> Type {
        unsafe {
            Type {
                type_: LLVMInt32TypeInContext(self.context)
            }
        }
    }

    fn i64_type(&self) -> Type {
        unsafe {
            Type {
                type_: LLVMInt64TypeInContext(self.context)
            }
        }
    }

    fn struct_type(&self, field_types: Vec<Type>) -> Type {
        // WARNING: transmute will no longer work correctly if Type gains more fields
        // We're avoiding reallocation by telling rust Vec<Type> is identical to Vec<LLVMTypeRef>
        let mut field_types: Vec<LLVMTypeRef> = unsafe {
            transmute(field_types)
        };

        unsafe {
            Type {
                type_: LLVMStructTypeInContext(self.context, field_types.as_mut_ptr(), field_types.len() as u32, 0) // REVIEW: 0 is what? Safe to cast usize as u32?
            }
        }

    }

    fn string_type(&self) -> Type {
        // TODO: Generic vec_type(i8)
        let field_types = vec![
            self.i8_type().ptr_type(0),
            self.i64_type(),
        ];

        self.struct_type(field_types)
    }

    fn append_basic_block(&self, name: &str, function: Value) -> BasicBlock {
        let c_string = CString::new(name).unwrap().as_ptr();

        BasicBlock {
            basic_block: unsafe {
                LLVMAppendBasicBlockInContext(self.context, function.value, c_string)
            }
        }
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            LLVMContextDispose(self.context);
        }
    }
}

pub struct Builder {
    builder: LLVMBuilderRef,
}

impl Builder {
    fn build_return(&self, value: Option<Value>) -> Value {
        Value {
            value: unsafe {
                value.map_or(LLVMBuildRetVoid(self.builder), |value| LLVMBuildRet(self.builder, value.value))
            }
        }
    }

    fn build_call(&self, function: Value, mut args: Vec<Value>, name: &str) -> Value {
        let c_string = CString::new(name).unwrap().as_ptr();

        // WARNING: transmute will no longer work correctly if Value gains more fields
        // We're avoiding reallocation by telling rust Vec<Value> is identical to Vec<LLVMValueRef>
        let mut args: Vec<LLVMValueRef> = unsafe {
            transmute(args)
        };

        Value {
            value: unsafe {
                LLVMBuildCall(self.builder, function.value, args.as_mut_ptr(), args.len() as u32, c_string)
            }
        }
    }

    fn build_gep(&self, ptr: Value, indicies: Vec<Value>, name: &str) -> Value {
        let c_string = CString::new(name).unwrap().as_ptr();

        // WARNING: transmute will no longer work correctly if Value gains more fields
        // We're avoiding reallocation by telling rust Vec<Value> is identical to Vec<LLVMValueRef>
        let mut indicies: Vec<LLVMValueRef> = unsafe {
            transmute(indicies)
        };

        Value {
            value: unsafe {
                LLVMBuildGEP(self.builder, ptr.value, indicies.as_mut_ptr(), indicies.len() as u32, c_string)
            }
        }
    }

    fn build_store(&mut self, value: Value, pointer: Value) -> Value {
        Value {
            value: unsafe {
                LLVMBuildStore(self.builder, value.value, pointer.value)
            }
        }
    }

    fn build_load(&self, name: &str, ptr: Value) -> Value {
        let c_string = CString::new(name).unwrap().as_ptr();

        Value {
            value: unsafe {
                LLVMBuildLoad(self.builder, ptr.value, c_string)
            }
        }
    }

    fn build_add(&self, left_value: Value, right_value: Value, name: &str) -> Value {
        let c_string = CString::new(name).unwrap().as_ptr();

        Value {
            value: unsafe {
                LLVMBuildAdd(self.builder, left_value.value, right_value.value, c_string)
            }
        }
    }

    fn build_cast(&self, op: LLVMOpcode, from_value: Value, to_type: Type, name: &str) -> Value {
        let c_string = CString::new(name).unwrap().as_ptr();

        Value {
            value: unsafe {
                LLVMBuildCast(self.builder, op, from_value.value, to_type.type_, c_string)
            }
        }
    }

    fn build_int_comparison(&self, op: LLVMIntPredicate, left_val: Value, right_val: Value, name: &str) -> Value {
        let c_string = CString::new(name).unwrap().as_ptr();

        Value {
            value: unsafe {
                LLVMBuildICmp(self.builder, op, left_val.value, right_val.value, c_string)
            }
        }
    }

    fn build_conditional_branch(&self, comparison: Value, then_block: BasicBlock, else_block: BasicBlock) -> Value {
        Value {
            value: unsafe {
                LLVMBuildCondBr(self.builder, comparison.value, then_block.basic_block, else_block.basic_block)
            }
        }
    }

    fn position_at_end(&self, basic_block: BasicBlock) {
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, basic_block.basic_block);
        }
    }

    fn extract_value(&self, param: ParamValue, index: u32, name: &str) -> Value {
        let c_string = CString::new(name).unwrap().as_ptr();

        Value {
            value: unsafe {
                LLVMBuildExtractValue(self.builder, param.param_value, index, c_string)
            }
        }
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
        }
    }
}

pub struct Module {
    module: LLVMModuleRef,
}

impl Module {
    fn add_function(&self, name: &str, return_type: FunctionType) -> FunctionValue {
        let c_string = CString::new(name).unwrap().as_ptr();

        FunctionValue {
            function_value: unsafe {
                LLVMAddFunction(self.module, c_string, return_type.function_type)
            }
        }
    }

    pub fn get_named_function(&self, name: &str) -> Option<FunctionValue> {
        let c_string = CString::new(name).unwrap().as_ptr();

        let value = unsafe {
            LLVMGetNamedFunction(self.module, c_string)
        };

        if value.is_null() {
            return None;
        }

        Some(FunctionValue { function_value: value })
    }

    pub fn create_execution_engine(&self) -> ExecutionEngine { // Result?
        let mut execution_engine = unsafe { uninitialized() };
        let mut out = unsafe { zeroed() };

        // TODO: Check that these calls are succesful or even needed
        // LLVM_InitializeNativeTarget();
        // LLVM_InitializeNativeAsmPrinter();
        // LLVM_InitializeNativeAsmParser();
        // LLVMLinkInInterpreter();

        let code = unsafe {
            LLVMCreateExecutionEngineForModule(&mut execution_engine, self.module, &mut out) // Should take ownership of module
        };

        // TODO: Check code/out, difference

        ExecutionEngine {
            execution_engine: execution_engine
        }
    }

    fn create_fn_pass_manager(&self) -> PassManager {
        PassManager {
            pass_manager: unsafe {
                LLVMCreateFunctionPassManagerForModule(self.module)
            }
        }
    }

    pub fn dump(&self) {
        unsafe {
            LLVMDumpModule(self.module);
        }
    }
}

pub struct ExecutionEngine {
    execution_engine: LLVMExecutionEngineRef,
}

impl ExecutionEngine {
    fn get_target_data(&self) -> LLVMTargetDataRef {
        unsafe {
            LLVMGetExecutionEngineTargetData(self.execution_engine)
        }
    }

    pub fn run_function(&self, function: FunctionValue) {
        let num_args = 0; // TODO
        let args = 0; // FIXME

        unsafe {
            LLVMRunFunction(self.execution_engine, function.function_value, num_args, args);
        }
    }

    pub fn run_function_as_main(&self, function: FunctionValue) {
        let arg_c = 0; // TODO
        let arg_v = 0; // FIXME
        let env_p = 0; // FIXME

        unsafe {
            LLVMRunFunctionAsMain(self.execution_engine, function.function_value, arg_c, arg_v, env_p); // ???
        }
    }
}

pub struct PassManager {
    pass_manager: LLVMPassManagerRef,
}

impl PassManager {
    fn initialize(&self) { // TODO: Result
        let code = unsafe {
            LLVMInitializeFunctionPassManager(self.pass_manager)
        };
    }
}

impl Drop for PassManager {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposePassManager(self.pass_manager)
        }
    }
}

struct Type {
    type_: LLVMTypeRef,
}

impl Type {
    // REVIEW: Design decisions, should we create new type from scratch? (So original type is reusable resource)
    fn ptr_type(&self, address_space: u32) -> Type {
        Type {
            type_: unsafe {
                LLVMPointerType(self.type_, address_space)
            }
        }
    }

    fn fn_type(&self, mut param_types: Vec<Type>, is_var_args: bool) -> FunctionType {
        // WARNING: transmute will no longer work correctly if Type gains more fields
        // We're avoiding reallocation by telling rust Vec<Type> is identical to Vec<LLVMTypeRef>
        let mut param_types: Vec<LLVMTypeRef> = unsafe {
            transmute(param_types)
        };

        FunctionType {
            function_type: unsafe {
                LLVMFunctionType(self.type_, param_types.as_mut_ptr(), param_types.len() as u32, is_var_args as i32) // REVIEW: safe to cast usize to u32?
            }
        }
    }

    fn const_int(&self, value: u64) -> Value {
        // REVIEW: What if type is void??

        Value {
            value: unsafe {
                LLVMConstInt(self.type_, value, 0) // REVIEW: What does 0 do?
            }
        }
    }
}

struct FunctionValue {
    function_value: LLVMValueRef,
}

impl FunctionValue {
    fn get_first_param(&self) -> ParamValue { // Result?
        ParamValue {
            param_value: unsafe {
                LLVMGetFirstParam(self.function_value)
            }
        }
    }
}

struct FunctionType {
    function_type: LLVMTypeRef,
}

struct ParamValue {
    param_value: LLVMValueRef,
}

impl ParamValue {
    fn set_name(&self, name: &str) {
        let c_string = CString::new(name).unwrap().as_ptr();

        unsafe {
            LLVMSetValueName(self.param_value, c_string)
        }
    }
}

struct Value {
    value: LLVMValueRef,
}

impl Value {
    fn set_name(&mut self, name: &str) {
        let s_string = CString::new(name).unwrap().as_ptr();

        unsafe {
            LLVMSetValueName(self.value, s_string);
        }
    }

    fn count_params(&self) -> u32 {
        // WARNING: Should only be used on functions. Consider FunctionValue Type
        unsafe {
            LLVMCountParams(self.value)
        }
    }
}

impl Iterator for Value {
    type Item = Value;

    // WARNING: This must only be used on functions and is worth considering a FunctionValue Type
    fn next(&mut self) -> Option<Self::Item> {
        // LLVMGetFirstParam(self.value)

        let next_value = unsafe {
            LLVMGetNextParam(self.value)
        };

        if next_value.is_null() {
            return None;
        }

        Some(Value { value: next_value })
    }

}

// Case for separate Value structs:
// LLVMValueRef can be a value (ie int)
// LLVMValueRef can be a function
// LLVMValueRef can be a function param
// LLVMValueRef can be a comparison_op

struct BasicBlock {
    basic_block: LLVMBasicBlockRef,
}
