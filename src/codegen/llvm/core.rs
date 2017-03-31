extern crate llvm_sys;

use self::llvm_sys::analysis::{LLVMVerifyModule, LLVMVerifierFailureAction};
use self::llvm_sys::core::{LLVMContextCreate, LLVMCreateBuilderInContext, LLVMModuleCreateWithNameInContext, LLVMContextDispose, LLVMDisposeBuilder, LLVMVoidTypeInContext, LLVMDumpModule, LLVMInt1TypeInContext, LLVMInt8TypeInContext, LLVMInt16TypeInContext, LLVMInt32TypeInContext, LLVMInt64TypeInContext, LLVMBuildRet, LLVMBuildRetVoid, LLVMPositionBuilderAtEnd, LLVMBuildCall, LLVMBuildStore, LLVMPointerType, LLVMStructTypeInContext, LLVMAddFunction, LLVMFunctionType, LLVMSetValueName, LLVMCreatePassManager, LLVMBuildExtractValue, LLVMAppendBasicBlockInContext, LLVMBuildLoad, LLVMBuildGEP, LLVMBuildCondBr, LLVMBuildICmp, LLVMBuildCast, LLVMGetNamedFunction, LLVMBuildAdd, LLVMConstInt, LLVMGetFirstParam, LLVMGetNextParam, LLVMCountParams, LLVMDisposePassManager, LLVMCreateFunctionPassManagerForModule, LLVMInitializeFunctionPassManager, LLVMDisposeMessage, LLVMArrayType, LLVMGetReturnType, LLVMTypeOf, LLVMGetElementType, LLVMBuildNeg, LLVMBuildNot, LLVMGetInsertBlock, LLVMGetBasicBlockParent, LLVMConstReal, LLVMBuildBr, LLVMBuildPhi, LLVMAddIncoming, LLVMBuildAlloca, LLVMBuildMalloc, LLVMGetUndef, LLVMSetDataLayout, LLVMGetBasicBlockTerminator, LLVMInsertIntoBuilder};
use self::llvm_sys::execution_engine::{LLVMGetExecutionEngineTargetData, LLVMCreateExecutionEngineForModule, LLVMExecutionEngineRef, LLVMRunFunction, LLVMRunFunctionAsMain, LLVMDisposeExecutionEngine, LLVMLinkInInterpreter};
use self::llvm_sys::prelude::{LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMTypeRef, LLVMValueRef, LLVMBasicBlockRef, LLVMPassManagerRef};
use self::llvm_sys::target::{LLVMOpaqueTargetData, LLVMTargetDataRef, LLVM_InitializeNativeTarget, LLVM_InitializeNativeAsmPrinter, LLVM_InitializeNativeAsmParser, LLVMCopyStringRepOfTargetData, LLVMAddTargetData};
use self::llvm_sys::{LLVMOpcode, LLVMIntPredicate};

use std::ffi::{CString, CStr};
use std::iter;
use std::mem::{transmute, uninitialized, zeroed};
use std::os::raw::c_char;

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

    pub fn void_type(&self) -> Type {
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

    pub fn i32_type(&self) -> Type {
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

    // fn string_type(&self) -> Type {
    //     // TODO: Generic vec_type(i8)
    //     let field_types = vec![
    //         self.i8_type().ptr_type(0), // REVIEW: Existing impl might have had this as array type
    //         self.i64_type(),
    //     ];

    //     self.struct_type(field_types)
    // }

    pub fn append_basic_block(&self, function: &FunctionValue, name: &str) -> BasicBlock {
        let c_string = CString::new(name).unwrap().as_ptr();

        BasicBlock {
            basic_block: unsafe {
                LLVMAppendBasicBlockInContext(self.context, function.function_value, c_string)
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

    pub fn build_call(&self, function: FunctionValue, mut args: Vec<Value>, name: &str) -> Value {
        let c_string = CString::new(name).unwrap().as_ptr();

        // WARNING: transmute will no longer work correctly if Value gains more fields
        // We're avoiding reallocation by telling rust Vec<Value> is identical to Vec<LLVMValueRef>
        let mut args: Vec<LLVMValueRef> = unsafe {
            transmute(args)
        };

        Value {
            value: unsafe {
                LLVMBuildCall(self.builder, function.function_value, args.as_mut_ptr(), args.len() as u32, c_string)
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

    fn build_phi(&self, type_: Type, name: &str) -> Value {
        let c_string = CString::new(name).unwrap().as_ptr();

        Value {
            value: unsafe {
                LLVMBuildPhi(self.builder, type_.type_, c_string)
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

    fn build_stack_allocation(&self, type_: Type, name: &str) -> Value {
        let c_string = CString::new(name).unwrap().as_ptr();

        Value {
            value: unsafe {
                LLVMBuildAlloca(self.builder, type_.type_, c_string)
            }
        }
    }

    fn build_heap_allocation(&self, type_: Type, name: &str) -> Value {
        let c_string = CString::new(name).unwrap().as_ptr();

        Value {
            value: unsafe {
                LLVMBuildMalloc(self.builder, type_.type_, c_string)
            }
        }
    }

    pub fn insert_instruction(&self, value: Value) {
        unsafe {
            LLVMInsertIntoBuilder(self.builder, value.value);
        }
    }

    fn get_insert_block(&self) -> BasicBlock {
        BasicBlock {
            basic_block: unsafe {
                LLVMGetInsertBlock(self.builder)
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

    fn build_int_compare(&self, op: LLVMIntPredicate, left_val: Value, right_val: Value, name: &str) -> Value {
        let c_string = CString::new(name).unwrap().as_ptr();

        Value {
            value: unsafe {
                LLVMBuildICmp(self.builder, op, left_val.value, right_val.value, c_string)
            }
        }
    }

    fn build_unconditional_branch(&self, destination_block: BasicBlock) -> Value {
        Value {
            value: unsafe {
                LLVMBuildBr(self.builder, destination_block.basic_block)
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

    fn build_neg(&self, value: Value, name: &str) -> Value {
        let c_string = CString::new(name).unwrap().as_ptr();

        Value {
            value: unsafe {
                LLVMBuildNeg(self.builder, value.value, c_string)
            }
        }
    }

    fn build_not(&self, value: Value, name: &str) -> Value {
        let c_string = CString::new(name).unwrap().as_ptr();

        Value {
            value: unsafe {
                LLVMBuildNot(self.builder, value.value, c_string)
            }
        }
    }

    pub fn position_at_end(&self, basic_block: &BasicBlock) {
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, basic_block.basic_block);
        }
    }

    fn build_extract_value(&self, param: ParamValue, index: u32, name: &str) -> Value {
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
    pub fn add_function(&self, name: &str, return_type: FunctionType) -> FunctionValue {
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

    pub fn create_execution_engine(&self) -> Result<ExecutionEngine, String> {
        let mut execution_engine = unsafe { uninitialized() };
        let mut err_str = unsafe { zeroed() };

        // TODO: Check that these calls are succesful or even needed
        let code = unsafe {
            LLVM_InitializeNativeTarget()
        };

        if code == 1 {
            return Err("Unknown error in initializing native target".into());
        }

        let code = unsafe {
            LLVM_InitializeNativeAsmPrinter()
        };

        if code == 1 {
            return Err("Unknown error in initializing native asm printer".into());
        }

        let code = unsafe {
            LLVM_InitializeNativeAsmParser()
        };

        if code == 1 { // REVIEW: Does parser need to go before printer?
            return Err("Unknown error in initializing native asm parser".into());
        }

        unsafe {
            LLVMLinkInInterpreter();
        }

        let code = unsafe {
            LLVMCreateExecutionEngineForModule(&mut execution_engine, self.module, &mut err_str) // Should take ownership of module
        };

        if code == 1 {
            let rust_str = unsafe {
                let rust_str = CStr::from_ptr(err_str).to_string_lossy().into_owned();

                LLVMDisposeMessage(err_str);

                rust_str
            };

            return Err(rust_str);
        }

        let ee = ExecutionEngine {
            execution_engine: execution_engine
        };

        Ok(ee)
    }

    pub fn create_function_pass_manager(&self) -> PassManager {
        PassManager {
            pass_manager: unsafe {
                LLVMCreateFunctionPassManagerForModule(self.module)
            }
        }
    }

    pub fn verify(&self, print: bool) -> bool {
        let mut err_str: *mut *mut i8 = unsafe { zeroed() };

        let action = if print == true {
            LLVMVerifierFailureAction::LLVMPrintMessageAction
        } else {
            LLVMVerifierFailureAction::LLVMReturnStatusAction
        };


        let code = unsafe {
            LLVMVerifyModule(self.module, action, err_str)
        };

        if code == 1 {
            unsafe {
                if print {
                    let rust_str = CStr::from_ptr(*err_str).to_str().unwrap();

                    println!("{}", rust_str); // FIXME: Should probably be stderr?
                }

                LLVMDisposeMessage(*err_str);
            }
        }

        code == 0
    }

    pub fn set_data_layout(&self, data_layout: DataLayout) {
        unsafe {
            LLVMSetDataLayout(self.module, data_layout.data_layout)
        }
    }

    pub fn dump(&self) {
        unsafe {
            LLVMDumpModule(self.module);
        }
    }
}

// REVIEW: Drop for Module? There's a LLVM method, but I read context dispose takes care of it...

pub struct ExecutionEngine {
    execution_engine: LLVMExecutionEngineRef,
}

impl ExecutionEngine {
    pub fn get_target_data(&self) -> TargetData {
        TargetData {
            target_data: unsafe {
                LLVMGetExecutionEngineTargetData(self.execution_engine)
            }
        }
    }

    pub fn run_function(&self, function: FunctionValue) {
        let mut args = vec![]; // TODO: Support args

        unsafe {
            LLVMRunFunction(self.execution_engine, function.function_value, args.len() as u32, args.as_mut_ptr()); // REVIEW: usize to u32 ok??
        }
    }

    pub fn run_function_as_main(&self, function: FunctionValue) {
        let args = vec![]; // TODO: Support argc, argv
        let env_p = vec![]; // REVIEW: No clue what this is

        unsafe {
            LLVMRunFunctionAsMain(self.execution_engine, function.function_value, args.len() as u32, args.as_ptr(), env_p.as_ptr()); // REVIEW: usize to u32 cast ok??
        }
    }
}

impl Drop for ExecutionEngine {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeExecutionEngine(self.execution_engine);
        }
    }
}

pub struct TargetData {
    target_data: LLVMTargetDataRef,
}

impl TargetData {
    pub fn get_data_layout(&self) -> DataLayout {
        DataLayout {
            data_layout: unsafe {
                LLVMCopyStringRepOfTargetData(self.target_data)
            }
        }
    }
}

pub struct DataLayout {
    data_layout: *mut c_char,
}

impl Drop for DataLayout {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeMessage(self.data_layout)
        }
    }
}

pub struct PassManager {
    pass_manager: LLVMPassManagerRef,
}

impl PassManager {
    pub fn initialize(&self) -> bool {
        // return true means some pass modified the module, not an error occurred
        unsafe {
            LLVMInitializeFunctionPassManager(self.pass_manager) == 1
        }
    }

    pub fn add_target_data(&self, target_data: TargetData) {
        unsafe {
            LLVMAddTargetData(target_data.target_data, self.pass_manager)
        }
    }
}

impl Drop for PassManager {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposePassManager(self.pass_manager)
        }
    }
}

pub struct Type {
    type_: LLVMTypeRef,
}

impl Type {
    fn ptr_type(&self, address_space: u32) -> Type {
        Type {
            type_: unsafe {
                LLVMPointerType(self.type_, address_space)
            }
        }
    }

    pub fn fn_type(&self, mut param_types: Vec<Type>, is_var_args: bool) -> FunctionType {
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

    fn array_type(&self, value: u32) -> Type {
        Type {
            type_: unsafe {
                LLVMArrayType(self.type_, value)
            }
        }
    }

    pub fn const_int(&self, value: u64, sign_extend: bool) -> Value {
        // REVIEW: What if type is void??

        Value {
            value: unsafe {
                LLVMConstInt(self.type_, value, sign_extend as i32)
            }
        }
    }

    fn const_real(&self, value: f64) -> Value {
        // REVIEW: What if type is void??

        Value {
            value: unsafe {
                LLVMConstReal(self.type_, value)
            }
        }
    }

    fn get_undef(&self, type_: Type) -> Value {
        Value {
            value: unsafe {
                LLVMGetUndef(self.type_)
            }
        }
    }
}

pub struct FunctionValue {
    pub function_value: LLVMValueRef, // TEMP: pub
}

impl FunctionValue {
    fn get_first_param(&self) -> ParamValue { // Result?
        ParamValue {
            param_value: unsafe {
                LLVMGetFirstParam(self.function_value)
            }
        }
    }

    pub fn count_params(&self) -> u32 {
        unsafe {
            LLVMCountParams(self.function_value)
        }
    }

    pub fn get_return_type(&self) -> Type {
        Type {
            type_: unsafe {
                LLVMGetReturnType(LLVMGetElementType(LLVMTypeOf(self.function_value)))
            }
        }
    }
}

impl Iterator for FunctionValue {
    type Item = ParamValue;

    fn next(&mut self) -> Option<Self::Item> {
        // LLVMGetFirstParam(self.value)

        let next_value = unsafe {
            LLVMGetNextParam(self.function_value)
        };

        if next_value.is_null() {
            return None;
        }

        Some(ParamValue { param_value: next_value })
    }

}

pub struct FunctionType {
    function_type: LLVMTypeRef,
}

pub struct ParamValue {
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

pub struct Value {
    pub value: LLVMValueRef, // TEMP: pub
}

impl Value {
    fn set_name(&mut self, name: &str) {
        let s_string = CString::new(name).unwrap().as_ptr();

        unsafe {
            LLVMSetValueName(self.value, s_string);
        }
    }

    fn add_incoming(&self, mut incoming_values: Value, mut incoming_basic_block: BasicBlock, count: u32) { // PhiValue (self) only?
        unsafe {
            LLVMAddIncoming(self.value, &mut incoming_values.value, &mut incoming_basic_block.basic_block, count);
        }
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

impl BasicBlock {
    fn get_parent(&self) -> Value { // REVIEW: Why does LLVM return a value instead of a bb??
        Value {
            value: unsafe {
                LLVMGetBasicBlockParent(self.basic_block)
            }
        }
    }

    pub fn get_terminator(&self) -> Value {
        Value {
            value: unsafe {
                LLVMGetBasicBlockTerminator(self.basic_block)
            }
        }
    }
}
