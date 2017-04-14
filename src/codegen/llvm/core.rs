extern crate llvm_sys;

use self::llvm_sys::analysis::{LLVMVerifyModule, LLVMVerifierFailureAction, LLVMVerifyFunction};
use self::llvm_sys::core::{LLVMContextCreate, LLVMCreateBuilderInContext, LLVMModuleCreateWithNameInContext, LLVMContextDispose, LLVMDisposeBuilder, LLVMVoidTypeInContext, LLVMDumpModule, LLVMInt1TypeInContext, LLVMInt8TypeInContext, LLVMInt16TypeInContext, LLVMInt32Type, LLVMInt32TypeInContext, LLVMInt64TypeInContext, LLVMBuildRet, LLVMBuildRetVoid, LLVMPositionBuilderAtEnd, LLVMBuildCall, LLVMBuildStore, LLVMPointerType, LLVMStructTypeInContext, LLVMAddFunction, LLVMFunctionType, LLVMSetValueName, LLVMGetValueName, LLVMCreatePassManager, LLVMBuildExtractValue, LLVMAppendBasicBlockInContext, LLVMBuildLoad, LLVMBuildGEP, LLVMBuildCondBr, LLVMBuildICmp, LLVMBuildCast, LLVMGetNamedFunction, LLVMBuildAdd, LLVMConstInt, LLVMGetFirstParam, LLVMGetNextParam, LLVMCountParams, LLVMDisposePassManager, LLVMCreateFunctionPassManagerForModule, LLVMInitializeFunctionPassManager, LLVMDisposeMessage, LLVMArrayType, LLVMGetReturnType, LLVMTypeOf, LLVMGetElementType, LLVMBuildNeg, LLVMBuildNot, LLVMGetInsertBlock, LLVMGetBasicBlockParent, LLVMConstReal, LLVMConstArray, LLVMBuildBr, LLVMBuildPhi, LLVMAddIncoming, LLVMBuildAlloca, LLVMBuildMalloc, LLVMBuildArrayMalloc, LLVMBuildArrayAlloca, LLVMGetUndef, LLVMSetDataLayout, LLVMGetBasicBlockTerminator, LLVMInsertIntoBuilder, LLVMIsABasicBlock, LLVMIsAFunction, LLVMIsFunctionVarArg, LLVMDumpType, LLVMPrintValueToString, LLVMPrintTypeToString, LLVMInsertBasicBlock, LLVMInsertBasicBlockInContext, LLVMGetParam, LLVMGetTypeKind, LLVMIsConstant, LLVMVoidType, LLVMSetLinkage, LLVMBuildInsertValue, LLVMIsNull, LLVMBuildIsNull};
use self::llvm_sys::execution_engine::{LLVMGetExecutionEngineTargetData, LLVMCreateExecutionEngineForModule, LLVMExecutionEngineRef, LLVMRunFunction, LLVMRunFunctionAsMain, LLVMDisposeExecutionEngine, LLVMLinkInInterpreter, LLVMGetFunctionAddress};
use self::llvm_sys::LLVMLinkage::LLVMCommonLinkage;
use self::llvm_sys::prelude::{LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMTypeRef, LLVMValueRef, LLVMBasicBlockRef, LLVMPassManagerRef};
use self::llvm_sys::target::{LLVMOpaqueTargetData, LLVMTargetDataRef, LLVM_InitializeNativeTarget, LLVM_InitializeNativeAsmPrinter, LLVM_InitializeNativeAsmParser, LLVMCopyStringRepOfTargetData, LLVMAddTargetData};
use self::llvm_sys::{LLVMOpcode, LLVMIntPredicate, LLVMTypeKind};

use std::ffi::{CString, CStr};
use std::fmt;
use std::iter;
use std::mem::{transmute, uninitialized, zeroed};
use std::os::raw::c_char;

// Misc Notes
// Always pass a c_string.as_ptr() call into the function call directly and never
// before hand. Seems to make a huge difference (stuff stops working) otherwise

pub struct Context {
    context: LLVMContextRef,
}

impl Context {
    pub fn new() -> Self {
        let context = unsafe {
            LLVMContextCreate()
        };

        assert!(!context.is_null()); // TODO: All of these on debug only

        Context {
            context: context
        }
    }

    pub fn create_builder(&self) -> Builder {
        let builder = unsafe {
            LLVMCreateBuilderInContext(self.context)
        };

        assert!(!builder.is_null());

        Builder {
            builder: builder
        }
    }

    pub fn create_module(&self, name: &str) -> Module {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let module = unsafe {
            LLVMModuleCreateWithNameInContext(c_string.as_ptr(), self.context)
        };

        assert!(!module.is_null());

        Module {
            module: module
        }
    }

    pub fn void_type(&self) -> Type {
        let void_type = unsafe {
            LLVMVoidTypeInContext(self.context)
        };

        Type::new(void_type)
    }

    fn bool_type(&self) -> Type {
        let bool_type = unsafe {
            LLVMInt1TypeInContext(self.context)
        };

        Type::new(bool_type)
    }

    pub fn i8_type(&self) -> Type {
        let i8_type = unsafe {
            LLVMInt8TypeInContext(self.context)
        };

        Type::new(i8_type)
    }

    fn i16_type(&self) -> Type {
        let i16_type = unsafe {
            LLVMInt16TypeInContext(self.context)
        };

        Type::new(i16_type)
    }

    pub fn i32_type(&self) -> Type {
        let i32_type = unsafe {
            LLVMInt32TypeInContext(self.context)
        };

        Type::new(i32_type)
    }

    pub fn i64_type(&self) -> Type {
        let i64_type = unsafe {
            LLVMInt64TypeInContext(self.context)
        };

        Type::new(i64_type)
    }

    pub fn struct_type(&self, field_types: Vec<Type>) -> Type {
        // WARNING: transmute will no longer work correctly if Type gains more fields
        // We're avoiding reallocation by telling rust Vec<Type> is identical to Vec<LLVMTypeRef>
        let mut field_types: Vec<LLVMTypeRef> = unsafe {
            transmute(field_types)
        };

        let struct_type = unsafe {
            LLVMStructTypeInContext(self.context, field_types.as_mut_ptr(), field_types.len() as u32, 0) // REVIEW: 0 is what? Safe to cast usize as u32?
        };

        Type::new(struct_type)
    }

    pub fn append_basic_block(&self, function: &FunctionValue, name: &str) -> BasicBlock {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let bb = unsafe {
            LLVMAppendBasicBlockInContext(self.context, function.fn_value, c_string.as_ptr())
        };

        BasicBlock::new(bb)
    }

    fn prepend_basic_block(&self, basic_block: &BasicBlock, name: &str) -> BasicBlock {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let bb = unsafe {
            LLVMInsertBasicBlockInContext(self.context, basic_block.basic_block, c_string.as_ptr())
        };

        BasicBlock::new(bb)
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
    pub fn build_return(&self, value: Option<Value>) -> Value {
        let value = unsafe {
            value.map_or(LLVMBuildRetVoid(self.builder), |value| LLVMBuildRet(self.builder, value.value))
        };

        Value::new(value)
    }

    pub fn build_call(&self, function: &FunctionValue, args: Vec<Value>, name: &str) -> Value {
        // LLVM gets upset when void calls are named because they don't return anything
        let name = unsafe {
            match LLVMGetTypeKind(LLVMGetReturnType(LLVMGetElementType(LLVMTypeOf(function.fn_value)))) {
                LLVMTypeKind::LLVMVoidTypeKind => "",
                _ => name,
            }
        };

        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        // WARNING: transmute will no longer work correctly if Value gains more fields
        // We're avoiding reallocation by telling rust Vec<Value> is identical to Vec<LLVMValueRef>
        let mut args: Vec<LLVMValueRef> = unsafe {
            transmute(args)
        };

        let value = unsafe {
            LLVMBuildCall(self.builder, function.fn_value, args.as_mut_ptr(), args.len() as u32, c_string.as_ptr())
        };

        Value::new(value)
    }

    pub fn build_gep<V: Into<Value> + Copy>(&self, ptr: &Value, ordered_indexes: &Vec<V>, name: &str) -> Value {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let mut index_values: Vec<Value> = Vec::with_capacity(ordered_indexes.len());

        // TODO: Assert vec values are all i32 => Result?
        for int_or_value in ordered_indexes {
            // REVIEW: Had to make Value Copy + Clone to get this to work...
            // Is this safe, given Value is a raw ptr wrapper?
            // I suppose in theory LLVM should never delete the values in the scope of this call, but still
            index_values.push(match int_or_value {
                &Value => (*int_or_value).into(),
                &int => int.into(),
            });
        }

        // WARNING: transmute will no longer work correctly if Value gains more fields
        // We're avoiding reallocation by telling rust Vec<Value> is identical to Vec<LLVMValueRef>
        let mut index_values: Vec<LLVMValueRef> = unsafe {
            transmute(index_values)
        };

        let value = unsafe {
            LLVMBuildGEP(self.builder, ptr.value, index_values.as_mut_ptr(), index_values.len() as u32, c_string.as_ptr())
        };

        Value::new(value)
    }

    fn build_phi(&self, type_: Type, name: &str) -> Value {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let value = unsafe {
            LLVMBuildPhi(self.builder, type_.type_, c_string.as_ptr())
        };

        Value::new(value)
    }

    pub fn build_store(&self, value: &Value, pointer: &Value) -> Value {
        let value = unsafe {
            LLVMBuildStore(self.builder, value.value, pointer.value)
        };

        Value::new(value)
    }

    pub fn build_load(&self, ptr: &Value, name: &str) -> Value {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let value = unsafe {
            LLVMBuildLoad(self.builder, ptr.value, c_string.as_ptr())
        };

        Value::new(value)
    }

    pub fn build_stack_allocation(&self, type_: &Type, name: &str) -> Value {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let value = unsafe {
            LLVMBuildAlloca(self.builder, type_.type_, c_string.as_ptr())
        };

        Value::new(value)
    }

    pub fn build_heap_allocation(&self, type_: &Type, name: &str) -> Value {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let value = unsafe {
            LLVMBuildMalloc(self.builder, type_.type_, c_string.as_ptr())
        };

        Value::new(value)
    }

    pub fn build_array_heap_allocation(&self, type_: &Type, value: &Value, name: &str) -> Value {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let value = unsafe {
            LLVMBuildArrayMalloc(self.builder, type_.type_, value.value, c_string.as_ptr())
        };

        Value::new(value)
    }

    pub fn insert_instruction(&self, value: Value) {
        unsafe {
            LLVMInsertIntoBuilder(self.builder, value.value);
        }
    }

    fn get_insert_block(&self) -> BasicBlock {
        let bb = unsafe {
            LLVMGetInsertBlock(self.builder)
        };

        BasicBlock::new(bb)
    }

    pub fn build_add(&self, left_value: &Value, right_value: &Value, name: &str) -> Value {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let value = unsafe {
            LLVMBuildAdd(self.builder, left_value.value, right_value.value, c_string.as_ptr())
        };

        Value::new(value)
    }

    pub fn build_cast(&self, op: LLVMOpcode, from_value: &Value, to_type: &Type, name: &str) -> Value {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let value = unsafe {
            LLVMBuildCast(self.builder, op, from_value.value, to_type.type_, c_string.as_ptr())
        };

        Value::new(value)
    }

    pub fn build_int_compare(&self, op: LLVMIntPredicate, left_val: &Value, right_val: &Value, name: &str) -> Value {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let value = unsafe {
            LLVMBuildICmp(self.builder, op, (*left_val).value, (*right_val).value, c_string.as_ptr())
        };

        Value::new(value)
    }

    fn build_unconditional_branch(&self, destination_block: BasicBlock) -> Value {
        let value = unsafe {
            LLVMBuildBr(self.builder, destination_block.basic_block)
        };

        Value::new(value)
    }

    pub fn build_conditional_branch(&self, comparison: &Value, then_block: &BasicBlock, else_block: &BasicBlock) -> Value {
        let value = unsafe {
            LLVMBuildCondBr(self.builder, comparison.value, then_block.basic_block, else_block.basic_block)
        };

        Value::new(value)
    }

    fn build_neg(&self, value: Value, name: &str) -> Value {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let value = unsafe {
            LLVMBuildNeg(self.builder, value.value, c_string.as_ptr())
        };

        Value::new(value)
    }

    fn build_not(&self, value: Value, name: &str) -> Value {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let value = unsafe {
            LLVMBuildNot(self.builder, value.value, c_string.as_ptr())
        };

        Value::new(value)
    }

    pub fn position_at_end(&self, basic_block: &BasicBlock) {
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, basic_block.basic_block);
        }
    }

    pub fn build_extract_value<V: AsRef<LLVMValueRef>>(&self, value: &V, index: u32, name: &str) -> Value {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let value = unsafe {
            LLVMBuildExtractValue(self.builder, *value.as_ref(), index, c_string.as_ptr())
        };

        Value::new(value)
    }

    pub fn build_insert_value(&self, value: &Value, ptr: &Value, index: u32, name: &str) -> Value {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let value = unsafe {
            LLVMBuildInsertValue(self.builder, value.value, ptr.value, index, c_string.as_ptr())
        };

        Value::new(value)
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
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let value = unsafe {
            LLVMAddFunction(self.module, c_string.as_ptr(), return_type.fn_type)
        };

        // unsafe {
        //     LLVMSetLinkage(value, LLVMCommonLinkage);
        // }

        FunctionValue::new(value)
    }

    pub fn get_function(&self, name: &str) -> Option<FunctionValue> {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let value = unsafe {
            LLVMGetNamedFunction(self.module, c_string.as_ptr())
        };

        if value.is_null() {
            return None;
        }

        Some(FunctionValue::new(value))
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
        let pass_manager = unsafe {
            LLVMCreateFunctionPassManagerForModule(self.module)
        };

        PassManager::new(pass_manager)
    }

    pub fn verify(&self, print: bool) -> bool {
        let err_str: *mut *mut i8 = unsafe { zeroed() };

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
    fn get_function_address(&self, name: &str) -> Option<u64> {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        let value = unsafe {
            LLVMGetFunctionAddress(self.execution_engine, c_string.as_ptr())
        };

        if value == 0 { // REVIEW: Need to test if 0 is actually returned
            return None;
        }

        Some(value)
    }

    pub fn get_target_data(&self) -> TargetData {
        let target_data = unsafe {
            LLVMGetExecutionEngineTargetData(self.execution_engine)
        };

        TargetData::new(target_data)
    }

    pub fn run_function(&self, function: FunctionValue) {
        let mut args = vec![]; // TODO: Support args

        unsafe {
            LLVMRunFunction(self.execution_engine, function.fn_value, args.len() as u32, args.as_mut_ptr()); // REVIEW: usize to u32 ok??
        }
    }

    pub fn run_function_as_main(&self, function: FunctionValue) {
        let args = vec![]; // TODO: Support argc, argv
        let env_p = vec![]; // REVIEW: No clue what this is

        unsafe {
            LLVMRunFunctionAsMain(self.execution_engine, function.fn_value, args.len() as u32, args.as_ptr(), env_p.as_ptr()); // REVIEW: usize to u32 cast ok??
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
    fn new(target_data: LLVMTargetDataRef) -> TargetData {
        assert!(!target_data.is_null());

        TargetData {
            target_data: target_data
        }
    }

    pub fn get_data_layout(&self) -> DataLayout {
        let data_layout = unsafe {
            LLVMCopyStringRepOfTargetData(self.target_data)
        };

        DataLayout::new(data_layout)
    }
}

pub struct DataLayout {
    data_layout: *mut c_char,
}

impl DataLayout {
    fn new(data_layout: *mut c_char) -> DataLayout {
        assert!(!data_layout.is_null());

        DataLayout {
            data_layout: data_layout
        }
    }
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
    fn new(pass_manager: LLVMPassManagerRef) -> PassManager {
        assert!(!pass_manager.is_null());

        PassManager {
            pass_manager: pass_manager
        }
    }

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
    fn new(type_: LLVMTypeRef) -> Type {
        assert!(!type_.is_null());

        Type {
            type_: type_
        }
    }

    pub fn dump_type(&self) {
        println!("DEBUG: ");

        unsafe {
            LLVMDumpType(self.type_);
        }
    }

    pub fn ptr_type(&self, address_space: u32) -> Type {
        let type_ = unsafe {
            LLVMPointerType(self.type_, address_space)
        };

        Type::new(type_)
    }

    pub fn fn_type(&self, param_types: &mut Vec<Type>, is_var_args: bool) -> FunctionType {
        // WARNING: transmute will no longer work correctly if Type gains more fields
        // We're avoiding reallocation by telling rust Vec<Type> is identical to Vec<LLVMTypeRef>
        let mut param_types: &mut Vec<LLVMTypeRef> = unsafe {
            transmute(param_types)
        };

        let fn_type = unsafe {
            LLVMFunctionType(self.type_, param_types.as_mut_ptr(), param_types.len() as u32, is_var_args as i32) // REVIEW: safe to cast usize to u32?
        };

        FunctionType::new(fn_type)
    }

    pub fn array_type(&self, size: u32) -> Type {
        let type_ = unsafe {
            LLVMArrayType(self.type_, size)
        };

        Type::new(type_)
    }

    pub fn const_int(&self, value: u64, sign_extend: bool) -> Value {
        // REVIEW: What if type is void??

        let value = unsafe {
            LLVMConstInt(self.type_, value, sign_extend as i32)
        };

        Value::new(value)
    }

    fn const_real(&self, value: f64) -> Value {
        // REVIEW: What if type is void??

        let value = unsafe {
            LLVMConstReal(self.type_, value)
        };

        Value::new(value)
    }

    pub fn const_array(&self, values: Vec<Value>) -> Value {
        // WARNING: transmute will no longer work correctly if Type gains more fields
        // We're avoiding reallocation by telling rust Vec<Type> is identical to Vec<LLVMTypeRef>
        let mut values: Vec<LLVMValueRef> = unsafe {
            transmute(values)
        };

        let value = unsafe {
            LLVMConstArray(self.type_, values.as_mut_ptr(), values.len() as u32)
        };

        Value::new(value)
    }

    fn get_undef(&self, type_: Type) -> Value {
        let value = unsafe {
            LLVMGetUndef(self.type_)
        };

        Value::new(value)
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let llvm_type = unsafe {
            CStr::from_ptr(LLVMPrintTypeToString(self.type_))
        };
        write!(f, "Type {{\n    address: {:?}\n    llvm_type: {:?}\n}}", self.type_, llvm_type)
    }
}

pub struct FunctionValue {
    pub fn_value: LLVMValueRef, // TEMP: pub
}

impl FunctionValue {
    fn new(value: LLVMValueRef) -> FunctionValue {
        // TODO: Debug mode only assertions:
        {
            assert!(!value.is_null());

            unsafe {
                assert!(!LLVMIsAFunction(value).is_null())
            }
        }


        FunctionValue {
            fn_value: value
        }
    }

    pub fn verify(&self, print: bool) {
        let err_str: *mut *mut i8 = unsafe { zeroed() };

        let action = if print == true {
            LLVMVerifierFailureAction::LLVMPrintMessageAction
        } else {
            LLVMVerifierFailureAction::LLVMReturnStatusAction
        };

        let code = unsafe {
            LLVMVerifyFunction(self.fn_value, action)
        };

        if code == 1 {
            panic!("LLVMGenError")
        }
    }

    pub fn get_first_param(&self) -> ParamValue { // REVIEW: Option?
        let param = unsafe {
            LLVMGetFirstParam(self.fn_value)
        };

        ParamValue::new(param)
    }

    fn get_nth_param(&self, nth: u32) -> ParamValue { // REVIEW: Option?
        let param = unsafe {
            LLVMGetParam(self.fn_value, nth)
        };

        ParamValue::new(param)
    }

    pub fn count_params(&self) -> u32 {
        unsafe {
            LLVMCountParams(self.fn_value)
        }
    }

    pub fn get_return_type(&self) -> Type {
        let type_ = unsafe {
            LLVMGetReturnType(LLVMGetElementType(LLVMTypeOf(self.fn_value)))
        };

        Type::new(type_)
    }
}

impl fmt::Debug for FunctionValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let llvm_value = unsafe {
            CStr::from_ptr(LLVMPrintValueToString(self.fn_value))
        };
        let llvm_type = unsafe {
            CStr::from_ptr(LLVMPrintTypeToString(LLVMTypeOf(self.fn_value)))
        };
        let name = unsafe {
            CStr::from_ptr(LLVMGetValueName(self.fn_value))
        };
        let is_const = unsafe {
            LLVMIsConstant(self.fn_value) == 1
        };
        let is_null = unsafe {
            LLVMIsNull(self.fn_value) == 1
        };

        write!(f, "FunctionValue {{\n    name: {:?}\n    address: {:?}\n    is_const: {:?}\n    is_null: {:?}\n    llvm_value: {:?}\n    llvm_type: {:?}\n}}", name, self.fn_value, is_const, is_null, llvm_value, llvm_type)
    }
}

impl Iterator for FunctionValue {
    type Item = ParamValue;

    fn next(&mut self) -> Option<Self::Item> {
        // LLVMGetFirstParam(self.value)

        let next_value = unsafe {
            LLVMGetNextParam(self.fn_value)
        };

        if next_value.is_null() {
            return None;
        }

        Some(ParamValue { param_value: next_value })
    }
}

impl AsRef<LLVMValueRef> for FunctionValue {
    fn as_ref(&self) -> &LLVMValueRef {
        &self.fn_value
    }
}

pub struct FunctionType {
    fn_type: LLVMTypeRef,
}

impl FunctionType {
    fn new(fn_type: LLVMTypeRef) -> FunctionType {
        assert!(!fn_type.is_null());

        FunctionType {
            fn_type: fn_type
        }
    }

    fn is_var_arg(&self) -> bool {
        unsafe {
            LLVMIsFunctionVarArg(self.fn_type) == 1
        }
    }
}

impl fmt::Debug for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let llvm_type = unsafe {
            CStr::from_ptr(LLVMPrintTypeToString(self.fn_type))
        };

        write!(f, "FunctionType {{\n    address: {:?}\n    llvm_type: {:?}\n}}", self.fn_type, llvm_type)
    }
}

pub struct ParamValue {
    param_value: LLVMValueRef,
}

impl ParamValue {
    pub fn new(param_value: LLVMValueRef) -> ParamValue { // FIXME: should not be pub
        assert!(!param_value.is_null());

        ParamValue {
            param_value: param_value
        }
    }

    pub fn set_name(&mut self, name: &str) {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        unsafe {
            LLVMSetValueName(self.param_value, c_string.as_ptr())
        }
    }
}

impl AsRef<LLVMValueRef> for ParamValue {
    fn as_ref(&self) -> &LLVMValueRef {
        &self.param_value
    }
}

impl fmt::Debug for ParamValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let llvm_value = unsafe {
            CStr::from_ptr(LLVMPrintValueToString(self.param_value))
        };
        let llvm_type = unsafe {
            CStr::from_ptr(LLVMPrintTypeToString(LLVMTypeOf(self.param_value)))
        };
        let name = unsafe {
            CStr::from_ptr(LLVMGetValueName(self.param_value))
        };
        let is_const = unsafe {
            LLVMIsConstant(self.param_value) == 1
        };
        let is_null = unsafe {
            LLVMIsNull(self.param_value) == 1
        };

        write!(f, "FunctionValue {{\n    name: {:?}\n    address: {:?}\n    is_const: {:?}\n    is_null: {:?}\n    llvm_value: {:?}\n    llvm_type: {:?}\n}}", name, self.param_value, is_const, is_null, llvm_value, llvm_type)
    }
}

#[derive(Clone, Copy)]
pub struct Value {
    pub value: LLVMValueRef, // TEMP: pub
}

impl Value {
    fn new(value: LLVMValueRef) -> Value {
        assert!(!value.is_null());

        Value {
            value: value
        }
    }

    fn set_name(&mut self, name: &str) {
        let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

        unsafe {
            LLVMSetValueName(self.value, c_string.as_ptr());
        }
    }

    fn add_incoming(&self, mut incoming_values: Value, mut incoming_basic_block: BasicBlock, count: u32) { // PhiValue (self) only?
        unsafe {
            LLVMAddIncoming(self.value, &mut incoming_values.value, &mut incoming_basic_block.basic_block, count);
        }
    }
}

impl From<u64> for Value {
    fn from(int: u64) -> Value {
        unsafe {
            Type::new(LLVMInt32Type()).const_int(int, false)
        }
    }
}

impl AsRef<LLVMValueRef> for Value {
    fn as_ref(&self) -> &LLVMValueRef {
        &self.value
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let llvm_value = unsafe {
            CStr::from_ptr(LLVMPrintValueToString(self.value))
        };
        let llvm_type = unsafe {
            CStr::from_ptr(LLVMPrintTypeToString(LLVMTypeOf(self.value)))
        };
        let name = unsafe {
            CStr::from_ptr(LLVMGetValueName(self.value))
        };
        let is_const = unsafe {
            LLVMIsConstant(self.value) == 1
        };
        let is_null = unsafe {
            LLVMIsNull(self.value) == 1
        };

        write!(f, "Value {{\n    name: {:?}\n    address: {:?}\n    is_const: {:?}\n    is_null: {:?}\n    llvm_value: {:?}\n    llvm_type: {:?}\n}}", name, self.value, is_const, is_null, llvm_value, llvm_type)
    }
}

// Case for separate Value structs:
// LLVMValueRef can be a value (ie int)
// LLVMValueRef can be a function
// LLVMValueRef can be a function param
// LLVMValueRef can be a comparison_op

pub struct BasicBlock {
    basic_block: LLVMBasicBlockRef,
}

impl BasicBlock {
    fn new(basic_block: LLVMBasicBlockRef) -> BasicBlock {
        // TODO: debug mode only assertions
        {
            assert!(!basic_block.is_null());

            unsafe {
                assert!(!LLVMIsABasicBlock(basic_block as LLVMValueRef).is_null()) // NOTE: There is a LLVMBasicBlockAsValue but it might be the same as casting
            }
        }

        BasicBlock {
            basic_block: basic_block
        }
    }

    fn get_parent(&self) -> Value { // REVIEW: Why does LLVM return a value instead of a bb??
        let value = unsafe {
            LLVMGetBasicBlockParent(self.basic_block)
        };

        Value::new(value)
    }

    pub fn get_terminator(&self) -> Option<Value> {
        let value = unsafe {
            LLVMGetBasicBlockTerminator(self.basic_block)
        };

        if value.is_null() {
            return None;
        }

        Some(Value::new(value))
    }

    // fn prepend_basic_block(&self, name: &str) -> BasicBlock {
    //     let c_string = CString::new(name).expect("Conversion to CString failed unexpectedly");

    //     let bb = unsafe {
    //         LLVMInsertBasicBlock(self.basic_block, c_string.as_ptr())
    //     };

    //     BasicBlock::new(bb)
    // }
}

impl fmt::Debug for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let llvm_value = unsafe {
            CStr::from_ptr(LLVMPrintValueToString(self.basic_block as LLVMValueRef))
        };
        let llvm_type = unsafe {
            CStr::from_ptr(LLVMPrintTypeToString(LLVMTypeOf(self.basic_block as LLVMValueRef)))
        };
        let is_const = unsafe {
            LLVMIsConstant(self.basic_block as LLVMValueRef) == 1
        };

        write!(f, "BasicBlock {{\n    address: {:?}\n    is_const: {:?}\n    llvm_value: {:?}\n    llvm_type: {:?}\n}}", self.basic_block, is_const, llvm_value, llvm_type)
    }
}
