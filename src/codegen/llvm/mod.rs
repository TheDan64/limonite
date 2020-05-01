mod builtins;
mod std;

use crate::codegen::llvm::builtins::PutcharBuiltin;
use crate::codegen::llvm::std::string::{PrintString, LimeString};
use crate::interner::StrId;
use crate::span::{Span, Spanned};
use crate::syntax::{Block, ExprKind, ItemKind, Literal, Stmt};
use crate::syntax::items::FnSig;
use crate::syntax::visitor::{AstVisitor, Visitor, VisitOutcome};

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::types::{AnyType, AnyTypeEnum, FunctionType, StructType};
use inkwell::values::{AnyValueEnum, BasicValueEnum, FunctionValue};
use rustc_hash::FxHashMap;

use ::std::convert::TryFrom;

trait Type<'ctx, B: AnyType<'ctx>> {
    const FULL_PATH: &'static str;

    // TODO: Remove module param in LLVM 11
    fn build_ty(ctx: &'ctx Context, module: &Module<'ctx>) -> B;
}

trait FnDecl<'ctx>: Type<'ctx, FunctionType<'ctx>> {
    // TODO: We might want linkage as a build_decl param eventually
    fn build_decl(fn_ty: FunctionType<'ctx>, module: &Module<'ctx>) -> FunctionValue<'ctx> {
        module.add_function(Self::FULL_PATH, fn_ty, None)
    }
}

// IDEA: Rm fn_ty & module. Instead pass in &mut TyValCache. Though unlikely to work.
trait FnValue<'ctx>: FnDecl<'ctx> {
    fn build_val(ctx: &'ctx Context, fn_decl: FunctionValue<'ctx>, module: &Module<'ctx>) -> FunctionValue<'ctx>;
}

// FIXME: Getting cached items is useless? Not differentiated per module..
struct TyValCache<'ctx> {
    context: &'ctx Context,
    types: FxHashMap<&'static str, AnyTypeEnum<'ctx>>,
    // Mapping of a value and whether or not it is a decl (only for fns)
    values: FxHashMap<(&'static str, bool), AnyValueEnum<'ctx>>,
}

impl<'ctx> TyValCache<'ctx> {
    fn new(context: &'ctx Context) -> Self {
        TyValCache {
            context,
            types: FxHashMap::default(),
            values: FxHashMap::default(),
        }
    }
}

impl<'ctx> TyValCache<'ctx> {
    fn get_type<T, B>(&mut self, module: &Module<'ctx>) -> Result<B, ()>
    where
        B: AnyType<'ctx> + TryFrom<AnyTypeEnum<'ctx>> + Into<AnyTypeEnum<'ctx>>,
        T: Type<'ctx, B>,
    {
        let ctx = self.context;
        let ty_enum = self.types
            .entry(T::FULL_PATH)
            .or_insert_with(|| T::build_ty(ctx, module).into());

        B::try_from(*ty_enum).or(Err(()))
    }

    fn get_fn_decl<F: FnDecl<'ctx>>(&mut self, module: &Module<'ctx>) -> FunctionValue<'ctx> {
        let fn_ty = self.get_type::<F, _>(module).expect("to have found a FunctionType");

        self.values
            .entry((F::FULL_PATH, true))
            .or_insert_with(|| F::build_decl(fn_ty, module).into())
            .into_function_value()
    }

    fn get_fn_value<F: FnValue<'ctx>>(&mut self, module: &Module<'ctx>) -> FunctionValue<'ctx> {
        let ctx = self.context;
        let fn_decl = self.get_fn_decl::<F>(module);

        self.values
            .entry((F::FULL_PATH, false))
            .or_insert_with(|| F::build_val(ctx, fn_decl, module).into())
            .into_function_value()
    }
}

pub struct LLVMCodeGen<'ctx> {
    modules: FxHashMap<StrId, Module<'ctx>>,
    ty_val_cache: TyValCache<'ctx>,
}

impl<'ctx> LLVMCodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        LLVMCodeGen {
            modules: FxHashMap::default(),
            ty_val_cache: TyValCache::new(context),
            // execution_engine: None,
            // pass_manager: None,
        }
    }

    pub fn add_module(&mut self, mut ast: Block, module_id: StrId, module_name: &str) {
        let module = self.ty_val_cache.context.create_module(module_name);

        // If we're dealing with main, and there isn't an explicitly defined main,
        // we must turn the top level block into a main fn decl
        if module_name == "main" && module.get_function("main").is_none() {
            Visitor::new(BlockIndentPlusPlus).run(&mut ast);

            ast.append_stmt(Stmt::new(Spanned::boxed(ExprKind::Return(None), Span::DUMMY)));

            let sig = FnSig::new(Vec::new(), None);
            let sp_name = Spanned::new("main", Span::DUMMY);
            let sp_sig = Spanned::new(sig, Span::DUMMY);
            let stmts = vec![
                Stmt::new(Spanned::new(ItemKind::FnDef(sp_name, sp_sig, ast), Span::DUMMY)),
            ];

            ast = Block::new(0, stmts);
        }

        // HACK: Remove :(
        PrintString::build_decl(PrintString::build_ty(self.ty_val_cache.context, &module), &module);

        // TODO: Immutable visitor only
        Visitor::new(CodeGen::new(self.ty_val_cache.context, &module)).run(&mut ast);

        self.modules.insert(module_id, module);
    }

    pub fn add_std_module(&mut self, module_id: StrId) {
        let module = self.ty_val_cache.context.create_module("std");

        // TODO: These should only be defined if they are referenced (ast knowledge?)
        self.ty_val_cache.get_fn_decl::<PutcharBuiltin>(&module);
        self.ty_val_cache.get_fn_value::<PrintString>(&module);

        self.modules.insert(module_id, module);
    }

    // fn get_type<T: LLVMType>(&self, module_id: StrId) -> Option<BasicTypeEnum<'ctx>> {
    //     // We should be able to call get_type from context once LLVM 11 rolls around...
    //     self.modules.get(&module_id)?.get_type(T::FULL_PATH)
    // }

    pub fn dump_ir(&self, module_id: StrId) {
        if let Some(module) = self.modules.get(&module_id) {
            module.print_to_stderr();
        }
    }

//     pub fn save_binary(&self) -> () {
//         // TODO
//     }

    // TODO: Improve errors
    pub fn build_jit<'cg>(&'cg self, main_id: StrId) -> Result<JitEngine<'cg, 'ctx>, ()> {
        let main_module = match self.modules.get(&main_id) {
            Some(module) => module,
            None => return Err(()),
        };

        Target::initialize_native(&InitializationConfig::default()).unwrap();

        let fn_pass_manager = PassManager::create(main_module);

        // TODO: Add more passes here
        fn_pass_manager.add_memcpy_optimize_pass();
        fn_pass_manager.initialize();

        let execution_engine = match main_module.create_execution_engine() {
            Ok(ee) => ee,
            Err(_err_str) => return Err(()),
        };

        Ok(JitEngine {
            modules: &self.modules,
            main_id,
            execution_engine,
            fn_pass_manager,
        })
    }

// REVIEW: Should scoped_variables take a COW keys?
//         match ast.get_expr() {
//             &Expr::Block(ref exprs) => {
//                 let mut last_value = None;

//                 for expr in exprs {
//                     last_value = self.generate_ir(module, expr, scoped_variables);
//                 }

//                 last_value
//             },
//             &Expr::FnCall(ref name, ref args) => {
//                 let function = match module.get_function(name) {
//                     Some(function) => function,
//                     None => {
//                         println!("LLVMGenError: Could not find function {}", name);

//                         return None; // REVIEW: Should this panic? We should've already known it was missing and safely unwrap
//                     }
//                 };

//                 let num_params = function.count_params() as usize;

//                 if num_params != args.len() {
//                     println!("LLVMGenError: Function {} requires {} args. Found {}", name, args.len(), num_params);

//                     return None; // REVIEW: panic?
//                 }

//                 let mut arg_values = Vec::with_capacity(num_params);

//                 for arg in args {
//                     let value = self.generate_ir(module, arg, scoped_variables).unwrap();

//                     arg_values.push(value);
//                 }

//                 Some(self.builder.build_call(&function, &arg_values, name)) // REVIEW: maybe tmp_ + name? Unclear if same name as fn is bad..
//             },
//             &Expr::Literal(ref literal_type) => {
//                 match literal_type {
//                     &Literals::UTF8Char(ref val) => Some(self.context.i32_type().const_int(*val as u64, false)),
//                     &Literals::I8Num(ref val) => Some(self.context.i8_type().const_int(*val as u64, true)),
//                     &Literals::I16Num(ref val) => Some(self.context.i16_type().const_int(*val as u64, true)),
//                     &Literals::I32Num(ref val) => Some(self.context.i32_type().const_int(*val as u64, true)),
//                     &Literals::I64Num(ref val) => Some(self.context.i64_type().const_int(*val as u64, true)),
//                     &Literals::U8Num(ref val) => Some(self.context.i8_type().const_int(*val as u64, false)),
//                     &Literals::U16Num(ref val) => Some(self.context.i16_type().const_int(*val as u64, false)),
//                     &Literals::U32Num(ref val) => Some(self.context.i32_type().const_int(*val as u64, false)),
//                     &Literals::U64Num(ref val) => Some(self.context.i64_type().const_int(*val as u64, false)),
//                     &Literals::F32Num(ref val) => Some(self.context.f32_type().const_float(*val as f64)),
//                     &Literals::F64Num(ref val) => Some(self.context.f64_type().const_float(*val as f64)),
//                     &Literals::Bool(ref val) => Some(self.context.bool_type().const_int(*val as u64, false)),
//                     &Literals::_None => panic!("LLVMGenError: Unimplemented for NoneType")
//                 }
//             },
//             &Expr::InfixOp(ref op, ref lhs_exprwrapper, ref rhs_exprwrapper) => {
//                 let (mut lhs_val, mut rhs_val) = match (self.generate_ir(module, lhs_exprwrapper, scoped_variables), self.generate_ir(module, rhs_exprwrapper, scoped_variables)) {
//                     (Some(val1), Some(val2)) => (val1, val2),
//                     (Some(_), None) => unreachable!("LLVMGenError: InfixOp only LHS contains value"),
//                     (None, Some(_)) => unreachable!("LLVMGenError: InfixOp only RHS contains value"),
//                     (None, None) => unreachable!("LLVMGenError: InfixOp has no values")
//                 };

//                 // REVIEW: I'm wondering if auto deref should be handled by semantic analysis
//                 // and insert a "deref" expr
//                 if lhs_val.is_pointer() {
//                     lhs_val = self.builder.build_load(&lhs_val, "deref"); // Think this is like Rust's deref trait...
//                 }

//                 if rhs_val.is_pointer() {
//                     rhs_val = self.builder.build_load(&rhs_val, "deref"); // Think this is like Rust's deref trait...
//                 }

//                 // REVIEW: Adding different types should never happen if SA is doing it's job, right?
//                 match op {
//                     &InfixOp::Add => {
//                         let add = match (lhs_val.get_type_kind(), rhs_val.get_type_kind()) { // REVIEW: Not fully tested
//                             (LLVMIntegerTypeKind, LLVMIntegerTypeKind) => self.builder.build_int_add(&lhs_val, &rhs_val, "int_add"),
//                             (LLVMFloatTypeKind, LLVMFloatTypeKind) => self.builder.build_float_add(&lhs_val, &rhs_val, "f32_add"), // REVIEW: How is this different from LLVMRealUEQ??
//                             (LLVMDoubleTypeKind, LLVMDoubleTypeKind) => self.builder.build_float_add(&lhs_val, &rhs_val, "f64_add"), // ^
//                             (LLVMFP128TypeKind, LLVMFP128TypeKind) => self.builder.build_float_add(&lhs_val, &rhs_val, "f128_add"), // ^
//                             (LLVMStructTypeKind, LLVMStructTypeKind) => panic!("LLVMGenError: Custom struct addition not yet implemented."),
//                             (_, _) => panic!("LLVMGenError: Unsupported type addition: {:?} + {:?}", lhs_val.get_name(), rhs_val.get_name()),
//                         };

//                         Some(add)
//                     },
//                     &InfixOp::Sub => {
//                         let sub = match (lhs_val.get_type_kind(), rhs_val.get_type_kind()) { // REVIEW: Not fully tested
//                             (LLVMIntegerTypeKind, LLVMIntegerTypeKind) => self.builder.build_int_sub(&lhs_val, &rhs_val, "int_sub"),
//                             (LLVMFloatTypeKind, LLVMFloatTypeKind) => self.builder.build_float_sub(&lhs_val, &rhs_val, "f32_sub"),
//                             (LLVMDoubleTypeKind, LLVMDoubleTypeKind) => self.builder.build_float_sub(&lhs_val, &rhs_val, "f64_sub"),
//                             (LLVMFP128TypeKind, LLVMFP128TypeKind) => self.builder.build_float_sub(&lhs_val, &rhs_val, "f128_sub"),
//                             (LLVMStructTypeKind, LLVMStructTypeKind) => panic!("LLVMGenError: Custom struct subtraction not yet implemented."),
//                             (_, _) => panic!("LLVMGenError: Unsupported type subtraction: {:?} - {:?}", lhs_val.get_name(), rhs_val.get_name()),
//                         };

//                         Some(sub)

//                     },
//                     &InfixOp::Mul => {
//                         let mul = match (lhs_val.get_type_kind(), rhs_val.get_type_kind()) { // REVIEW: Not fully tested
//                             (LLVMIntegerTypeKind, LLVMIntegerTypeKind) => self.builder.build_int_mul(&lhs_val, &rhs_val, "int_mul"),
//                             (LLVMFloatTypeKind, LLVMFloatTypeKind) => self.builder.build_float_mul(&lhs_val, &rhs_val, "f32_mul"),
//                             (LLVMDoubleTypeKind, LLVMDoubleTypeKind) => self.builder.build_float_mul(&lhs_val, &rhs_val, "f64_mul"),
//                             (LLVMFP128TypeKind, LLVMFP128TypeKind) => self.builder.build_float_mul(&lhs_val, &rhs_val, "f128_mul"),
//                             (LLVMStructTypeKind, LLVMStructTypeKind) => panic!("LLVMGenError: Custom struct multiplication not yet implemented."),
//                             (_, _) => panic!("LLVMGenError: Unsupported type multiplication: {:?} * {:?}", lhs_val.get_name(), rhs_val.get_name()),
//                         };

//                         Some(mul)
//                     },
//                     &InfixOp::Div => {
//                         let div = match (lhs_val.get_type_kind(), rhs_val.get_type_kind()) { // REVIEW: Not fully tested
//                             (LLVMIntegerTypeKind, LLVMIntegerTypeKind) => self.builder.build_int_div(&lhs_val, &rhs_val, "int_div"), // TODO: Signed support
//                             (LLVMFloatTypeKind, LLVMFloatTypeKind) => self.builder.build_float_div(&lhs_val, &rhs_val, "f32_div"),
//                             (LLVMDoubleTypeKind, LLVMDoubleTypeKind) => self.builder.build_float_div(&lhs_val, &rhs_val, "f64_div"),
//                             (LLVMFP128TypeKind, LLVMFP128TypeKind) => self.builder.build_float_div(&lhs_val, &rhs_val, "f128_div"),
//                             (LLVMStructTypeKind, LLVMStructTypeKind) => panic!("LLVMGenError: Custom struct division not yet implemented."),
//                             (_, _) => panic!("LLVMGenError: Unsupported type division: {:?} / {:?}", lhs_val.get_name(), rhs_val.get_name()),
//                         };

//                         Some(div)
//                     },
//                     &InfixOp::Mod => match (lhs_val, rhs_val) {
//                         _ => panic!("LLVMGenError: Unimplemented infix operator mod")
//                     },
//                     &InfixOp::Pow => match (lhs_val, rhs_val) {
//                         _ => panic!("LLVMGenError: Unimplemented infix operator pow")
//                     },
//                     &InfixOp::Equ => {
//                         let equ = match (lhs_val.get_type_kind(), rhs_val.get_type_kind()) { // REVIEW: Not fully tested
//                             (LLVMIntegerTypeKind, LLVMIntegerTypeKind) => self.builder.build_int_compare(LLVMIntEQ, &lhs_val, &rhs_val, "int_equ"),
//                             (LLVMFloatTypeKind, LLVMFloatTypeKind) => self.builder.build_float_compare(LLVMRealOEQ, &lhs_val, &rhs_val, "f32_equ"), // REVIEW: How is this different from LLVMRealUEQ??
//                             (LLVMDoubleTypeKind, LLVMDoubleTypeKind) => self.builder.build_float_compare(LLVMRealOEQ, &lhs_val, &rhs_val, "f64_equ"), // ^
//                             (LLVMFP128TypeKind, LLVMFP128TypeKind) => self.builder.build_float_compare(LLVMRealOEQ, &lhs_val, &rhs_val, "f128_equ"), // ^
//                             (LLVMStructTypeKind, LLVMStructTypeKind) => panic!("LLVMGenError: Custom struct equality not yet implemented."),
//                             (_, _) => panic!("LLVMGenError: Unsupported type equality: {:?} == {:?}", lhs_val.get_name(), rhs_val.get_name()),
//                         };

//                         Some(equ)
//                     },
//                     &InfixOp::Lt => {
//                         let lt = match (lhs_val.get_type_kind(), rhs_val.get_type_kind()) { // REVIEW: Not fully tested
//                             (LLVMIntegerTypeKind, LLVMIntegerTypeKind) => self.builder.build_int_compare(LLVMIntULT, &lhs_val, &rhs_val, "int_lt"), // TODO: Signed compare
//                             (LLVMFloatTypeKind, LLVMFloatTypeKind) => self.builder.build_float_compare(LLVMRealOLT, &lhs_val, &rhs_val, "f32_lt"), // REVIEW: How is this different from LLVMRealULT??
//                             (LLVMDoubleTypeKind, LLVMDoubleTypeKind) => self.builder.build_float_compare(LLVMRealOLT, &lhs_val, &rhs_val, "f64_lt"), // ^
//                             (LLVMFP128TypeKind, LLVMFP128TypeKind) => self.builder.build_float_compare(LLVMRealOLT, &lhs_val, &rhs_val, "f128_lt"), // ^
//                             (LLVMStructTypeKind, LLVMStructTypeKind) => panic!("LLVMGenError: Custom struct less than not yet implemented."),
//                             (_, _) => panic!("LLVMGenError: Unsupported type equality: {:?} < {:?}", lhs_val.get_name(), rhs_val.get_name()),
//                         };

//                         Some(lt)
//                     },
//                     &InfixOp::Lte => {
//                         let lte = match (lhs_val.get_type_kind(), rhs_val.get_type_kind()) { // REVIEW: Not fully tested
//                             (LLVMIntegerTypeKind, LLVMIntegerTypeKind) => self.builder.build_int_compare(LLVMIntULE, &lhs_val, &rhs_val, "int_lte"), // TODO: Signed compare
//                             (LLVMFloatTypeKind, LLVMFloatTypeKind) => self.builder.build_float_compare(LLVMRealOLE, &lhs_val, &rhs_val, "f32_lte"), // REVIEW: How is this different from LLVMRealULE??
//                             (LLVMDoubleTypeKind, LLVMDoubleTypeKind) => self.builder.build_float_compare(LLVMRealOLE, &lhs_val, &rhs_val, "f64_lte"), // ^
//                             (LLVMFP128TypeKind, LLVMFP128TypeKind) => self.builder.build_float_compare(LLVMRealOLE, &lhs_val, &rhs_val, "f128_lte"), // ^
//                             (LLVMStructTypeKind, LLVMStructTypeKind) => panic!("LLVMGenError: Custom struct less than equal not yet implemented."),
//                             (_, _) => panic!("LLVMGenError: Unsupported type equality: {:?} <= {:?}", lhs_val.get_name(), rhs_val.get_name()),
//                         };

//                         Some(lte)
//                     },
//                     &InfixOp::Gt => {
//                         let gt = match (lhs_val.get_type_kind(), rhs_val.get_type_kind()) { // REVIEW: Not fully tested
//                             (LLVMIntegerTypeKind, LLVMIntegerTypeKind) => self.builder.build_int_compare(LLVMIntUGT, &lhs_val, &rhs_val, "int_gt"), // TODO: Signed compare
//                             (LLVMFloatTypeKind, LLVMFloatTypeKind) => self.builder.build_float_compare(LLVMRealOGT, &lhs_val, &rhs_val, "f32_gt"), // REVIEW: How is this different from LLVMRealUGT??
//                             (LLVMDoubleTypeKind, LLVMDoubleTypeKind) => self.builder.build_float_compare(LLVMRealOGT, &lhs_val, &rhs_val, "f64_gt"), // ^
//                             (LLVMFP128TypeKind, LLVMFP128TypeKind) => self.builder.build_float_compare(LLVMRealOGT, &lhs_val, &rhs_val, "f128_gt"), // ^
//                             (LLVMStructTypeKind, LLVMStructTypeKind) => panic!("LLVMGenError: Custom struct greater than not yet implemented."),
//                             (_, _) => panic!("LLVMGenError: Unsupported type equality: {:?} > {:?}", lhs_val.get_name(), rhs_val.get_name()),
//                         };

//                         Some(gt)
//                     },
//                     &InfixOp::Gte => {
//                         let gte = match (lhs_val.get_type_kind(), rhs_val.get_type_kind()) { // REVIEW: Not fully tested
//                             (LLVMIntegerTypeKind, LLVMIntegerTypeKind) => self.builder.build_int_compare(LLVMIntUGE, &lhs_val, &rhs_val, "int_gte"), // TODO: Signed compare
//                             (LLVMFloatTypeKind, LLVMFloatTypeKind) => self.builder.build_float_compare(LLVMRealOGE, &lhs_val, &rhs_val, "f32_gte"), // REVIEW: How is this different from LLVMRealUGE??
//                             (LLVMDoubleTypeKind, LLVMDoubleTypeKind) => self.builder.build_float_compare(LLVMRealOGE, &lhs_val, &rhs_val, "f64_gte"), // ^
//                             (LLVMFP128TypeKind, LLVMFP128TypeKind) => self.builder.build_float_compare(LLVMRealOGE, &lhs_val, &rhs_val, "f128_gte"), // ^
//                             (LLVMStructTypeKind, LLVMStructTypeKind) => panic!("LLVMGenError: Custom struct greater than equal not yet implemented."),
//                             (_, _) => panic!("LLVMGenError: Unsupported type equality: {:?} >= {:?}", lhs_val.get_name(), rhs_val.get_name()),
//                         };

//                         Some(gte)
//                     }
//                 }
//             },
//             // REVIEW: Needs further testing
//             &Expr::UnaryOp(ref op, ref expr) => {
//                 match op {
//                     &UnaryOp::Negate => self.generate_ir(module, expr, scoped_variables).map(|val| self.builder.build_neg(&val, "neg")),
//                     &UnaryOp::Not => self.generate_ir(module, expr, scoped_variables).map(|val| self.builder.build_not(&val, "not")),
//                 }
//             },
//             &Expr::FnDecl(ref name, ref arg_defs, ref return_type, ref body_expr) => {
//                 let mut fn_variable_scope = HashMap::new(); // REVIEW: This will exclude globals
//                 let mut arg_types: Vec<Type> = arg_defs.iter().map(|&(_, ref type_string)| self.string_to_type(&type_string[..], &module).expect("Did not find specified type")).collect();

//                 // TODO: Support args types and return types
//                 let return_type = match return_type {
//                     &Some(ref type_string) => self.string_to_type(&type_string[..], &module).expect("Did not find speficied type"),
//                     &None => self.context.void_type(),
//                 };

//                 let function = module.add_function(name, return_type.fn_type(&mut arg_types, false));

//                 let name_value_data = arg_defs.iter().map(|&(ref name, _)| name).zip(function.params());

//                 for (name, mut param_value) in name_value_data {
//                     param_value.set_name(&name);
//                     fn_variable_scope.insert(name.to_string(), param_value.as_value()); // REVIEW: Cow?
//                 }

//                 let bb_enter = self.context.append_basic_block(&function, "enter");

//                 self.builder.position_at_end(&bb_enter);

//                 // REVIEW: This will return the last generated value... is that what we want?
//                 // Or should it go back to the global scope after generating ir?
//                 self.generate_ir(module, body_expr, &mut fn_variable_scope)
//             },
//             &Expr::Return(ref return_type_expr) => {
//                 match return_type_expr {
//                     &Some(ref return_type) => match self.generate_ir(module, return_type, scoped_variables) {
//                         Some(mut t) => {
//                             // REVIEW: I'm wondering if auto deref should be handled by semantic analysis
//                             // and insert a "deref" expr
//                             if t.is_pointer() {
//                                 t = self.builder.build_load(&t, "deref"); // Think this is like Rust's Deref Trait
//                             }

//                             Some(self.builder.build_return(Some(t)))
//                         }
//                         None => unreachable!("LLVMGenError: Hit unreachable return type generation")
//                     },
//                     &None => Some(self.builder.build_return(None)),
//                 }
//             },
//             &Expr::Var(ref name) => {
//                 match scoped_variables.get(name) {
//                     Some(val) => Some(*val),
//                     None => unreachable!("LLVMGenError: Unknown variable {} was uncaught", name)
//                 }
//             },
//             &Expr::VarDecl(_, ref name, ref val_type, ref expr) => {
//                 assert!(val_type.is_some(), "LLVMGenError: Variable declaration not given a type by codegen phase");

//                 // Assign to a literal
//                 match self.generate_ir(module, expr, scoped_variables) {
//                     Some(val) => {
//                         let val = if !val.is_pointer() {
//                             let alloca = self.builder.build_stack_allocation(&val.get_type(), "stored_ptr");
//                             self.builder.build_store(&val, &alloca);

//                             alloca
//                         } else {
//                             val
//                         };

//                         // Couldn't figure out how to not clone this string
//                         scoped_variables.insert(name.clone(), val);

//                         Some(val)
//                     },
//                     None => None
//                 }
//             },
//             &Expr::If(ref cond_expr, ref body_expr, ref opt_else_expr) => {
//                 let cond_val = match self.generate_ir(module, cond_expr, scoped_variables) {
//                     Some(val) => val,
//                     None => return None
//                 };

//                 let type_ = self.context.bool_type();

//                 let zero = type_.const_int(0, false);
//                 let op = LLVMIntEQ;

//                 let block = self.builder.get_insert_block();

//                 let cond_cmp = self.builder.build_int_compare(op, &cond_val, &zero, "ifcond");

//                 let parent_fn = block.get_parent();

//                 let body_block = self.context.append_basic_block(&parent_fn, "if");
//                 let else_block = self.context.append_basic_block(&parent_fn, "else");
//                 let merge_block = self.context.append_basic_block(&parent_fn, "merge");

//                 // If the condition is true:
//                 self.builder.build_conditional_branch(&cond_cmp, &body_block, &else_block);
//                 self.builder.position_at_end(&body_block);

//                 let mut body_val = match self.generate_ir(module, body_expr, scoped_variables) {
//                     Some(val) => val,
//                     None => return None
//                 };

//                 // Merge into the above layer when done
//                 self.builder.build_unconditional_branch(&merge_block);

//                 // Call else codegen if it exists
//                 let mut body_end_block = self.builder.get_insert_block();

//                 self.builder.position_at_end(&else_block);

//                 // Optional, doesn't need to return on None
//                 let opt_else_val = match opt_else_expr {
//                     &Some(ref expr) => self.generate_ir(module, expr, scoped_variables),
//                     &None => None
//                 };

//                 let else_br = self.builder.build_unconditional_branch(&merge_block);
//                 let mut else_end_block = self.builder.get_insert_block();

//                 // Finish up
//                 self.builder.position_at_end(&merge_block);

//                 let phi = self.builder.build_phi(&type_, "phi");

//                 phi.add_incoming(&mut body_val, &mut body_end_block, 1);
//                 phi.add_incoming(&mut opt_else_val.unwrap_or(else_br), &mut else_end_block, 1);

//                 Some(phi)
//             },
//             &Expr::WhileLoop(ref condition, ref body) => {
//                 let one = self.context.bool_type().const_int(1, false);

//                 let start_block = self.builder.get_insert_block();
//                 let cond_check_block = self.context.insert_basic_block_after(&start_block, "cond_check");
//                 let loop_block = self.context.insert_basic_block_after(&cond_check_block, "loop");
//                 let end_block = self.context.insert_basic_block_after(&loop_block, "end");

//                 self.builder.position_at_end(&start_block);
//                 self.builder.build_unconditional_branch(&cond_check_block);
//                 self.builder.position_at_end(&cond_check_block);

//                 let cond_val = match self.generate_ir(module, condition, scoped_variables) {
//                     Some(val) => val,
//                     None => return None
//                 };
//                 let cond_cmp = self.builder.build_int_compare(LLVMIntEQ, &cond_val, &one, "cmp");

//                 self.builder.build_conditional_branch(&cond_cmp, &loop_block, &end_block);
//                 self.builder.position_at_end(&loop_block);

//                 let body = self.generate_ir(module, body, scoped_variables);

//                 self.builder.build_unconditional_branch(&cond_check_block);
//                 self.builder.position_at_end(&end_block);

//                 body
//             },
//             &Expr::Assign(ref lhs_exprwrapper, ref rhs_exprwrapper) => { // REVIEW: Should we assume SA would stop us from mutating an immutable?
//                 // REVIEW: Does it ever make sense for the lhs to be anything other than a string?
//                 // We could just look it up in the hash table directly...
//                 // if let &Expr::Var(ref string) = lhs_exprwrapper.get_expr() {
//                 //     scoped_variables.get(string);
//                 // }

//                 let (lhs_val, rhs_val) =  match (self.generate_ir(module, lhs_exprwrapper, scoped_variables), self.generate_ir(module, rhs_exprwrapper, scoped_variables)) {
//                     (Some(val1), Some(val2)) => (val1, val2),
//                     (Some(_), None) => unreachable!("LLVMGenError: Assign only LHS contains value"),
//                     (None, Some(_)) => unreachable!("LLVMGenError: Assign only RHS contains value"),
//                     (None, None) => unreachable!("LLVMGenError: Assign has no values")
//                 };

//                 // let lhs_val = self.builder.build_gep(&lhs_val, &vec![0], "gep");

//                 Some(self.builder.build_store(&rhs_val, &lhs_val))
//             },
//             &Expr::NoOp => None,
//         }
//     }

//     fn string_to_type(&self, name: &str, module: &Module) -> Option<Type> {
//         match name {
//             "bool" => Some(self.context.bool_type()),
//             "i8" => Some(self.context.i8_type()),
//             "u8" => Some(self.context.i8_type()),
//             "i16" => Some(self.context.i16_type()),
//             "u16" => Some(self.context.i16_type()),
//             "f32" => Some(self.context.f32_type()),
//             "i32" => Some(self.context.i32_type()),
//             "u32" => Some(self.context.i32_type()),
//             "i64" => Some(self.context.i64_type()),
//             "u64" => Some(self.context.i64_type()),
//             "f64" => Some(self.context.f64_type()),
//             "f128" => Some(self.context.f128_type()),
//             "i128" => Some(self.context.i128_type()),
//             "u128" => Some(self.context.i128_type()),
//             "void" => Some(self.context.void_type()), // TODO: Not use name "void"
//             _ => module.get_type(name),
//         }
//     }
}

struct CodeGen<'tmp, 'ctx: 'tmp> {
    builder: Builder<'ctx>,
    context: &'ctx Context,
    module: &'tmp Module<'ctx>,
}

impl<'tmp, 'ctx> CodeGen<'tmp, 'ctx> {
    fn new(context: &'ctx Context, module: &'tmp Module<'ctx>) -> Self {
        let builder = context.create_builder();

        CodeGen {
            builder,
            context,
            module,
        }
    }

    fn get_or_insert_struct_type<T: Type<'ctx, StructType<'ctx>>>(&self) -> StructType<'ctx> {
        if let Some(ty) = self.module.get_struct_type(T::FULL_PATH) {
            return ty;
        }

        T::build_ty(self.context, &self.module)
    }
}

impl<'s, 'ctx> AstVisitor<'s, BasicValueEnum<'ctx>> for CodeGen<'_, 'ctx> {
    fn visit_item_kind(&mut self, item_kind: &mut ItemKind<'s>) -> VisitOutcome<BasicValueEnum<'ctx>> {
        match item_kind {
            ItemKind::FnDef(name, fn_sig, _block) => {
                // let mut fn_variable_scope = HashMap::new(); // REVIEW: This will exclude globals
                // let mut arg_types: Vec<Type> = arg_defs.iter().map(|&(_, ref type_string)| self.string_to_type(&type_string[..], &module).expect("Did not find specified type")).collect();

                // TODO: Support args types and return types
                let params = [];
                let fn_ty = match fn_sig.get_node().return_type() {
                    Some(ty) => unimplemented!("{:?}", ty),
                    None => self.context.void_type().fn_type(&params, false),
                };

                let function = self.module.add_function(name.node(), fn_ty, None);

                // let name_value_data = arg_defs.iter().map(|&(ref name, _)| name).zip(function.params());

                // for (name, mut param_value) in name_value_data {
                //     param_value.set_name(&name);
                //     fn_variable_scope.insert(name.to_string(), param_value.as_value()); // REVIEW: Cow?
                // }

                let bb_enter = self.context.append_basic_block(function, "enter");

                self.builder.position_at_end(bb_enter);

                VisitOutcome::default()
            },
            i => unimplemented!("{:?}", i),
        }
    }

    fn visit_expr_kind(&mut self, expr_kind: &mut ExprKind<'s>) -> VisitOutcome<BasicValueEnum<'ctx>> {
        match expr_kind {
            ExprKind::FnCall(name, params) => {
                let function = match self.module.get_function(name.node()) {
                    Some(function) => function,
                    None => {
                        self.module.print_to_stderr();
                        panic!("LLVMGenError: Could not find function {}", name.node());
                    },
                };

                let num_params = function.count_params() as usize;

                if num_params != params.len() {
                    // If this happens then it means semantic analysis didn't do its job.
                    unreachable!("LLVMGenError: Function {} requires {} args. Found {}", name.node(), params.len(), num_params);
                }

                let mut param_values = Vec::with_capacity(num_params);

                for param in params {
                    let value = self.visit_expr_kind(param.deref_node_mut()).into_node();

                    param_values.push(value);
                }

                let fn_call = self.builder.build_call(function, &param_values, name.node());

                VisitOutcome::new_without_visit(fn_call.try_as_basic_value().left())
            },
            ExprKind::Literal(Literal::UTF8String(s)) => {
                // Trim beginning and end quotes
                let s = &s[1..(s.len() - 1)];
                let string_type = self.get_or_insert_struct_type::<LimeString>();
                let void_type = self.context.void_type();
                let bool_type = self.context.bool_type();
                let i8_type = self.context.i8_type();
                let i8_ptr_type = i8_type.ptr_type(AddressSpace::Generic);
                let i32_type = self.context.i32_type();
                let i64_type = self.context.i64_type();
                let i8_array_type = i8_type.array_type(s.len() as u32);
                let i32_zero = i32_type.const_int(0, false);
                let i32_one = i32_type.const_int(1, false);
                let i32_two = i32_type.const_int(2, false);
                let bool_false = bool_type.const_int(0, false);
                let len = i64_type.const_int(s.len() as u64, false);

                let mut chars = Vec::with_capacity(s.len());

                for chr in s.bytes() {
                    chars.push(i8_type.const_int(chr as u64, false));
                }

                let const_str_array = i8_type.const_array(&chars);

                // TODO: Use module.get_global so we don't have a million dupes?
                let global_str = self.module.add_global(i8_array_type, Some(AddressSpace::Generic), "global_str");

                global_str.set_initializer(&const_str_array);

                let stack_struct_ptr = self.builder.build_alloca(string_type, "string_struct_ptr");

                let str_ptr = unsafe { self.builder.build_gep(stack_struct_ptr, &[i32_zero, i32_zero], "str_ptr") };
                let len_ptr = unsafe { self.builder.build_gep(stack_struct_ptr, &[i32_zero, i32_one], "len_ptr") };
                let cap_ptr = unsafe { self.builder.build_gep(stack_struct_ptr, &[i32_zero, i32_two], "cap_ptr") };

                self.builder.build_store(len_ptr, len);
                self.builder.build_store(cap_ptr, len);

                let i8_heap_array = self.builder.build_array_alloca(i8_array_type, len, "i8_heap_array");
                let i8_heap_ptr = self.builder.build_pointer_cast(i8_heap_array, i8_ptr_type, "i8_heap_ptr");

                self.builder.build_store(str_ptr, i8_heap_ptr);

                let memcpy_fn = match self.module.get_function("llvm.memcpy.p0i8.p0i8.i64") {
                    Some(f) => f,
                    None => {
                        let args = [i8_ptr_type.into(), i8_ptr_type.into(), i64_type.into(), bool_type.into()];
                        let fn_type2 = void_type.fn_type(&args, false);

                        self.module.add_function("llvm.memcpy.p0i8.p0i8.i64", fn_type2, None)
                    }
                };

                let global_i8_ptr = unsafe { self.builder.build_gep(global_str.as_pointer_value(), &[i32_zero, i32_zero], "global_i8_ptr") };

                self.builder.build_call(memcpy_fn, &[i8_heap_ptr.into(), global_i8_ptr.into(), len.into(), bool_false.into()], "llvm.memcpy.p0i8.p0i8.i64");

                VisitOutcome::new(BasicValueEnum::from(stack_struct_ptr))
            },
            ExprKind::Return(ret_val) => {
                match ret_val {
                    Some(expr) => {
                        self.visit_expr_kind(expr.deref_node_mut()).without_visit()
                    },
                    None => {
                        self.builder.build_return(None);

                        VisitOutcome::default()
                    },
                }
            },
            e => unimplemented!("{:?}", e),
        }
    }
}

/// This struct is used when main isn't specified so we have to wrap
/// the topmost block in a main function. We increment indent values
/// so that they are normalized to being inside main.
struct BlockIndentPlusPlus;

impl<'s> AstVisitor<'s> for BlockIndentPlusPlus {
    fn visit_block(&mut self, block: &mut Block<'s>) -> VisitOutcome<()> {
        block.indent += 1;

        VisitOutcome::default()
    }
}

pub struct JitEngine<'codegen, 'context: 'codegen> {
    modules: &'codegen FxHashMap<StrId, Module<'context>>,
    main_id: StrId,
    execution_engine: ExecutionEngine<'context>,
    fn_pass_manager: PassManager<FunctionValue<'context>>,
}

impl<'cg, 'ctx> JitEngine<'cg, 'ctx> {
    pub fn run(&self) -> Result<(), String> {
        let main_module = self.modules.get(&self.main_id).unwrap();
        let main = match main_module.get_function("main") {
            Some(main) => main,
            None => return Err(String::from("Could not find main function")),
        };

        main_module.verify().map_err(|llvm_str| llvm_str.to_string())?;

        for (id, module) in self.modules {
            if *id == self.main_id {
                continue;
            }

            module.verify().map_err(|llvm_str| llvm_str.to_string())?;

            self.execution_engine.add_module(module).or_else(|_| Err(String::from("Failed to add module")))?;
        }

        unsafe {
            self.execution_engine.run_function_as_main(main, &[]);
        }

        Ok(())
    }
}
