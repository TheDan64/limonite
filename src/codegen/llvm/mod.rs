mod builtins;
mod std;

use crate::codegen::llvm::builtins::PutcharBuiltin;
use crate::codegen::llvm::std::string::{PrintString, LimeString};
use crate::interner::StrId;
use crate::span::{Span, Spanned};
use crate::syntax::{Block, ExprKind, InfixOp, ItemKind, Literal, Local, Stmt, StmtKind, UnaryOp};
use crate::syntax::items::FnSig;
use crate::syntax::visitor::{AstVisitor, Visitor, VisitOutcome};

use hash_chain::ChainMap;
use inkwell::{AddressSpace, IntPredicate, FloatPredicate};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::types::{AnyType, AnyTypeEnum, FunctionType, StructType};
use inkwell::values::{AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue};
use rustc_hash::{FxHasher, FxHashMap};

use ::std::convert::{TryFrom, TryInto};
use ::std::hash::BuildHasherDefault;

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

use ::std::collections::HashMap;

type FxHasherWrapper = BuildHasherDefault<FxHasher>;

struct CodeGen<'tmp, 'context: 'tmp, 's> {
    builder: Builder<'context>,
    context: &'context Context,
    module: &'tmp Module<'context>,
    values: ChainMap<&'s str, BasicValueEnum<'context>, FxHasherWrapper>,
}

impl<'tmp, 'ctx, 's> CodeGen<'tmp, 'ctx, 's> {
    fn new(context: &'ctx Context, module: &'tmp Module<'ctx>) -> Self {
        let builder = context.create_builder();

        CodeGen {
            builder,
            context,
            module,
            values: ChainMap::new(HashMap::default()),
        }
    }

    fn get_or_insert_struct_type<T: Type<'ctx, StructType<'ctx>>>(&self) -> StructType<'ctx> {
        if let Some(ty) = self.module.get_struct_type(T::FULL_PATH) {
            return ty;
        }

        T::build_ty(self.context, &self.module)
    }

    fn add_scope(&mut self) {
        self.values.new_child();
    }

    fn pop_scope(&mut self) -> FxHashMap<&'s str, BasicValueEnum<'ctx>> {
        // Unwrap should never fail based on how remove_child is written
        self.values.remove_child().unwrap()
    }

    fn insert_value<I: Into<BasicValueEnum<'ctx>>>(&mut self, s: &'s str, bv: I) {
        self.values.insert(s, bv.into());
    }

    /// Workaround to having to call visit_block tree in expr codegen
    /// because we don't want to modify the normal block codegen path.
    fn visit_block_ext(&mut self, block: &mut Block<'s>) {
        self.add_scope();
        for stmt in block.stmts_mut() {
            match stmt.kind_mut() {
                StmtKind::Item(i) => self.visit_item_kind(i.get_node_mut()),
                StmtKind::Local(l) => self.visit_local(l),
                StmtKind::Expr(e) => self.visit_expr_kind(e.deref_node_mut()),
            };
        }
        self.pop_scope();
    }
}

impl<'s, 'ctx> AstVisitor<'s, BasicValueEnum<'ctx>> for CodeGen<'_, 'ctx, 's> {
    fn visit_block(&mut self, _block: &mut Block<'s>) -> VisitOutcome<BasicValueEnum<'ctx>> {
        self.add_scope();

        VisitOutcome::default()
    }

    fn exit_block(&mut self, _block: &mut Block<'s>) -> VisitOutcome<BasicValueEnum<'ctx>> {
        self.pop_scope();

        VisitOutcome::default()
    }

    fn visit_local(&mut self, local: &mut Local<'s>) -> VisitOutcome<BasicValueEnum<'ctx>> {
        let init = self.visit_expr_kind(local.init.deref_node_mut()).into_node();

        // FIXME: use local.init.ty
        let alloca = self.builder.build_alloca(self.context.i32_type(), "stored_ptr");

        self.builder.build_store(alloca, init);

        self.insert_value(local.ident.node(), alloca);

        VisitOutcome::default().without_visit()
    }

    fn visit_item_kind(&mut self, item_kind: &mut ItemKind<'s>) -> VisitOutcome<BasicValueEnum<'ctx>> {
        match item_kind {
            ItemKind::FnDef(name, fn_sig, _block) => {
                // Since Fn params live in the fn scope, not the block, this allows shadowing of params
                // with locals.
                self.add_scope();
                // let mut arg_types: Vec<Type> = arg_defs.iter().map(|&(_, ref type_string)| self.string_to_type(&type_string[..], &module).expect("Did not find specified type")).collect();

                // TODO: Support args types and return types
                let params = [];
                let fn_ty = match fn_sig.get_node().return_type() {
                    Some(ty) => unimplemented!("{:?}", ty),
                    None => self.context.void_type().fn_type(&params, false),
                };

                let function = self.module.add_function(name.node(), fn_ty, None);

                for (i, (name, _)) in fn_sig.get_node().params().iter().enumerate() {
                    let param = function.get_nth_param(i.try_into().expect("ICE")).expect("ICE");

                    param.set_name(name.node());

                    self.values.insert(name.node(), param);
                }

                let bb_enter = self.context.append_basic_block(function, "enter");

                self.builder.position_at_end(bb_enter);

                VisitOutcome::default()
            },
            i => unimplemented!("{:?}", i),
        }
    }

    fn exit_item_kind(&mut self, item_kind: &mut ItemKind<'s>) -> VisitOutcome<BasicValueEnum<'ctx>> {
        match item_kind {
            ItemKind::FnDef(..) => { self.pop_scope(); },
            _ => ()
        }

        VisitOutcome::default()
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
            ExprKind::Literal(Literal::UTF8Char(ch)) => {
                let val = self.context.i32_type().const_int(*ch as u64, false);

                VisitOutcome::new(BasicValueEnum::from(val))
            },
            ExprKind::Literal(lit_kind) => {
                // REVIEW: Do we pass in the sign_extend flag for i* types or just if the value is negative?
                match lit_kind {
                    Literal::Bool(n) => {
                        let val = self.context.bool_type().const_int(*n as u64, false);

                        VisitOutcome::new(BasicValueEnum::from(val))
                    },
                    Literal::I8Num(n) => {
                        let val = self.context.i8_type().const_int(*n as u64, true);

                        VisitOutcome::new(BasicValueEnum::from(val))
                    },
                    Literal::U8Num(n) => {
                        let val = self.context.i8_type().const_int(*n as u64, false);

                        VisitOutcome::new(BasicValueEnum::from(val))
                    },
                    Literal::I16Num(n) => {
                        let val = self.context.i16_type().const_int(*n as u64, true);

                        VisitOutcome::new(BasicValueEnum::from(val))
                    },
                    Literal::U16Num(n) => {
                        let val = self.context.i16_type().const_int(*n as u64, false);

                        VisitOutcome::new(BasicValueEnum::from(val))
                    },
                    Literal::I32Num(n) => {
                        let val = self.context.i32_type().const_int(*n as u64, true);

                        VisitOutcome::new(BasicValueEnum::from(val))
                    },
                    Literal::U32Num(n) => {
                        let val = self.context.i32_type().const_int(*n as u64, false);

                        VisitOutcome::new(BasicValueEnum::from(val))
                    },
                    Literal::I64Num(n) => {
                        let val = self.context.i64_type().const_int(*n as u64, true);

                        VisitOutcome::new(BasicValueEnum::from(val))
                    },
                    Literal::U64Num(n) => {
                        let val = self.context.i64_type().const_int(*n as u64, false);

                        VisitOutcome::new(BasicValueEnum::from(val))
                    },
                    Literal::I128Num(n) => {
                        let bytes = n.to_ne_bytes();
                        let words = [
                            u64::from_ne_bytes(bytes[0..=7].try_into().unwrap()),
                            u64::from_ne_bytes(bytes[8..=16].try_into().unwrap()),
                        ];
                        let val = self.context.i128_type().const_int_arbitrary_precision(&words);

                        VisitOutcome::new(BasicValueEnum::from(val))
                    },
                    Literal::U128Num(n) => {
                        let bytes = n.to_ne_bytes();
                        let words = [
                            u64::from_ne_bytes(bytes[0..=7].try_into().unwrap()),
                            u64::from_ne_bytes(bytes[8..=16].try_into().unwrap()),
                        ];
                        let val = self.context.i128_type().const_int_arbitrary_precision(&words);

                        VisitOutcome::new(BasicValueEnum::from(val))
                    },
                    Literal::F32Num(f) => {
                        let val = self.context.f32_type().const_float(*f as f64);

                        VisitOutcome::new(BasicValueEnum::from(val))
                    },
                    Literal::F64Num(f) => {
                        let val = self.context.f64_type().const_float(*f);

                        VisitOutcome::new(BasicValueEnum::from(val))
                    },
                    Literal::_None => panic!("todo?"),
                    _ => unreachable!("ICE"),
                }
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
            ExprKind::InfixOp(op, lhs_expr, rhs_expr) => {
                let mut lhs_val = self.visit_expr_kind(lhs_expr.deref_node_mut()).into_node();
                let mut rhs_val = self.visit_expr_kind(rhs_expr.deref_node_mut()).into_node();

                // Deref if ptr to value
                if let BasicValueEnum::PointerValue(lhs_ptr) = lhs_val {
                    if !lhs_ptr.get_type().get_element_type().is_pointer_type() {
                        lhs_val = self.builder.build_load(lhs_ptr, "auto_deref");
                    }
                }

                if let BasicValueEnum::PointerValue(rhs_ptr) = rhs_val {
                    if !rhs_ptr.get_type().get_element_type().is_pointer_type() {
                        rhs_val = self.builder.build_load(rhs_ptr, "auto_deref");
                    }
                }

                // TODO: If lhs/rhs points to a non ptr, we probably want to deref it prior to the match.
                // this is because it's likely an alloca

                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                        let iv = match op.node() {
                            InfixOp::Add => self.builder.build_int_add(lhs, rhs, "add"),
                            InfixOp::Sub => self.builder.build_int_sub(lhs, rhs, "sub"),
                            // TODO: nsw, nuw mul?
                            InfixOp::Mul => self.builder.build_int_sub(lhs, rhs, "mul"),
                            // TODO: Unsigned div?
                            InfixOp::Div => self.builder.build_int_signed_div(lhs, rhs, "div"),
                            // TODO: Unsigned variants?
                            InfixOp::Lt => self.builder.build_int_compare(IntPredicate::SLT, lhs, rhs, "lt"),
                            InfixOp::Lte => self.builder.build_int_compare(IntPredicate::SLE, lhs, rhs, "lte"),
                            InfixOp::Gt => self.builder.build_int_compare(IntPredicate::SLT, lhs, rhs, "gt"),
                            InfixOp::Gte => self.builder.build_int_compare(IntPredicate::SLE, lhs, rhs, "gte"),
                            InfixOp::Equ => self.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "eq"),
                            _ => unimplemented!(),
                        };

                        VisitOutcome::new_without_visit(BasicValueEnum::from(iv))
                    },
                    (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                        let bv: BasicValueEnum = match op.node() {
                            InfixOp::Add => self.builder.build_float_add(lhs, rhs, "fadd").into(),
                            InfixOp::Sub => self.builder.build_float_sub(lhs, rhs, "fsub").into(),
                            InfixOp::Mul => self.builder.build_float_mul(lhs, rhs, "fmul").into(),
                            InfixOp::Div => self.builder.build_float_div(lhs, rhs, "fdiv").into(),
                            // TODO: U* variants?:
                            InfixOp::Lt => self.builder.build_float_compare(FloatPredicate::OLT, lhs, rhs, "lt").into(),
                            InfixOp::Lte => self.builder.build_float_compare(FloatPredicate::OLE, lhs, rhs, "lte").into(),
                            InfixOp::Gt => self.builder.build_float_compare(FloatPredicate::OGT, lhs, rhs, "gt").into(),
                            InfixOp::Gte => self.builder.build_float_compare(FloatPredicate::OGE, lhs, rhs, "gte").into(),
                            InfixOp::Equ => self.builder.build_float_compare(FloatPredicate::OEQ, lhs, rhs, "eq").into(),
                            _ => unimplemented!(),
                        };

                        VisitOutcome::new_without_visit(bv)
                    },
                    pair => unimplemented!("Op {:?} pair: {:?}", op.node(), pair),
               }
            },
            ExprKind::UnaryOp(op, expr) => {
                let val = self.visit_expr_kind(expr.deref_node_mut())
                    .into_node()
                    .into_int_value();
                let val = match op.node() {
                    // REVIEW: nsw, nuw neg
                    UnaryOp::Negate => self.builder.build_int_neg(val, "neg"),
                    UnaryOp::Not => self.builder.build_not(val, "not"),
                };

                VisitOutcome::new_without_visit(BasicValueEnum::from(val))
            },
            ExprKind::Var(name) => VisitOutcome::new(*self.values.get(name).unwrap_or_else(|| panic!("ICE: Var {}", name))),
            ExprKind::If(cond, block, opt_elif) => {
                let start_block = self.builder.get_insert_block().expect("ICE");
                let fn_val = start_block.get_parent().expect("ICE");
                let then_block = self.context.append_basic_block(fn_val, "if_then_block");
                let else_block = self.context.append_basic_block(fn_val, "if_else_block");
                let exit_block = self.context.append_basic_block(fn_val, "if_exit_block");

                // self.builder.position_at_end(start_block);

                let cond = self.visit_expr_kind(cond.deref_node_mut()).into_node().into_int_value();

                self.builder.build_conditional_branch(cond, then_block, else_block);
                self.builder.position_at_end(then_block);

                self.visit_block_ext(block);
                self.builder.build_unconditional_branch(exit_block);

                self.builder.position_at_end(else_block);

                if let Some(elif) = opt_elif {
                    assert!(matches!(elif.deref_node_mut(), ExprKind::If(..) | ExprKind::Block(..)));

                    self.visit_expr_kind(elif.deref_node_mut());
                }

                self.builder.build_unconditional_branch(exit_block);
                self.builder.position_at_end(exit_block);

                VisitOutcome::default().without_visit()
            },
            ExprKind::WhileLoop(cond, block) => {
                let start_block = self.builder.get_insert_block().expect("ICE");
                let fn_val = start_block.get_parent().expect("ICE");
                let cond_block = self.context.append_basic_block(fn_val, "while_cond_block");
                let then_block = self.context.append_basic_block(fn_val, "while_then_block");
                let exit_block = self.context.append_basic_block(fn_val, "while_exit_block");

                self.builder.build_unconditional_branch(cond_block);
                self.builder.position_at_end(cond_block);

                let cond = self.visit_expr_kind(cond.deref_node_mut()).into_node().into_int_value();

                self.builder.build_conditional_branch(cond, then_block, exit_block);
                self.builder.position_at_end(then_block);

                self.visit_block_ext(block);

                self.builder.build_unconditional_branch(cond_block);
                self.builder.position_at_end(exit_block);

                VisitOutcome::default().without_visit()
            },
            ExprKind::Assign(lhs, rhs) => {
                let lhs = self.visit_expr_kind(lhs.deref_node_mut()).into_node().into_pointer_value();
                let rhs = self.visit_expr_kind(rhs.deref_node_mut()).into_node();

                self.builder.build_store(lhs, rhs);

                VisitOutcome::default().without_visit()
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
