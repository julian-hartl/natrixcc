use iced_x86::code_asm::{CodeAssembler, rbp, rsp};
use iced_x86::OpCodeOperandKind;
use iced_x86::OpCodeOperandKind::{al, eax};
use rustc_hash::FxHashMap;

use crate::cfg::{BasicBlock, Terminator};
use crate::function::Function;
use crate::instruction::{Instr, InstrKind, Place};
use crate::module::Module;
use crate::ty::Type;

pub struct X86_64Backend {
    asm: CodeAssembler,
    allocator: Allocator,
}

impl X86_64Backend {
    pub fn new() -> Self {
        Self {
            asm: CodeAssembler::new(64).unwrap(),
            allocator: Allocator::new(),
        }
    }

    pub fn emit_module(&mut self, module: &Module) {
        for function in module.functions.indices() {
            self.emit_function(module, function);
        }
    }

    pub fn emit_function(&mut self, module: &Module, function: Function) {
        self.asm.push(rbp).unwrap();
        self.asm.mov(rbp, rsp).unwrap();
        for bb in module.functions[function].cfg.basic_blocks.indices() {
            self.emit_basic_block(module, function, bb);
        }
        self.asm.mov(rsp, rbp).unwrap();
        self.asm.pop(rbp).unwrap();
    }

    pub fn emit_basic_block(&mut self, module: &Module, function: Function, bb: BasicBlock) {
        let bb_data = module.functions[function].cfg.basic_blocks[bb].as_ref();
        match bb_data {
            Some(bb_data) => {
                for instr in bb_data.instructions.iter().copied() {
                    self.emit_instr(module, function, instr);
                }
                self.emit_terminator(module, function, &bb_data.terminator);
            }
            None => {}
        }
    }

    pub fn emit_instr(&mut self, module: &Module, function: Function, instr: Instr) {
        let function = &module.functions[function];
        match &function.cfg.instructions[instr].kind {
            InstrKind::Alloca(alloca) => {
                let allocation_size = alloca.total_size();
                self.asm.sub(rsp, allocation_size as i32).unwrap();
                self.allocator.alloc_on_stack(alloca.place, &alloca.ty);
            }
            InstrKind::Sub(sub) => {
                let temp_reg = match &function.cfg.places[sub.place].ty {
                    Type::I8 => al,
                    Type::I32 => eax,
                    Type::ZeroSized |
                    Type::Ptr(_) => {
                        unreachable!()
                    }
                };
                // self.asm.mov(temp_reg, sub.lhs).unwrap();
            }
            InstrKind::Op(op) => {
                unimplemented!("Use stack allocations")
            }
            InstrKind::Store(store) => {
                unimplemented!()
                // self.asm.store(store.place, store.value);
            }
            InstrKind::Load(load) => {
                unimplemented!()
                // self.asm.load(load.place, load.source);
            }
            InstrKind::Phi(_) => {
                unimplemented!()
            }
        }
    }

    pub fn emit_terminator(&mut self, module: &Module, function: Function, terminator: &Terminator) {
        unimplemented!()
        // match terminator {
        //     Terminator::Ret(ret) => {
        //         self.asm.ret(ret.value);
        //     }
        //     Terminator::Br(br) => {
        //         self.asm.br(br.target);
        //     }
        //     Terminator::CondBr(cond_br) => {
        //         self.asm.cond_br(cond_br.condition, cond_br.true_target, cond_br.false_target);
        //     }
        // }
    }
}

struct Allocator {
    locations: FxHashMap<Place, Location>,
    sf: StackFrame,
}

impl Allocator {
    pub fn new() -> Self {
        Self {
            sf: StackFrame::new(),
            locations: FxHashMap::default(),
        }
    }

    pub fn alloc_on_stack(&mut self, place: Place, ty: &Type) {
        let size = ty.size();
        self.locations.insert(place, Location::Stack(self.sf.total_size()));
        self.sf.push(size);
    }

    pub fn get_location(&self, place: Place) -> &Location {
        &self.locations[&place]
    }
}

enum Location {
    Stack(usize),
    Register(iced_x86::Register),
}

struct StackFrame {
    size: usize,
}

impl StackFrame {
    pub fn new() -> Self {
        Self {
            size: 0,
        }
    }

    pub fn push(&mut self, size: usize) {
        self.size += size;
    }

    pub fn pop(&mut self, size: usize) {
        self.size -= size;
    }

    pub fn total_size(&self) -> usize {
        self.size
    }
}
