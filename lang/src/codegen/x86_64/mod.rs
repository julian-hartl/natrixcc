use std::collections::{HashMap, HashSet};

use anyhow::Result;
use iced_x86::{Code, Decoder, DecoderOptions, Formatter, Instruction, IntelFormatter, MemoryOperand, NumberBase, Register};
use iced_x86::code_asm::*;

use fusion_compiler::bug;

use crate::lir;
use crate::lir::{ConstOp, InstructionKind, OperandKind, PlaceIdx, Terminator, Type};

pub struct CallingConvention<'a>(&'a lir::Function);

impl<'a> CallingConvention<'a> {
    pub fn new(function: &'a lir::Function) -> Self {
        Self(function)
    }
}

pub struct X86_64Codegen {
    asm: CodeAssembler,
    allocator: Allocator,
    labels: HashMap<lir::BasicBlockIdx, CodeLabel>,
}

impl X86_64Codegen {
    pub fn new() -> Self {
        Self {
            asm: CodeAssembler::new(64).unwrap(),
            allocator: Allocator::default(),
            labels: HashMap::new(),
        }
    }

    pub fn gen(&mut self, lir: &lir::LIR) -> Result<()> {
        for (function_idx, function) in lir.functions.indexed_iter() {
            let mut function_label = self.asm.create_label();
            self.asm.set_label(&mut function_label)?;
            for bb_idx in function.basic_blocks.iter().copied() {
                let label = self.labels.entry(bb_idx).or_insert_with(|| self.asm.create_label());
                self.asm.set_label(label)?;
                let bb = &lir.basic_blocks[bb_idx];
                for instr in &bb.instructions {
                    match &instr.kind {
                        InstructionKind::Add {
                            target,
                            lhs,
                            rhs
                        } => {
                            assert_eq!(lhs.ty.layout(), rhs.ty.layout());
                            let target_loc = self.allocator.get_location_or_alloc(&lir.places[*target]);
                            match (&lhs.kind, &rhs.kind) {
                                (OperandKind::Deref(l_place), OperandKind::Deref(r_place)) => {
                                    // Determine destination and source
                                    //
                                    // 1. `lhs` is equal to `target` (a = a + b)
                                    // => we can override the old value of `a` => use `lhs` as the destination
                                    // and `rhs` as the source
                                    //
                                    // 2. `rhs` is equal to `target` (b = a + b)
                                    // => reorder to b = b + a and apply 1)
                                    //
                                    // 3. `lhs`, `rhs` & `target` are distinct (a = b + c)
                                    // => we need to move `b` to `a` and add `c` to `a`
                                    // mov a, b
                                    // add a, c
                                    // => the value in `a` now is `b` + `c`
                                    let (dest, src) = if l_place == target {
                                        (*l_place, *r_place)
                                    } else if r_place == target {
                                        (*r_place, *l_place)
                                    } else {
                                        // todo: swap l & r if l is in a register, but r is not

                                        let l_place_loc = *self.allocator.get_location_or_panic(l_place);
                                        self.mov(&target_loc, &l_place_loc)?;
                                        (*target, *r_place)
                                    };

                                    let dest_loc = *self.allocator.get_location_or_panic(&dest);
                                    let src_loc = *self.allocator.get_location_or_panic(&src);
                                    self.add(&dest_loc, &src_loc)?;
                                }
                                (OperandKind::Const(_), OperandKind::Const(_)) => {
                                    bug!("This operation should have been evaluated during constant propagation");
                                }
                                (OperandKind::Deref(l_place), OperandKind::Const(r_const)) => {
                                    let dest_loc = *self.allocator.get_location_or_panic(l_place);
                                    self.add_const(&dest_loc, r_const)?;
                                }
                                (OperandKind::Const(l_const), OperandKind::Deref(r_place)) => {
                                    let dest_loc = *self.allocator.get_location_or_panic(r_place);
                                    self.add_const(&dest_loc, l_const)?;
                                }
                            }
                        }
                        InstructionKind::Sub { .. } => unimplemented!(),
                        InstructionKind::AddrOf { .. } => unimplemented!(),
                        InstructionKind::AllocInit { value, target } => {
                            let target = &lir.places[*target];
                            let target_loc = self.allocator.alloc_place(target);
                            match &value.kind {
                                OperandKind::Deref(place) => {
                                    let loc = *self.allocator.get_location_or_panic(place);
                                    self.mov(&target_loc, &loc)?;
                                }
                                OperandKind::Const(const_op) => {
                                    match const_op {
                                        ConstOp::Int32(value) => {
                                            self.move_i32(target_loc, *value)?;
                                        }
                                    }
                                }
                            }
                        }
                        InstructionKind::Gt { target, lhs, rhs } => {
                            let target_loc = self.allocator.get_location_or_alloc(&lir.places[*target]);
                            match (&lhs.kind, &rhs.kind) {
                                (OperandKind::Deref(l_place), OperandKind::Deref(r_place)) => {
                                    let l_loc = *self.allocator.get_location_or_panic(l_place);
                                    let r_loc = *self.allocator.get_location_or_panic(r_place);
                                    match (l_loc, r_loc) {
                                        (Location::Register(l_reg), Location::Register(r_reg)) => {
                                            self.asm.add_instruction(
                                                Instruction::with2(
                                                    Code::Cmp_r64_rm64,
                                                    l_reg,
                                                    r_reg,
                                                )?
                                            )?;
                                        }
                                        (Location::Register(l_reg), Location::Stack(r_entry_idx)) => {
                                            self.asm.add_instruction(
                                                Instruction::with2(
                                                    Code::Cmp_r64_rm64,
                                                    l_reg,
                                                    self.mem_op_from_stack_entry(r_entry_idx),
                                                )?
                                            )?;
                                        }
                                        (Location::Stack(l_entry_idx), Location::Register(r_reg)) => {
                                            self.asm.add_instruction(
                                                Instruction::with2(
                                                    Code::Cmp_rm64_r64,
                                                    self.mem_op_from_stack_entry(l_entry_idx),
                                                    r_reg,
                                                )?
                                            )?;
                                        }
                                        (Location::Stack(l_entry_idx), Location::Stack(r_entry_idx)) => {
                                            let (l_reg, spilled) = self.allocator.move_stack_to_free_reg(
                                                l_entry_idx
                                            );
                                            if let Some(spilled) = spilled {
                                                self.do_spilling(spilled)?;
                                            }
                                            self.asm.add_instruction(
                                                Instruction::with2(
                                                    Code::Cmp_rm64_r64,
                                                    l_reg,
                                                    self.mem_op_from_stack_entry(r_entry_idx),
                                                )?
                                            )?;
                                        }
                                    }
                                }
                                (OperandKind::Const(_), OperandKind::Const(_)) => {
                                    bug!("This operation should have been evaluated during constant propagation");
                                }
                                (OperandKind::Deref(l_place), OperandKind::Const(const_op)) |
                                (OperandKind::Const(const_op), OperandKind::Deref(l_place)) => {
                                    let l_loc = *self.allocator.get_location_or_panic(l_place);
                                    match l_loc {
                                        Location::Register(reg) => {
                                            match *const_op {
                                                ConstOp::Int32(value) => {
                                                    self.asm.add_instruction(
                                                        Instruction::with2(
                                                            Code::Cmp_rm64_imm32,
                                                            reg,
                                                            value,
                                                        )?
                                                    )?;
                                                }
                                            }
                                        }
                                        Location::Stack(entry_idx) => {
                                            match *const_op {
                                                ConstOp::Int32(value) => {
                                                    self.asm.add_instruction(
                                                        Instruction::with2(
                                                            Code::Cmp_rm64_imm32,
                                                            self.mem_op_from_stack_entry(entry_idx),
                                                            value,
                                                        )?
                                                    )?;
                                                }
                                            }
                                        }
                                    }
                                }
                            };
                            let (flag_reg, spilled) = self.allocator.get_free_reg(Type::Int8);
                            if let Some(spilled) = spilled {
                                self.do_spilling(spilled)?;
                            }
                            self.asm.add_instruction(
                                Instruction::with1(
                                    Code::Setg_rm8,
                                    flag_reg,
                                )?
                            )?;
                            // todo: revert spilling
                            self.allocator.free_register(flag_reg);

                            let (target_reg, spilled) = self.allocator.ensure_in_register(*target)?;
                            if let Some(spilled) = spilled {
                                self.do_spilling(spilled)?;
                            }

                            self.asm.add_instruction(
                                Instruction::with2(
                                    Code::Movzx_r64_rm8,
                                    target_reg,
                                    flag_reg,
                                )?
                            )?;
                        }
                    }
                }

                match bb.terminator.as_ref() {
                    None => bug!("Basic block {:?} has no terminator", bb_idx),
                    Some(terminator) => {
                        match terminator {
                            Terminator::Return { value } => {
                                if let Some(value) = value {
                                    match &value.kind {
                                        OperandKind::Deref(place) => {
                                            let loc = self.allocator.get_location_or_panic(place);
                                            match loc {
                                                Location::Register(reg) => {
                                                    self.asm.add_instruction(
                                                        Instruction::with2(
                                                            Code::Mov_r64_rm64,
                                                            Register::RAX,
                                                            *reg,
                                                        )?
                                                    )?;
                                                }
                                                Location::Stack(entry_idx) => {
                                                    self.asm.add_instruction(
                                                        Instruction::with2(
                                                            Code::Mov_r64_rm64,
                                                            Register::RAX,
                                                            self.mem_op_from_stack_entry(*entry_idx),
                                                        )?
                                                    )?;
                                                }
                                            }
                                        }
                                        OperandKind::Const(const_op) => {
                                            match const_op {
                                                ConstOp::Int32(value) => {
                                                    self.asm.add_instruction(
                                                        Instruction::with2(
                                                            Code::Mov_rm64_imm32,
                                                            Register::RAX,
                                                            *value,
                                                        )?
                                                    )?;
                                                }
                                            }
                                        }
                                    }
                                }
                                self.asm.ret()?;
                            }
                            Terminator::Jump { target } => {
                                let target_label = self.labels.entry(*target).or_insert_with(|| self.asm.create_label());
                                self.asm.jmp(*target_label)?;
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn add_const(&mut self, dest_loc: &Location, r_const: &ConstOp) -> Result<()> {
        match r_const {
            ConstOp::Int32(value) => {
                match dest_loc {
                    Location::Register(reg) => {
                        self.asm.add_instruction(
                            Instruction::with2(
                                Code::Add_rm64_imm32,
                                *reg,
                                *value,
                            )?
                        )?;
                    }
                    Location::Stack(entry_idx) => {
                        self.asm.add_instruction(
                            Instruction::with2(
                                Code::Add_rm64_imm32,
                                self.mem_op_from_stack_entry(*entry_idx),
                                *value,
                            )?
                        )?;
                    }
                }
            }
        }
        Ok(())
    }

    fn add(&mut self, dest_loc: &Location, src_loc: &Location) -> Result<()> {
        // Determine which instruction variant to use
        //
        // 1. `dest` in reg && `src` in reg: Add_r64_rm64
        //
        // 2. `dest` in reg && `src` in mem: Add_r64_rm64
        //
        // 3. `dest` in mem && `src` in reg: Add_rm64_r64
        //
        // 4. `dest` in mem && `src` in mem
        // Move `dest` to reg, apply 2)
        match (dest_loc, src_loc) {
            (Location::Register(dest_reg), Location::Register(src_reg)) => {
                self.asm.add_instruction(
                    Instruction::with2(
                        Code::Add_r64_rm64,
                        *dest_reg,
                        *src_reg,
                    )?
                )?;
            }
            (Location::Register(dest_reg), Location::Stack(src_entry_idx)) => {
                self.asm.add_instruction(
                    Instruction::with2(
                        Code::Add_r64_rm64,
                        *dest_reg,
                        self.mem_op_from_stack_entry(*src_entry_idx),
                    )?
                )?;
            }
            (Location::Stack(dest_entry_idx), Location::Register(src_reg)) => {
                self.asm.add_instruction(
                    Instruction::with2(
                        Code::Add_rm64_r64,
                        self.mem_op_from_stack_entry(*dest_entry_idx),
                        *src_reg,
                    )?
                )?;
            }
            (Location::Stack(dest_entry_idx), Location::Stack(src_entry_idx)) => {
                let (dest_reg, spilled) = self.allocator.move_stack_to_free_reg(
                    *dest_entry_idx
                );
                if let Some(spilled) = spilled {
                    self.do_spilling(spilled)?;
                }
                self.asm.add_instruction(
                    Instruction::with2(
                        Code::Add_rm64_r64,
                        dest_reg,
                        self.mem_op_from_stack_entry(*src_entry_idx),
                    )?
                )?;
            }
        }
        Ok(())
    }

    fn mov(&mut self, dest: &Location, src: &Location) -> Result<()> {
        match (dest, src) {
            (Location::Register(dest_reg), Location::Register(src_reg)) => {
                self.asm.add_instruction(
                    Instruction::with2(
                        Code::Mov_r64_rm64,
                        *dest_reg,
                        *src_reg,
                    )?
                )?;
            }
            (Location::Register(dest_reg), Location::Stack(src_entry_idx)) => {
                self.asm.add_instruction(
                    Instruction::with2(
                        Code::Mov_r64_rm64,
                        *dest_reg,
                        self.mem_op_from_stack_entry(*src_entry_idx),
                    )?
                )?;
            }
            (Location::Stack(dest_entry_idx), Location::Register(src_reg)) => {
                self.asm.add_instruction(
                    Instruction::with2(
                        Code::Mov_rm64_r64,
                        self.mem_op_from_stack_entry(*dest_entry_idx),
                        *src_reg,
                    )?
                )?;
            }
            (Location::Stack(dest_entry_idx), Location::Stack(src_entry_idx)) => {
                let (dest_reg, spilled) = self.allocator.move_stack_to_free_reg(
                    *dest_entry_idx
                );
                if let Some(spilled) = spilled {
                    self.do_spilling(spilled)?;
                }
                self.asm.add_instruction(
                    Instruction::with2(
                        Code::Mov_rm64_r64,
                        dest_reg,
                        self.mem_op_from_stack_entry(*src_entry_idx),
                    )?
                )?;
            }
        }
        Ok(())
    }

    fn move_i32(&mut self, loc: Location, value: i32) -> Result<()> {
        match loc {
            Location::Register(reg) => {
                self.asm.add_instruction(
                    Instruction::with2(
                        Code::Mov_rm64_imm32,
                        reg,
                        value,
                    )?
                )?;
            }
            Location::Stack(entry_idx) => {
                assert_eq!(entry_idx, self.allocator.frame.entries.len() - 1);
                self.asm.add_instruction(
                    Instruction::with2(
                        Code::Pushq_imm32,
                        self.mem_op_from_stack_entry(entry_idx),
                        value,
                    )?
                )?;
            }
        };
        Ok(())
    }

    // fn alloc_i64(&mut self, value: i64) -> Result<Location> {
    //     let loc = self.allocator.alloc(Type::Int64);
    //     self.move_i64(loc, value)?;
    //     Ok(loc)
    // }

    fn move_i64(&mut self, loc: Location, value: i64) -> Result<()> {
        match loc {
            Location::Register(reg) => {
                self.asm.add_instruction(
                    Instruction::with2(
                        Code::Mov_r64_imm64,
                        reg,
                        value,
                    )?
                )?;
            }
            Location::Stack(entry_idx) => {
                assert_eq!(entry_idx, self.allocator.frame.entries.len() - 1);
                // Split the value into two 32 bit values
                let (low, high) = (value as u32, (value >> 32) as u32);
                self.asm.add_instruction(
                    Instruction::with2(
                        Code::Pushq_imm32,
                        self.mem_op_from_stack_entry(entry_idx),
                        low,
                    )?
                )?;
                self.asm.add_instruction(
                    Instruction::with2(
                        Code::Pushq_imm32,
                        self.mem_op_from_stack_entry(entry_idx),
                        high,
                    )?
                )?;
            }
        };
        Ok(())
    }

    fn do_spilling(&mut self, (_, reg, stack_idx): Spilled) -> Result<()> {
        // todo: adjust for smaller types
        self.asm.add_instruction(Instruction::with2(
            Code::Mov_rm64_r64,
            self.mem_op_from_stack_entry(stack_idx),
            reg,
        )?)?;
        Ok(())
    }

    // fn revert_spilling(&mut self, spilled: Spilled) -> Result<()> {
    //     let (_, reg, stack_idx) = spilled;
    //     self.asm.add_instruction(Instruction::with2(
    //         Code::Mov_r64_rm64,
    //         reg,
    //         self.mem_op_from_stack_entry(stack_idx),
    //     )?)?;
    //     Ok(())
    // }

    fn mem_op_from_stack_entry(&self, stack_entry_idx: usize) -> MemoryOperand {
        let stack_offset = self.allocator.frame.get_entry(stack_entry_idx).offset;
        MemoryOperand::with_base_displ(
            Register::RBP,
            stack_offset as i64,
        )
    }

    pub fn get_asm_output(&mut self) -> Result<String> {
        let bytes = self.asm.assemble(0)?;
        let mut decoder =
            Decoder::with_ip(64, &bytes, 0, DecoderOptions::NONE);
        let mut formatter = IntelFormatter::new();
        formatter.options_mut().set_number_base(NumberBase::Decimal);
        let mut temp_instr_output = String::new();
        let mut output = String::new();
        let mut instruction = Instruction::default();
        while decoder.can_decode() {
            decoder.decode_out(&mut instruction);
            temp_instr_output.clear();
            formatter.format(&instruction, &mut temp_instr_output);
            output.push_str(&temp_instr_output);
            output.push('\n');
        }
        Ok(output)
    }

    // fn gen_function(&mut self, lir: &lir::LIR, function_idx: lir::FunctionIdx, function: &lir::Function) -> Result<CodeLabel> {
    //
    //     Ok(function_label)
    // }
}

#[derive(Debug, Clone, Default)]
struct Allocator {
    frame: StackFrame,
    places: HashMap<PlaceIdx, Location>,
    used_registers: HashSet<Register>,
}

type Spilled = (PlaceIdx, Register, usize);

impl Allocator {
    pub fn alloc_place(&mut self, place: &lir::Place) -> Location {
        let location = self.alloc(place.ty);
        self.places.insert(place.idx, location);
        location
    }

    pub fn ensure_in_register(&mut self, place: PlaceIdx) -> Result<(Register, Option<Spilled>)> {
        Ok(match self.get_location(&place).copied() {
            None => {
                anyhow::bail!("Cannot ensure place {:?} is in register because it is not allocated", place);
            }
            Some(location) => {
                match location {
                    Location::Register(reg) => {
                        (reg, None)
                    }
                    Location::Stack(entry_idx) => {
                        self.move_stack_to_free_reg(entry_idx)
                    }
                }
            }
        })
    }

    pub fn move_stack_to_free_reg(&mut self, entry_idx: usize) -> (Register, Option<Spilled>) {
        let entry = self.frame.get_entry(entry_idx);
        let entry_ty = entry.ty;
        let (reg, spilled) = self.get_free_reg(entry_ty);
        self.free_stack(entry_idx);
        self.use_register(reg);
        let place = self.places.iter().find(
            |(_, loc)| match loc {
                Location::Register(_) => false,
                Location::Stack(idx) => *idx == entry_idx
            }
        ).map(|(place, _)| *place);
        if let Some(place) = place {
            self.places.insert(place, Location::Register(reg));
        }
        if let Some((spilled_place, _, spilled_to)) = spilled {
            self.places.insert(spilled_place, Location::Stack(spilled_to));
        }
        (reg, spilled)
    }

    fn get_free_reg(&mut self, ty: Type) -> (Register, Option<Spilled>) {
        let mut spilled = None;
        let reg = self.find_register_for_ty(ty).unwrap_or_else(
            || {
                let res = self.find_register_to_spill(ty);
                spilled = Some(res);
                res.1
            }
        );
        (reg, spilled)
    }

    fn find_register_to_spill(&mut self, ty: Type) -> Spilled {
        let (reg_to_spill, place_to_spill) = self.places.iter().find_map(|(place, loc)| {
            match loc {
                Location::Register(reg) => {
                    if reg.size() == ty.layout().size {
                        return Some((*reg, *place));
                    }
                    None
                }
                Location::Stack(_) => None
            }
        }).unwrap();
        let stack_idx = self.alloc_stack(ty);
        return (place_to_spill, reg_to_spill, stack_idx);
    }

    pub fn alloc(&mut self, ty: Type) -> Location {
        let reg = self.find_register_for_ty(ty);
        match reg {
            None => {
                let offset = self.alloc_stack(ty);
                Location::Stack(offset)
            }
            Some(reg) => {
                self.use_register(reg);
                Location::Register(reg)
            }
        }
    }

    fn find_register_for_ty(&mut self, ty: Type) -> Option<Register> {
        for reg in Register::values().filter(
            // todo: once we use lower than 64 bit integers, we need to check if the smaller registers are available
            |reg| !self.used_registers.contains(reg) && reg.is_gpr() && reg.size() == ty.layout().size
        ) {
            self.used_registers.insert(reg);
            return Some(reg);
        }
        None
    }

    fn use_register(&mut self, reg: Register) {
        self.used_registers.insert(reg);
    }

    fn alloc_stack(&mut self, ty: Type) -> usize {
        self.frame.alloc(ty)
    }

    pub fn get_location(&self, place: &PlaceIdx) -> Option<&Location> {
        self.places.get(place)
    }
    pub fn get_location_or_panic(&self, place: &PlaceIdx) -> &Location {
        match self.places.get(place) {
            None => {
                bug!("{:?} has not yet been allocated.", place);
            }
            Some(loc) => loc
        }
    }

    pub fn get_location_or_alloc(&mut self, place: &lir::Place) -> Location {
        self.places.get(&place.idx).copied().unwrap_or_else(|| {
            self.alloc_place(place)
        })
    }

    pub fn free_place(&mut self, place: &lir::Place) -> Result<()> {
        match self.places.get(&place.idx).copied() {
            None => {
                anyhow::bail!("Cannot free place {:?} because it is not allocated", place);
            }
            Some(location) => {
                self.free_location(&location);
                self.places.remove(&place.idx);
            }
        }
        Ok(())
    }

    pub fn free_location(&mut self, location: &Location) {
        match location {
            Location::Register(reg) => {
                self.used_registers.remove(reg);
            }
            Location::Stack(idx) => {
                self.free_stack(*idx)
            }
        }
    }

    fn free_stack(&mut self, idx: usize) {
        self.frame.dealloc(idx)
    }

    fn free_register(&mut self, reg: Register) {
        self.used_registers.remove(&reg);
    }
}

#[derive(Debug, Clone, Copy)]
enum Location {
    Register(Register),
    /// The n-th entry in the stack frame. 0 is bottom.
    Stack(usize),
}


#[derive(Debug, Clone, Default)]
struct StackFrame {
    entries: Vec<StackFrameEntry>,
    offset: usize,
}

impl StackFrame {
    pub fn alloc(&mut self, ty: Type) -> usize {
        let entry_index = self.entries.len();
        self.entries.push(StackFrameEntry {
            ty,
            offset: self.offset,
            live: true,
        });
        self.offset += ty.layout().size;
        entry_index
    }

    pub fn dealloc(&mut self, entry_idx: usize) {
        self.cascading_free(entry_idx)
    }

    /// See [`StackFrame::free`].
    ///
    /// In addition to freeing the entry at the given index, it also frees all dead entries above it.
    ///
    /// E.g.:
    ///
    /// ```text
    /// -----------------
    /// | 0 | live
    /// | 1 | live
    /// | 2 | live
    ///
    /// -----------------
    ///
    /// cascading_free(1)
    ///
    /// -----------------
    /// | 0 | live
    /// | 1 | dead
    /// | 2 | live
    /// -----------------
    ///
    /// cascading_free(2)
    ///
    /// -----------------
    /// | 0 | live
    /// -----------------
    ///
    /// ```
    ///
    fn cascading_free(&mut self, mut idx: usize) {
        while self.free(idx) {
            idx -= 1;
        }
    }

    /// Free the stack frame entry at the given index.
    ///
    /// It sets the entry to not live and removes it from the stack frame and adjusts the offset as long as it is the last entry.
    ///
    /// Returns true if the entry was removed from the stack frame, false otherwise.
    fn free(&mut self, idx: usize) -> bool {
        if idx != self.entries.len() - 1 {
            return false;
        }
        let entry = &mut self.entries[idx];
        if !entry.live {
            return false;
        }
        entry.live = false;
        self.offset -= entry.ty.layout().size;
        self.entries.pop();
        true
    }

    pub fn get_entry(&self, idx: usize) -> &StackFrameEntry {
        self.entries.get(idx).unwrap()
    }
}

#[derive(Debug, Clone)]
struct StackFrameEntry {
    ty: Type,
    offset: usize,
    live: bool,
}

