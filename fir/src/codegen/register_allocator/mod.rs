mod greedy;

use crate::codegen::machine::{Abi, VirtualRegister};
use crate::ty::Type;

pub trait RegisterAllocator<A: Abi> {
    fn new() -> Self;

    fn allocate(&mut self, vreg: VirtualRegister, ty: Type) -> A::REG;

    fn get(&self, vreg: VirtualRegister) -> A::REG;
    
    fn live(&mut self, reg: A::REG);
    
    fn kill(&mut self, reg: A::REG);
}