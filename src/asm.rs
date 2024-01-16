use std::fmt::Display;

use ahash::AHashMap;
use anyhow::Context;
use either::Either;
use ethnum::U256;
use melvm::opcode::OpCode;
use smol_str::SmolStr;

#[derive(Clone, Debug)]
pub enum Asm {
    // control flow
    Label(SmolStr),
    Jmp(SmolStr),
    Bnz(SmolStr),
    Bez(SmolStr),
    DynJmp,
    PushI(Either<SmolStr, U256>),

    // variables
    StoreImm(SmolStr),
    LoadImm(SmolStr),

    // ops
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Exp,
    BwOr,
    BwXor,

    Vref,
    Vcons,
    Vempty,
    Vappend,
}

impl Display for Asm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let dbg_repr = format!("{:?}", self);
        dbg_repr
            .replace('(', " ")
            .replace(')', "")
            .replace("Left", "")
            .replace("Right", "")
            .to_lowercase()
            .fmt(f)
    }
}

/// Assembles a slice of assembly instructions into melvm.
pub fn assemble(asm: &[Asm]) -> anyhow::Result<Vec<OpCode>> {
    let mut label_to_pc = AHashMap::new();
    let mut var_to_heap = AHashMap::new();
    let mut heap_ptr = 100u16;
    let mut pc = 0usize;
    // populate the maps
    for asm in asm {
        match asm {
            Asm::Label(label) => {
                label_to_pc.insert(label.clone(), pc);
            }
            Asm::StoreImm(var) | Asm::LoadImm(var) => {
                if !var_to_heap.contains_key(var) {
                    var_to_heap.insert(var.clone(), heap_ptr);
                    heap_ptr = heap_ptr.checked_add(1).context("too many variables")?;
                }
                pc += 1;
            }
            _ => pc += 1,
        }
    }

    // translation
    let mut output = vec![];
    pc = 0;
    let rel_tgt = |pc: usize, tgt: &str| {
        let dest_pc = *label_to_pc.get(tgt).context("undefined label")? as i64;
        let diff = dest_pc - (pc as i64) - 1;
        let diff = i16::try_from(diff).context("jump is too long")?;
        anyhow::Ok(diff)
    };
    for asm in asm {
        match asm {
            Asm::Label(_) => continue,
            Asm::Jmp(tgt) => output.push(OpCode::Jmp(rel_tgt(pc, tgt)?)),
            Asm::Bnz(tgt) => output.push(OpCode::Bnz(rel_tgt(pc, tgt)?)),
            Asm::Bez(tgt) => output.push(OpCode::Bez(rel_tgt(pc, tgt)?)),
            Asm::DynJmp => output.push(OpCode::DynJmp),
            Asm::PushI(Either::Left(tgt)) => output.push(OpCode::PushI(U256::from(
                *label_to_pc.get(tgt).context("undefined label")? as u64,
            ))),
            Asm::PushI(Either::Right(val)) => output.push(OpCode::PushI(*val)),
            Asm::StoreImm(var) => output.push(OpCode::StoreImm(*var_to_heap.get(var).unwrap())),
            Asm::LoadImm(var) => output.push(OpCode::LoadImm(*var_to_heap.get(var).unwrap())),
            Asm::Add => output.push(OpCode::Add),
            Asm::Sub => output.push(OpCode::Sub),
            Asm::Mul => output.push(OpCode::Mul),
            Asm::Div => output.push(OpCode::Div),
            Asm::Rem => output.push(OpCode::Rem),
            Asm::Exp => output.push(OpCode::Exp(u8::MAX)),
            Asm::Vref => output.push(OpCode::VRef),
            Asm::Vcons => output.push(OpCode::VCons),
            Asm::Vempty => output.push(OpCode::VEmpty),
            Asm::Vappend => output.push(OpCode::VAppend),
            Asm::BwXor => output.push(OpCode::Xor),
            Asm::BwOr => output.push(OpCode::Or),
        }
        pc += 1;
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use either::Either;
    use melvm::opcode::OpCode;

    use crate::asm::assemble;

    use super::Asm;

    #[test]
    fn test_assembly() {
        let asm = vec![
            Asm::Jmp("hello".into()),
            Asm::Label("hello".into()),
            Asm::PushI(Either::Left("hello".into())),
        ];
        assert_eq!(
            assemble(&asm).unwrap(),
            vec![OpCode::Jmp(0), OpCode::PushI(1u64.into())]
        );
    }
}
