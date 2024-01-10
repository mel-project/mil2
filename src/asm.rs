use ahash::AHashMap;
use anyhow::Context;
use melvm::opcode::OpCode;
use smol_str::SmolStr;

pub enum Asm {
    // control flow
    Label(SmolStr),
    Jmp(SmolStr),
    Bnz(SmolStr),
    Bez(SmolStr),
    DynJmp,

    // variables
    StoreImm(SmolStr),
    LoadImm(SmolStr),

    // ops
    Add,
    Sub,
    Mul,
    Div,
    Rem,
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
        let diff = dest_pc - (pc as i64 + 1);
        let diff = i16::try_from(diff).context("jump is too long")?;
        anyhow::Ok(diff)
    };
    for asm in asm {
        match asm {
            Asm::Label(_) => {}
            Asm::Jmp(tgt) => {
                output.push(OpCode::Jmp(rel_tgt(pc, tgt)?));
                pc += 1;
            }
            Asm::Bnz(tgt) => {
                output.push(OpCode::Bnz(rel_tgt(pc, tgt)?));
                pc += 1;
            }
            Asm::Bez(tgt) => {
                output.push(OpCode::Bez(rel_tgt(pc, tgt)?));
                pc += 1;
            }
            Asm::DynJmp => {
                output.push(OpCode::DynJmp);
                pc += 1;
            }
            Asm::StoreImm(var) => {
                output.push(OpCode::StoreImm(*var_to_heap.get(var).unwrap()));
                pc += 1;
            }
            Asm::LoadImm(var) => {
                output.push(OpCode::LoadImm(*var_to_heap.get(var).unwrap()));
                pc += 1;
            }
            Asm::Add => {
                output.push(OpCode::Add);
                pc += 1;
            }
            Asm::Sub => {
                output.push(OpCode::Sub);
                pc += 1;
            }
            Asm::Mul => {
                output.push(OpCode::Mul);
                pc += 1;
            }
            Asm::Div => {
                output.push(OpCode::Div);
                pc += 1;
            }
            Asm::Rem => {
                output.push(OpCode::Rem);
                pc += 1;
            }
        }
    }
    Ok(output)
}
