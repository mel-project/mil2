use either::Either;
use smol_str::SmolStr;

use crate::{
    asm::Asm,
    mil::{BinOp, Mil, TriOp, UniOp},
    util::{gensym, Map},
};

/// Compiles Mil to assembly.
#[tracing::instrument]
pub fn compile_mil(mil: Mil) -> anyhow::Result<Vec<Asm>> {
    // first, we eliminate all closures
    let mil = mil.enclose()?;
    // then, we name all the variables uniquely
    let mil = deshadow(&mil, &Map::new());
    // then we emit everything
    let mut buffer = vec![];
    emit_asm(&mil, &mut buffer);
    Ok(buffer)
}

#[tracing::instrument]
fn emit_asm(mil: &Mil, buffer: &mut Vec<Asm>) {
    match mil {
        Mil::UniOp(opcode, inner) => {
            emit_asm(inner, buffer);
            match opcode {
                UniOp::Bnot => buffer.push(Asm::Bnot),
                UniOp::Vlen => buffer.push(Asm::Vlen),
                UniOp::Blen => buffer.push(Asm::Blen),
                UniOp::TypeQ => buffer.push(Asm::TypeQ),
            }
        }
        Mil::BinOp(opcode, left, right) => {
            emit_asm(right, buffer);
            emit_asm(left, buffer);
            buffer.push(match opcode {
                BinOp::Add => Asm::Add,
                BinOp::Sub => Asm::Sub,
                BinOp::Mul => Asm::Mul,
                BinOp::Div => Asm::Div,
                BinOp::Vref => Asm::Vref,
                BinOp::Bref => Asm::Bref,
                BinOp::Vcons => Asm::Vcons,
                BinOp::Vappend => Asm::Vappend,
                BinOp::Rem => Asm::Vappend,
                BinOp::Exp => Asm::Exp,
                BinOp::Bor => Asm::Bor,
                BinOp::Band => Asm::Band,
                BinOp::Bxor => Asm::Bxor,
                BinOp::Eql => Asm::Eql,
                BinOp::Lt => Asm::Lt,
                BinOp::Gt => Asm::Gt,

                BinOp::Rshift => Asm::Shr,
                BinOp::Lshift => Asm::Shl,
                BinOp::Bappend => Asm::Bappend,
            })
        }
        Mil::TriOp(opcode, a, b, c) => {
            emit_asm(c, buffer);
            emit_asm(b, buffer);
            emit_asm(a, buffer);
            buffer.push(match opcode {
                TriOp::Vupdate => Asm::Vupdate,
                TriOp::Vslice => Asm::Vslice,
                TriOp::Bupdate => Asm::Bupdate,
                TriOp::Bslice => Asm::Bslice,
            })
        }
        Mil::Call(f, args) => {
            // to call a function on the top of the stack, we first push the return address, then push the arguments in reverse order
            let retaddr_label = gensym("$raddr");
            buffer.push(Asm::PushI(Either::Left(retaddr_label.clone())));
            for arg in args.iter().rev() {
                emit_asm(arg, buffer);
            }
            // then, we place the address of the function on the top of the stack
            emit_asm(f, buffer);
            // then, we call the function by dynjmp
            buffer.push(Asm::DynJmp);
            // the return address is here
            buffer.push(Asm::Label(retaddr_label));
            // okay, after we return, we get the return value by loading the return value register
            buffer.push(Asm::LoadImm("$ret".into()));
        }
        Mil::Letrec(bindings, inner) => {
            // for each of the bindings, we insert them into the "heap"
            for (var_name, binding) in bindings {
                emit_asm(binding, buffer);
                buffer.push(Asm::StoreImm(var_name.clone()));
            }
            // then, we emit the code for inner, which now would have access to the right things
            emit_asm(inner, buffer);
        }
        Mil::Let(var_name, binding, inner) => {
            emit_asm(binding, buffer);
            buffer.push(Asm::StoreImm(var_name.clone()));
            emit_asm(inner, buffer);
        }
        Mil::Lambda(args, inner) => {
            // this is relatively tricky! The following is very naive.
            // First, we jump to the end, since we do not execute the function right now.
            let end_label = gensym("$fun_end");
            buffer.push(Asm::Jmp(end_label.clone()));
            // We then make a label for the beginning of the function:
            let fun_label = gensym("$fun_start");
            buffer.push(Asm::Label(fun_label.clone()));
            // this is the actual code of the function:
            {
                // We then generate code to pop the arguments off the stack, "naming" them appropriately
                for arg in args {
                    buffer.push(Asm::StoreImm(arg.clone()));
                }
                // We then generate code for the the interior of the function
                emit_asm(inner, buffer);
                // Now, the return value of the function is at the top of the stack. We pop it and stick it into the retval register instead
                buffer.push(Asm::StoreImm("$ret".into()));
                // Now what's at the top of the stack is the return address. We dynjmp back.
                buffer.push(Asm::DynJmp);
            }
            buffer.push(Asm::Label(end_label.clone()));
            // We then push the address of the function to the stack
            buffer.push(Asm::PushI(Either::Left(fun_label)))
        }
        Mil::List(lst) => {
            if lst.is_empty() {
                buffer.push(Asm::Vempty)
            } else {
                // recurse
                let (head, tail) = lst.clone().split_at(1);
                emit_asm(&Mil::List(tail), buffer);
                emit_asm(&head[0], buffer);
                buffer.push(Asm::Vcons);
            }
        }
        Mil::IfThenElse(cond, then_case, else_case) => {
            let end_label = gensym("$end");
            let else_label = gensym("$else");
            emit_asm(cond, buffer);
            buffer.push(Asm::Bez(else_label.clone()));
            emit_asm(then_case, buffer);
            buffer.push(Asm::Jmp(end_label.clone()));
            buffer.push(Asm::Label(else_label));
            emit_asm(else_case, buffer);
            buffer.push(Asm::Label(end_label));
        }
        Mil::Number(num) => buffer.push(Asm::PushI(Either::Right(*num))),
        Mil::Var(var) => buffer.push(Asm::LoadImm(var.clone())),
    }
}

#[tracing::instrument]
fn deshadow(mil: &Mil, local_to_unique: &Map<SmolStr, SmolStr>) -> Mil {
    tracing::trace!(
        "deshadowing {:?} with {} variables",
        mil,
        local_to_unique.len()
    );
    match mil {
        Mil::UniOp(op, inner) => Mil::UniOp(*op, deshadow(inner, local_to_unique).into()),
        Mil::BinOp(op, left, right) => Mil::BinOp(
            *op,
            deshadow(left, local_to_unique).into(),
            deshadow(right, local_to_unique).into(),
        ),
        Mil::TriOp(op, a, b, c) => Mil::TriOp(
            *op,
            deshadow(a, local_to_unique).into(),
            deshadow(b, local_to_unique).into(),
            deshadow(c, local_to_unique).into(),
        ),
        Mil::Call(fun, args) => Mil::Call(
            deshadow(fun, local_to_unique).into(),
            args.iter()
                .map(|arg| deshadow(arg, local_to_unique))
                .collect(),
        ),
        Mil::Letrec(bindings, inner) => {
            let mut local_to_unique = local_to_unique.clone();
            for key in bindings.keys() {
                local_to_unique.insert(key.clone(), gensym(key));
            }
            let new_bindings = bindings
                .iter()
                .map(|(var, value)| {
                    let new_var = local_to_unique[var].clone();
                    (new_var, deshadow(value, &local_to_unique))
                })
                .collect();
            let new_inner = deshadow(inner, &local_to_unique);
            Mil::Letrec(new_bindings, new_inner.into())
        }
        Mil::Let(var, value, inner) => {
            let new_value = deshadow(value, local_to_unique);
            let mut local_to_unique = local_to_unique.clone();
            let new_var = gensym(var);
            local_to_unique.insert(var.clone(), new_var.clone());
            let new_inner = deshadow(inner, &local_to_unique);
            Mil::Let(new_var, new_value.into(), new_inner.into())
        }
        Mil::Lambda(args, inner) => {
            let mut local_to_unique = local_to_unique.clone();
            for arg in args {
                local_to_unique.insert(arg.clone(), gensym(arg));
            }
            let new_inner = deshadow(inner, &local_to_unique);
            Mil::Lambda(
                args.iter()
                    .map(|arg| local_to_unique[arg].clone())
                    .collect(),
                new_inner.into(),
            )
        }
        Mil::List(lst) => Mil::List(lst.iter().map(|v| deshadow(v, local_to_unique)).collect()),
        Mil::IfThenElse(condition, then_case, else_case) => {
            let condition = deshadow(condition, local_to_unique);
            let then_case = deshadow(then_case, local_to_unique);
            let else_case = deshadow(else_case, local_to_unique);
            Mil::IfThenElse(condition.into(), then_case.into(), else_case.into())
        }
        Mil::Number(number) => Mil::Number(*number),
        Mil::Var(var) => Mil::Var(
            local_to_unique
                .get(var)
                .cloned()
                .unwrap_or_else(|| SmolStr::from("!!invalid!!")),
        ),
    }
}

#[cfg(test)]
mod tests {
    use ethnum::U256;
    use tracing_test::traced_test;

    use crate::{
        asm::assemble,
        compile::compile_mil,
        mil::{BinOp, Mil},
    };

    fn test_compile(mil: Mil, assert_result: melvm::Value) {
        let compiled = compile_mil(mil).unwrap();
        for compiled in compiled.iter() {
            eprintln!("{compiled}")
        }

        eprintln!();

        let assembled = assemble(&compiled).unwrap();
        for (ptr, assembled) in assembled.iter().enumerate() {
            eprintln!("{ptr}\t{}", assembled);
        }

        let result = melvm::Covenant::from_ops(&assembled)
            .debug_execute(&[], 1000)
            .unwrap();
        assert_eq!(result.0, assert_result);
    }

    #[traced_test]
    #[test]
    fn simple_compile() {
        let one_plus_one = Mil::BinOp(
            BinOp::Add,
            Mil::Number(U256::ONE).into(),
            Mil::Number(U256::ONE).into(),
        );
        test_compile(one_plus_one, melvm::Value::Int(2u64.into()))
    }

    #[traced_test]
    #[test]
    fn simple_list_compile() {
        let list = Mil::List(vec![Mil::Number(U256::ONE), Mil::Number(U256::ZERO)].into());
        test_compile(
            list,
            melvm::Value::Vector(
                vec![
                    melvm::Value::Int(1u64.into()),
                    melvm::Value::Int(0u64.into()),
                ]
                .into(),
            ),
        )
    }

    #[traced_test]
    #[test]
    fn simple_fun_compile() {
        let call = Mil::Call(
            Mil::Lambda(vec!["x".into()].into(), Mil::Var("x".into()).into()).into(),
            vec![Mil::Number(U256::ONE)].into(),
        );
        test_compile(call, melvm::Value::Int(1u64.into()))
    }

    #[test]
    fn function_returning_function_compile() {
        // make_adder is a function that takes 'x' and returns a new function
        // that takes 'y' and returns the sum of 'x' and 'y'.
        let make_adder = Mil::Lambda(
            vec!["x".into()].into(),
            Mil::Lambda(
                vec!["y".into()].into(),
                Mil::BinOp(
                    BinOp::Add,
                    Mil::Var("x".into()).into(),
                    Mil::Var("y".into()).into(),
                )
                .into(),
            )
            .into(),
        );

        // We make two separate adders, which must behave differently.
        let add_five = Mil::Call(
            Mil::Var("make_adder".into()).into(),
            vec![Mil::Number(U256::from(5u32))].into(),
        );
        let add_six = Mil::Call(
            Mil::Var("make_adder".into()).into(),
            vec![Mil::Number(U256::from(6u32))].into(),
        );

        // Let's call both with the argument 10.
        let call_add_both = Mil::Let(
            "make_adder".into(),
            make_adder.into(),
            Mil::Let(
                "add_six".into(),
                add_six.into(),
                Mil::Let(
                    "add_five".into(),
                    add_five.into(),
                    Mil::Call(
                        Mil::Var("add_six".into()).into(),
                        vec![Mil::Call(
                            Mil::Var("add_five".into()).into(),
                            vec![Mil::Number(U256::from(10u32))].into(),
                        )]
                        .into(),
                    )
                    .into(),
                )
                .into(),
            )
            .into(),
        );
        test_compile(call_add_both, melvm::Value::Int(21u64.into()))
    }
}
