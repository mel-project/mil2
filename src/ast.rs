mod scope;

use anyhow::Context;

use derivative::Derivative;
use ethnum::U256;
use smol_str::SmolStr;
use std::sync::{Arc, RwLock};
use tap::Tap;

use crate::util::{List, Map};

use self::scope::Scope;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ast {
    BinOp(BinOp, Arc<Self>, Arc<Self>),
    Call(Arc<Self>, List<Self>),
    Letrec(Map<SmolStr, Self>, Arc<Self>),
    Lambda(List<SmolStr>, Arc<Self>),
    List(List<Self>),
    IfThenElse(Arc<Self>, Arc<Self>, Arc<Self>),
    Number(U256),
    Var(SmolStr),
}

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub enum Value {
    Number(U256),
    List(List<Self>),
    Lambda(List<SmolStr>, Ast, Scope),
}

impl Ast {
    /// Evaluates the AST, given the mapping
    pub fn eval(&self, env: &Scope) -> anyhow::Result<Value> {
        match self {
            Ast::BinOp(op, x, y) => {
                let x = x.eval(env)?;
                let y = y.eval(env)?;
                match (x, y, op) {
                    (Value::Number(x), Value::Number(y), BinOp::Add) => {
                        Ok(Value::Number(x.wrapping_add(y)))
                    }
                    (Value::Number(x), Value::Number(y), BinOp::Sub) => {
                        Ok(Value::Number(x.wrapping_sub(y)))
                    }
                    (Value::Number(x), Value::Number(y), BinOp::Div) => {
                        Ok(Value::Number(x.wrapping_div(y)))
                    }
                    (Value::Number(x), Value::Number(y), BinOp::Mul) => {
                        Ok(Value::Number(x.wrapping_mul(y)))
                    }
                    (op, x, y) => anyhow::bail!("cannot apply {:?} to {:?} and {:?}", op, x, y),
                }
            }
            Ast::Call(f, args) => {
                let args: Result<List<_>, _> = args.iter().map(|v| v.eval(env)).collect();
                let args = args?;
                if let Value::Lambda(arg_names, body, mut orig_env) = f.eval(env)? {
                    if args.len() != arg_names.len() {
                        anyhow::bail!("called function with wrong arity")
                    }
                    for (arg_name, arg) in arg_names.into_iter().zip(args.into_iter()) {
                        orig_env.insert(arg_name, arg);
                    }
                    body.eval(&orig_env)
                } else {
                    anyhow::bail!("cannot call a non-function")
                }
            }
            Ast::Letrec(bindings, inner) => {
                let mut env = env.clone();
                // first, we insert mutable dummies into the environment
                for (k, _) in bindings.iter() {
                    env.insert(k.clone(), Value::Number(U256::ZERO));
                }
                // then, we actually evaluate the bindings and mutate the cells
                for (k, v) in bindings.iter() {
                    let res = v.eval(&env)?;
                    env.mutate(k, res);
                }
                // now, we evaluate the body with the right env
                inner.eval(&env)
            }
            Ast::Lambda(args, body) => Ok(Value::Lambda(
                args.clone(),
                body.as_ref().clone(),
                env.clone().tap_mut(|env| env.weaken()),
            )),
            Ast::List(v) => {
                let v: Result<List<_>, _> = v.iter().map(|v| v.eval(env)).collect();
                let v = v?;
                Ok(Value::List(v))
            }
            Ast::IfThenElse(condition, t_case, f_case) => {
                let condition = condition.eval(env)?;
                if matches!(condition, Value::Number(U256::ZERO)) {
                    f_case.eval(env)
                } else {
                    t_case.eval(env)
                }
            }
            Ast::Number(num) => Ok(Value::Number(*num)),
            Ast::Var(s) => {
                let res = env.get(s).context(format!("no such variable: {s}"))?;
                eprintln!("resolving {s} as {:?}", res);
                Ok(res)
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper function to create an environment with number values.
    fn num_env(vals: Vec<(&str, u64)>) -> Scope {
        let mut env = Scope::new();
        for (name, value) in vals {
            env.insert(SmolStr::new(name), Value::Number(U256::from(value)));
        }
        env
    }

    // Helper function to compare Value::Number for equality in tests.
    fn assert_number(value: Value, expected: u64) {
        if let Value::Number(num) = value {
            assert!(num == U256::from(expected))
        } else {
            panic!("Expected Value::Number, found {:?}", value);
        }
    }

    #[test]
    fn test_simple_arithmetic() {
        let env = num_env(vec![("x", 10), ("y", 5)]);

        let expr = Ast::BinOp(
            BinOp::Add,
            Ast::Var("x".into()).into(),
            Ast::Var("y".into()).into(),
        );

        assert_number(expr.eval(&env).unwrap(), 15);
    }

    #[test]
    fn test_recursive_function() {
        let env = num_env(vec![]);

        let factorial = Ast::Letrec(
            vec![(
                "fact".into(),
                Ast::Lambda(
                    vec!["n".into()].into(),
                    Ast::IfThenElse(
                        Ast::Var("n".into()).into(),
                        Ast::BinOp(
                            BinOp::Mul,
                            Ast::Var("n".into()).into(),
                            Ast::Call(
                                Ast::Var("fact".into()).into(),
                                vec![Ast::BinOp(
                                    BinOp::Sub,
                                    Ast::Var("n".into()).into(),
                                    Ast::Number(U256::from(1u32)).into(),
                                )]
                                .into(),
                            )
                            .into(),
                        )
                        .into(),
                        Ast::Number(U256::from(1u32)).into(),
                    )
                    .into(),
                ),
            )]
            .into_iter()
            .collect(),
            Ast::Call(
                Ast::Var("fact".into()).into(),
                vec![Ast::Number(U256::from(5u32))].into(),
            )
            .into(),
        );

        assert_number(dbg!(factorial.eval(&env).unwrap()), 120);
    }

    #[test]
    fn test_mutually_recursive_functions() {
        let mutual_recursion = Ast::Letrec(
            vec![
                (
                    "is_even".into(),
                    Ast::Lambda(
                        vec!["n".into()].into(),
                        Ast::IfThenElse(
                            Ast::Var("n".into()).into(),
                            Ast::Call(
                                Ast::Var("is_odd".into()).into(),
                                vec![Ast::BinOp(
                                    BinOp::Sub,
                                    Ast::Var("n".into()).into(),
                                    Ast::Number(U256::from(1u32)).into(),
                                )]
                                .into(),
                            )
                            .into(),
                            Ast::Number(U256::from(1u32)).into(),
                        )
                        .into(),
                    ),
                ),
                (
                    "is_odd".into(),
                    Ast::Lambda(
                        vec!["n".into()].into(),
                        Ast::IfThenElse(
                            Ast::Var("n".into()).into(),
                            Ast::Call(
                                Ast::Var("is_even".into()).into(),
                                vec![Ast::BinOp(
                                    BinOp::Sub,
                                    Ast::Var("n".into()).into(),
                                    Ast::Number(U256::from(1u32)).into(),
                                )]
                                .into(),
                            )
                            .into(),
                            Ast::Number(U256::from(0u32)).into(),
                        )
                        .into(),
                    ),
                ),
            ]
            .into_iter()
            .collect(),
            Ast::Call(
                Ast::Var("is_even".into()).into(),
                vec![Ast::Number(U256::from(4u32))].into(),
            )
            .into(),
        );
        assert_number(mutual_recursion.eval(&Scope::new()).unwrap(), 1);
    }
}
