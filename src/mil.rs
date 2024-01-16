mod scope;

use anyhow::Context;

use derivative::Derivative;
use ethnum::U256;
use smol_str::SmolStr;
use std::sync::Arc;
use tap::Tap;

use crate::util::{gensym, List, Map, Set};

use self::scope::Scope;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Mil {
    BinOp(BinOp, Arc<Self>, Arc<Self>),
    Call(Arc<Self>, List<Self>),
    Letrec(Map<SmolStr, Self>, Arc<Self>),
    Let(SmolStr, Arc<Self>, Arc<Self>),
    Lambda(List<SmolStr>, Arc<Self>),

    List(List<Self>),

    IfThenElse(Arc<Self>, Arc<Self>, Arc<Self>),
    Number(U256),
    Var(SmolStr),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
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
    Vappend,
}

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub enum Value {
    Number(U256),
    List(List<Self>),
    Lambda(List<SmolStr>, Mil, #[derivative(Debug = "ignore")] Scope),
}

impl Mil {
    /// Creates closures for all lambdas.
    #[tracing::instrument]
    pub fn enclose(&self) -> anyhow::Result<Self> {
        tracing::trace!("removing captured variables from lambdas");
        match self {
            Mil::BinOp(binop, left, right) => Ok(Mil::BinOp(
                *binop,
                left.enclose()?.into(),
                right.enclose()?.into(),
            )),
            Mil::Call(fun, args) => {
                let enclosed_fun = fun.enclose()?;
                let temp = gensym("$callsite");
                let to_call = Mil::BinOp(
                    BinOp::Vref,
                    Mil::Var(temp.clone()).into(),
                    Mil::Number(U256::ZERO).into(),
                );
                let environment = Mil::BinOp(
                    BinOp::Vref,
                    Mil::Var(temp.clone()).into(),
                    Mil::Number(U256::ONE).into(),
                );
                Ok(Mil::Let(
                    temp.clone(),
                    enclosed_fun.into(),
                    Mil::Call(
                        to_call.into(),
                        args.iter()
                            .map(|a| a.enclose())
                            .chain(std::iter::once(anyhow::Ok(environment)))
                            .collect::<Result<_, _>>()?,
                    )
                    .into(),
                ))
            }
            Mil::Letrec(bindings, body) => Ok(Mil::Letrec(
                bindings
                    .iter()
                    .map(|(k, v)| anyhow::Ok((k.clone(), v.enclose()?)))
                    .collect::<Result<_, _>>()?,
                body.enclose()?.into(),
            )),
            Mil::Let(var, binding, body) => Ok(Mil::Let(
                var.clone(),
                binding.enclose()?.into(),
                body.enclose()?.into(),
            )),
            Mil::Lambda(args, inner) => {
                // find free variables
                let free_vars = self.free_vars(&Set::new());
                // the new lambda has an extra "env" that is a list of all free variables here
                let env_name = gensym("$env");
                let new_lambda = Mil::Lambda(
                    args.iter()
                        .cloned()
                        .chain(std::iter::once(env_name.clone()))
                        .collect(),
                    free_vars.iter().enumerate().fold(
                        inner.enclose()?.into(),
                        |inner, (var_idx, var)| {
                            Mil::Let(
                                var.clone(),
                                Mil::BinOp(
                                    BinOp::Vref,
                                    Mil::Var(env_name.clone()).into(),
                                    Mil::Number(U256::from(var_idx as u64)).into(),
                                )
                                .into(),
                                inner,
                            )
                            .into()
                        },
                    ),
                );
                let env_value = Mil::List(free_vars.into_iter().map(Mil::Var).collect());
                Ok(Mil::List(vec![new_lambda, env_value].into()))
            }
            Mil::List(list) => Ok(Mil::List(
                list.iter().map(|x| x.enclose()).collect::<Result<_, _>>()?,
            )),
            Mil::IfThenElse(cond, then_branch, else_branch) => Ok(Mil::IfThenElse(
                cond.enclose()?.into(),
                then_branch.enclose()?.into(),
                else_branch.enclose()?.into(),
            )),
            Mil::Number(_) => Ok(self.clone()),
            Mil::Var(_) => Ok(self.clone()),
        }
    }

    /// Finds all the free variables in the AST.
    pub fn free_vars(&self, bound_vars: &Set<SmolStr>) -> Set<SmolStr> {
        match self {
            Mil::BinOp(_, left, right) => {
                let mut vars = left.free_vars(bound_vars);
                vars.extend(right.free_vars(bound_vars));
                vars
            }
            Mil::Call(fun, args) => {
                let mut vars = fun.free_vars(bound_vars);
                for arg in args {
                    vars.extend(arg.free_vars(bound_vars));
                }
                vars
            }
            Mil::Letrec(bindings, body) => {
                let mut new_bound_vars = bound_vars.clone();
                for (var, _) in bindings {
                    new_bound_vars.insert(var.clone());
                }
                let mut vars = Set::new();
                for (_, expr) in bindings {
                    vars.extend(expr.free_vars(&new_bound_vars));
                }
                vars.extend(body.free_vars(&new_bound_vars));
                vars
            }
            Mil::Let(var, binding, body) => {
                let mut vars = binding.free_vars(bound_vars);
                let mut new_bound_vars = bound_vars.clone();
                new_bound_vars.insert(var.clone());
                vars.extend(body.free_vars(&new_bound_vars));
                vars
            }
            Mil::Lambda(args, body) => {
                let mut new_bound_vars = bound_vars.clone();
                for arg in args {
                    new_bound_vars.insert(arg.clone());
                }
                body.free_vars(&new_bound_vars)
            }
            Mil::List(items) => {
                let mut vars = Set::new();
                for item in items {
                    vars.extend(item.free_vars(bound_vars));
                }
                vars
            }
            Mil::IfThenElse(cond, then_branch, else_branch) => {
                let mut vars = cond.free_vars(bound_vars);
                vars.extend(then_branch.free_vars(bound_vars));
                vars.extend(else_branch.free_vars(bound_vars));
                vars
            }
            Mil::Number(_) => Set::new(),
            Mil::Var(var) => {
                if bound_vars.contains(var) {
                    Set::new()
                } else {
                    let mut vars = Set::new();
                    vars.insert(var.clone());
                    vars
                }
            }
        }
    }

    /// Evaluates the AST, given the mapping
    #[tracing::instrument(skip(env))]
    pub fn eval(&self, env: &Scope) -> anyhow::Result<Value> {
        tracing::trace!("entering eval");
        match self {
            Mil::BinOp(op, x, y) => {
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
                    (Value::List(vec), Value::Number(i), BinOp::Vref) => {
                        Ok(vec.get(i.as_usize()).context("out of bounds")?.clone())
                    }
                    (x, y, op) => anyhow::bail!("cannot apply {:?} to {:?} and {:?}", op, x, y),
                }
            }
            Mil::Call(f, args) => {
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
            Mil::Letrec(bindings, inner) => {
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
            Mil::Let(var, binding, inner) => {
                let binding = binding.eval(env)?;
                let mut env = env.clone();
                env.insert(var.clone(), binding);
                inner.eval(&env)
            }
            Mil::Lambda(args, body) => Ok(Value::Lambda(
                args.clone(),
                body.as_ref().clone(),
                env.clone().tap_mut(|env| env.weaken()),
            )),

            Mil::IfThenElse(condition, t_case, f_case) => {
                let condition = condition.eval(env)?;
                if matches!(condition, Value::Number(U256::ZERO)) {
                    f_case.eval(env)
                } else {
                    t_case.eval(env)
                }
            }
            Mil::List(list) => Ok(Value::List(
                list.iter().map(|l| l.eval(env)).collect::<Result<_, _>>()?,
            )),
            Mil::Number(num) => Ok(Value::Number(*num)),
            Mil::Var(s) => {
                let res = env.get(s).context(format!("no such variable: {s}"))?;
                Ok(res)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

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

    #[traced_test]
    #[test]
    fn test_simple_arithmetic() {
        let env = num_env(vec![("x", 10), ("y", 5)]);

        let expr = Mil::BinOp(
            BinOp::Add,
            Mil::Var("x".into()).into(),
            Mil::Var("y".into()).into(),
        );

        assert_number(expr.eval(&env).unwrap(), 15);
    }

    #[traced_test]
    #[test]
    fn test_recursive_function() {
        let env = num_env(vec![]);

        let factorial = Mil::Letrec(
            vec![(
                "fact".into(),
                Mil::Lambda(
                    vec!["n".into()].into(),
                    Mil::IfThenElse(
                        Mil::Var("n".into()).into(),
                        Mil::BinOp(
                            BinOp::Mul,
                            Mil::Var("n".into()).into(),
                            Mil::Call(
                                Mil::Var("fact".into()).into(),
                                vec![Mil::BinOp(
                                    BinOp::Sub,
                                    Mil::Var("n".into()).into(),
                                    Mil::Number(U256::from(1u32)).into(),
                                )]
                                .into(),
                            )
                            .into(),
                        )
                        .into(),
                        Mil::Number(U256::from(1u32)).into(),
                    )
                    .into(),
                ),
            )]
            .into_iter()
            .collect(),
            Mil::Call(
                Mil::Var("fact".into()).into(),
                vec![Mil::Number(U256::from(5u32))].into(),
            )
            .into(),
        );

        assert_number(dbg!(factorial.eval(&env).unwrap()), 120);
    }

    #[traced_test]
    #[test]
    fn test_mutually_recursive_functions() {
        let mutual_recursion = Mil::Letrec(
            vec![
                (
                    "is_even".into(),
                    Mil::Lambda(
                        vec!["n".into()].into(),
                        Mil::IfThenElse(
                            Mil::Var("n".into()).into(),
                            Mil::Call(
                                Mil::Var("is_odd".into()).into(),
                                vec![Mil::BinOp(
                                    BinOp::Sub,
                                    Mil::Var("n".into()).into(),
                                    Mil::Number(U256::from(1u32)).into(),
                                )]
                                .into(),
                            )
                            .into(),
                            Mil::Number(U256::from(1u32)).into(),
                        )
                        .into(),
                    ),
                ),
                (
                    "is_odd".into(),
                    Mil::Lambda(
                        vec!["n".into()].into(),
                        Mil::IfThenElse(
                            Mil::Var("n".into()).into(),
                            Mil::Call(
                                Mil::Var("is_even".into()).into(),
                                vec![Mil::BinOp(
                                    BinOp::Sub,
                                    Mil::Var("n".into()).into(),
                                    Mil::Number(U256::from(1u32)).into(),
                                )]
                                .into(),
                            )
                            .into(),
                            Mil::Number(U256::from(0u32)).into(),
                        )
                        .into(),
                    ),
                ),
            ]
            .into_iter()
            .collect(),
            Mil::Call(
                Mil::Var("is_even".into()).into(),
                vec![Mil::Number(U256::from(4u32))].into(),
            )
            .into(),
        );
        assert_number(mutual_recursion.eval(&Scope::new()).unwrap(), 1);
    }

    #[traced_test]
    #[test]
    fn test_function_returning_function() {
        let env = num_env(vec![]);

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

        // We create a call to make_adder with the argument 5,
        // which should return a function that adds 5 to its argument.
        let add_five = Mil::Call(
            make_adder.clone().into(),
            vec![Mil::Number(U256::from(5u32))].into(),
        );

        // We now have a function that should add 5 to its argument.
        // Let's call this function with the argument 10.
        let call_add_five_with_10 = Mil::Call(
            add_five.clone().into(),
            vec![Mil::Number(U256::from(10u32))].into(),
        );

        // Evaluate the AST and check the result is 15.
        assert_number(call_add_five_with_10.eval(&env).unwrap(), 15);

        // Enclose and evaluate again.
        // eprintln!("unenclosed: {:?}", call_add_five_with_10);
        let enclosed = call_add_five_with_10.enclose().unwrap();
        eprintln!("enclosed: {:?}", enclosed);
        assert_number(enclosed.eval(&env).unwrap(), 15);
    }
}
