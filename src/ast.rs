use anyhow::Context;
use derivative::Derivative;

use ethnum::U256;
use smol_str::SmolStr;
use std::sync::Arc;

use crate::util::{List, Map};

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

#[derive(Clone, Debug)]
pub enum Value {
    Number(U256),
    List(List<Self>),
    Lambda(List<SmolStr>, Ast, Map<SmolStr, Value>),
}

impl Ast {
    /// Evaluates the AST, given the mapping
    pub fn eval(&self, env: &Map<SmolStr, Value>) -> anyhow::Result<Value> {
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
                if let Value::Lambda(arg_names, body, orig_env) = f.eval(env)? {
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
            Ast::Letrec(bindings, _) => {
                let env = env.clone();
                let mut closures = vec![];
                for (k, v) in bindings {
                    let res = v.eval(&env)?;
                    if matches!(&res, Value::Lambda(_, _, _)) {
                        closures.push(k);
                    }
                    env.insert(k.clone(), v.eval(&env)?);
                }
                // this fixes recursion
                for k in closures {
                    if let Value::Lambda(_, _, captured_env) = env.get_mut(k).unwrap() {
                        
                    }
                }
            }
            Ast::Lambda(args, body) => Ok(Value::Lambda(
                args.clone(),
                body.as_ref().clone(),
                env.clone(),
            )),
            Ast::List(v) => {
                let v: Result<List<_>, _> = v.iter().map(|v| v.eval(env)).collect();
                let v = v?;
                Ok(Value::List(v))
            }
            Ast::IfThenElse(condition, t_case, f_case) => {
                let condition = condition.eval(env)?;
                if matches!(condition, Value::Number(U256::ZERO)) {
                    t_case.eval(env)
                } else {
                    f_case.eval(env)
                }
            }
            Ast::Number(num) => Ok(Value::Number(*num)),
            Ast::Var(s) => Ok(env
                .get(s)
                .context(format!("no such variable: {s}"))?
                .clone()),
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
