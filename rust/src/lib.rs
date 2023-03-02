use std::fmt::Debug;
use std::{collections::HashMap, rc::Rc};

type Name = String;

#[derive(Clone)]
enum Term {
    Var(String),
    Con(i32),
    Add(Box<Term>, Box<Term>),
    Lam(Name, Box<Term>),
    App(Box<Term>, Box<Term>),
}

struct M<A> {
    a: Rc<A>,
}

impl<A> M<A> {
    fn unit_M(a: &Rc<A>) -> M<A> {
        M { a: a.clone() }
    }
    fn unit__M(a: A) -> M<A> {
        M { a: Rc::new(a) }
    }
    fn bind_M<B>(m: &M<A>, f: impl Fn(&A) -> M<B>) -> M<B> {
        f(&m.a)
    }
}

struct FunBox(Rc<dyn Fn(Value) -> M<Value>>);

enum Value {
    Wrong,
    Num(i32),
    Fun(FunBox),
}

impl Value {
    fn new_fun(f: impl Fn(Value) -> M<Value> + 'static) -> Self {
        Self::Fun(FunBox(Rc::new(f)))
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Wrong => Self::Wrong,
            Self::Num(arg0) => Self::Num(arg0.clone()),
            Self::Fun(_arg0) => Self::Wrong,
        }
    }
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Self::Num(l0), Self::Num(r0)) => l0 == r0,
            (Self::Fun(_l0), Self::Fun(_r0)) => panic!("boom"),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Wrong => write!(f, "Wrong"),
            Self::Num(arg0) => f.debug_tuple("Num").field(arg0).finish(),
            Self::Fun(_arg0) => write!(f, "Fun(...)"),
        }
    }
}

type Environment = HashMap<Name, Rc<Value>>;
use Term::*;
use Value::*;

struct Interpreter;

impl Interpreter {
    fn interp(t: &Box<Term>, e: &Environment) -> M<Value> {
        match t.as_ref() {
            Var(name) => Self::lookup(name, e),
            Con(i) => M::unit__M(Num(i.clone())),
            Add(a, b) => Self::add_M(&Self::interp(a, e), &Self::interp(b, e)),
            Lam(x, tt) => {
                let fun = Self::interp_fun(x, tt, e);
                M::unit__M(fun)
            }
            App(f, t) => Self::apply_M(&Self::interp(f, e), &Self::interp(t, e)),
        }
    }

    fn interp_fun(x: &String, t: &Box<Term>, e: &Environment) -> Value {
        let x1 = x.clone();
        let t1 = t.clone();
        let e1 = e.clone();
        let fun = Value::new_fun(move |xx| {
            let fw = Rc::new(xx);
            let mut e2 = e1.clone();
            e2.insert(x1.clone(), fw);
            Self::interp(&t1, &e2)
        });
        fun
    }

    fn add_M(a: &M<Value>, b: &M<Value>) -> M<Value> {
        M::bind_M(a, |a| M::bind_M(b, |b| Self::add_value(a, b)))
    }

    fn add_value(a: &Value, b: &Value) -> M<Value> {
        let result = match (a, b) {
            (Num(a), Num(b)) => Num(a + b),
            _ => Wrong,
        };
        M::unit__M(result)
    }

    fn apply_M(f: &M<Value>, t: &M<Value>) -> M<Value> {
        M::bind_M(f, |f| M::bind_M(t, |t| Self::apply(f, t)))
    }

    fn apply(f: &Value, t: &Value) -> M<Value> {
        match f {
            Fun(fun) => (*fun.0)(t.clone()),
            _ => M::unit__M(Wrong),
        }
    }

    fn lookup(name: &String, e: &Environment) -> M<Value> {
        let fw = e
            .get(name)
            .map(|v| M::unit_M(v))
            .unwrap_or(M::unit__M(Wrong));
        fw
    }
}

#[cfg(test)]
mod tests {

    use crate::Environment;

    use super::Interpreter;
    use super::Term;
    use super::Term::*;
    use super::Value::*;

    fn term42() -> Term {
        let x = "x";
        App(
            Box::new(Lam(
                x.to_string(),
                Box::new(Add(
                    Box::new(Var(x.to_string())),
                    Box::new(Var(x.to_string())),
                )),
            )),
            Box::new(Add(Box::new(Con(10)), Box::new(Con(11)))),
        )
    }

    #[test]
    fn first() {
        let t = Box::new(term42());
        let e = Environment::new();
        let actual = Interpreter::interp(&t, &e);
        assert_eq!(actual.a.as_ref(), &Num(42));
    }
}
