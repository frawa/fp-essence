use std::fmt::{Debug, Display};
use std::{collections::HashMap, rc::Rc};

type Name = String;

#[derive(Clone)]
pub enum Term {
    Var(String),
    Con(i32),
    Add(Box<Term>, Box<Term>),
    Lam(Name, Box<Term>),
    App(Box<Term>, Box<Term>),
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Var(x) => write!(f, "{}", x),
            Con(x) => write!(f, "{}", x),
            Add(a, b) => write!(f, "({}+{})", a, b),
            Lam(x, t) => write!(f, "(\\{}->{})", x, t),
            App(ff, t) => write!(f, "{} {}", ff, t),
        }
    }
}

impl Debug for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Var(x) => write!(f, "Var({:?})", x),
            Con(x) => write!(f, "Con({:?})", x),
            Add(a, b) => write!(f, "Add({:?},{:?})", a, b),
            Lam(x, t) => write!(f, "Lam({:?},{:?})", x, t),
            App(ff, t) => write!(f, "App({:?},{:?})", ff, t),
        }
    }
}

trait TheMonad {
    type TheA;
    type TheM<B>: TheMonad;

    fn unit_rc(a: &Rc<Value>) -> Self::TheM<Value>;
    fn unit(a: Self::TheA) -> Self;
    fn bind<B, F>(&self, f: F) -> Self::TheM<B>
    where
        F: Fn(&Self::TheA) -> Self::TheM<B>;

    fn show(v: &Self::TheM<Value>) -> String;
}

struct I<A> {
    a: Rc<A>,
}

impl<A> TheMonad for I<A> {
    type TheA = A;
    type TheM<B> = I<B>;

    fn unit_rc(a: &Rc<Value>) -> Self::TheM<Value> {
        I { a: a.clone() }
    }
    fn unit(a: Self::TheA) -> Self {
        I { a: Rc::new(a) }
    }
    fn bind<B, F>(&self, f: F) -> Self::TheM<B>
    where
        F: Fn(&A) -> Self::TheM<B>,
    {
        f(&self.a)
    }

    fn show(v: &Self::TheM<Value>) -> String {
        Value::showval(&v.a)
    }
}

// TODO change currently used TheMonad impl here
type M<A> = I<A>;
struct FunBox(Box<dyn Fn(Value) -> M<Value>>);

enum Value {
    Wrong,
    Num(i32),
    Fun(FunBox),
}

impl Value {
    fn new_fun(f: impl Fn(Value) -> M<Value> + 'static) -> Self {
        let fw1 = f;
        let fw2 = Box::new(fw1);
        Self::Fun(FunBox(fw2))
    }

    fn showval(v: &Value) -> String {
        match v {
            Wrong => "Wrong".to_string(),
            Num(x) => x.to_string(),
            Fun(_) => "<fun>".to_string(),
        }
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

use Term::*;
use Value::*;

type Environment = HashMap<Name, Rc<Value>>;

pub struct Interpreter();

impl Interpreter {
    pub fn test(t: &Box<Term>) -> String {
        let e = Environment::new();
        let v = Interpreter::interp(t, &e); //.bind(|v| M::unit(*v));
        <M<Value>>::show(&v)
    }

    fn interp(t: &Box<Term>, e: &Environment) -> <I<Value> as TheMonad>::TheM<Value> {
        match t.as_ref() {
            Var(name) => Interpreter::lookup(name, e),
            Con(i) => M::unit(Num(i.clone())),
            Add(a, b) => Interpreter::add(&Interpreter::interp(a, e), &Interpreter::interp(b, e)),
            Lam(x, tt) => {
                let fun = Interpreter::interp_fun(x, tt, e);
                M::unit(fun)
            }
            App(f, t) => Interpreter::apply(&Interpreter::interp(f, e), &Interpreter::interp(t, e)),
        }
    }

    fn interp_fun(x: &String, t: &Box<Term>, e: &Environment) -> Value {
        let x = x.clone(); // TODO clone less
        let t = t.clone();
        let e = e.clone();

        let fun = Value::new_fun(move |v| {
            let mut e = e.clone();
            let fw1 = x.clone();
            let fw2 = Rc::new(v);
            e.insert(fw1, fw2);
            Interpreter::interp(&t, &e)
        });
        fun
    }

    fn add(a: &M<Value>, b: &M<Value>) -> M<Value> {
        a.bind(|a| b.bind(|b| Interpreter::add_value(a, b)))
    }

    fn add_value(a: &Value, b: &Value) -> M<Value> {
        let result = match (a, b) {
            (Num(a), Num(b)) => Num(a + b),
            _ => Wrong,
        };
        M::unit(result)
    }

    fn apply(f: &M<Value>, t: &M<Value>) -> M<Value> {
        f.bind(|f| t.bind(|t| Interpreter::apply_value(f, t)))
    }

    fn apply_value(f: &Value, t: &Value) -> M<Value> {
        match f {
            Fun(fun) => (*fun.0)(t.clone()),
            _ => M::unit(Wrong),
        }
    }

    fn lookup(name: &String, e: &Environment) -> M<Value> {
        e.get(name)
            .map(|v| <M<Value>>::unit_rc(v))
            .unwrap_or(M::unit(Wrong))
    }
}

#[cfg(test)]
mod tests {

    use super::Interpreter;
    use super::Term;
    use super::Term::*;

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
        let actual = Interpreter::test(&t);
        assert_eq!(actual, "42");
    }
}
