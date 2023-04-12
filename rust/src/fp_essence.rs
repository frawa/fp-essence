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

pub trait TheMonad {
    type TheA;
    type TheM<B>: TheMonad;

    fn unit_rc(a: &Rc<Value>) -> Self::TheM<Value>;
    fn unit(a: Self::TheA) -> Self;
    fn bind<B, F>(&self, f: F) -> Self::TheM<B>
    where
        F: Fn(&Self::TheA) -> Self::TheM<B>;

    fn show(v: &Self::TheM<Value>) -> String;

    fn wrong(message: &str) -> Self::TheM<Value>;
}

pub struct I<A> {
    a: Rc<A>,
}

impl I<Value> {
    pub fn dummy() -> Self {
        I { a: Rc::new(Wrong) }
    }
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

    fn wrong(_message: &str) -> Self::TheM<Value> {
        Self::unit_rc(&Rc::new(Wrong))
    }
}

impl E<Value> {
    fn _dummy() -> Self {
        E {
            e: Rc::new(Ok(Rc::new(Wrong))),
        }
    }
}

#[derive(Debug)]
pub struct E<A> {
    e: Rc<Result<Rc<A>, String>>,
}

use Result::Ok;

impl<A> TheMonad for E<A> {
    type TheA = A;
    type TheM<B> = E<B>;

    fn unit_rc(a: &Rc<Value>) -> Self::TheM<Value> {
        E {
            e: Rc::new(Ok(a.clone())),
        }
    }
    fn unit(a: Self::TheA) -> Self {
        E {
            e: Rc::new(Ok(Rc::new(a))),
        }
    }
    fn bind<B, F>(&self, f: F) -> Self::TheM<B>
    where
        F: Fn(&A) -> Self::TheM<B>,
    {
        let e = self.e.as_ref();
        match e {
            Ok(a) => f(&a),
            Err(err) => E {
                e: Rc::new(Err(err.clone())),
            },
        }
    }

    fn show(v: &Self::TheM<Value>) -> String {
        let e = v.e.as_ref();
        match e {
            Ok(a) => format!("Success: {}", Value::showval(&a)),
            Err(err) => format!("Error: {}", err),
        }
    }

    fn wrong(message: &str) -> Self::TheM<Value> {
        E {
            e: Rc::new(Err(message.to_string())),
        }
    }
}

// TODO change currently used TheMonad impl here
// type M<A> = I<A>;
type M<A> = E<A>;
pub struct FunBox(Box<dyn Fn(Value) -> M<Value>>);

pub enum Value {
    Wrong,
    Num(i32),
    Fun(FunBox),
}

impl Value {
    fn new_fun(f: impl Fn(Value) -> M<Value> + 'static) -> Self {
        Self::Fun(FunBox(Box::new(f)))
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

pub struct Interpreter<MM: TheMonad>(pub MM);

impl<MM: TheMonad> Interpreter<MM> {
    pub fn test(&self, t: &Box<Term>) -> String {
        let e = Environment::new();
        let v = Self::interp(t, &e); //.bind(|v| M::unit(*v));
        <M<Value>>::show(&v)
    }

    fn interp(t: &Box<Term>, e: &Environment) -> <M<Value> as TheMonad>::TheM<Value> {
        match t.as_ref() {
            Var(name) => Self::lookup(name, e),
            Con(i) => M::unit(Num(i.clone())),
            Add(a, b) => Self::add(&Self::interp(a, e), &Self::interp(b, e)),
            Lam(x, tt) => {
                let fun = Self::interp_fun(x, tt, e);
                M::unit(fun)
            }
            App(f, t) => Self::apply(&Self::interp(f, e), &Self::interp(t, e)),
        }
    }

    fn interp_fun(x: &String, t: &Box<Term>, e: &Environment) -> Value {
        let x = x.clone(); // TODO clone less
        let t = t.clone();
        let e = e.clone();

        let fun = Value::new_fun(move |v| {
            let mut e = e.clone();
            e.insert(x.clone(), Rc::new(v));
            Self::interp(&t, &e)
        });
        fun
    }

    fn add(a: &M<Value>, b: &M<Value>) -> M<Value> {
        a.bind(|a| b.bind(|b| Self::add_value(a, b)))
    }

    fn add_value(a: &Value, b: &Value) -> M<Value> {
        match (a, b) {
            (Num(a), Num(b)) => <M<Value>>::unit(Num(a + b)),
            _ => <M<Value>>::wrong(
                format!(
                    "should be numbers: {}, {}",
                    Value::showval(a).as_str(),
                    Value::showval(b).as_str()
                )
                .as_str(),
            ),
        }
    }

    fn apply(f: &M<Value>, t: &M<Value>) -> M<Value> {
        f.bind(|f| t.bind(|t| Self::apply_value(f, t)))
    }

    fn apply_value(f: &Value, t: &Value) -> M<Value> {
        match f {
            Fun(fun) => (*fun.0)(t.clone()),
            _ => <M<Value>>::wrong(
                format!("should be function: {}", Value::showval(f).as_str()).as_str(),
            ),
        }
    }

    fn lookup(name: &String, e: &Environment) -> M<Value> {
        e.get(name)
            .map(|v| <M<Value>>::unit_rc(v))
            .unwrap_or(<M<Value>>::wrong(format!("missing {}", name).as_str()))
    }
}

#[cfg(test)]
mod tests {

    use super::Interpreter;
    use super::Term;
    use super::Term::*;
    use super::Value;
    use super::E;
    use super::I;

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

    fn term_wrong() -> Term {
        App(Box::new(Con(1)), Box::new(Con(2)))
    }

    #[test]
    #[ignore]
    fn test_i_term42() {
        let t = Box::new(term42());
        let dummy = I::dummy();
        let interpreter = Interpreter::<I<Value>>(dummy);
        let actual = interpreter.test(&t);
        assert_eq!(actual, "42");
    }

    #[test]
    fn test_e_term42() {
        let t = Box::new(term42());
        let dummy = E::_dummy();
        let interpreter = Interpreter::<E<Value>>(dummy);
        let actual = interpreter.test(&t);
        assert_eq!(actual, "Success: 42");
    }

    #[test]
    fn test_e_term_wrong() {
        let t = Box::new(term_wrong());
        let dummy = E::_dummy();
        let interpreter = Interpreter::<E<Value>>(dummy);
        let actual = interpreter.test(&t);
        assert_eq!(actual, "Error: should be function: 1");
    }
}
