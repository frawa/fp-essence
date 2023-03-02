mod fp_essence;

use fp_essence::{Interpreter, Term, Term::*};

fn main() {
    let t = Box::new(term42());
    let tested = Interpreter::test(&t);
    println!("{} for {}, aka {:?}", tested, t, t)
}

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
