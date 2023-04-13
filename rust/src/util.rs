mod util;

// see https://www.fpcomplete.com/blog/monads-gats-nightly-rust/

trait MonoFunctor {
    type Unwrapped; // value "contained inside"
    fn map<F>(self, f: F) -> Self
    where
        F: Fn(Self::Unwrapped) -> Self::Unwrapped;
}

trait Functor {
    type Unwrapped;
    type Wrapped<B>: Functor;

    fn map<F, B>(self, f: F) -> Self::Wrapped<B>
    where
        F: Fn(Self::Unwrapped) -> B;
}

trait Pointed: Functor {
    fn wrap<T>(t: T) -> Self::Wrapped<T>;
}

trait Applicative: Pointed {
    fn lift_a2<F, B, C>(self, b: Self::Wrapped<B>, f: F) -> Self::Wrapped<C>
    where
        F: Fn(Self::Unwrapped, B) -> C;
}

trait Semigroup {
    fn append(self, rhs: Self) -> Self;
}

trait Monad: Applicative {
    fn bind<B, F>(self, f: F) -> Self::Wrapped<B>
    where
        F: FnMut(Self::Unwrapped) -> Self::Wrapped<B>;
}

struct IdentityT<M>(M);

impl<M: Functor> Functor for IdentityT<M> {
    type Unwrapped = M::Unwrapped;
    type Wrapped<A> = IdentityT<M::Wrapped<A>>;

    fn map<F, B>(self, f: F) -> Self::Wrapped<B>
    where
        F: FnMut(M::Unwrapped) -> B,
    {
        IdentityT(self.0.map(f))
    }
}

impl<M: Pointed> Pointed for IdentityT<M> {
    fn wrap<T>(t: T) -> IdentityT<M::Wrapped<T>> {
        IdentityT(M::wrap(t))
    }
}

impl<M: Applicative> Applicative for IdentityT<M> {
    fn lift_a2<F, B, C>(self, b: Self::Wrapped<B>, f: F) -> Self::Wrapped<C>
    where
        F: FnMut(Self::Unwrapped, B) -> C,
    {
        IdentityT(self.0.lift_a2(b.0, f))
    }
}

impl<M: Monad> Monad for IdentityT<M> {
    fn bind<B, F>(self, mut f: F) -> Self::Wrapped<B>
    where
        F: FnMut(Self::Unwrapped) -> Self::Wrapped<B>,
    {
        IdentityT(self.0.bind(|x| f(x).0))
    }
}

trait MonadTrans {
    type Base: Monad;

    fn lift(base: Self::Base) -> Self;
}

impl<M: Monad> MonadTrans for IdentityT<M> {
    type Base = M;

    fn lift(base: M) -> Self {
        IdentityT(base)
    }
}
