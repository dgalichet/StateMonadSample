package monads

/**
 * @author David Galichet.
 */
sealed trait Maybe[+A]
case class Value[+A](a: A) extends Maybe[A]
case object Empty extends Maybe[Nothing]


object Maybe {

/*
    This is now defined in Monad (extends Functor)
    implicit val maybeIsAFunctor = new Functor[Maybe] {

        def pure[A](a: A) = Value(a)

        def map[A, B](fa: Maybe[A])(f: A => B) = fa.map(f)
    }*/

    class MaybeIsAFunctor extends Functor[Maybe] {
        def pure[A](a: A) = Value(a)

        def map[A, B](fa: Maybe[A])(f: (A) => B) = fa match {
            case Empty => Empty
            case Value(a) => Value(f(a))
        }
    }

    implicit object maybeIsAFunctor extends MaybeIsAFunctor

    class MaybeIsAMonad extends MaybeIsAFunctor with Monad[Maybe] {
        def bind[A, B](ma: Maybe[A])(f: A => Maybe[B]) = ma match {
            case Empty => Empty
            case Value(a) => f(a)
        }
    }

    implicit object maybeIsAMonad extends MaybeIsAMonad

/*    implicit val maybeIsAMonad = new Monad[Maybe] {

        def pure[A](a: A): Maybe[A] = Value(a)

        def map[A, B](fa: Maybe[A])(f: (A) => B) = bind(fa) { a => unit(f(a)) }

        def bind[A, B](ma: Maybe[A])(f: A => Maybe[B]) = ma match {
            case Empty => Empty
            case Value(a) => f(a)
        }
    }*/
}
