package monads

/**
 * @author David Galichet.
 */
trait Functor[F[+_]] {
    def pure[A](a: A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B]

    def lift[A,B](f: A => B): F[A] => F[B] = { fa: F[A] => map(fa)(f) }
}


trait Monad[M[+_]] extends Functor[M] {
    def unit[A](a: A): M[A] = pure(a)

    def bind[A, B](ma: M[A])(f: A => M[B]): M[B]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = bind(ma)(f)
}


object Monad {

    implicit class MonadWrapper[A, M[+_]](ma: M[A])(implicit MA: Monad[M]) {
        def map[B](f: A => B): M[B] = MA.map(ma)(f)

        def flatMap[B](f: A => M[B]): M[B] = MA.flatMap(ma)(f)
    }

    def sequence[A, M[+_]](ms: List[M[A]])(implicit MA: Monad[M]): M[List[A]] = ms match {
        case Nil => MA.unit(List.empty[A])
        case head::tail => for {
            x <- head
            xs <- sequence(tail)
        } yield x::xs
    }
}

