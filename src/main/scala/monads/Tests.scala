package monads

/**
 * @author David Galichet.
 */
object Tests {
    def twice[F[+_]](fa: F[Int])(implicit FA: Functor[F]): F[Int] = {
        FA.map(fa){ x => x*2 }
    }

    def add[M[+_]](ma: M[Int], mb: M[Int])(implicit MA: Monad[M]): M[Int] = {
        MA.bind(ma) { x => MA.map(mb) { y => x + y} }
    }

    def addFor[M[+_]](ma: M[Int], mb: M[Int])(implicit MA: Monad[M]): M[Int] = {
        import Monad.MonadWrapper

        for {
            a <- ma
            b <- mb
        } yield a + b
    }
}

