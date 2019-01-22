import org.scalatest.FreeSpec

class StudyYourGrammar extends FreeSpec {

  "definition" - {
    sealed trait KVStore[A]
    case class Put[T](key: String, value: T) extends KVStore[Unit]
    case class Get[T](key: String)           extends KVStore[Option[T]]
    case class Delete(key: String)           extends KVStore[Unit]

    import cats.free.Free

    type KVStoreFree[A] = Free[KVStore, A]

    import cats.free.Free.liftF

    def put[T](key: String, value: T): KVStoreFree[Unit] =
      liftF[KVStore, Unit](Put(key, value))

    def get[T](key: String): KVStoreFree[Option[T]] =
      liftF[KVStore, Option[T]](Get(key))

    def delete(key: String): KVStoreFree[Unit] =
      liftF[KVStore, Unit](Delete(key))

    def update[T](key: String, f: T => T): KVStoreFree[Unit] =
      for {
        vMaybe <- get[T](key)
        _      <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
      } yield ()

    def program: KVStoreFree[Option[Int]] =
      for {
        _ <- put("wild-cats", 2)
        _ <- update[Int]("wild-cats", _ + 12)
        _ <- put("tame-cats", 5)
        n <- get[Int]("wild-cats")
        _ <- delete("tame-cats")
      } yield n

    "run your program" in {
      import cats.{ ~>, Id }

      import scala.collection.mutable

      def impureCompiler: KVStore ~> Id =
        new (KVStore ~> Id) {

          val kvs = mutable.Map.empty[String, Any]

          override def apply[A](fa: KVStore[A]): Id[A] =
            fa match {
              case Put(key, value) =>
                println(s"put($key, $value)")
                kvs(key) = value
                ().asInstanceOf[A]
              case Get(key) =>
                println(s"get($key")
                kvs.get(key).asInstanceOf[Id[A]]
              case Delete(key) =>
                println(s"delete($key)")
                kvs.remove(key)
                ().asInstanceOf[A]
            }
        }
      assert {
        program.foldMap(impureCompiler) === Some(14)
      }
    }
  }

}
