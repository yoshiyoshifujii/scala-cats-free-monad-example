import org.scalatest.FreeSpec

class ExampleFreeTSPec extends FreeSpec {

  "success" in {
    import cats.free._
    import cats._
    import cats.data._

    sealed abstract class Teletype[A] extends Product with Serializable
    final case class WriteLine(line: String) extends Teletype[Unit]
    final case class ReadLine(prompt: String) extends Teletype[String]

    type TeletypeT[M[_], A] = FreeT[Teletype, M, A]
    type Log = List[String]

    trait TeletypeOps[F[_]] {
      def writeLine(line: String): TeletypeT[F, Unit]
      def readLine(prompt: String): TeletypeT[F, String]
      def log(s: String): TeletypeT[F, Unit]
    }

    object TeletypeOps {
      def apply[F[_]](implicit F: TeletypeOps[F]): TeletypeOps[F] = F
    }

    def program[F[_]: Monad: TeletypeOps]: TeletypeT[F, Unit] =
      for {
        userSaid <- TeletypeOps[F].readLine("what's up?!")
        _ <- TeletypeOps[F].log(s"user said : $userSaid")
        _ <- TeletypeOps[F].writeLine("thanks, see you soon!")
      } yield ()

    type TeletypeState[A] = State[List[String], A]

    implicit def teletypeOps: TeletypeOps[TeletypeState] =
      new TeletypeOps[TeletypeState] {
        def writeLine(line: String): TeletypeT[TeletypeState, Unit] =
          FreeT.liftF[Teletype, TeletypeState, Unit](WriteLine(line))
        def readLine(prompt: String): TeletypeT[TeletypeState, String] =
          FreeT.liftF[Teletype, TeletypeState, String](ReadLine(prompt))
        def log(s: String): TeletypeT[TeletypeState, Unit] =
          FreeT.liftT[Teletype, TeletypeState, Unit](State.modify(s :: _))
      }

    def interpreter: Teletype ~> TeletypeState =
      new (Teletype ~> TeletypeState) {
        def apply[A](fa: Teletype[A]): TeletypeState[A] = {
          fa match {
            case ReadLine(prompt) =>
              println(prompt)
              val userInput = "hanging in here" //scala.io.StdIn.readLine()
              StateT.pure[Eval, List[String], A](userInput)
            case WriteLine(line) =>
              StateT.pure[Eval, List[String], A](println(line))
          }
        }
      }

    val state = program[TeletypeState].foldMap(interpreter)
    val initialState = Nil
    val (stored, _) = state.run(initialState).value

    assert(stored.toString === "List(user said : hanging in here)")

  }

}
