import zio.{Cause, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Handler1 extends ZIOAppDefault{
  val aBadFailure: ZIO[Any, Nothing, Int] = ZIO.succeed[Int](throw new RuntimeException("bad failure" ))
  val aBetterFailure: ZIO[Any, Cause[Nothing], Int] = aBadFailure.sandbox
  val aBetterFailure2: ZIO[Any, Throwable, Int] = aBadFailure.unrefine {
    case e => e
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ???
}
