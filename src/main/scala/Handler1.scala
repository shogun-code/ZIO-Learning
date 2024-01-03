import zio.{Cause, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.IOException
object Handler1 extends ZIOAppDefault{
  //1 make this effect failed with a TYPED error
  val aBadFailure: ZIO[Any, Nothing, Int] = ZIO.succeed[Int](throw new RuntimeException("bad failure" ))
  val aBetterFailure: ZIO[Any, Cause[Nothing], Int] = aBadFailure.sandbox
  val aBetterFailure2: ZIO[Any, Throwable, Int] = aBadFailure.unrefine {
    case e => e
  }

  // 2 transform zio to to another zio with a narrower error type
  def ioException[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] = zio.refineOrDie {
    case e: IOException => e
  }

  //3
  def left[R,E,A,B](zio: ZIO[R, E, Either[A,B]]): ZIO[R, Either[E,A], B] = zio.foldZIO(
    e => ZIO.fail(Left(e)),
    either => either match {
      case Left(a) => ZIO.fail(Right(a))
      case Right(b) => ZIO.succeed(b)
    }
  )

  //4
  val database = Map(
    "A" -> 80,
    "B" -> 90
  )

  case class QueryError(reason: String)
  case class UserProfile(name: String, phone: Int)

  def lookupUserProfile(userId: String): ZIO[Any, QueryError, Option[UserProfile]] =
    if (userId != userId.toLowerCase) ZIO.fail(QueryError("Invalid user id"))
    else ZIO.succeed(database.get(userId).map(phone => UserProfile(userId, phone)))

  def betterLookupProfile(userId: String): ZIO[Any, Option[QueryError], UserProfile] =
    lookupUserProfile(userId).foldZIO(
      err => ZIO.fail(Some(err)),
      maybeProfile => maybeProfile match {
        case Some(profile) => ZIO.succeed(profile)
        case None => ZIO.fail(None)
      }
    )

  def betterLookupProfile_v2(userId: String): ZIO[Any, Option[QueryError], UserProfile] =
    lookupUserProfile(userId).some

  // Turn failure into defects
  // val failedDefect: ZIO[Any, Nothing, Int] = failedEffect.orDie

  // Narrow failure type, leave the rest as defects
//  failedEffect.refineToOrDie[IOException] {
//    case e: IOException => e
//    case _ => new IOException("Unknown")
//  }

  // Treat failure causes, inclduing defects
//  val foldWithCause = failedEffect.foldCauseZIO(
//      cause => ZIO.succed(s"This failed with ${cause.defects}"),
//      value => ZIO.succeed(s"This success with ${value}")
//  )

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ???
}
