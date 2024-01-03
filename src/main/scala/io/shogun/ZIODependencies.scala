package io.shogun

import zio._

object ZIODependencies extends ZIOAppDefault {

  // app to subscribe users to newsletter
  case class User(name: String, email: String)

  class UserSubscription(emailService: EmailService, userDatabase: UserDatabase) {
    def subscribeUser(user: User): Task[Unit] =
      for {
        _ <- emailService.email(user)
        _ <- userDatabase.insert(user)
      } yield ()
  }

  object UserSubscription {
    def create(emailService: EmailService, userDatabase: UserDatabase): UserSubscription = {
      new UserSubscription(emailService, userDatabase)
    }
  }

  class EmailService {
    def email(user: User): Task[Unit] =
      ZIO.succeed(println(s"You've just been subscribed to Rock the JVM. Welcome, ${user.name}"))
  }

  object EmailService {
    def create(): EmailService = new EmailService
  }

  class UserDatabase(connectionPool: ConnectionPool) {
    def insert(user: User): Task[Unit] = for {
      conn <- connectionPool.get
      _ <- conn.runQuery(s"insert into subscriber(name, email) values (${user.name}, ${user.email})")
    } yield ()
  }

  object UserDatabase {
    def create(connectionPool: ConnectionPool): UserDatabase =
      new UserDatabase(connectionPool)
  }

  class ConnectionPool(nConnections: Int) {
    def get: Task[Connection] =
      ZIO.succeed(println("Acquired connection")) *> ZIO.succeed(Connection())
  }

  object ConnectionPool {
    def create(nConnections: Int): ConnectionPool =
      new ConnectionPool(nConnections)
  }

  case class Connection() {
    def runQuery(query: String): Task[Unit] =
      ZIO.succeed(println(s"Executing query: $query"))
  }

  val subscriptionService: ZIO[Any, Nothing, UserSubscription] = ZIO.succeed( // Dependency Injection
    UserSubscription.create(
      EmailService.create(),
      UserDatabase.create(
        ConnectionPool.create(10)
      )
    )
  )

  /*
    "clean DI" has drawbacks
    - does not scale for many services
    - DI can be 100x worse
      - pass dependencies partially
      - not having all deps in the same place
      - passing dependencies multiple times

   */

  def subscribe(user: User): ZIO[Any, Throwable, Unit] = for {
    sub <- subscriptionService // service is instantiated at the point of call
    _ <- sub.subscribeUser(user)
  } yield ()

  // risk leaking resources if you subscribe multiple users in the same program
  val program = for {
    // you might lose track of the effect which created the servcies in the first place
    // and you might oblivious to the fact that the subscribe API actually spins up a new subscription service in the first place
    _ <- subscribe(User("Tom", "tom@example.com"))
    _ <- subscribe(User("John", "john@example.com"))
  } yield ()

  // If you wanna mitigate those you can alternatively pass this subscription service as an argument to subscribe to force the user to pass
  // the right effect but that becomes annoying
  // Because you subscribe with a user and the subscription service and you have to pass that argument over and over again.
  // If you have to insert multiple users so pretty much all options are bad now.

  // alternative
  def subscribe_v2(user: User): ZIO[UserSubscription, Throwable, Unit] = for {
    sub <- ZIO.service[UserSubscription] // ZIO[UserSubscription, Nothing, UserSubscription]
    _ <- sub.subscribeUser(user)
  } yield ()

  val program_v2 = for {
    _ <- subscribe_v2(User("Tom", "tom@example.com"))
    _ <- subscribe_v2(User("John", "john@example.com"))
  } yield ()

  //  def run = subscribe(User("Will", "will.lee@example.com"))


  /*
    - we don't need to care about dependencies until the end of the world
    - all ZIOs requiring this dependency will use the same instance
    - can use different instances of the same type for different needs (e.g. testing)
    - layers can be created and composed much like regular ZIOs + rich API
   */
  def run = program_v2.provide(
    ZLayer.succeed(
      UserSubscription.create(
        EmailService.create(),
        UserDatabase.create(
          ConnectionPool.create(10)
        )
      )
    )
  )
}