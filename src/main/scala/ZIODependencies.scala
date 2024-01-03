import zio._

object ZIODependencies extends ZIOAppDefault {
  case class User(name: String, email: String)
  class UserSubscription(emailService: EmailService, userDatabase: UserDatabase) {
    def subscribe(user: User): Task[Unit] =
      for {
        _ <- userDatabase.insert(user)
        _ <- emailService.sendEmail(user)
      } yield ()
  }

  object UserSubscription {
    def create(emailService: EmailService, userDatabase: UserDatabase): UserSubscription =
      new UserSubscription(emailService, userDatabase)
  }

  class EmailService {
    def sendEmail(user: User): Task[Unit] =
      ZIO.succeed(s"You have subscribed. Welcome ${user.email}").unit
  }

  object EmailService {
    def create: EmailService = new EmailService()
  }

  class UserDatabase(connectionPool: ConnectionPool) {
    def insert(user: User): Task[Unit] = for {
      connection <- connectionPool.get
      _ <- connection.runQuery(s"INSERT INTO users VALUES (${user.name}, ${user.email})")
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
      ZIO.succeed(println(s"Running query: $query"))
  }

  val subscriptionService = ZIO.succeed(
    UserSubscription.create(
      EmailService.create,
      UserDatabase.create(
        ConnectionPool.create(10)))
  )

  def subscribe(user: User) = for {
    service <- subscriptionService
    _ <- service.subscribe(user)
  } yield ()

  def subscribe_v2(user: User): ZIO[Any, Throwable, Unit] = for {
    sub <- ZIO.service[UserSubscription]
    _ <- sub.subscribe(user)
  } yield ()

  val program = for {
    _ <- subscribe_v2(User("A", "a@"))
    _ <- subscribe_v2(User("B", "b@"))
  } yield ()

  // Zlayer
  val connectionPoolLayer: ZLayer[Any, Nothing, ConnectionPool] = ZLayer.succeed(ConnectionPool.create(10))
  val databaseLayer: ZLayer[ConnectionPool, Nothing, UserDatabase] = ZLayer.fromFunction(UserDatabase.create _)
  val emailServiceLayer: ZLayer[Any, Nothing, EmailService] = ZLayer.succeed(EmailService.create)
  val userSubscriptionLayer: ZLayer[EmailService with UserDatabase, Nothing, UserSubscription] =
    ZLayer.fromFunction(UserSubscription.create _)

  // Composing layers
  // vertical composition
  val databaseLayerFull: ZLayer[Any, Nothing, UserDatabase] = connectionPoolLayer >>> databaseLayer

  // horizontal composition
  val subscriptionRequirementsLayer: ZLayer[Any, Nothing, EmailService with UserDatabase] =
    databaseLayerFull ++ emailServiceLayer


  override def run = program.provide(
    ZLayer.succeed(UserSubscription.create(
      EmailService.create,
      UserDatabase.create(
        ConnectionPool.create(10))))
  )
}
