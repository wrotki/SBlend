package playground

import java.net.URI

import scalaz._
import Scalaz._

/**
  * Created by mariusz on 7/10/16.
  */
object ReaderTOptionExample {


  case class User(id: Long, parentId: Long, name: String, email: String)
  trait HttpService {
    def get(uri: URI): String
  }

  trait UserRepo {
    def get(id: Long): Option[User]
    def find(name: String): Option[User]
  }
  trait Config {
    def userRepo: UserRepo
    def httpService: Option[HttpService]
  }

  type ReaderTOption[A, B] = Kleisli[Option, A, B]
  object ReaderTOption {
    def ro[A, B](f: A => Option[B]): ReaderTOption[A, B] = Kleisli(f)
  }


  trait Users {
    def getUser(id: Long): ReaderTOption[Config, User] =
      ReaderTOption.ro {
        case config => config.userRepo.get(id)
      }
    def findUser(name: String): ReaderTOption[Config, User] =
      ReaderTOption.ro {
        case config => config.userRepo.find(name)
      }
  }
  trait Https {
    def getHttp(uri: URI): ReaderTOption[Config, String] =
      ReaderTOption.ro {
        case config => config.httpService map {_.get(uri)}
      }
  }

  trait Program extends Users with Https {
    def userSearch(id: Long): ReaderTOption[Config, String] =
      for {
        u <- getUser(id)
        r <- getHttp(new URI(s"http://www.google.com/?q=${u.name}"))
      } yield r
  }

  val dummyConfig: Config = new Config {
    val testUsers = List(User(0, 0, "Vito", "vito@example.com"),
      User(1, 0, "Michael", "michael@example.com"),
      User(2, 0, "Fredo", "fredo@example.com"))
    def userRepo: UserRepo = new UserRepo {
      def get(id: Long): Option[User] =
        testUsers find { _.id === id }
      def find(name: String): Option[User] =
        testUsers find { _.name === name }
    }
    def httpService: Option[HttpService] = None
  }

  object Main extends Program {
    def run(config: Config): Option[String] =
      userSearch(1).run(config)
  }

  def runSth: Unit = {

    val res: Option[String] = Main.run(dummyConfig)
    println(res)
  }


}
