package config

import cats.effect.{Blocker, ContextShift, IO, Resource}
import com.typesafe.config.ConfigFactory
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import pureconfig.module.catseffect.syntax._

case class Config(server: ServerConfig, database: DatabaseConfig)

object Config {
  def load(configFile: String = "application.conf")(implicit cs: ContextShift[IO]): Resource[IO, Config] = {
    Blocker[IO].flatMap { blocker =>
      Resource.liftF(ConfigSource.fromConfig(ConfigFactory.load(configFile)).loadF[IO, Config](blocker))
    }
  }
}
