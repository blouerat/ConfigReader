package configreader

import scalaz._
import scalaz.Scalaz._
import com.typesafe.config._

case class ConfigReader[A](value: Config => ValidationNEL[String, A]) extends NewType[Config => ValidationNEL[String, A]]

object ConfigReader {
  implicit def ConfigReaderApplicative[A] = new Applicative[ConfigReader] {
    def pure[A](a: => A): ConfigReader[A] = ConfigReader(c => a.success)
    override def fmap[A, B](r: ConfigReader[A], f: A => B): ConfigReader[B] = ConfigReader(c => r(c).map(f))
    override def apply[A, B](f: ConfigReader[A => B], a: ConfigReader[A]): ConfigReader[B] = ConfigReader(c => a(c) <*> f(c))
  }
}

trait Field[A] {
  def get(key: String): ConfigReader[A]
}
object Field {
  def toValidation[A](f: Config => A): ConfigReader[A] = ConfigReader(c => try { f(c).success } catch { case ce: ConfigException => ce.getMessage.failNel })
  
  implicit val stringField = new Field[String] { 
    def get(key: String): ConfigReader[String] = toValidation(c => c.getString(key))
  }
  implicit val intField = new Field[Int] { 
    def get(key: String): ConfigReader[Int] = toValidation(c => c.getInt(key))
  }
}

object FieldValue {
  def apply[A](key: String)(implicit f: Field[A]): ConfigReader[A] = f.get(key)
}
