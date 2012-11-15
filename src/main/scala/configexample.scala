package configreader.example

case class DD(z: String, y: Int)
case class Bravo(aa: String, bb: String, cc: String, dd: DD, ee: String)
case class Bar(baz: String)
case class Foo(bar: Bar, alpha: String, bravo: Bravo)

object Example {
  import com.typesafe.config.ConfigFactory
  import scalaz._
  import scalaz.Scalaz._
  import configreader._
  
  def main(args: Array[String]) {
    
    val dd = 
      FieldValue[String]("foo.bravo.dd.z") |@| 
      FieldValue[Int]("foo.bravo.dd.y") apply DD

    val bravo = 
      FieldValue[String]("foo.bravo.aa")  |@|
      FieldValue[String]("foo.bravo.bb")  |@|
      FieldValue[String]("foo.bravo.cc")  |@|
      dd                                  |@|
      FieldValue[String]("foo.bravo.ee") apply Bravo

    val bar = FieldValue[String]("foo.bar.baz") map Bar

    val foo = 
      bar |@|
      FieldValue[String]("foo.alpha") |@|
      bravo apply Foo
      
    val conf = ConfigFactory.load()
    println(foo(conf).fold(_.list.mkString("\n"), _.toString))
  }
}

