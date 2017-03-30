package ch4

object Exercise4Dot1 extends App {
  override def main(args: Array[String]): Unit = {
    assert(Some("a").map(s => s*2) == Some("aa"))
    assert(None.map(identity) == None)

    assert(Some("a").flatMap(s => Some(s)) == Some("a"))
    assert(None.flatMap(identity) == None)

    assert(Some("a").getOrElse("b") == "a")
    assert(None.getOrElse("b") == "b")

    assert(Some("a").orElse(Some("b")) == Some("a"))
    assert(None.orElse(Some("b")) == Some("b"))

    assert(Some("a").filter(_ == "a") == Some("a"))
    assert(Some("a").filter(_ == "b") == None)
  }
}