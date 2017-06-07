package ch8

trait Prop {
  def check: Boolean = ???
  def &&(p: Prop): Boolean = p.check && check
}
