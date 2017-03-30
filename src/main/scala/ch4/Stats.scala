package ch4

object Stats {
  def variance(xs: Seq[Double]): Option[Double] = {
    // The first stage, which can fail
    val maybeMean = xs match {
      case Nil => None
      case _ => Some(xs.sum / xs.length)
    }

    /*

    Without flatMap

    val variance = maybeMean match {
      case Nil => None
      case Some(mean) => Some(xs.foldLeft(0.0){(v, elem)=> v + math.pow(v - mean, 2)})
    }

    */

    maybeMean.flatMap {mean => Some(xs.foldLeft(0.0){(v, elem)=> v + math.pow(v - mean, 2)})}
  }
}
