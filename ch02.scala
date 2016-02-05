object fpsc extends App {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => ((b: B) => f(a, b))
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  val tos = (a: Long) => a.toString
  val tol = (b: Int) => b.asInstanceOf[Long]
  val add = (a: Long, b: Long) => a + b

  val composed = compose(tos, tol)
  val r1 = composed(5)
  println(r1.getClass)


  val curried = curry(add)
  val i2 = curry(add)
  val i3 = i2(5)
  println(i3(6))

  val uncurried = uncurry(i2)
  println(uncurried(3, 2))
}
