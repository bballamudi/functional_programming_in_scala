
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def tail[A](x: List[A]) = x match {
    case Nil => Nil
    case Cons(head, tail) => tail
  }

  def setHead[A](x: List[A], next: A) = x match {
    case Nil => Nil
    case Cons(head, tail) => Cons(next, tail)
  }

  def drop[A](list: List[A], n: Int): List[A] = list match {
    case Nil => Nil
    case Cons(head, tail) => if (n == 0) list else drop(tail, n -1)
  }

  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(head, tail) => if (f(head)) dropWhile(tail, f) else list
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(head, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = 
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = 
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)
  def product2(ns: List[Double]) = foldRight(ns, 1.0)((x, y) => x * y)

  def length[A](ns: List[A]) = foldRight(ns, 0)((x, y) => y + 1)


  def sumLeft(ns: List[Int]) = foldLeft(ns, 0)((x, y) => x + y)
  def productLeft(ns: List[Double]) = foldLeft(ns, 1.0)((x, y) => x * y)
  def lengthLeft[A](ns: List[A]) = foldLeft(ns, 0)((cnt, xs) => cnt + 1)

  // Write foldRight via foldLeft (HARD)
  // Implement append with either foldLeft or foldRight
  
  def append[A](as: List[A], bs: List[A]) = foldRight(as, bs)(Cons(_,_))

  // Implement a fn that concatenates a list of lists into a single list, try to use fns we already have
  // Use foldRight
  def concatenate[A](aas: List[List[A]]): List[A] = foldRight(aas, Nil:List[A])(append)
}

object ChapterThree extends App {
  val w = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9, 10))
  val y = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val x = List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
  val z = List()
  println(List.tail(y))
  println(List.setHead(y, 13))
  println(List.drop(y, 2))
  println(List.drop(z, 3))
  println(List.dropWhile(y, (x: Int) => x < 3))
  println(List.dropWhile(z, (x: Int) => x < 3))
  println(List.init(y))

  println("= Sum, Product, Length =")
  println(List.sum2(y))
  println(List.product2(x))

  //  println(List.foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_,_)))
  println(List.length(y))


  println("= Sum, Product, Length (Left) =")
  println(List.sumLeft(y))
  println(List.productLeft(x))
  println(List.lengthLeft(y))

  println(List.concatenate(w))
}
