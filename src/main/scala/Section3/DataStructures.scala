object DataStructures {

  val x = List(1, 2, 3, 4, 5) match {
    case ::(x, ::(2, ::(4, _)))        => x
    case Nil                           => 42
    case ::(x, ::(y, ::(3, ::(4, _)))) => x + y
    case ::(h, t)                      => h + t.sum
    case _                             => 101
  }

  // (Ex3.2)Listの最初の要素を削除する関数
  def tail[A](as: List[A]): List[A] = {
    as match {
      case _ :: next => next
      case Nil       => Nil
    }
  }

  // (Ex3.3)Listの最初の要素を別の要素に置き換える関数
  def setHead[A](as: List[A], head: A): List[A] = {
    as match {
      case _ :: next => head :: next
      case Nil       => Nil
    }
  }

  // (Ex3.4)tailを一般化
  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def tail(as: List[A], count: Int): List[A] = {
      as match {
        case _ :: next if (n == count) => next
        case _ :: next if (n > count)  => tail(next, count + 1)
        case bs                        => bs
      }
    }
    tail(l, 1)
  }

  // (Ex3.5)
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case head :: next if f(head) => dropWhile(next, f)
      case bs                      => bs
    }
  }

  // (Ex3.6)
  @annotation.tailrec
  def init[A](l: List[A], acc: List[A] = Nil): List[A] = {
    l match {
      case head :: Nil  => acc
      case head :: next => init(next, acc :++ List(head))
      case Nil          => acc
    }
  }

  // (Ex3.6) answer
  def init_ans[A](l: List[A]): List[A] = {
    l match {
      case head :: Nil  => Nil
      case head :: next => head :: init(next)
      case Nil          => sys.error("init of empty list")
    }
  }

  // (Ex3.8)
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil          => z
      case head :: next => f(head, foldRight(next, z)(f))
    }
  }

  def ex38_result = foldRight(List(1, 2, 3), Nil: List[Int])(::(_, _))

  // (Ex3.9)
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, b) => b + 1)
  }

  // (Ex3.10)
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil          => z
      case head :: next => foldLeft(next, f(z, head))(f)
    }
  }

  // (Ex3.11)
  def sum2(ns: List[Int]): Int = {
    foldLeft(ns, 0)(_ + _)
  }

  def product2(ns: List[Double]): Double = {
    foldLeft(ns, 1.0)(_ * _)
  }

  def length2(ns: List[Int]): Int = {
    foldLeft(ns, 0)((z, _) => z + 1)
  }

  // (Ex3.12)
  def reverse(ns: List[Int]): List[Int] =
    foldLeft(ns, Nil: List[Int])((a, b) => ::(b, a))

}
