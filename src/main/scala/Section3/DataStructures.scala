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
}
