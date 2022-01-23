object DataStructures {

  val x = List(1, 2, 3, 4, 5) match {
    case ::(x, ::(2, ::(4, _)))        => x
    case Nil                           => 42
    case ::(x, ::(y, ::(3, ::(4, _)))) => x + y
    case ::(h, t)                      => h + t.sum
    case _                             => 101
  }

  // (ex3.2)Listの最初の要素を削除する関数
  def tail[A](as: List[A]): List[A] = {
    as match {
      case _ :: next => next
      case Nil       => Nil
    }
  }

  // (ex3.3)Listの最初の要素を別の要素に置き換える関数
  def setHead[A](as: List[A], head: A): List[A] = {
    as match {
      case _ :: next => head :: next
      case Nil       => Nil
    }
  }

}
