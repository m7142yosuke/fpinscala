object GettingStarted {
  // 末尾呼び出しでない実装
  def fib_(n: Int): Int = {
    // @annotation.tailrec
    // 末尾呼び出しでないのでアノテーションをつけるとコンパイルエラーとなる。
    def go(n: Int): Int = {
      n match {
        case 1 => 0
        case 2 => 1
        case _ => go(n - 1) + go(n - 2)
      }
    }
    go(n)
  }

  // 末尾再帰関数
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, count: Int, last: Int, second_last: Int): Int = {
      (n, count) match {
        case (1, _) => 0
        case (2, _) => 1
        case (_, count) if (n > count) =>
          go(n, count + 1, last + second_last, last)
        case (_, count) if (n == count) => last + second_last
      }
    }
    go(n, 3, 1, 0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    (for {
      (f, b) <- as.dropRight(1).lazyZip(as.drop(1))
    } yield {
      ordered(f, b)
    }).forall(_ == true)
  }

  // 答え
  def isSorted_ans[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (gt(as(n), as(n + 1))) false
      else go(n + 1)

    go(0)
  }

  // カリー化
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = { (a: A) => (b: B) =>
    f(a, b)
  }

  // uncurry
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a, b) =>
    f(a)(b)
  }

  // 合成関数
  def compose[A, B, C](f: B => C, g: A => B): A => C = { (a: A) =>
    f(g(a))
  }
}
