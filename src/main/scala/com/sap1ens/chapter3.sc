import scala.annotation.tailrec

// 3.1

def sum(ints: List[Int]): Int = ints match {
  case Nil => 0
  case ::(x,xs) => x + sum(xs)
}

val x = List(1,2,3,4,5) match {
  case ::(x, ::(2, ::(4, _))) => x
  case Nil => 42
  case ::(x, ::(y, ::(3, ::(4, _)))) => x + y
  case ::(h, t) => h + sum(t)
  case _ => 101
}

// 3.2
// 3.3
// 3.4
// 3.5
// 3.6

object ListC {
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => throw new IllegalArgumentException("Provided list is empty")
    case x :: xs => xs
  }

  def setHead[A](list: List[A], elem: A): List[A] = list match {
    case Nil => throw new IllegalArgumentException("Provided list is empty")
    case x :: xs => elem :: xs
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    def loop(list: List[A], counter: Int): List[A] = {
      list match {
        case Nil => Nil
        case res if counter > n => res
        case _ :: xs if counter <= n => loop(xs, counter + 1)
      }
    }

    loop(l, 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    def loop(list: List[A], res: List[A]): List[A] = {
      list match {
        case Nil => res.reverse
        case x :: xs if f(x) => loop(xs, res)
        case x :: xs => loop(xs, x :: res)
      }
    }

    loop(l, Nil)
  }

  def init[A](l: List[A]): List[A] = {
    def loop(list: List[A], res: List[A]): List[A] = {
      list match {
        case Nil => Nil
        case x :: Nil => res.reverse
        case x :: xs => loop(xs, x :: res)
      }
    }

    loop(l, Nil)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case x :: xs => f(x, foldRight(xs, z)(f))
    }

  // 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((a, b) => b + 1)
  }


  // 3.10
  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }
  }

  // 3.11
   def length2[A](as: List[A]): Int = {
    foldLeft(as, 0)((b, a) => b + 1)
  }

  def sum(as: List[Int]): Int = {
    foldLeft(as, 0)(_ + _)
  }

  def product[A](as: List[Int]): Double = {
    foldLeft(as, 1.0)(_ * _)
  }

  // 3.12
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List.empty[A])((b, a) => a :: b)
  }

  // 3.14
  def append[A](as: List[A], elem: A): List[A] = {
    foldRight(as, List(elem))((b, a) => List(b) ::: a)
  }

  // 3.16

  def transformPlusOne(as: List[Int]): List[Int] = {
    foldLeft(as, List.empty[Int])((b, a) => b ::: List(a + 1))
  }

  // 3.17
  def doubleToString(as: List[Double]): List[String] = {
    foldLeft(as, List.empty[String])((b, a) => b ::: List(a.toString))
  }

  // 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldLeft(as, List.empty[B])((b, a) => b ::: List(f(a)))
  }

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldLeft(as, List.empty[A]) {(b, a) =>
      if(f(a))
        b ::: List(a)
      else
        b
    }
  }

  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, List.empty[B])((b, a) => f(b) ::: a)
  }

  // 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(i => if(f(i)) List(i) else Nil)
  }
}

val l = List(1, 2, 3, 4, 5, 6, 7, 8)

ListC.tail(l)
ListC.setHead(l, 10)
ListC.drop(l, 4)
ListC.dropWhile(l, (x: Int) => x % 2 == 0)
ListC.init(l)

ListC.length2(l)
ListC.sum(l)
ListC.product(l)
ListC.reverse(l)
ListC.append(l, 9)
ListC.transformPlusOne(l)
ListC.map(l)(_ * 10)
ListC.filter(l)(_ % 2 == 0)
ListC.flatMap(List(1,2,3))(i => List(i,i))
ListC.filter2(l)(_ % 2 == 0)