// 2.1

def fib(n: Int): Int = {
  def loop(num: Int, sum: Int, count: Int): Int = {
    if(count == n - 1) sum
    else loop(sum, sum + num, count + 1)
  }

  if(n == 1) 0
  else loop(0, 1, 1)
}

fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
fib(10)

// 2.2

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  if(as.isEmpty || as.tail.isEmpty) true
  else
    if(!ordered(as.head, as.tail.head)) false
    else isSorted(as.tail, ordered)
}

isSorted[Int](Array(1, 2, 3), (a, b) => a < b)
isSorted[Int](Array(8, 2, 3), (a, b) => a < b)
isSorted[String](Array("a", "bc", "qewq"), (a, b) => a.length < b.length)

// 2.3

def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
  (a: A) => (b: B) => f(a, b)
}

// 2.4

def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
  (a, b) => f(a)(b)
}

// 2.5

def compose[A,B,C](f: B => C, g: A => B): A => C = {
  (a) => f(g(a))
}