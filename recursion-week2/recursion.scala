// Design a tail recursive implementation of factorial
// factorial(n) = 1 * 2 * ... * (n - 1) * n

// Not tail recursive
def factorial(n: Int): Int = {
  if (n == 1) 1 else n * factorial(n - 1)
}

// @tailrec
def factorialTail(n: Int): Int = {
  def loop(acc: Int, m: Int): Int = {
    if (m == 1)
      acc
    else
      loop(m * acc, m - 1)
  }
  loop(1, n)
}

println(factorialTail(4))
