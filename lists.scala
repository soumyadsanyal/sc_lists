package soumyaLists

trait List[T] {
  def isEmpty : Boolean
  def head : T
  def tail : List[T]
}


class Cons[T](_head: T, _tail: List[T]) extends List[T]{
  def isEmpty = false
  val head = _head
  val tail = _tail
}

class Nil[T] extends List[T]{

  def isEmpty = true
  def head = throw new NoSuchElementException("no head of empty list!")
  def tail = throw new NoSuchElementException("no tail of empty list!")
}

class ListTraversal[T]{

  def ListTraversal(n:Int, list:List[T]):T = if (list.isEmpty || n<0) throw new IndexOutOfBoundsException("list wasn't long enough") else
  if (n==0) return list.head else
  ListTraversal( (n-1):Int, list.tail : List[T])


}

class scratch{

  def makeList(n:Int, k:Int):List[Int] = {
    if (k==n) return new Nil[Int] else
    return new Cons[Int](k, makeList(n, (k+1)))
  }
}




