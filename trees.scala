package soumyaTrees

trait Tree[T] {
  def isEmpty : Boolean
  def node : T
  def left : Tree[T]
  def right: Tree[T]
}

class NilNode[T] extends Tree[T] {
  def isEmpty = true
  def node = throw new NoSuchElementException("nothing in nilnode")
  def left = throw new NoSuchElementException("no left child for nilnode")
  def right = throw new NoSuchElementException("no right child for nilnode")
}

class NonNilNode[T](val node:T, val left:Tree[T], val right:Tree[T]) extends Tree[T] {
  def isEmpty = false
}


