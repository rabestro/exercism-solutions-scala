import scala.annotation.tailrec

case class Node[T](override val value: T,
                   override val next: SimpleLinkedList[T]) extends SimpleLinkedList[T]:
  override def isEmpty: Boolean = false

  override def add(item: T): SimpleLinkedList[T] =
    Node(item, reverse).reverse

  override def reverse: SimpleLinkedList[T] =
    @tailrec
    def loop(xs: SimpleLinkedList[T], ys: SimpleLinkedList[T]): SimpleLinkedList[T] =
      xs match
        case Empty() => ys
        case Node(x, xs) => loop(xs, Node(x, ys))
    end loop

    loop(this, Empty())


  override def toSeq: Seq[T] = value +: next.toSeq
