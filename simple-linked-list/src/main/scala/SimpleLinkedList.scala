trait SimpleLinkedList[T]:
  def isEmpty: Boolean

  def value: T

  def add(item: T): SimpleLinkedList[T]

  def next: SimpleLinkedList[T]

  def reverse: SimpleLinkedList[T]

  def toSeq: Seq[T]


object SimpleLinkedList:
  def apply[T](ts: T*): SimpleLinkedList[T] = fromSeq(ts)

  def fromSeq[T](ts: Seq[T]): SimpleLinkedList[T] =
    ts.foldRight(SimpleLinkedList.empty[T])(Node(_, _))

  def empty[T]: SimpleLinkedList[T] = Empty[T]()
