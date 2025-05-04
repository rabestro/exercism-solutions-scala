case object Empty extends SimpleLinkedList[Any]:
  def apply[T](): SimpleLinkedList[T] = this.asInstanceOf[SimpleLinkedList[T]]

  def unapply[T](xs: SimpleLinkedList[T]): Boolean = xs == this

  override def isEmpty: Boolean = true

  override def value: Any = throw new Exception("value called on Empty")

  override def add(item: Any): SimpleLinkedList[Any] = SimpleLinkedList(item)

  override def next: SimpleLinkedList[Any] = throw new Exception("next called on Empty")

  override def reverse: SimpleLinkedList[Any] = this

  override def toSeq: Seq[Any] = Seq.empty
