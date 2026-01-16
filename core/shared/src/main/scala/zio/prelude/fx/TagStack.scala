package zio.prelude.fx

/**
 * Lightweight port of zio.internal.Stack, optimized for usage with ZPure
 */
private final class TagStack[A <: AnyRef] { self =>
  import TagStack._

  private[this] var array      = new Array[AnyRef](ArrSize + 1)
  private[this] var packed     = 0
  private[this] var packedTags = 0
  array(ArrSize) = new Array[Int](1)

  def clear(): Unit = {
    var i = 0
    while (i < ArrSize && (array(i) ne null)) {
      array(i) = null
      i += 1
    }
    packed = 0
  }

  /**
   * Pushes an item onto the stack.
   */
  def push(tag: Boolean, a: A): Unit = {
    val packed0 = packed
    val used    = packed0 & 0xf
    val array   = this.array
    if (used == ArrSize) {
      val newArr    = new Array[AnyRef](ArrSize + 1)
      val newTagArr = new Array[Int](1)
      newArr(ArrSize) = newTagArr
      newArr(0) = array
      newArr(1) = a
      packedTags = if (tag) 2 else 0
      newTagArr(0) = packedTags
      this.array = newArr
      packed += 3
    } else {
      array(used) = a
      if (tag) {
        packedTags |= 1 << used
      } else {
        packedTags &= ~(1 << used)
      }
      (array(ArrSize).asInstanceOf[Array[Int]])(0) = packedTags
      packed += 1
    }
  }

  /**
   * Pops an item off the stack, or returns `null` if the stack is empty.
   */
  def pop(): A = {
    val packed0 = packed
    if (packed0 == 0) {
      null.asInstanceOf[A]
    } else {
      val used = packed0 & 0xf
      val idx  = used - 1
      var a    = array(idx)
      if (idx == 0 && packed0 != 1) {
        val arr0 = a.asInstanceOf[Array[AnyRef]]
        a = arr0(ArrSize - 1)
        packedTags = arr0(ArrSize).asInstanceOf[Array[Int]](0)
        array = arr0
        packed -= 3
      } else {
        packed -= 1
      }
      a.asInstanceOf[A]
    }
  }

  def peek: Boolean = {
    val packed0 = packed
    if (packed0 == 0) {
      false
    } else {
      val used = packed0 & 0xf
      val idx  = used - 1
      if (idx == 0 && packed0 != 1) {
        val tags = (array(idx).asInstanceOf[Array[AnyRef]])(ArrSize).asInstanceOf[Array[Int]](0)
        (tags >> (ArrSize - 1) & 1) == 1
      } else {
        (packedTags >> idx & 1) == 1
      }
    }
  }
}

private object TagStack {
  private final val ArrSize = 15 // Can be made smaller, but not larger
}
