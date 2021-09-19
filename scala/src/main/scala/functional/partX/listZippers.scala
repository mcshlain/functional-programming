package functional.part4

import functional.part1.maybePart4.Maybe

object listZippers {

  case class ZipperState[A](
      before: List[A], // the values before the current in reversed order
      after: List[A]
  )

  case class ListZipper[A, B](f: ZipperState[A] => (B, ZipperState[A])) {

    def apply(s: ZipperState[A]): (B, ZipperState[A]) = f(s)

    def map[C](f: B => C): ListZipper[A, C] = ListZipper.map(this, f)

    def flatMap[C](f: B => ListZipper[A, C]): ListZipper[A, C] =
      ListZipper.flatMap(this, f)

  }

  object ListZipper {

    def pure[A, B](v: B): ListZipper[A, B] = ListZipper(s => (v, s))

    def map[A, B, C](zipper: ListZipper[A, B], f: B => C): ListZipper[A, C] =
      ListZipper(s => {
        val (r, s1) = zipper(s)
        (f(r), s1)
      })

    def flatMap[A, B, C](zipper: ListZipper[A, B],
                         f: B => ListZipper[A, C]): ListZipper[A, C] =
      ListZipper(s => {
        val (r, s1) = zipper(s)
        f(r)(s1)
      })

  }

  def moveRight[A]: ListZipper[A, Unit] =
    ListZipper(s =>
      s.after match {
        case Nil => ((), s) // can't move right, stay where you are
        case h :: t => // pattern matching can destruct on custom operators using a special function calle unapply
          ((), ZipperState(h :: s.before, t))
    })

  def moveLeft[A]: ListZipper[A, Unit] =
    ListZipper(s =>
      s.before match {
        case Nil => ((), s) // can't move left, stay where you are
        case h :: t => // pattern matching can destruct on custom operators using a special function calle unapply
          ((), ZipperState(t, h :: s.after))
    })

  def insert[A](v: A): ListZipper[A, Unit] =
    ListZipper(s => {
      ((), s.copy(before = v :: s.before))
    })

  def delete[A]: ListZipper[A, Unit] =
    ListZipper(s =>
      s.before match {
        case Nil    => ((), s)
        case h :: t => ((), s.copy(before = t))
    })

  def isEnd[A]: ListZipper[A, Boolean] = ListZipper(s => (s.after.isEmpty, s))

  def isStart[A]: ListZipper[A, Boolean] =
    ListZipper(s => (s.before.isEmpty, s))

  // this gets the value before
  def get[A]: ListZipper[A, Option[A]] =
    ListZipper(s =>
      s.before match {
        case Nil    => (None, s)
        case h :: _ => (Some(h), s)
    })

  // conditions
  def NOP[A]: ListZipper[A, Unit] = ListZipper.pure(())

  def ifM[A, B](
      cond: ListZipper[A, Boolean],
      ifTrue: => ListZipper[A, B],
      ifFalse: => ListZipper[A, B]
  ): ListZipper[A, B] =
    ListZipper(s => {
      val (r, _) = cond(s)
      if (r) {
        ifTrue(s)
      } else {
        ifFalse(s)
      }
    })

  // looping
  def applyUntilRight[A](
      cond: ListZipper[A, Boolean],
      op: => ListZipper[A, Unit]
  ): ListZipper[A, Unit] = {
    ifM(
      cond,
      NOP,
      for (_ <- op;
           _ <- moveRight;
           _ <- applyUntilRight(cond, op)) yield ()
    )
  }

  def moveToStart[A]: ListZipper[A, Unit] =
    ListZipper(s => {
      ((), ZipperState(List.empty, s.before.reverse ::: s.after)) // can implement it more efficiently here
    })
 
  // scala 3 syntax to add methods to existing classes (in scala 2 this can be done with implicit classes conversions)
  extension[A](lst: List[A]) {
    def runZipper(zipper: ListZipper[A, Unit]): List[A] = {
      val (r, s) =
        zipper.flatMap(_ => moveToStart)(ZipperState(List.empty, lst))
      s.after
    }
  }

}
