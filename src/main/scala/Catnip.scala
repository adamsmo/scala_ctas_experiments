object Catnip {

  implicit class IdOp[A](val a: A) extends AnyVal {
    def some: Option[A] = Some(a)
  }

  def none[A]: Option[A] = None
}