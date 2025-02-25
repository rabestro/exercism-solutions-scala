final case class DNA(value: String):
  type Error = String
  def nucleotideCounts: Either[Error, String] = ???