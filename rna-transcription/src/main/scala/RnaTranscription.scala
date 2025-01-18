object RnaTranscription {
  private val DnaRnaMap = Map(
    'C' -> 'G', 'G' -> 'C', 'T' -> 'A', 'A' -> 'U'
  )

  def toRna(dna: String): Option[String] =
    Option.when(dna.forall(DnaRnaMap.contains))(
      dna.map(DnaRnaMap)
    )
}