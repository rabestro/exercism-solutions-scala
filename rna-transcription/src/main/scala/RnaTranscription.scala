object RnaTranscription:
  private type Strand = String

  private val DnaRnaMap = Map('C' -> 'G', 'G' -> 'C', 'T' -> 'A', 'A' -> 'U')

  def toRna(dna: Strand): Option[Strand] =
    val result = dna.foldLeft(Option(new StringBuilder)) {
      case (Some(builder), base) => DnaRnaMap get base map builder.append
      case _ => None
    }
    result.map(_.toString())
