enum Allergen:
  case Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats

object Allergies:
  def allergicTo(allergen: Allergen, allergies: Int): Boolean =
    ((1 << allergen.ordinal) & allergies) != 0

  def list(allergies: Int): List[Allergen] =
    Allergen.values.filter(allergicTo(_, allergies)).toList