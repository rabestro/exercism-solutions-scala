final case class ComplexNumber(real: Double = 0, imaginary: Double = 0):
  def +(that: ComplexNumber): ComplexNumber =
    ComplexNumber(real + that.real, imaginary + that.imaginary)
  def -(that: ComplexNumber): ComplexNumber =
    ComplexNumber(real - that.real, imaginary - that.imaginary)
  def *(that: ComplexNumber): ComplexNumber =
    ComplexNumber(real * that.real - imaginary * that.imaginary, real * that.imaginary + imaginary * that.real)
  def /(that: ComplexNumber): ComplexNumber = ???
  def abs: Double = ???
  def conjugate: ComplexNumber = ???


object ComplexNumber:
  def exp(z: ComplexNumber): ComplexNumber = ???
