/**
 * EXTENSION METHODS
 * 
 * Scala 3 brings first-class support for "extension methods", which allow adding methods to 
 * classes after their definition. Previously, this feature was emulated using implicits.
 */
object ext_methods:
  final case class Email(value: String)
   /* EXERCISE 1
   * 
   * Add an extension method to `Email` to retrieve the username of the email address (the part 
   * of the string before the `@` symbol).
   */
  extension (e: Email) def username: String = "e.value.substring(0, e.value.indexOf('@'))"

  val sherlock = Email("sherlock@holmes.com").username

  /**
   * EXERCISE 2
   * 
   * Add an extension method to `Email` to retrieve the server of the email address (the part of 
   * the string after the `@` symbol).
   */
  // extension

  extension (e: Email) def server: String = "e.value.substring(e.value.indexOf('@') + 1, e.value.length)"

  /**
   * EXERCISE 3
   * 
   * Add an extension method to `Option[A]` that can zip one option with another `Option[B]`, to 
   * return an `Option[(A, B)]`.
   */
  // extension 

  extension [A, B](op1: Option[A]) def zip(op2: Option[B]): Option[(A, B)] = op1.zip(op2)

  /**
   * A rational number is one in the form n/m, where n and m are integers.
   */
  final case class Rational(numerator: BigInt, denominator: BigInt)

  /**
   * EXERCISE 4
   * 
   * Add a collection of extension methods to `Rational`, including `+`, to add two rational 
   * numbers, `*`, to multiply two rational numbers, and `-`, to subtract one rational number 
   * from another rational number.
   */
  // extension

  extension (r1: Rational) {
    def + (r2: Rational): Rational = Rational((r1.numerator * r2.denominator) + (r2.numerator * r1.denominator), r1.denominator * r2.denominator)

    def - (r2: Rational): Rational = Rational((r1.numerator * r2.denominator) - (r2.numerator * r1.denominator), r1.denominator * r2.denominator)

    def * (r2: Rational): Rational = Rational(r1.numerator * r2.numerator, r1.denominator * r2.denominator)
  }

  /**
   * 
   * EXERCISE 5
   * 
   * Convert this implicit syntax class to use extension methods.
   */
  // implicit class StringOps(self: String):
  //   def equalsIgnoreCase(that: String) = self.toLowerCase == that.toLowerCase

  
  extension (self: String) def equalsIgnoreCase(that: String) = self.toLowerCase == that.toLowerCase
  /**
   * EXERCISE 6
   * 
   * Import the extension method `isSherlock` into the following object so the code will compile.
   */
  object test:
    import string_extensions._
    val test: Boolean = "John Watson".isSherlock

  object string_extensions:
    extension (s: String) def isSherlock: Boolean = s.startsWith("Sherlock")