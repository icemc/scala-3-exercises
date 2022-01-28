import scala.{util => ju}
/**
 * ENUMS
 * 
 * Scala 3 adds support for "enums", which are to sealed traits like case classes 
 * were to classes. That is, enums cut down on the boilerplate required to use 
 * the "sealed trait" pattern for modeling so-called sum types, in a fashion very 
 * similar to how case classes cut down on the boilerplate required to use 
 * classes to model so-called product types.
 * 
 * Strictly speaking, Scala 3 enums are not the same as Java enums: while the 
 * constructors of enums are finite, and defined statically at compile-time in the 
 * same file, these constructors may have parameters, and therefore, the total 
 * number of values of any enum type could be large or infinite.
 * 
 * Enums and case classes provide first-class support for "algebraic data types" 
 * in Scala 3.
 */
package enums: 
  /**
   * EXERCISE 1
   * 
   * Convert this "sealed trait" to an enum.
   */
  enum DayOfWeek:
    case Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday
  
  /**
   * EXERCISE 2
   * 
   * Explore interop with Java enums by finding all values of `DayOfWeek`, and by 
   * finding the value corresponding to the string "Sunday".
   */
  def daysOfWeek: Array[DayOfWeek] = DayOfWeek.values
  def sunday: DayOfWeek = DayOfWeek.valueOf("Sunday")

  /**
   * EXERCISE 3
   * 
   * Convert this "sealed trait" to an enum.
   * 
   * Take special note of the inferred type of any of the case constructors!
   */
  enum Color(red: Int, green: Int, blue: Int):
    case Red extends Color(255, 0, 0)
    case Green extends Color(0, 255, 0)
    case Blue extends Color(0, 0, 255)
    case Custom(red: Int, green: Int, blue: Int) extends Color(red, green, blue)

  /**
   * EXERCISE 4
   * 
   * Convert this "sealed trait" to an enum.
   * 
   * Take special note of the inferred type parameters in the case constructors!
   */
  enum Result[+Error, +Value]:
    case Succeed[Value](value: Value) extends Result[Nothing, Value]
    case Fail[Error](error: Error) extends Result[Error, Nothing]
    
  /**
   * EXERCISE 5
   * 
   * Convert this "sealed trait" to an enum.
   * 
   * Take special note of the inferred type parameters in the case constructors!
   */
  enum Workflow[-Input, +Output]:
    case End[Output](value: Output) extends Workflow[Any, Output]

  /**
   * EXERCISE 6
   * 
   * Convert this "sealed trait" to an enum.
   */
  enum Conversion[-From, +To]:
    case AnyToString extends Conversion[Any, String]
    case StringToInt extends Conversion[String, Option[Int]]

/**
 * CASE CLASSES
 * 
 * Scala 3 makes a number of improvements to case classes.
 */
package case_classes:
  /**
   * EXERCISE 1
   * 
   * By making the public constructor private, make a smart constructor for `Email` so that only 
   * valid emails may be created.
   */
  object test:
    abstract case class Email private (value: String)
    object Email:
      def fromString(v: String): Option[Email] = if isValidEmail(v) then Some(new Email(v){}) else None
      def isValidEmail(v: String): Boolean = v.matches("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,6}$")

  /**
   * EXERCISE 2
   * 
   * Try to make a copy of an existing `Email` using `Email#copy` and note what happens.
   * 
   */
  // def changeEmail(email: Email): Email = email.copy
  //Error: method copy cannot be accessed as a member of (email : Email)

  /**
   * EXERCISE 3
   * 
   * Try to create an Email directly by using the generated constructor in the companion object.
   * 
   */
  // def caseClassApply(value: String): Email = Email("abc@gmail.com")
  //Error: method apply cannot be accessed as a member of Email.type
/**
 * PATTERN MATCHING
 * 
 * Scala 3 provides upgrades to the power and flexibility of pattern matching.
 */  
object pattern_matching:
  /**
   * Augmented custom extractors
   */
  