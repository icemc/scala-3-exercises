import scala.languageFeature.implicitConversions
/**
 * TYPECLASSES
 * 
 * Scala 3 introduces direct support for typeclasses using contextual features of the language.
 * Typeclasses provide a way to abstract over similar data types, without having to change the 
 * inheritance hierarchy of those data types, providing the power of "mixin" interfaces, but 
 * with additional flexibility that plays well with third-party data types.
 */
object typeclass_basics:
  trait PrettyPrint[-A]:
    extension (a: A) def prettyPrint: String

  given PrettyPrint[String] with
    extension (a: String) def prettyPrint: String = a

  "foo".prettyPrint

  final case class Person(name: String, age: Int)

  /**
   * EXERCISE 1
   * 
   * With the help of the `given` keyword, create an instance of the `PrettyPrint` typeclass for the 
   * data type `Person` that renders the person in a pretty way.
   */
  given PrettyPrint[Person] with 
    extension (p: Person) def prettyPrint: String = s"${p.name} is ${p.age} years old."

  /**
   * EXERCISE 2
   * 
   * With the help of the `given` keyword, create a **named* instance of the `PrettyPrint` typeclass 
   * for the data type `Int` that renders the integer in a pretty way.
   */
  given PrettyPrint[Int] with 
    extension  (i: Int) def prettyPrint: String = s"${i}"

  /**
   * EXERCISE 3
   * 
   * Using the `summon` function, summon an instance of `PrettyPrint` for `String`.
   */
  val stringPrettyPrint: PrettyPrint[String] = summon[PrettyPrint[String]]

  /**
   * EXERCISE 4
   * 
   * Using the `summon` function, summon an instance of `PrettyPrint` for `Int`.
   */
  val intPrettyPrint: PrettyPrint[Int] = summon[PrettyPrint[Int]]

  /**
   * EXERCISE 5
   * 
   * With the help of the `using` keyword, create a method called `prettyPrintIt` that, for any type 
   * `A` for which a `PrettyPrint` instance exists, can both generate a pretty-print string, and 
   * print it out to the console using `println`.
   */
  def prettyPrintIt[A](obj: A)(using i: PrettyPrint[A]) : Unit = println(obj.prettyPrint)

  /**
   * EXERCISE 6
   * 
   * With the help of both `given` and `using`, create an instance of the `PrettyPrint` type class
   * for a generic `List[A]`, given an instance of `PrettyPrint` for the type `A`.
   */
  given [A](using p: PrettyPrint[A]): PrettyPrint[List[A]] with
    extension (a: List[A]) def prettyPrint: String = 
      var result = ""

      for e <- a.zip(0 until a.size) do 
        if e._2 == 0 then result = e._1.prettyPrint
        else result = s"${result} ${e._1.prettyPrint}"
      
      result

  /**
   * EXERCISE 7
   * 
   * With the help of both `given` and `using`, create a **named** instance of the `PrettyPrint` 
   * type class for a generic `Vector[A]`, given an instance of `PrettyPrint` for the type `A`.
   */
  given vectorPrettyPrint[A](using p: PrettyPrint[A]): PrettyPrint[Vector[A]] with 
    extension (v: Vector[A]) def prettyPrint: String = 
      var result = ""

      for e <- v.zip(0 until v.size) do 
        if e._2 == 0 then result = e._1.prettyPrint
        else result = s"${result} ${e._1.prettyPrint}"
      
      result

  import scala.CanEqual._ 

object given_scopes:
  trait Hash[-A]:
    extension (a: A) def hash: Int

  object Hash:
    given Hash[Int] = _.hashCode
    given Hash[Long] = _.hashCode
    given Hash[Float] = _.hashCode
    given Hash[Double] = _.hashCode

  object givens:
    /**
     * EXERCISE 1
     * 
     * Import the right given into the scope (but ONLY this given) so the following code will compile.
     */
    import Hash.given_Hash_Int
    12.hash 

    /**
     * EXERCISE 2
     * 
     * Import the right given into the scope (but ONLY this given) so the following code will compile.
     */
    import Hash.given_Hash_Double
    12.123.hash   

  object usings:
    /**
     * EXERCISE 3
     * 
     * Adding the right `using` clause to this function so that it compiles.
     */
    def hashingInts(using hash: Hash[Int]) = 12.hash 

    /**
     * EXERCISE 4
     * 
     * Adding the right `using` clause to this function so that it compiles.
     */
    def hashingDoubles(using hash: Hash[Double]) = 12.123.hash   

  
object typeclass_derives:
  /**
   * EXERCISE 1
   * 
   * Using the `derives` clause, derive an instance of the type class `CanEqual` for 
   * `Color`.
   */
  enum Color derives CanEqual:
    case Red 
    case Green 
    case Blue

  /**
   * EXERCISE 2
   * 
   * Using the `derives` clause, derive an instance of the type class `CanEqual` for 
   * `Person`.
   */
  final case class Person(name: String, age: Int) derives CanEqual

/**
 * IMPLICIT CONVERSIONS
 * 
 * Scala 3 introduces a new type class called `Conversion` to perform "implicit 
 * conversions"--the act of automatically converting one type to another.
 */
object conversions:
  final case class Rational(n: Int, d: Int)
  
  /**
   * EXERCISE 1
   * 
   * Create an instance of the type class `Conversion` for the combination of types
   * `Rational` (from) and `Double` (to).
   */
  // given ...
  given Conversion[Rational, Double] = r => r.n.toDouble/r.d.toDouble

  /**
   * EXERCISE 2
   * 
   * Multiply a rational number by 2.0 (a double) to verify your automatic
   * conversion works as intended.
   */
  Rational(1, 2) * 2

object typeclass_graduation:
  /**
   * EXERCISE 1
   * 
   * Add cases to this enum for every primitive type in Scala.
   */
  enum PrimType[A]:
    case Int extends PrimType[Int]
  
  /**
   * EXERCISE 2
   * 
   * Add another case to `Data` to model enumerations, like `Either`.
   */
  enum Data:
    case Record(fields: Map[String, Data])
    case Primitive[A](primitive: A, primType: PrimType[A])
    case Collection(elements: Vector[Data])

  /**
   * EXERCISE 3
   * 
   * Develop a type class called `EncodeData[A]`, that can encode an `A` into `Data`.
   */
  trait EncodeData[A]

  /**
   * EXERCISE 4
   * 
   * In the companion object of `Data`, write encoders for different primitive types in Scala,
   * including lists and collections.
   */
  object EncodeData

  /**
   * EXERCISE 5
   * 
   * Create an instance of `EncodeData` for `Person`.
   */
  final case class Person(name: String, age: Int)
  object Person