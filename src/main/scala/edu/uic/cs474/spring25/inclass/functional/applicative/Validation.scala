package edu.uic.cs474.spring25.inclass.functional
package applicative
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.*
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid

def ioPrint(s: String): IO[Unit] =
  IO:
    print(s)

def getInputWithPrompt(prompt: String): IO[String] =
  IO:
    print(prompt + " ")
    scala.io.StdIn.readLine()

/** Validation Methods */
object Validation:
  opaque type Error      = String
  opaque type Name       = String
  opaque type Occupation = String

  def validateName(name: String): Validated[List[Error], Name] =
    val isAlphaNumeric = name.forall(_.isLetterOrDigit)
    if !isAlphaNumeric then
      Invalid[List[Error]](List("Name contains invalid characters"))
    else
      Valid[Name](name)
  end validateName

  def validateOccupation(occ: String): Validated[List[Error], Occupation] =
    val isSpecialChar = occ.exists(c => !c.isLetterOrDigit && !c.isWhitespace)
    if isSpecialChar then
      Invalid[List[Error]](List("Occupation contains special characters"))
    else
      Valid[Occupation](occ)
  end validateOccupation

  enum Color:
    case Red, Blue, Yellow, Purple, Orange, Green, Brown, Pink, Black, White

  def validateColor(color: String): Validated[List[Error], Color] =
    Try(Color.valueOf(color)) match
      case Failure(exception) =>
        Invalid[List[Error]](List("Color did not match known color"))
      case Success(value) => Valid[Color](value)
  end validateColor
end Validation

case class PersonInfo(
    name: Validation.Name,
    occupation: Validation.Occupation,
    color: Validation.Color
)
object ValidationApplication:
  val firstPart =
    for
      _          <- ioPrint("Welcome to my program!\n")
      name       <- getInputWithPrompt("What is your name?")
      occupation <- getInputWithPrompt("What is your occupation?")
      color      <- getInputWithPrompt("What is your favorite color?")
    yield (name, occupation, color)

  // Problem: Because we are using flatMap here, we get the fail-fast behavior
  //  of monads. We actually don't want that here, because we want to show
  //  **all** of the errors that occur, not just the first.
  // format: off
  /*
  val programBroken = firstPart.map: tu =>
    for
      validName       <- Validation.validateName(tu._1)
      validOccupation <- Validation.validateOccupation(tu._2)
      validColor      <- Validation.validateColor(tu._3)
    yield (validName, validOccupation, validColor)
  */
  // format: on
  val programFixed = firstPart.map: tu =>
    (
      Validation.validateName(tu._1),
      Validation.validateOccupation(tu._2),
      Validation.validateColor(tu._3)
    ).mapN(PersonInfo.apply)

  def main(args: Array[String]): Unit =
    println(programFixed.unsafeRunSync())
end ValidationApplication
