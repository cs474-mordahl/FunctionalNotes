package edu.uic.cs474.spring25.inclass.functional
package applicative
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import scala.runtime.RichChar
import cats.implicits.*

def ioPrint(s: String): IO[Unit] =
  IO:
    print(s)

def getInputWithPrompt(prompt: String): IO[String] =
  IO:
    print(prompt + " ")
    scala.io.StdIn.readLine()

/** Validation Methods */
type Error      = String
type Name       = String
type Occupation = String
type Color      = String
def validateName(name: String): Either[Error, Name] =
  val isAlpha =
    name.map(c =>
      List[Char](
        '1',
        '2',
        '3',
        '4',
        '5',
        '6',
        '7',
        '8',
        '9',
        '0'
      ).contains(c)
    ).toList
  if isAlpha.exists(b => b) then
    "There is a number".asLeft[Name]
  else
    name.asRight[Error]
end validateName

def validateOccupation(occ: String): Either[Error, Occupation] =
  val isAlpha =
    occ.map(c =>
      List[Char](
        '1',
        '2',
        '3',
        '4',
        '5',
        '6',
        '7',
        '8',
        '9',
        '0'
      ).contains(c)
    ).toList
  if isAlpha.exists(b => b) then
    "There is a number".asLeft[Occupation]
  else
    occ.asRight[Error]
end validateOccupation
def validateColor(color: String): Either[Error, Color] = color.asRight[Error]
object Validation:
  val firstPart =
    for
      _          <- ioPrint("Welcome to my program!\n")
      name       <- getInputWithPrompt("What is your name?")
      occupation <- getInputWithPrompt("What is your occupation?")
      color      <- getInputWithPrompt("What is your favorite color?")
    yield (name, occupation, color)

  val program = firstPart.map: tu =>
    for
      validName       <- validateName(tu._1)
      validOccupation <- validateOccupation(tu._2)
      validColor      <- validateColor(tu._3)
    yield (validName, validOccupation, validColor)

  def main(args: Array[String]): Unit =
    println(program.unsafeRunSync())
end Validation
