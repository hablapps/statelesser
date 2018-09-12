package org.hablapps
package statelesser

package test
package university

import scalaz._
import org.scalatest._
import GetEvidence._

class UniversitySpec extends FlatSpec with Matchers {
  
  case class SPerson(first: String, last: String)
  val john = SPerson("john", "doe")

  "Automagic instances" should "be generated for a one-field algebra" in {

    case class Person[P[_], Per: MonadState[P, ?]](last: LensAlg[P, String])

    val personState = make[Person[State[SPerson, ?], SPerson]]

    val getLast = personState.last.get
    val upcLast = personState.last.modify(_.toUpperCase)

    getLast(john) shouldBe ((john, john.last))
    upcLast.exec(john) shouldBe john.copy(last = john.last.toUpperCase)
  }

  it should "be generated for an algebra with several fields" in {

    case class Person[P[_], Per: MonadState[P, ?]](
      first: LensAlg[P, String],
      last:  LensAlg[P, String])

    val personState = make[Person[State[SPerson, ?], SPerson]]

    val getFirst = personState.first.get
    val revFirst = personState.first.modify(_.reverse)
    val getLast  = personState.last.get
    val upcLast  = personState.last.modify(_.toUpperCase)

    getFirst(john) shouldBe ((john, john.first))
    revFirst.exec(john) shouldBe john.copy(first = john.first.reverse)
    getLast(john) shouldBe ((john, john.last))
    upcLast.exec(john) shouldBe john.copy(last = john.last.toUpperCase)
  }
}

