package org.hablapps.statelesser
package sql

import _root_.monocle._, _root_.monocle.macros.Lenses, function.all._

object Example extends App {

  // data layer

  @Lenses case class Person(name: String, age: Int)
  @Lenses case class Couple(her: Person, him: Person, since: Long)

  type People  = List[Person]
  type Couples = List[Couple]

  // logic

  import OpticAlg.syntax._

  def couples[E[_], O[_]: OpticAlg[E, ?[_]]]: E[Traversal[Couples, Couple]] = 
    traversal(each, "Couples")

  def people[E[_], O[_]: OpticAlg[E, ?[_]]]: E[Traversal[People, Person]] = 
    traversal(each, "People")

  def since[E[_], O[_]: OpticAlg[E, ?[_]]]: E[Lens[Couple, Long]] =
    lens(Couple.since, "Couples", "since")
  
  def her[E[_], O[_]: OpticAlg[E, ?[_]]]: E[Lens[Couple, Person]] =
    lens(Couple.her, "Couples", "her")

  def age[E[_], O[_]: OpticAlg[E, ?[_]]]: E[Lens[Person, Int]] =
    lens(Person.age, "People", "age")

  def name[E[_], O[_]: OpticAlg[E, ?[_]]]: E[Lens[Person, String]] =
    lens(Person.name, "People", "name")

  def getPeople[E[_], O[_]: OpticAlg[E, ?[_]]]: O[People => List[Person]] =
    traversalGetAll(people)

  def getPeopleName[E[_], O[_]: OpticAlg[E, ?[_]]]: O[People => List[String]] =
    traversalGetAll(traversalComposeLens(people, name))

  def getPeopleAgeAndName[E[_], O[_]: OpticAlg[E, ?[_]]]
      : O[People => List[(Int, String)]] =
    traversalGetAll(traversalComposeLens(
      people, 
      lensHorizComposeLens(age, name)))

  def getHerAges[E[_], O[_]: OpticAlg[E, ?[_]]]: O[Couples => List[Int]] =
    traversalGetAll(traversalComposeLens(couples, lensComposeLens(her, age)))

  def getHerAges2[E[_], O[_]: OpticAlg[E, ?[_]]]: O[Couples => List[Int]] =
    traversalGetAll(traversalComposeLens(traversalComposeLens(couples, her), age))

  def getHerAgeAndName[E[_], O[_]: OpticAlg[E, ?[_]]]
      : O[Couples => List[(Int, String)]] =
    traversalGetAll(traversalComposeLens(
      couples, 
      lensComposeLens(her, lensHorizComposeLens(age, name))))

  def getHer[E[_], O[_]: OpticAlg[E, ?[_]]]: O[Couples => List[Person]] =
    traversalGetAll(traversalComposeLens(couples, her))

  def getHerAgeAndSince[E[_], O[_]: OpticAlg[E, ?[_]]]
      : O[Couples => List[(Long, Int)]] =
    traversalGetAll(traversalComposeLens(
      couples, 
      lensHorizComposeLens(since, lensComposeLens(her, age))))

  println(getPeople[At, Sql](SqlOpticAlg))
  println(getPeopleName[At, Sql](SqlOpticAlg))
  println(getPeopleAgeAndName[At, Sql](SqlOpticAlg))
  println(getHerAges[At, Sql](SqlOpticAlg))
  println(getHerAges2[At, Sql](SqlOpticAlg))
  println(getHerAgeAndName[At, Sql](SqlOpticAlg))
  println(getHer[At, Sql](SqlOpticAlg))
  println(getHerAgeAndSince[At, Sql](SqlOpticAlg))
}

