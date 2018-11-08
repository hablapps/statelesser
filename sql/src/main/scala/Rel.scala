package org.hablapps.statelesser
package sql

sealed trait RelTree {

  def leafs: List[Node] = this match {
    case n @ Node(_, _, Bunch(Nil)) => List(n)
    case Node(_, _, tree) => tree.leafs
    case Bunch(xs) => xs.flatMap(_.leafs)
  }

  def innerTabs: Set[Table] = this match {
    case Node(tab, _, tree) => Set(tab) ++ tree.innerTabs
    case Bunch(xs) => xs.flatMap(_.innerTabs.toList).toSet
  }

  def append(other: RelTree): RelTree = this match {
    case n @ Node(_, _, Bunch(Nil)) => n.copy(tree = other)
    case n: Node => n.copy(tree = n.tree.append(other))
    case Bunch(xs) => Bunch(xs.map(_.append(other)))
  }
}

case class Node(tab: Table, att: Column, tree: RelTree = Bunch()) extends RelTree

case class Bunch(trees: List[RelTree] = List.empty) extends RelTree

case class Rel(sym: Map[Any, RelTree] = Map.empty)

