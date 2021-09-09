package valmUtil

import lts.{Label, Node}

sealed abstract class Item

case class State(s:Node) extends Item


case class Transition(t:(Node,Label,Node)) extends Item
