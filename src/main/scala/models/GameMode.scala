package models

sealed trait GameMode
case object Attack extends GameMode
case object Defend extends GameMode

