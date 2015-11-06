package simx.components.renderer.gui

import simx.core.worldinterface.eventhandling.EventDescription
import simx.components.renderer.gui.local.Symbols

object Events {

  //Has to contain a Position2D and may contain a ShaderColor
  val explosion = new EventDescription(Symbols.explosion)
  val playerHit = new EventDescription(Symbols.playerHit)

  //Has to contain a String and a Position2D
  //May contain a Color and a Font
  val notification = new EventDescription(Symbols.notification)
  val buttonPressed = new EventDescription(Symbols.buttonPressed)
}

