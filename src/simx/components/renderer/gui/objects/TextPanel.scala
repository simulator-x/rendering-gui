package simx.components.renderer.gui.objects

import simplex3d.math.float.{Vec2, ConstVec2}
import simx.components.renderer.gui.QuadShape
import java.awt.{Color, Font}

import simx.core.entity.description.SValSet
import simx.core.ontology._

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 5/12/13
 * Time: 1:46 PM
 */

case class TextPanel(
  override val name: String,
  override val text: String,
  override val pos: ConstVec2,
  override val angle: Float = 0f,
  override val layer: Int = 0,
  override val font: Font = new Font(null, Font.PLAIN, 72),
  override val fontColor: Color = Color.BLACK,
  override val subElements: List[Panel] = Nil,
  override val annotations: Set[Annotation] = Set(),
  val enabled: Boolean = true,
  additionalProperties: SValSet = SValSet())
  extends Panel(
    name = name,
    text = text,
    shape = QuadShape(Vec2(Vec2.One)),
    pos = pos,
    angle = angle,
    layer = layer,
    font = font,
    fontColor = fontColor,
    subElements = subElements,
    additionalProperties = additionalProperties,
    _enabled = enabled)
