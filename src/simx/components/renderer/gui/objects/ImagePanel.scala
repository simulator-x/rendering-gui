package simx.components.renderer.gui.objects

import simplex3d.math.float.ConstVec2
import java.io.File
import simx.components.renderer.gui.Shape
import simx.core.ontology._
import simx.core.ontology.entities.GuiEffectEntityDescription

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 5/12/13
 * Time: 1:46 PM
 */

case class ImagePanel(
  override val name: String,
  imageFile: File,
  override val pos: ConstVec2,
  override val shape: Shape,
  override val angle: Float = 0f,
  override val layer: Int = 0,
  override val subElements: List[Panel] = Nil,
  override val annotations: Set[Annotation] = Set(),
  override val effects: List[GuiEffectEntityDescription] = Nil,
  val enabled: Boolean = true)
  extends Panel(
    name = name,
    textureFile = Some(imageFile),
    shape = shape,
    pos = pos,
    angle = angle,
    layer = layer,
    subElements = subElements,
    annotations =annotations,
    effects = effects,
    _enabled = enabled
    )