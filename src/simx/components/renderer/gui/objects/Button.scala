package simx.components.renderer.gui.objects

import java.io.File
import simplex3d.math.float.ConstVec2
import simx.components.renderer.gui.Shape

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 5/5/13
 * Time: 10:50 PM
 */


case class Button(
  override val name: String,
  imageFile: File,
  override val pos: ConstVec2,
  override val shape: Shape,
  override val angle: Float = 0f,
  override val layer: Int = 0,
  isElastic: Boolean = false,
  isLoot: Boolean = false,
  isDraggable: Boolean = false,
  override val subElements: List[Panel] = Nil)
  extends Panel(
    name = name,
    textureFile = Some(imageFile),
    shape = shape,
    pos = pos,
    angle = angle,
    layer = layer,
    isClickable = true,
    _isElastic = isElastic,
    _isLoot = isLoot,
    _isDraggable = isDraggable,
    subElements = subElements)



