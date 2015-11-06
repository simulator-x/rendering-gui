package simx.components.renderer.gui.objects

import simplex3d.math.float.ConstVec2
import java.io.File
import simx.components.renderer.gui.Shape

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 5/5/13
 * Time: 10:50 PM
 */


case class DragButton(
  override val name: String,    // what is the point to override a immutable variable...martin.. really...why do you do that... are you drunk again.. ;)
  imageFile: File,
  override val pos: ConstVec2,        // what is the point to override a immutable variable...martin.. really...why do you do that... are you drunk again.. ;)
  override val shape: Shape,        // what is the point to override a immutable variable...martin.. really...why do you do that... are you drunk again.. ;)
  override val angle: Float = 0f,    // what is the point to override a immutable variable...martin.. really...why do you do that... are you drunk again.. ;)
  override val layer: Int = 0,       // what is the point to override a immutable variable...martin.. really...why do you do that... are you drunk again.. ;)
  override val subElements: List[Panel] = Nil)     // what is the point to override a immutable variable...martin.. really...why do you do that... are you drunk again.. ;)
  extends Panel(
    name = name,
    textureFile = Some(imageFile),
    shape = shape,
    pos = pos,
    angle = angle,
    layer = layer,
    _isDraggable = true,
    subElements = subElements)