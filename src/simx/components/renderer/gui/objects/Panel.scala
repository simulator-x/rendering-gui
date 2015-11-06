package simx.components.renderer.gui.objects

import java.awt.{Color, Font}
import java.io.File
import javax.imageio.ImageIO

import simplex3d.math.float._
import simplex3d.math.{ConstVec2i, Vec2i}
import simx.components.renderer.gui._
import simx.core.components.physics.ImplicitEitherConversion._
import simx.core.components.renderer.createparameter.{ReadFromElseWhere, ShapeFromFile}
import simx.core.components.renderer.setup.DisplaySetupDesc
import simx.core.entity.description.{SValSet, EntityAspect}
import simx.core.ontology
import simx.core.ontology._
import simx.core.ontology.entities.GuiEffectEntityDescription
import simx.core.worldinterface.naming.NameIt

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 5/5/13
 * Time: 10:50 PM
 */

object Panel {
  def getNextModelFile = "assets/gui/1x1-xy-square.dae"
  val fullScreenResolution = ConstVec2i(1920, 1080)

  def nativeScreenSizeOf(displaySetup: DisplaySetupDesc)(imgFile: File) = {
    val screenResolution =
      displaySetup.firstDisplay.resolution.map(res => ConstVec2i(res._1, res._2)).getOrElse(fullScreenResolution)
    //Meter per Pixel
    val screenMPP = displaySetup.firstDisplay.sizeVec / ConstVec2(screenResolution)

    val img = ImageIO.read(imgFile)
    val size = Vec2i(img.getWidth, img.getHeight)
    size * screenMPP
  }

  def nativeShapeOf(displaySetup: DisplaySetupDesc)(imgFile: File) =
    QuadShape(nativeScreenSizeOf(displaySetup)(imgFile))

  private def toSubElement(p: Panel): EntityAspect =
    new SpecificEntityDescription(
      if(p.annotations.isEmpty) ontology.types.GuiElement else ontology.types.GuiElement.setAnnotations(p.annotations),
      p.aspects,
      Symbol(p.name)
    )

  private def toEffect(effect: GuiEffectEntityDescription): EntityAspect =
    new SpecificEntityDescription(
      ontology.types.GuiEffect.setAnnotations(Symbols.circularProgressBar),
      effect.aspects,
      effect.name
    )
}

abstract class Panel( val name: String,
             val pos: ConstVec2,
             val shape: Shape,
             val angle : Float = 0f,
             val layer: Int = 0,
             val text: String = "",
             val textureFile: Option[File] = None,
             val isClickable: Boolean = false,
             val font: Font = new Font(null, Font.PLAIN, 72),
             val fontColor: Color = Color.BLACK,
             val _isDraggable: Boolean = false,
             val _isElastic: Boolean = false,
             val _isLoot: Boolean = false,
             val subElements: List[Panel] = Nil,
             val effects: List[GuiEffectEntityDescription] = Nil,
             val annotations: Set[Annotation] = Set(),
             val _enabled: Boolean = true,
             additionalProperties: SValSet = SValSet())
    extends EntityDescription(
      List[EntityAspect](
        GuiElement(
          shape = shape,
          position = pos,
          angle = angle,
          layer = layer,
          text = text,
          textureFile = Some(textureFile.map(_.getPath).getOrElse("")),
          isClickable = isClickable,
          font = font,
          fontColor = fontColor,
          isDraggable = _isDraggable,
          isElastic = _isElastic,
          isLoot = _isLoot,
          enabled = _enabled
        ),
        ShapeFromFile(
          file = Panel.getNextModelFile,
          transformation = ReadFromElseWhere,
          scale = ConstMat4(Mat3x4.Identity),
          provideTexture = false
        ),
        NameIt(name)
      ) ::: subElements.map(Panel.toSubElement) ::: effects.map(Panel.toEffect),
      Symbol(name),
      Nil,
      annotations,
      additionalProperties
    )

