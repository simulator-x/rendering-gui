package simx.components.renderer.gui

import java.awt.image.BufferedImage

import simx.core.entity.description.{NamedSValSet, EntityAspect}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.components.renderer.gui.ontology.{types => lt}
import simx.core.ontology.{types => gt, _}
import java.awt.{Color, Font}
import simplex3d.math.float._
import simx.core.helper.TextureData

case class GuiElement(
  shape: Shape,
  position: ConstVec2,
  angle: Float = 0f,
  layer: Int = 0,
  text: String = "",
  textureFile: Option[String] = None,
  textureData: Option[BufferedImage] = None,
  isClickable: Boolean = false,
  font: Font = new Font(null, Font.PLAIN, 72),
  fontColor: Color = Color.BLACK,
  isDraggable: Boolean = false,
  isElastic: Boolean = false,
  isLoot: Boolean = false,
  enabled: Boolean = true,
  override val targets: List[Symbol] = Nil) extends EntityAspect(Symbols.gui, Symbols.guiElement, targets) {

  def getFeatures =
    Set(gt.Transformation, lt.TangibleList, gt.Angle, gt.Position2D,
        gt.Name.addAnnotations(Symbols.identifier).asConst, gt.String, gt.File, lt.Shape,
        lt.Layer, gt.Color.addAnnotations(Symbols.font), gt.Font, gt.Enabled, gt.Texture)

  def getProvidings: Set[ConvertibleTrait[_]] =
    Set(gt.Transformation, lt.TangibleList, gt.Angle, gt.Position2D, gt.String, gt.File, lt.Shape,
        lt.Layer, gt.Color.addAnnotations(Symbols.font), gt.Font, gt.Enabled, gt.Texture)

  def getCreateParams = {
    val res = new NamedSValSet(aspectType)

    res.add(lt.Layer(layer))
    res.add(lt.Shape(shape))
    res.add(gt.Position2D(position))
    res.add(gt.Angle(angle))
    res.add(gt.String(text))
    res.add(gt.Boolean.addAnnotations(local.Symbols.isClickable)(isClickable))
    res.add(lt.IsDraggable(isDraggable)) //TODO: aks dennis for equals on ConvertibleTrait
    res.add(lt.IsElastic(isElastic))
    res.add(lt.IsLoot(isLoot))
    res.add(lt.TangibleList(List[Symbol]()))
    res.add(gt.Color.addAnnotations(Symbols.font)(fontColor))
    res.add(gt.Font(font))
    res.add(gt.Enabled(enabled))


    if (textureData.isDefined) {
      res.add(gt.Image(textureData.get))
    } else if (textureFile.isDefined) {
      if (!textureFile.get.equals("")) {
        res.add(gt.File(textureFile.get))
      }
    }

    res
  }
}

case class CircularProgressBar(
  override val targets: List[Symbol] = Nil
) extends EntityAspect(Symbols.gui, Symbols.circularProgressBar, targets)
{
  def getFeatures = Set(gt.Integer, gt.Color)
  def getProvidings = getFeatures
  def getCreateParams = NamedSValSet(aspectType, gt.Integer(0), gt.Color(Color.ORANGE))
}

case class OverlayTexture(pos : ConstVec2, tex : TextureData)
  extends EntityAspect(Symbols.gui, local.Symbols.overlayTexture){

  def getFeatures: Set[ConvertibleTrait[_]] =
    Set(gt.Position2D, gt.Texture)


  def getProvidings: Set[ConvertibleTrait[_]] =
    getFeatures

  def getCreateParams: NamedSValSet =
    addCVars(gt.Position2D(pos) and gt.Texture(tex))
}

case class OverlayText(pos : ConstVec2, text : String, color  : Color, font : Font)
  extends EntityAspect(Symbols.gui, local.Symbols.overlayText){

  def getFeatures: Set[ConvertibleTrait[_]] =
    Set(gt.Position2D, gt.Texture, gt.String)


  def getProvidings: Set[ConvertibleTrait[_]] =
    getFeatures

  def getCreateParams: NamedSValSet =
    addCVars(gt.Position2D(pos) and gt.String(text) and gt.Color(color) and gt.Font(font))
}