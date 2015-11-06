package simx.components.renderer.gui
import java.io.File
import simx.core.entity.typeconversion.TypeInfo.DataTag
import simx.core.svaractor.{SingletonActor, SVarActor}
import simplex3d.math.Vec2i
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import simx.core.helper.TextureData
import java.awt.{RenderingHints, Color, Font}

import scala.reflect.ClassTag


/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 5/20/13
 * Time: 9:18 AM
 */
object GuiResourceManager extends SingletonActor(new GuiResourceManager(), "GuiResourceManager")

case class AskImgFile(f : File)
case class AskRenderedText(text: String, font: Font, fontColor: Color)

class GuiResourceManager extends SVarActor {

  var loadedTextures: Map[File, TextureData] = Map()
  var loadedTexts: Map[AskRenderedText, TextureData] = Map()

  private var loading = Map[File, List[TextureData => Any]]()
  private var loadingTexts = Map[AskRenderedText, List[TextureData => Any]]()

  private def getLoading(f : File) = {
    loading.getOrElse(f, Nil)
  }

  private def getLoadingTexts(i : AskRenderedText) = {
    loadingTexts.getOrElse(i, Nil)
  }

  private def loadTexture(f: File) = {
    //println("Loading: " + f)
    val img = ImageIO.read(f)
    TextureData(img)
  }

  private def loadText(info: AskRenderedText) = {
    val preG = new BufferedImage(1, 1, BufferedImage.TYPE_4BYTE_ABGR).createGraphics()
    //See: http://docs.oracle.com/javase/tutorial/2d/text/measuringtext.html
    val metrics = preG.getFontMetrics(info.font)
    val size = Vec2i(metrics.stringWidth(info.text), metrics.getHeight)
    preG.dispose()

    val img = new BufferedImage(size.x, size.y, BufferedImage.TYPE_4BYTE_ABGR)
    val g = img.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g.setColor(new Color(0,0,0,0))
    g.fillRect(0, 0, img.getWidth, img.getHeight)
    g.setColor(info.fontColor)
    g.setFont(info.font)
    g.drawString(info.text, 0, size.y - metrics.getDescent)
    g.dispose()

    TextureData(size, TextureData.imageToTextureData(img))
  }


  addHandler[AskImgFile]{ msg =>
    val file = msg.f
    checkFile(file)

    loadedTextures.get(file) match {
      case Some(loaded) =>
        loaded
      case None =>
        loading get file match {
          case None =>
            loading = loading.updated(file, Nil)
            delayedReplyWith(asyncLoad[File, TextureData](loadTexture, file)){
            loaded  : TextureData =>
              loadedTextures = loadedTextures + (file -> loaded)
              getLoading(file).foreach(_.apply(loaded))
              loading = loading - file
              loaded
          }
          case Some(list) =>
            loading = loading.updated(file,  provideAnswer :: list )
            DelayedAnswer
        }
    }
  }

  addHandler[AskRenderedText]{ msg =>
    val info = msg

    Match(loadedTexts.get(info)) {
      case Some(loaded) => loaded
      case None =>
        loadingTexts get info match {
          case None =>
            loadingTexts = loadingTexts.updated(info, Nil)
            delayedReplyWith(asyncLoad[AskRenderedText, TextureData](loadText, info)){
            loaded  : TextureData =>
              loadedTexts = loadedTexts + (info -> loaded)
              getLoadingTexts(info).foreach(_.apply(loaded))
              loadingTexts = loadingTexts - info
              loaded
          }
          case Some(list) =>
            loadingTexts = loadingTexts.updated(info,  provideAnswer :: list )
            DelayedAnswer
        }
    }
  }

  private def checkFile( file : File ){
    require( file != null, "The parameter 'file' must not be 'null'" )
    require( file.isFile, "The parameter 'file' must point to a file: " + file )
    require( file.exists, "The parameter 'file' must point to a existing file." )
  }

  protected def asyncLoad[T, U : DataTag : ClassTag]( loadFunc : T => U , param : T) =
    (handler : U => Any) => ask[U](spawnActor(new LoaderActor(loadFunc)), Load(param))(handler(_))

  protected case class Load[T](param : T)
  protected class LoaderActor[T, U](loader : T => U) extends SVarActor{
    addHandler[Load[_]]{ msg  =>
      val retVal = loader( msg.param.asInstanceOf[T] )
      context.stop(self)
      provideAnswer(retVal)
    }
  }
}