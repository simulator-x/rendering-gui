package simx.components.renderer.gui

import java.awt.image.BufferedImage
import java.awt.{Color, Font, GraphicsEnvironment}
import java.io.File
import java.util.UUID

import simplex3d.math.ConstVec2i
import simplex3d.math.float._
import simplex3d.math.float.functions._
import simx.components.renderer.gui.objects.{Panel, TextPanel}
import simx.components.renderer.gui.ontology.{types => lt}
import simx.components.renderer.jvr.{PPE, PostProcessingEffect}
//import simx.components.tuio
import simx.core.component.{Component, ComponentHandling}
import simx.core.components.io.IORegistryHandling
import simx.core.components.renderer.setup.DisplaySetupDesc
import simx.core.entity.Entity
import simx.core.entity.component.{ComponentAspect, EntityConfigLayer, EntityCreationHandling}
import simx.core.entity.description.{EntityAspect, NamedSValSet, SValSet}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.helper.{SVarUpdateFunctionMap, TextureData}
import simx.core.ontology.{EntityDescription, Symbols, types => gt}
import simx.core.svaractor.SVarActor
import simx.core.worldinterface.eventhandling.{Event, EventHandler, EventProvider}
import simx.core.worldinterface.naming.NameIt

import scala.collection.immutable
import scala.collection.immutable.HashMap
import scala.util.Random

case class Ask2dWorldPositionOf(e: Entity)

case class Provide2dWorldPositionOf(pos: ConstVec2)

object GuiComponent {
  def convertSurfaceToWorld(v: ConstVec2, screenSize: ConstVec2) =
    ConstVec2(((screenSize * v) - (screenSize * 0.5f)) * ConstVec2(1f, -1f))

  def convertWorldToSurface(v: ConstVec2, screenSize: ConstVec2) =
    ConstVec2(((v * ConstVec2(1f, -1f)) / screenSize) + (Vec2.One * 0.5f))
}

case class GuiComponentAspect(name: Symbol, displaySetupDesc: DisplaySetupDesc, layerDistance: Float = 0.0001f)
  extends ComponentAspect[GuiComponent](Symbols.gui, name, Seq[Any](layerDistance)) {
  def getComponentFeatures: Set[ConvertibleTrait[_]] = Set()

  def getCreateParams: NamedSValSet = NamedSValSet(aspectType, gt.DisplaySetupDescription(displaySetupDesc))
}

/**
 * Component for creating and managing 2D widgets that are realized in a 3D environment
 * @param componentName Name of this GuiComponent
 * @param layerDistance Distance (along the z-axis) between layers of 2D gui elements
 */
class GuiComponent(override val componentName: Symbol, layerDistance: Float)
  extends Component(componentName, Symbols.gui)
  with SVarUpdateFunctionMap with EntityConfigLayer with EventProvider with IORegistryHandling with EventHandler
  with ComponentHandling with EntityCreationHandling {
  private var configured = false

  private var _screenSize: Option[ConstVec2] = None

  private def setScreenSize(v: ConstVec2) {
    _screenSize = Some(v)
  }

  private def screenSize = _screenSize.getOrElse {
    println("[GUI-Component][warn] Screen size not available. Configuration missing!"); Vec2.One
  }

  private var _screenResolution: Option[ConstVec2i] = None
  private val fullScreenResolution = ConstVec2i(1920, 1080)

  private def screenResolution = _screenResolution.getOrElse(fullScreenResolution)

  lazy val sHalf = screenSize * 0.5f

  //Meter per Pixel
  private def screenMPP = screenSize / ConstVec2(screenResolution)

  private var elements = Map[Entity, GuiElementData]()
  private var effects = Map[Entity, GuiEffectData]()
  private var buttons = List[Entity]()

  private var draggableButtons = List[Entity]()
  private var draggedButtons = Map[Entity, SimulationContext]()
  private var droppedButtons = Map[Entity, SimulationContext]()

  val voidPos = ConstMat4(Mat4x3.translate(Vec3(0, 0, 1000f)))

  private var explosion: Option[ExplosionContext] = None

  var notificationDurationInSec = 5f

  private val scaleFactorOnPress = 0.95f

  private val notificationLayer = 3

  private def notificationPanelName(id: UUID) = "Notification_" + id.toString

  private def notificationTextName(id: UUID, nr: Int) = notificationPanelName(id) + "_txt_" + nr

  private val hitDuration = 2400L
  private val hitBlinks = 4L
  private var hitIsActive = false

  private var notifications = List[NotificationContext]()
  private var flyingNotifications = List[FlyingNotificationContext]()


  private val minActivationTimeDifferenceInMillis = 100

  private var activeElasticButtons = List[ElasticButtonSimulationCtx]()

  private var activeLoots = List[LootSimulationCtx]()

  private var startOfLastFrame = 0L
  private val simulationStepPeriod = 16L

  val notificationAngularVelocity = 10f
  val notificationAngularAcceleration = 20f
  val notificationLinearVelocity = 0.02f

  val explosionDurationInSec = 3f

  //Parameters for dropped buttons
  val springConstant = 50f
  val linearDamping = 0.98f
  lazy val epsilon = screenSize.x / 30f

  //Parameters for elastic buttons
  private val elasticButtonSwingDurationInSec = 2f
  private val swingFrequencyFactor = 10f
  private val swingFalloffFactor = 20f
  private val maxSwingScaleAmplitude = 0.3f

  private var explosionShader: Option[Entity] = None

  private var latestMousePosition = Vec2.Zero
  private val mouseCursorId = 'mouse

  private var latestTuioObjects = Map[Symbol, (Vec2, Float)]()

  private val noTextTexture = prepareNoTextTexture()
  private val transparentTex = TextureData(ConstVec2i(1, 1), Array[Byte](0.toByte, 0.toByte, 0.toByte, 0.toByte))

  /**
   * Use SVarUpdateFunctionMap to ignore own writes when observing SVars
   */
  ignoredWriters = immutable.Set(self)

  var isInShaderCreation = false
  var openRequests = List[Event]()

  //Register for events
  Events.explosion.observe { e =>
    if (explosionShader.isEmpty && !isInShaderCreation) {
      val shaderEffect =
        PostProcessingEffect("Explosion").
          describedByShaders("pipeline_shader/quad.vs" :: "assets/shader/explosion.fs" :: Nil).
          usingColorBufferAsName("sceneMap").
          where("time").
          hasValue(-1f).
          isReachableBy(gt.TimeInSeconds).
          and("posX").
          hasValue(0.5f).
          isReachableBy(lt.PosX).
          and("posY").
          hasValue(0.5f).
          isReachableBy(lt.PosY).
          and("color").
          hasValue(ConstVec4(0f, 1f, 0f, 0f)).
          isReachableBy(lt.ShaderColor).
          pack

      isInShaderCreation = true
      openRequests ::= e

      new EntityDescription(PPE(ppe = shaderEffect), NameIt(name = "ExplosionShader")).
        realize { ent =>
        explosionShader = Some(ent)
        openRequests.reverse.foreach(handleExplosionRequest)
        openRequests = Nil
        isInShaderCreation = false
      }
    }
    else if (isInShaderCreation)
      openRequests ::= e
    else
      handleExplosionRequest(e)
  }
  Events.notification.observe(createNotification)
//  tuio.Events.tuioCursorRemoved.observe(e => cursorRemoved(Vec2(e.get(gt.Position2D).get), e.get(gt.Identifier).get))
//  tuio.Events.tuioCursorAdded.observe(e => cursorAdded(Vec2(e.get(gt.Position2D).get), e.get(gt.Identifier).get))
  Events.playerHit.observe(e => if (!hitIsActive) simulateHit())

  //Define answers to requests
  addHandler[Ask2dWorldPositionOf](msg => {
    Provide2dWorldPositionOf((get2dWordTransformation(elements.get(msg.e)) * ConstVec3(Vec3.UnitZ)).xy)
  })

  // ---------------------------------------
  // Component Methods
  // ---------------------------------------
  protected def requestInitialConfigValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity) = {
    configure(aspect.getCreateParams)
    SValSet()
  }

  protected def finalizeConfiguration(e: Entity) = {}

  /**
   * Starting up the component.
   * Executed after Actor.start and before the first message handling.
   */
  override def startUp() {
    handleDevice(gt.TangibleSurface) { surfaceEntity =>
      surfaceEntity.getSVars(gt.TuioCursors).collect {
        case cursorsSVar => cursorsSVar._2.observe(cursors => dragButtons(cursors))
      }
      surfaceEntity.getSVars(gt.TuioObjects).collect {
        case objectsSVar => objectsSVar._2.observe(objects => {
          latestTuioObjects =
            objects.mapValues((obj) => (Vec2(GuiComponent.convertSurfaceToWorld(Vec2(obj._1), screenSize)), obj._2))
          checkEnclosureFor(elements.values)
          //Important: Make changes available system-wide
          updateAllSVars()
        })
      }
    }
    handleDevice(gt.Mouse) { mouseEntity =>
      mouseEntity.getSVars(gt.Position2D).headOption.collect { case posSVar => posSVar._2.observe(newPos => {
        val adaptedPos = newPos / Vec2(screenResolution)
        latestMousePosition = adaptedPos
        dragButtons(HashMap((mouseCursorId, adaptedPos)))
      })
      }
      mouseEntity.getSVars(gt.Button_Left).headOption.collect { case leftSVar => leftSVar._2.observe(pressed => {
        if (pressed) cursorAdded(Vec2(latestMousePosition), mouseCursorId)
        else cursorRemoved(Vec2(latestMousePosition), mouseCursorId)
      })
      }
    }
  }

  /**
   * (re)configure the component
   * @param params the configuration params
   */
  protected def configure(params: SValSet) {
    if (configured) {
      warn("Tuio component '" + componentName.name + "' was configured twice. Reconfiguration is not supported by " +
        "tuio components. The second configuration attempt has been skipped.")
    } else {
      //Display desc from config msg
      val displaySetupDesc =
        params.getFirstValueForOrElse(gt.DisplaySetupDescription)(
          throw new Exception("[GUI Component] Configuration did not contain a " + gt.DisplaySetupDescription.toString))
      //Transformation of the first display in the first display group
      val screen =
        try {
          displaySetupDesc.deviceGroups.head._2.dpys.head.displayDescs.head
        } catch {
          case _: Throwable => throw new Exception("[GUI Component] At least one display is required.")
        }
      setScreenSize(ConstVec2(screen.size._1.toFloat, screen.size._2.toFloat))
      _screenResolution =
        if (screen.resolution.isDefined)
          screen.resolution.map(res => ConstVec2i(res._1, res._2))
        else //TODO: check this for fullscreen
          GraphicsEnvironment.getLocalGraphicsEnvironment.getScreenDevices.find { screenDevice =>
            displaySetupDesc.deviceGroups.values.exists { deviceGroup =>
              deviceGroup.dpys.exists {
                dpy =>
                  val hwh = dpy.hardwareHandle.getOrElse((0, 0, 0))
                  screenDevice.getIDstring equals (":" + hwh._1 + "." + hwh._2)
              }
            }
          }.collect {
            case x => ConstVec2i(x.getDisplayMode.getWidth, x.getDisplayMode.getHeight)
          }
      val screenTransformation = screen.transformation
      val screenOffset = ConstVec3(screenTransformation.m30, screenTransformation.m31, screenTransformation.m32)
      if ((screenOffset.x != 0.0f) || (screenOffset.y != 0.0f))
        throw new Exception("[GUI Component] Screen offsets in x or y direction are not supported.")
      //Set ViewPlatform according to display desc
      handleDevice(gt.User) {
        user => user.getSVars(gt.ViewPlatform).collect {
          case pos => addJobIn(2000) {
            pos._2.set(ConstMat4(Mat4x3.translate(screenOffset * -1f)))
          } //TODO fix addJobIn
        }
      }



      //new EntityDescription(PPE( ppe = shaderEffect ), NameIt(name = "ExplosionShader")).
      //  realize(e => explosionShader = Some(e))

      configured = true
    }
  }

  protected def performSimulationStep() {
    val now = System.currentTimeMillis
    var deltaT = if (startOfLastFrame == 0L) simulationStepPeriod else now - startOfLastFrame
    if (deltaT > (4 * simulationStepPeriod)) deltaT = 4 * simulationStepPeriod
    startOfLastFrame = now
    val deltaTinSeconds = deltaT.toFloat / 1000f
    simulateDroppedButtons(deltaTinSeconds)
    simulateExplosion(deltaT)
    simulateNotifications(deltaTinSeconds)
    simulateElasticButtons(deltaTinSeconds)
    simulateFlyingNotifications(deltaTinSeconds)
    simulateLoot(deltaTinSeconds)

    if (effects.nonEmpty) {
      clearEffectShaderData()
      effects.foreach(effect => effect._2.simulate(this, deltaTinSeconds, effect._1))
      updateEffectShader()
    }

    simulationCompleted()
  }

  var effectShader: Option[Entity] = None
  var effectShaderIsBeingCreated = false
  private var effectShaderData: Option[EffectShaderData] = None

  def clearEffectShaderData() {
    effectShaderData.foreach(data => data.circularProgressBars = Nil)
  }

  def toShaderPos(guiPos: ConstVec2): Vec2 =
    Vec2(guiPos + sHalf) / Vec2(screenSize)

  def toShaderColor(c: Color): ConstVec4 =
    ConstVec4(c.getRed / 255f, c.getGreen / 255f, c.getBlue / 255f, c.getAlpha / 255f)

  def updateEffectShader() {
    getEffectShader.foreach(eShader => {
      var positions = List[Vec2]()
      var valuesAndSize = List[Vec2]()
      var colors = List[ConstVec4]()
      effectShaderData.get.circularProgressBars.foreach(cpbData => {
        positions ::= toShaderPos(cpbData.pos)
        valuesAndSize ::= Vec2(cpbData.value, cpbData.size)
        colors ::= toShaderColor(cpbData.color)
      })
      eShader.set(gt.Vector2List.setAnnotations(Symbols.circularProgressBar, Symbols.position2D)(positions))
      eShader.set(gt.Vector2List.setAnnotations(Symbols.circularProgressBar, Symbols.integer, Symbols.size)(valuesAndSize))
      eShader.set(gt.Vector4List.setAnnotations(Symbols.circularProgressBar, Symbols.color)(colors))
    })
  }

  def getEffectShader: Option[Entity] = {
    if (effectShader.isEmpty && !effectShaderIsBeingCreated) {
      val shaderEffect =
        PostProcessingEffect("Effect").
          describedByShaders("pipeline_shader/quad.vs" :: "assets/shader/effect.fs" :: Nil).
          usingColorBufferAsName("sceneMap").
          where("circularProgressBarPositions").
          hasValue(List[Vec2]()).
          isReachableBy(gt.Vector2List.setAnnotations(Symbols.circularProgressBar, Symbols.position2D)).
          and("circularProgressBarValuesAndSizes").
          hasValue(List[Vec2]()).
          isReachableBy(gt.Vector2List.setAnnotations(Symbols.circularProgressBar, Symbols.integer, Symbols.size)).
          and("aspectRatio").
          hasValue(screenSize.x / screenSize.y).
          isReachableBy(gt.Real).
          and("circularProgressBarColors").
          hasValue(List[ConstVec4]()).
          isReachableBy(gt.Vector4List.setAnnotations(Symbols.circularProgressBar, Symbols.color)).
          pack

      effectShaderIsBeingCreated = true
      new EntityDescription(PPE(ppe = shaderEffect), NameIt(name = "EffectShader")).realize { ent =>
        effectShader = Some(ent)
        effectShaderData = Some(EffectShaderData())
        effectShaderIsBeingCreated = false
      }
      None
    } else if (effectShaderIsBeingCreated) None
    else effectShader
  }

  def simulate(effectData: CircularProgressBarData, deltaTinSeconds: Float, e: Entity) {
    effectData.parent.foreach(parent => {
      effectShaderData.foreach(data => data.circularProgressBars ::=
        CircularProgressBarShaderData(
          parent._2.position,
          effectData.value,
          parent._2.shape.asInstanceOf[QuadShape].size.x,
          effectData.color
        )
      )
    })
  }

  def requestInitialValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity, given: SValSet) {
    val sem = aspect.getCreateParams.semantics
    if (sem == Symbols.guiElement) {
      val t = voidPos
      val initialValues = aspect.getCreateParams.combineWithValues(toProvide)._1
      initialValues.addIfNew(gt.Transformation(t))
      initialValues.addIfNew(gt.File(""))


      //TODO Check impl
      val text = aspect.getCreateParams.firstValueFor(gt.String)
      val font = aspect.getCreateParams.firstValueFor(gt.Font)
      val fontColor = aspect.getCreateParams.firstValueFor(gt.Color.addAnnotations(Symbols.font))
      val file = initialValues.getFirstValueFor(gt.File)
      var fPresent = false
      if (file.isDefined) {
        if (!file.get.equals(""))
          fPresent = true
      }
      val img = aspect.getCreateParams.getFirstValueFor(gt.Image)
      val texture = if (img.isDefined) FromBufferedImage(img.get) else if (fPresent) FromFile(file.get) else NoTexture()

      if (text != "")
        ask(GuiResourceManager.self, AskRenderedText(text, font, fontColor)) { texData: TextureData =>
          initialValues.addIfNew(gt.Texture(texData))
          provideInitialValues(e, initialValues)
        }
      else {
        texture match {
          case n: NoTexture =>
            initialValues.addIfNew(gt.Texture(transparentTex))
            provideInitialValues(e, initialValues)
          case img: FromBufferedImage =>
            initialValues.addIfNew(gt.Texture(TextureData(img.img)))
            provideInitialValues(e, initialValues)
          case str: FromFile =>
            if (str.file.equals("")) {
              initialValues.addIfNew(gt.Texture(transparentTex))
              provideInitialValues(e, initialValues)
            }
            else {
              ask(GuiResourceManager.self, AskImgFile(new File(str.file))) { tex: TextureData =>
                initialValues.addIfNew(gt.Texture(tex))
                provideInitialValues(e, initialValues)
              }
            }

        }
      }

    } else if (sem == local.Symbols.overlayTexture) {
      handleOverlayAspect(e, aspect.getCreateParams, toProvide)
    } else if (sem == local.Symbols.overlayText) {
      handleOverlayText(e, aspect.getCreateParams, toProvide)
    } else provideInitialValues(e, aspect.getCreateParams.combineWithValues(toProvide)._1)
  }

  /**
   * used to integrate the entity into the local representation
   * @param e the entity to be integrated
   */
  protected def entityConfigComplete(e: Entity, aspect: EntityAspect) {
    val sem = aspect.getCreateParams.semantics
    if (sem == Symbols.guiElement) createGuiElement(e, aspect)
    else if (sem == local.Symbols.overlayText) handleOverlayTextEntity(e, aspect)
    else if (sem == local.Symbols.overlayTexture) activateOverlayAspect(e)
    else if (sem == Symbols.circularProgressBar) createCircularProgressBar(e, aspect)
    else println("[GUI-Component] Unknown Semantics " + sem)
  }

  def createCircularProgressBar(e: Entity, aspect: EntityAspect) {

    def create(collectedValues: SValSet) {
      val data = CircularProgressBarData(
        collectedValues.firstValueFor(gt.Integer),
        collectedValues.firstValueFor(gt.Color)
      )
      effects = effects.updated(e, data)

      updateSVarUpdateFunctions(
        e.getSVars(gt.Integer).head._2,
        (newValue: Int) => {
          data.value = newValue
        },
        data.value _)
      updateSVarUpdateFunctions(
        e.getSVars(gt.Color).head._2,
        (newValue: Color) => {
          data.color = newValue
        },
        data.color _)
    }

    val requiredSVars = gt.Integer :: gt.Color :: Nil
    collectSVars(e, requiredSVars: _*)(create, keepRegistered = true)
  }

  /**
   * Removes an entity from this component.
   */
  def removeFromLocalRep(e: Entity) {
//    println("Removing " + e.getSimpleName)
    deactivatedOverlays = deactivatedOverlays - e
    overlays = overlays - e
    effects -= e
    effects = effects.filterNot(_._2.parent.exists(_._1 == e))
    updateOverlayTexture()

    unregisterEntity(e)
    elements.get(e).collect {
      case guiData => guiData.children.foreach(child => child._1.remove())
    }
    elements = elements - e
    buttons = buttons.filterNot(_ == e)
    activeLoots = activeLoots.filterNot(_.e == e)
    draggableButtons = draggableButtons.filterNot(_ == e)
    draggedButtons -= e
  }

  // ---------------------------------------
  // Component Methods End
  // ---------------------------------------

  private def handleExplosionRequest(e: Event) {
    explosion = Some(ExplosionContext(
      pos = Vec2(GuiComponent.convertWorldToSurface(Vec2(e.get(gt.Position2D).get), screenSize)),
      radius = e.get(lt.ShaderRadiusInMeter).headOption.getOrElse(2f),
      durationInSec = e.get(lt.ShaderDurationInSec).headOption.getOrElse(2f)
    ))
    explosion.collect {
      case _explosion =>
        explosionShader.collect {
          case _explosionShader =>
            _explosionShader.set(lt.PosX(_explosion.pos.x))
            _explosionShader.set(lt.PosY(_explosion.pos.y))
            val color = e.get(lt.ShaderColor).headOption.getOrElse(Vec4.Zero)
            _explosionShader.set(lt.ShaderColor(color))
        }
    }
  }

  private def simulateHit(startTime: Long = System.currentTimeMillis(), now: Long = System.currentTimeMillis()) {
    hitIsActive = true
    val f = hitDuration / hitBlinks
    overlayShaderEntity.collect {
      case e => e.getSVars(gt.Health).collect {
        case hSVar => hSVar._2.set(
          abs(f - ((now - startTime) % (f * 2))) / f.toFloat
        )
      }
    }
    if (now < startTime + hitDuration + f)
      addJobIn(16) {
        simulateHit(startTime, now + 16)
      }
    else
      hitIsActive = false
  }

  //  private def createFlyingNotification(e: Event) {
  //    val context = FlyingNotificationContext()
  //
  //    val textID = UUID.randomUUID()
  //    val text = e.get(gt.String).getOrElse("")
  //    val textColor = e.get(gt.Color).getOrElse(Color.BLACK)
  //    val textFont = e.get(gt.Font).getOrElse(new Font(null, Font.PLAIN, 48))
  //    val textLayer = e.get(gt.Integer).getOrElse(3)
  //
  //    val panelId = UUID.randomUUID()
  //    val startPos = e.get(gt.Position2D.withAnnotations(simx.core.ontology.Symbols.begin)).getOrElse(Vec2(0f, 0f))
  //    val endPos = e.get(gt.Position2D.withAnnotations(simx.core.ontology.Symbols.end)).getOrElse(Vec2(0f, 0f))
  //    val size = e.get(gt.Vector2).getOrElse(Vec2(0.2f, 0.1f))
  //    val duration = e.get(gt.TimeInSeconds).getOrElse(3f)
  //    val image: String = e.get(gt.File).getOrElse("")
  //    val panelLayer = textLayer - 1
  //
  //    val angle = e.get(gt.Angle).getOrElse(0f)
  //
  //    context.startPos = Vec2(startPos.x, startPos.y)
  //    context.endPos = Vec2(endPos.x, endPos.y)
  //    context.duration = duration
  //
  //    val textPanel = TextPanel(
  //      name = textID.toString,
  //      text = text,
  //      pos = Vec2(Vec2.Zero),
  //      layer = textLayer,
  //      font = textFont,
  //      fontColor = textColor
  //    )
  //
  //    new Panel(
  //      name = panelId.toString,
  //      pos = startPos,
  //      text = "",
  //      shape = QuadShape(Vec2(size.x, size.y)),
  //      layer = panelLayer,
  //      textureFile = Some(new File(image)),
  //      subElements = textPanel :: Nil,
  //      angle = angle
  //    ).realize(e => context.entity = Some(e))
  //
  //    onNextCreation(Symbol(panelId.toString) :: Nil)(e => context.entity = Some(e))
  //    flyingNotifications = context :: flyingNotifications
  //  }

  private def createNotification(e: Event) {
    val context = NotificationContext()

    val pos = e.get(gt.Position2D).get
    val text = e.get(gt.String).get
    val color = e.get(gt.Color).getOrElse(Color.BLACK)
    val font = e.get(gt.Font).getOrElse(new Font(null, Font.PLAIN, 72))
    val id = UUID.randomUUID()
    context.linearVelocity = e.get(lt.NotificationLinearVelocity).getOrElse(true)
    context.linearVelocity match {
      case false =>
        notificationDurationInSec = 3f
      case true =>
    }
    context.linearOffset = e.get(lt.NotificationLinearOffset).getOrElse(0.0f)

    def sub(nr: Int) =
      TextPanel(notificationTextName(id, nr), text, Vec2(Vec2.Zero), nr * 90f, notificationLayer, font, color)

    //TODO ask dennis why sVars of type GuiElement are only added to the top panel (observable in createGuiElement for the top panel), if another actor realizes the entity
    SVarActor.createActor(new SVarActor with EntityCreationHandling {

      var count = 5

      override protected def removeFromLocalRep(e: Entity): Unit = {
        observeId.foreach(e.ignore)
        count -= 1
        if(count == 0) {
          //println("Shutting down")
          context.stop(self)
        }
      }
      var observeId: Option[UUID] = None
      var first = true
      override def startUp(): Unit = {
        new TextPanel(
          name = notificationPanelName(id),
          pos = Vec2(pos),
          angle = 0f,
          layer = 1000,
          text = "",
          font = new Font(null, Font.PLAIN, 72),
          fontColor = Color.BLACK,
          subElements = sub(0) :: sub(1) :: sub(2) :: sub(3) :: Nil
        ).realize(e => {
//          observeId = e.observe(gt.Transformation).head(m => {if(first){println("O "+m(3)); first = false}})
//          e.get(gt.Transformation).head(m => {println("G " + m(3))})
        })
      }
    })


    notifications = context :: notifications

    handleOrWaitForEntityRegistration(Symbol(notificationPanelName(id)) :: Nil)(e => context.basePanel = Some(e))
    (0 until 4).foreach(nr =>
      handleOrWaitForEntityRegistration(Symbol(notificationTextName(id, nr)) :: Nil)(e => context.textPanels.+=((nr, e))))
  }

  private def activateButton(button: Entity, elem: GuiElementData) {
    val now = System.currentTimeMillis()
    if (now - elem.lastActivationTimestamp >= minActivationTimeDifferenceInMillis) {
      elem.lastActivationTimestamp = now
      Events.buttonPressed.emit(Set(button), gt.Name(elem.name), gt.Identifier.addAnnotations(Symbols.componentName)(componentName))
    }
  }

  private def cursorRemoved(pos: ConstVec2, id: Symbol) {
    //Activate buttons on release only
    checkInclusion(
      Iterable(Vec2(GuiComponent.convertSurfaceToWorld(pos, screenSize))), activateButton)
    checkInclusion(
      Iterable(Vec2(GuiComponent.convertSurfaceToWorld(pos, screenSize))),
      (button, elem) => release(elem)
    )
    checkDrop(id)
  }

  private def press(elem: GuiElementData) {
    if (!elem.pressed) {
      elem.pressed = true
      activeElasticButtons.find(_.elem.id == elem.id).collect { case ctx =>
        activeElasticButtons = activeElasticButtons.filterNot(_.elem.id == elem.id)
        elem.shape = ctx.initialShape
      }
      scale(elem, scaleFactorOnPress)
    }
  }

  private def release(elem: GuiElementData) {
    if (elem.pressed) {
      elem.pressed = false
      scale(elem, 1f / scaleFactorOnPress)
      if (elem.isElastic && !activeElasticButtons.exists(_.elem.id == elem.id))
        activeElasticButtons = ElasticButtonSimulationCtx(elem, elem.shape) :: activeElasticButtons
    }
  }

  private def scale(elem: GuiElementData, scaleFactor: Float) {
    elem.shape match {
      case QuadShape(size) =>
        elem.shape = QuadShape(size * scaleFactor)
      case _ =>
    }
    updateTransformation(elem)
  }

  private def cursorAdded(pos: Vec2, id: Symbol) {
    checkInclusion(
      Iterable(Vec2(GuiComponent.convertSurfaceToWorld(pos, screenSize))),
      (button, elem) => press(elem)
    )
    checkDragActivation(Vec2(GuiComponent.convertSurfaceToWorld(pos, screenSize)), id)
  }

  private def simulateLoot(deltaTinSeconds: Float) {
    activeLoots.foreach(lootCtx => {
      lootCtx.acc = lootCtx.acc - lootCtx.acc * deltaTinSeconds * 6
      lootCtx.vel = lootCtx.vel + lootCtx.acc * deltaTinSeconds
      lootCtx.vel = lootCtx.vel - lootCtx.vel * deltaTinSeconds * 6
      lootCtx.elem.position = clamp(lootCtx.elem.position + lootCtx.vel * deltaTinSeconds, -sHalf, sHalf)
      updateTransformation(lootCtx.elem)
    })
  }

  private def simulateNotifications(deltaTinSeconds: Float) {
    notifications.foreach(notification => {
      notification.timePassedInSec += deltaTinSeconds
      notification.basePanel.collect { case base =>
        if (elements.get(base).isDefined) {
          elements(base).angle = notification.timePassedInSec * notificationAngularVelocity
          var distance = 0.0f
          if (notification.linearVelocity) {
            distance = notificationLinearVelocity * notification.timePassedInSec + notification.linearOffset
          } else {
            distance = notification.linearOffset
            elements(base).angle *= notificationAngularAcceleration
          }
          notification.textPanels.foreach(numberedTxtPanel => {
            numberedTxtPanel._1 match {
              case 0 => elements.get(numberedTxtPanel._2).foreach(_.position = Vec2.UnitX * distance)
              case 1 => elements.get(numberedTxtPanel._2).foreach(_.position = Vec2.UnitY * distance)
              case 2 => elements.get(numberedTxtPanel._2).foreach(_.position = Vec2.UnitX * -distance)
              case 3 => elements.get(numberedTxtPanel._2).foreach(_.position = Vec2.UnitY * -distance)
            }
          })
          updateTransformation(elements(base))
        }
      }
    })

    //Remove notification if n.timePassedInSec > notificationDurationInSec
    //only if it has been realized already
    notifications = notifications.filterNot(n => {
      n.basePanel.exists(base =>
        if (n.timePassedInSec > notificationDurationInSec) {
          base.remove()
          true
        } else false)
    })
  }

  private def simulateExplosion(deltaTinMillis: Long) {
    explosion.collect { case _explosion =>
      explosionShader.collect { case _explosionShader =>
        val timeInSec = (System.currentTimeMillis - _explosion.startTime).toFloat / 1000f
        val explosionSpeed = _explosion.radius / _explosion.durationInSec
        val explosionExtension = explosionSpeed * timeInSec
        if (explosionExtension > _explosion.radius) {
          _explosionShader.set(gt.TimeInSeconds(-1f))
          explosion = None
        } else _explosionShader.set(gt.TimeInSeconds(explosionExtension))
      }
    }
  }

  private def removeScale(t: ConstMat4): ConstMat4 = {
    val scale = Vec3(1f / length(t(0).xyz), 1f / length(t(1).xyz), 1f / length(t(2).xyz))
    t * ConstMat4(Mat4x3.scale(scale))
  }

  private def extractInverseScaleAndLayer(t: ConstMat4): ConstMat4 = {
    val inverseScale = Vec3(1f / length(t(0).xyz), 1f / length(t(1).xyz), 1f / length(t(2).xyz))
    val inverseLayer = Vec3(0, 0, -t.m32)
    ConstMat4(Mat4x3.scale(inverseScale).translate(inverseLayer))
  }

  def removeScaleAndLayer(t: ConstMat4): ConstMat4 = {
    val noScale = Mat4x4(removeScale(t))
    noScale.m32 = 0f
    ConstMat4(noScale)
  }

  private def getWorldTransform(
                                 elem: GuiElementData,
                                 transformSoFar: ConstMat4 = ConstMat4(Mat4x3.Identity)
                                 ): ConstMat4 = elem.parent match {
    case Some((_, parent)) => getWorldTransform(parent, elem.transformation * transformSoFar)
    case None => elem.transformation * transformSoFar
  }

  private def calcTWorldToElem(elem: GuiElementData) = {
    val tElem = getWorldTransform(elem)
    inverse(removeScale(tElem))
  }

  private def convertWorldToElem(v: Vec2, tWorldToElem: ConstMat4) = (tWorldToElem * Vec4(v, 0f, 1f)).xy

  /**
   * Coordinates have to be converted to the world coordinate system already
   * @param cursors Cursors that are used for the activation check
   * @param processElement Method that is applied for elements that include one cursor
   */
  private def checkInclusion(cursors: Iterable[Vec2], processElement: (Entity, GuiElementData) => Unit) {
    buttons.foreach(button => {
      val elem = elements(button)
      val tWorldToElem = calcTWorldToElem(elem)
      cursors.foreach(cursor => {
        if (elem.shape.isInside(Vec2(convertWorldToElem(cursor, tWorldToElem))))
          processElement(button, elem)
      })
    })
  }

  /**
   * Coordinates have to be converted to the world coordinate system already
   */
  private def checkDragActivation(cursorPos: Vec2, cursorId: Symbol) {
    draggableButtons.foreach(button => {
      val elem = elements(button)
      val tWorldToElem = calcTWorldToElem(elem)
      if (!draggedButtons.values.exists(_.dragTouchId == cursorId) && elem.shape.isInside(Vec2(convertWorldToElem(cursorPos, tWorldToElem)))) {
        draggableButtons = draggableButtons.filterNot(_ == button)
        draggedButtons = draggedButtons.updated(button, SimulationContext(Vec2(elem.position), cursorId))
      }
    })
  }

  /**
   * Coordinates have to be converted to the world coordinate system already
   */
  private def checkDrop(cursorId: Symbol) {
    draggedButtons.filter(_._2.dragTouchId == cursorId).foreach(draggedButton => {
      draggedButtons = draggedButtons - draggedButton._1
      if (!elements(draggedButton._1).isLoot)
        droppedButtons = droppedButtons.updated(draggedButton._1, draggedButton._2)
      else
        draggableButtons = draggedButton._1 :: draggableButtons
    })
  }

  private def dragButtons(cursors: HashMap[Symbol, ConstVec2]) {
    draggedButtons.foreach(draggedButton => {
      cursors.get(draggedButton._2.dragTouchId).collect { case cursor =>
        val elem = elements(draggedButton._1)
        val parentOffset = get2dWordTransformation(elem.parent.map(_._2))
        elem.position = (inverse(parentOffset) * ConstVec3(GuiComponent.convertSurfaceToWorld(Vec2(cursor), screenSize), 1f)).xy
        updateTransformation(elem)
      }
    })
  }

  private def get2dWordTransformation(element: Option[GuiElementData]): ConstMat3 = {
    element match {
      case None => ConstMat3(Mat2x3.Identity)
      case Some(elem) =>
        ConstMat3(get2dWordTransformation(elem.parent.map(_._2)) *
          ConstMat3(Mat3x2.rotate(radians(elem.angle)).translate(elem.position)))
    }
  }

  private def simulateDroppedButtons(deltaTinSeconds: Float) {
    droppedButtons.foreach(button => {
      val simC = button._2
      val elem = elements(button._1)
      val toInit = simC.initialPos - elem.position
      simC.acc = toInit * springConstant
      simC.vel = simC.vel + (simC.acc * deltaTinSeconds)
      simC.vel = simC.vel * linearDamping
      elem.position = elem.position + (simC.vel * deltaTinSeconds)

      if (length(toInit) < epsilon) {
        elem.position = simC.initialPos
        droppedButtons = droppedButtons - button._1
        draggableButtons = button._1 :: draggableButtons
        Events.buttonPressed.emit(Set(button._1), gt.Name(elem.name), gt.Identifier.addAnnotations(Symbols.componentName)(componentName))
      }

      updateTransformation(elem)
    })
  }

  private val springConstantFlying = 45f
  private val velocityDampingFlying = 0.9f

  private def simulateFlyingNotifications(deltaTinSeconds: Float) {
    flyingNotifications.foreach(notification => {
      notification.entity.collect {
        case e =>
          notification.timePassedInSec += deltaTinSeconds
          val toInit = notification.endPos - notification.startPos
          notification.acc = toInit * springConstantFlying
          notification.vel = notification.vel + (notification.acc * deltaTinSeconds)
          notification.vel = notification.vel * velocityDampingFlying
          notification.startPos = notification.startPos + (notification.vel * deltaTinSeconds)
          if (length(toInit) < epsilon && notification.timePassedInSec >= notification.duration) {
            notification.startPos = notification.endPos
            flyingNotifications = flyingNotifications.filterNot(not => not.startPos == not.endPos)
            e.remove()
          }
          elements.get(e).collect { case elem =>
            elem.position = notification.startPos
            updateTransformation(elem)
          }
      }
    })

  }

  private def simulateElasticButtons(deltaTinSeconds: Float) {
    activeElasticButtons.foreach(ctx => {
      ctx.timePassed += deltaTinSeconds
      ctx.initialShape match {
        case QuadShape(size) =>
          val t = ctx.timePassed
          val f = (1f + math.sin(t * swingFrequencyFactor) * maxSwingScaleAmplitude *
            (1 / (1 + t * t * swingFalloffFactor))).toFloat
          ctx.elem.shape = QuadShape(size * f)
        case _ =>
      }
      if (ctx.timePassed >= elasticButtonSwingDurationInSec) ctx.elem.shape = ctx.initialShape
      updateTransformation(ctx.elem)
    })
    activeElasticButtons = activeElasticButtons.filterNot(_.timePassed >= elasticButtonSwingDurationInSec)
  }

  /**
   * Coordinates have to be converted to the world coordinate system already
   */
  private def checkEnclosureFor(_elements: Iterable[GuiElementData]) {
    _elements.foreach(elem => {
      val tWorldToElem = calcTWorldToElem(elem)
      elem.tangibleList = Nil
      latestTuioObjects.foreach(obj => {
        if (elem.shape.isInside(Vec2(convertWorldToElem(obj._2._1, tWorldToElem))))
          elem.tangibleList = obj._1 :: elem.tangibleList
      })
    })
  }

  private def transformationFor(shape: Shape, angle: Float, position: ConstVec2, layer: Float, tParent: ConstMat4) = {
    ConstMat4(extractInverseScaleAndLayer(tParent) * (shape match {
      case QuadShape(size) =>
        ConstMat4(
          Mat4x3.scale(ConstVec3(size.x, size.y, 1f))
            .rotateZ(radians(angle))
            .translate(Vec3(position, layerDistance * layer)))
      case _ => ConstMat4(Mat4x3.Identity)
    }))
  }

  //Todo ask dennis how to integrate atomicSet and renderer-sideded multiobserve for subelements
  private def updateTransformation(elem: GuiElementData) {
    updateTransformationStep(elem)
    //Important: Make changes available system-wide
    updateAllSVars()
  }

  private def updateTransformationStep(elem: GuiElementData) {
    elem.transformation =
      if (elem.enabled) transformationFor(
        elem.shape,
        elem.angle,
        elem.position,
        elem.layer,
        elem.parent.fold(ConstMat4(Mat4x3.Identity))(_._2.transformation))
      else voidPos

    checkEnclosureFor(elem :: Nil)
    elem.children.foreach(child => updateTransformationStep(child._2))
  }

  private def prepareNoTextTexture() = {
    val img = new BufferedImage(1, 1, BufferedImage.TYPE_4BYTE_ABGR)
    val g = img.createGraphics()
    g.setColor(new Color(0, 0, 0, 0))
    g.fillRect(0, 0, img.getWidth, img.getHeight)
    g.dispose()
    TextureData(ConstVec2i(1, 1), TextureData.imageToTextureData(img))
  }

  private def updateText(e: Entity, elem: GuiElementData, updateTransform: Boolean = true) {

    if (elem.text == "") {
      e.set(gt.Texture(noTextTexture))
      return
    }

    ask(GuiResourceManager.self, AskRenderedText(elem.text, elem.font, elem.fontColor)) { texData: TextureData =>
      e.set(gt.Texture(texData))

      val sizeInMeter = texData.size * screenMPP
      elem.shape = QuadShape(sizeInMeter)

      if (updateTransform) updateTransformation(elem)
    }
  }

  private def updateTexture(e: Entity, elem: GuiElementData, updateTransform: Boolean = true) {
    elem.texture match {
      case n: NoTexture =>
        e.set(gt.Texture(transparentTex))
      case img: FromBufferedImage =>
        e.set(gt.Texture(TextureData(img.img)))
        if (updateTransform)
          updateTransformation(elem)
      case str: FromFile =>
        if (str.file.equals(""))
          e.set(gt.Texture(transparentTex))
        else {
          ask(GuiResourceManager.self, AskImgFile(new File(str.file))) { tex: TextureData =>
            e.set(gt.Texture(tex))
            if (updateTransform)
              updateTransformation(elem)
          }
        }

    }
  }

  private def isGuiElementAspect(aspect: EntityAspect): Boolean =
    aspect.aspectType == Symbols.guiElement

  private def createGuiElement(e: Entity, aspect: EntityAspect) {
    try {
      def create(collectedValues: SValSet) {

        val file = collectedValues.getFirstValueFor(gt.File)
        var fPresent = false
        if (file.isDefined) {
          if (!file.get.equals(""))
            fPresent = true
        }
        val img = aspect.getCreateParams.getFirstValueFor(gt.Image)
        val text = if (img.isDefined) FromBufferedImage(img.get) else if (fPresent) FromFile(file.get) else NoTexture()

        //Create local rep
        val elem =
          GuiElementData(
            collectedValues.firstValueFor(lt.Shape),
            collectedValues.firstValueFor(gt.Name.addAnnotations(Symbols.identifier)),
            collectedValues.firstValueFor(gt.Position2D),
            collectedValues.firstValueFor(gt.Transformation),
            collectedValues.firstValueFor(gt.Angle),
            collectedValues.firstValueFor(lt.TangibleList),
            collectedValues.firstValueFor(lt.Layer),
            collectedValues.firstValueFor(gt.String),
            text,
            collectedValues.firstValueFor(gt.Font),
            collectedValues.firstValueFor(gt.Color.addAnnotations(Symbols.font)),
            aspect.getCreateParams.firstValueFor(lt.IsElastic),
            aspect.getCreateParams.firstValueFor(lt.IsLoot),
            aspect.getCreateParams.firstValueFor(gt.Enabled)
          )
        //Add children
        val children = collectedValues.getAllValuesFor(gt.GuiElement)
        children.foreach(eChild => {
          elements(eChild).parent = Some((e, elem))
          elem.children = (eChild, elements(eChild)) :: elem.children
        })

        //Add effects
        val effectEntities = collectedValues.getAllValuesFor(gt.GuiEffect)
        effectEntities.foreach(effectEntity => {
          effects(effectEntity).parent = Some((e, elem))
          elem.effects = (effectEntity, effects(effectEntity)) :: elem.effects
        })

        //Add to local rep storage
        elements = elements.updated(e, elem)

        if (aspect.getCreateParams.firstValueFor(gt.Boolean.addAnnotations(local.Symbols.isClickable)))
          buttons = e :: buttons

        if (aspect.getCreateParams.firstValueFor(lt.IsDraggable))
          draggableButtons = e :: draggableButtons

        if (aspect.getCreateParams.firstValueFor(lt.IsLoot) && !aspect.getCreateParams.firstValueFor(lt.IsDraggable))
          activeLoots ::= LootSimulationCtx(e, elem)

        //Connect SVars
        try {
          updateSVarUpdateFunctions(
            e.getSVars(lt.TangibleList).head._2, //Define SVar
            (newValue: List[Symbol]) => { elem.tangibleList = newValue }, //Define action after new svar value is observed
            elem.tangibleList _) //Define how to update the svar from the local rep
          updateSVarUpdateFunctions(
            e.getSVars(gt.Transformation).head._2,
            (newValue: ConstMat4) => { elem.transformation = newValue },
            elem.transformation _)
          updateSVarUpdateFunctions(
            e.getSVars(gt.Position2D).head._2,
            (newValue: ConstVec2) => { elem.position = newValue; updateTransformation(elem) },
            elem.position _)
          updateSVarUpdateFunctions(
            e.getSVars(gt.Angle).head._2,
            (newValue: Float) => { elem.angle = newValue; updateTransformation(elem) },
            elem.angle _)
          updateSVarUpdateFunctions(
            e.getSVars(gt.String).head._2,
            (newValue: String) => { elem.text = newValue; updateText(e, elem) },
            elem.text _)
          if (fPresent) {
            updateSVarUpdateFunctions[String](
              e.getSVars(gt.File).head._2,
              (newValue: String) => { elem.texture = FromFile(newValue); updateTexture(e, elem) },
              () => elem.texture.asInstanceOf[FromFile].file)
          }
          updateSVarUpdateFunctions(
            e.getSVars(lt.Shape).head._2,
            (newValue: Shape) => { elem.shape = newValue; updateTransformation(elem) },
            elem.shape _)
          updateSVarUpdateFunctions(
            e.getSVars(lt.Layer).head._2,
            (newValue: Int) => { elem.layer = newValue; updateTransformation(elem) },
            elem.layer _)
          updateSVarUpdateFunctions(
            e.getSVars(gt.Font).head._2,
            (newValue: Font) => { elem.font = newValue; updateText(e, elem) },
            elem.font _)
          updateSVarUpdateFunctions(
            e.getSVars(gt.Color.addAnnotations(Symbols.font)).head._2,
            (newValue: Color) => { elem.fontColor = newValue; updateText(e, elem) },
            elem.fontColor _)
          updateSVarUpdateFunctions(
            e.getSVars(gt.Enabled).head._2,
            (newValue: Boolean) => { elem.enabled = newValue; updateTransformation(elem) },
            elem.enabled _)

          //TODO: Create text label aspect
          if (elem.text != "") updateText(e, elem, !aspect.hasParent)

          //TODO: Create image label aspect
          updateTexture(e, elem, !aspect.hasParent)

          //Update to get the children pos updated as well
          if (!aspect.hasParent || !aspect.parent.description.aspects.exists(isGuiElementAspect))
            updateTransformation(elem)

          //registerEntity(Symbol(elem.name), e)
        } catch {
          case e: NoSuchElementException =>
            throw new Exception("[GUI-Component] Entity did not contain the assumed SVars.")
          case exception: Throwable =>
            exception.printStackTrace()
            throw new Exception("[GUI-Component] Error during entity creation.")
        }
      }

      val requiredSVars = gt.Name.addAnnotations(Symbols.identifier) :: gt.Transformation :: lt.TangibleList ::
        gt.Angle :: gt.Position2D :: gt.String :: lt.Shape :: lt.Layer ::
        gt.File :: gt.Color.addAnnotations(Symbols.font) :: gt.Font :: gt.Enabled ::
        gt.GuiElement :: gt.GuiEffect :: Nil

      collectSVars(e, requiredSVars: _*)(create, keepRegistered = true)
    }
    catch {
      case e: NoSuchElementException =>
        throw new Exception("[GUI-Component] Entity did not contain the assumed SVars. " + e.toString)
      case exception: Throwable =>
        exception.printStackTrace()
        throw new Exception("[GUI-Component] Error during entity creation.\n")

    }
  }

  // ---------------------------------------
  // Overlay Section
  // ---------------------------------------
  private var overlays = Map[Entity, (ConstVec2i, TextureData)]()
  private var deactivatedOverlays = Map[Entity, (ConstVec2i, TextureData)]()

  private def updateOverlayTexture() {
    overlayShaderEntity.collect { case ose =>
      //create image
      val img = new BufferedImage(screenResolution.x, screenResolution.y, BufferedImage.TYPE_4BYTE_ABGR)

      //fill image
      overlays.values.foreach { o =>
        val xPos = max(0, o._1.x)
        val yPos = max(0, screenResolution.y - o._1.y - o._2.size.y)
        if (xPos < img.getWidth && yPos < img.getHeight) {
          val offsetX = max(0, -o._1.x)
          val offsetY = max(0, -o._1.y)
          val width   = min(o._2.size.x - offsetX, img.getWidth  - xPos - 1)
          val height  = min(o._2.size.y - offsetY, img.getHeight - yPos - 1)
          val src = o._2.toImage.get
          if (offsetX < src.getWidth && offsetY < src.getHeight && width > 0 && height > 0) {
            val data = src.getSubimage(offsetX, offsetY, width, height)
            img.getRaster.setRect(xPos, yPos, data.getData)
          }
        }
      }
      ose.set(gt.Texture(TextureData(ConstVec2i(img.getWidth, img.getHeight), TextureData.imageToTextureData(img))))
    }
  }

  //  private val texConverter : (TextureData => Texture2D, Texture2D => TextureData) =
  //    {td : TextureData => new Texture2D(td.size.x, td.size.y, td.data) } ->
  //      {tx : Texture2D => TextureData(ConstVec2i(tx.getWidth, tx.getHeight), tx.getImageData) }

  private val overlayShaderDescription = new EntityDescription(
    PPE(
      PostProcessingEffect("textureOverlayGUI").
        usingColorBufferAsName("jvr_Texture0")
        describedByShaders List("pipeline_shader/quad.vs", "assets/shader/interface.fs") where
        "alpha" hasValue 0.75f isReachableBy gt.Factor and
        "eyeSep" hasValue 0.0f isReachableBy gt.Threshold and
        "redFactor" hasValue 0.0f isReachableBy gt.Health and
        "overlay" hasValue TextureData.apply(Color.WHITE) isReachableBy gt.Texture
        pack
    ),
    NameIt("Overlay Shader")
  )

  private var overlayShaderEntity: Option[Entity] = None
  private var waitingHandlers = List[() => Any]()
  private var osIsInitializing = false

  private def initializeOverlay(handler: () => Any) {
    overlayShaderEntity match {
      case Some(entity) =>
        handler()
      case None if osIsInitializing =>
        waitingHandlers = handler :: waitingHandlers
      case None =>
        osIsInitializing = true
        waitingHandlers = handler :: waitingHandlers
        overlayShaderDescription.realize {
          shaderEntity =>
            overlayShaderEntity = Some(shaderEntity)
            osIsInitializing = false
            addJobIn(100) {
              waitingHandlers.foreach(_.apply())
              waitingHandlers = Nil
            }
        }
    }
  }

  protected def handleOverlayText(e: Entity, params: NamedSValSet, toProvide: Set[ConvertibleTrait[_]]) {
    val color = params.firstValueFor(gt.Color)
    val text = params.firstValueFor(gt.String)
    val font = params.firstValueFor(gt.Font)
    ask(GuiResourceManager.self, AskRenderedText(text, font, color)) {
      texData: TextureData =>
        val newParams = NamedSValSet(params.semantics, (params.toSValSeq.toSet + gt.Texture(texData)).toSeq: _*)
        handleOverlayAspect(e, newParams, toProvide)
    }
  }

  protected def handleOverlayAspect(e: Entity, params: NamedSValSet, toProvide: Set[ConvertibleTrait[_]]) {
    initializeOverlay { () =>
      val pos = params.firstValueFor(gt.Position2D)
      val tex = params.firstValueFor(gt.Texture)
      overlays = overlays.updated(e, ConstVec2i(pos.x.toInt, pos.y.toInt) -> tex)
      provideInitialValues(e, params.combineWithValues(toProvide)._1)
    }
  }

  protected var textMap = Map[Entity, (Font, Color)]()

  def handleOverlayTextEntity(e: Entity, aspect: EntityAspect) {
    val font = aspect.getCreateParams.firstValueFor(gt.Font)
    val color = aspect.getCreateParams.firstValueFor(gt.Color)
    textMap = textMap.updated(e, font -> color)

    e.getSVars(gt.String).headOption.collect {
      case txtSVar => txtSVar._2.observe {
        newText =>
          if (newText.isEmpty) {
            deactivateOverlayAspect(e)
          } else {
            ask(GuiResourceManager.self, AskRenderedText(newText, textMap(e)._1, textMap(e)._2)) {
              newTexture: TextureData =>
                e.set(gt.Texture(newTexture))
                updateTuple(e, None, Some(newTexture))
                activateOverlayAspect(e)
            }
          }
      }
    }

    e.getSVars(gt.Position2D).headOption.collect { case svar =>
      svar._2.observe(pos => updateTuple(e, Some(ConstVec2i(pos.x.toInt, pos.y.toInt)), None))
      svar._2.get(pos => updateTuple(e, Some(ConstVec2i(pos.x.toInt, pos.y.toInt)), None))
    }
  }

  private def updateTuple(e: Entity, pos: Option[ConstVec2i], tex: Option[TextureData]) {
    if (!deactivatedOverlays.contains(e))
      overlays = overlays.updated(e, pos.getOrElse(overlays(e)._1) -> tex.getOrElse(overlays(e)._2))
    else
      deactivatedOverlays = deactivatedOverlays.updated(e,
        pos.getOrElse(deactivatedOverlays(e)._1) -> tex.getOrElse(deactivatedOverlays(e)._2))
    updateOverlayTexture()
  }

  protected def activateOverlayAspect(e: Entity) {
    if (deactivatedOverlays.contains(e)) {
      overlays = overlays + (e -> deactivatedOverlays(e))
      deactivatedOverlays = deactivatedOverlays - e
    } else if (!overlays.contains(e)) {
      val (pos, tex) = overlays(e)
      updateTuple(e, Some(ConstVec2i(pos.x, pos.y)), Some(tex))

      e.getSVars(gt.Position2D).headOption.collect { case svar =>
        svar._2.observe(pos => updateTuple(e, Some(ConstVec2i(pos.x.toInt, pos.y.toInt)), None))
        svar._2.get(pos => updateTuple(e, Some(ConstVec2i(pos.x.toInt, pos.y.toInt)), None))
      }

      e.getSVars(gt.Texture).headOption.collect { case svar =>
        svar._2.observe(tex => updateTuple(e, None, Some(tex)))
        svar._2.get(tex => updateTuple(e, None, Some(tex)))
      }
    }
    updateOverlayTexture()
  }

  protected def deactivateOverlayAspect(e: Entity) {
    if (overlays.contains(e)) {
      deactivatedOverlays = deactivatedOverlays + (e -> overlays(e))
      overlays = overlays - e
    }
    e.set(gt.Texture(noTextTexture))
    updateOverlayTexture()
  }
}

private trait GuiEffectData {
  var parent: Option[(Entity, GuiElementData)] = None

  def simulate(guiComp: GuiComponent, deltaTinSeconds: Float, e: Entity)
}

private case class CircularProgressBarData(
                                            var value: Int,
                                            var color: Color
                                            ) extends GuiEffectData {
  def simulate(guiComp: GuiComponent, deltaTinSeconds: Float, e: Entity) {
    guiComp.simulate(this, deltaTinSeconds, e)
  }
}

private case class SimulationContext(
                                      var initialPos: Vec2,
                                      var dragTouchId: Symbol,
                                      var acc: ConstVec2 = Vec2.Zero,
                                      var vel: ConstVec2 = Vec2.Zero
                                      )

private case class ExplosionContext(
                                     pos: Vec2,
                                     startTime: Long = System.currentTimeMillis,
                                     radius: Float = 2f,
                                     durationInSec: Float = 2f
                                     )

private case class NotificationContext(
                                        var basePanel: Option[Entity] = None,
                                        textPanels: scala.collection.mutable.HashSet[(Int, Entity)] = scala.collection.mutable.HashSet[(Int, Entity)](),
                                        var timePassedInSec: Float = 0f,
                                        var linearVelocity: Boolean = false,
                                        var linearOffset: Float = 0.3f
                                        )

private case class FlyingNotificationContext(
                                              var entity: Option[Entity] = None,
                                              var timePassedInSec: Float = 0f,
                                              var startPos: Vec2 = Vec2(0f, 0f),
                                              var endPos: Vec2 = Vec2(0f, 0f),
                                              var duration: Float = 0f,
                                              var acc: Vec2 = Vec2(0f, 0f),
                                              var vel: Vec2 = Vec2(0f, 0f),
                                              var springConstantFlying: Float = 0f,
                                              var velocityDampingFlying: Float = 0f
                                              )

private case class ElasticButtonSimulationCtx(elem: GuiElementData, initialShape: Shape, var timePassed: Float = 0f)

private case class LootSimulationCtx(
                                      e: Entity,
                                      elem: GuiElementData,
                                      var vel: ConstVec2 = Vec2.Zero,
                                      var acc: ConstVec2 = normalize(ConstVec2(Random.nextFloat() - 0.5f, Random.nextFloat() - 0.5f)) * 1.5f
                                      )

private case class GuiElementData(
                                   var shape: Shape,
                                   var name: String,
                                   var position: ConstVec2,
                                   var transformation: ConstMat4,
                                   var angle: Float,
                                   var tangibleList: List[Symbol] = List[Symbol](),
                                   var layer: Int,
                                   var text: String,
                                   var texture: TextureType,
                                   var font: Font,
                                   var fontColor: Color,
                                   var isElastic: Boolean,
                                   isLoot: Boolean,
                                   var enabled: Boolean,
                                   var parent: Option[(Entity, GuiElementData)] = None,
                                   var children: List[(Entity, GuiElementData)] = Nil,
                                   var effects: List[(Entity, GuiEffectData)] = Nil,
                                   id: UUID = UUID.randomUUID(),
                                   var pressed: Boolean = false,
                                   var lastActivationTimestamp: Long = 0L)

private case class EffectShaderData(
                                     var circularProgressBars: List[CircularProgressBarShaderData] = Nil
                                     )

private case class CircularProgressBarShaderData(
                                                  pos: ConstVec2,
                                                  value: Int,
                                                  size: Float,
                                                  color: Color
                                                  )

abstract class TextureType

case class NoTexture() extends TextureType

case class FromFile(file: String) extends TextureType

case class FromBufferedImage(img: BufferedImage) extends TextureType

//TODO: Fix an re-include into example GuiTest
//new EntityDescription(
//  OverlayTexture(
//    pos = ConstVec2(100f, 50f),
//    tex = TextureData(ImageIO.read(new File(base, "textures/button-mid-res.png")))
//  )
//) realize {
//  overlay => println("created overlay " + overlay)
//}
//
//new EntityDescription(
//  OverlayTexture(
//    pos = ConstVec2(400f, 250f),
//    tex = TextureData(ImageIO.read(new File(base, "textures/button-mid-res2.png")))
//  )
//) realize {
//  overlay => println("created overlay " + overlay)
//}
//
//new EntityDescription(
//  OverlayText(
//    pos = ConstVec2(200, 200),
//    text= "does not work ;(",
//    color = java.awt.Color.red,
//    font = new Font(null, Font.PLAIN, 72)
//  )
//).realize()