package simx.components.renderer.gui

import simx.core.components.renderer.setup.DisplaySetupDesc
import simx.core.svaractor.SVarActor
import simx.core.component.ConfigureComponentMessage
import simx.core.entity.description.SValSet
import simx.core.ontology.types.DisplaySetupDescription

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 5/4/13
 * Time: 2:57 PM
 */

case class GuiConfiguration (displaySetupDesc: DisplaySetupDesc)(implicit self : SVarActor.Ref)
  extends ConfigureComponentMessage(SValSet(DisplaySetupDescription(displaySetupDesc))) {}
