package simx.components.renderer.gui.objects

import simx.core.components.physics.ImplicitEitherConversion._
import simplex3d.math.float._
import simx.core.ontology.EntityDescription
import simx.core.components.renderer.createparameter.PointLight
import java.awt.Color
import simx.core.worldinterface.naming.NameIt


/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 5/4/13
 * Time: 3:44 PM
 */
case class Light(lightName: String, pos: ConstVec3) extends EntityDescription(
  PointLight(
    name = lightName,
    transformation = ConstMat4(Mat4x3.translate(pos)),
    diffuseColor = new Color(0.6f, 0.6f, 0.6f),
    specularColor = new Color(0.5f, 0.5f, 0.5f)
  ),
  NameIt("Light")
) {
  require(lightName != null, "The parameter 'lightName' must not be null!")
  require(pos != null, "The parameter 'pos' must not be null!")
}