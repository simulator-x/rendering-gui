package simx.components.renderer.gui

import simplex3d.math.floatx.functions._
import simplex3d.math.float.ConstVec2

abstract class Shape {
  /**
   * Check if a given point (in shape's coordinates) is inside the shape
   * @param p The point to test
   * @return True if p is inside this shape, false otherwise.
   */
  def isInside(p: ConstVec2): Boolean
}

case class QuadShape(size: ConstVec2) extends Shape {
  def width = size.x
  def height = size.y
  def isInside(p: ConstVec2): Boolean = {
    val res = p.x <= width / 2f && p.x >= -(width / 2f) && p.y <= height / 2f && p.y >= -(height / 2f)
    res
  }
}

case class CircleShape(radius: Float) extends Shape {
  def isInside(p: ConstVec2) = length(p) <= radius
}

case class TriangleShape(baselineLength: Float, height: Float) extends Shape {
  def isInside(p: ConstVec2) = {
    val xPercentage = p.x / (baselineLength / 2)
    val yPercentage = p.y / height
    p.y >= 0f && math.abs(xPercentage) + yPercentage <= 1f
  }
}