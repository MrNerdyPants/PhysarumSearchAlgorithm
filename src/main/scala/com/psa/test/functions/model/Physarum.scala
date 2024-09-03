package com.psa.test.functions.model

import lombok.Data

import scala.util.Random

/**
 *
 * @project PhysarumSearchAlgorithm
 * @file Physarum
 * @author Kashan Asim on 02/09/2024.
 *
 */
@Data
class Physarum(var pos: Array[Double], var pop: Int, var energy: Double, var Dim: Int, var min: Double, var max: Double, var fitness: Double) extends Serializable {
  //Array for physarum position x at i'th iteration
  var x: Array[Double] = pos.clone()

  //Array for memory of the hiding space and the position of best fitness value
  var y: Array[Double] = x.clone()

  //  Energy of the physarum
  var e: Double = energy

  //storing the fitness value of current iteration
  var f: Double = fitness

  //storing the overall best fitness value of the physarum
  var fBest: Double = fitness


  private def pulse(x: Array[Double]): Array[Double] = {
    x.map(xi => if (xi < min) {
      min * (0.1 + (Random.nextDouble() * (0.5 - 0.1)))
    } else if (xi > max) {
      max * (0.5 + (Random.nextDouble() * (1.0 - 0.5)))
    } else {
      xi
    })
  }

  private def sdf(value: Double): Double = {
    max = 3.02;
    min = 1.02

    // Clamp the value to min and max range
    val newValue = Math.max(min, Math.min(max, value));

    // Scale the value to 0 - 1
    (newValue - min) / (max - min);
  }

  def migrate(x: Array[Double], w: Double, g: Double): Array[Double] = {
    pulse(x.map(xi => xi * (1 + w) * sdf(g)))
  }



}
