package com.psa.model

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
class Physarum(var pos: Array[Double], var pop: Int, var Dim: Int, var min: Double, var max: Double, var fitness: Double) extends Serializable {
  //Array for physarum position x at i'th iteration
  var x: Array[Double] = pos.clone()

  //Array for memory of the hiding space and the position of best fitness value
  var y: Array[Double] = x.clone()

  //  Energy of the physarum
  var e: Double = 2.02

  //  Weight for the physarum
  var w: Double = Random.nextDouble()

  //storing the fitness value of current iteration
  var f: Double = fitness

  //storing the overall best fitness value of the physarum
  var fBest: Double = fitness


  private def pulse(x: Array[Double]): Array[Double] = {
    x.map(xi => if (xi < min) {
//      println("++++BOOOM++++")
      min * (0.1 + (Random.nextDouble() * (0.5 - 0.1)))
    } else if (xi > max) {
//      println("----BOOOM----")
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

  def migrate(g: Double) = {
    x = pulse(x.map(xi => xi * g + w * e))
  }

  //determines the energy length
  def energy(max_lifeCycles: Int, lifeCycle: Int): Double = {
    e = (2.02 - lifeCycle * ((1.08) / max_lifeCycles))
    e
  }


}
