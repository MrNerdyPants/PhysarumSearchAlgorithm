package com.psa.service

import com.psa.model.Physarum
import com.psa.test.functions.FunctionHandler.matchTest

import scala.util.Random

/**
 *
 * @project PhysarumSearchAlgorithm
 * @file PhysarumService
 * @author Kashan Asim on 03/09/2024.
 *
 */
object PhysarumService {

  def serialPhysarum(mi: Int, Pop: Int, dim: Int, min: Double, max: Double, max_lifCycles: Int, selection: Int, casef: String) = {
    //Initializing the List of pos for List of Physarums
    var x = List.fill(Pop)(List.fill(dim)((min + Random.nextDouble() * (max - min))).toArray).toArray
    println("Initialization space : ")
    println(x)


    //mapping fitness to place it in objects
    var fitnessesC = x.map(matchTest(_, casef))
    println("Fitness", fitnessesC.toList)

    // creating instances of objects
    var population = (x, List.range(0, Pop)).zipped.map((x_p, i) => new Physarum(x_p, Pop, dim, min, max, fitnessesC(i))).toArray
    println("Population ", population, "\n|Population length : ", population.length)

    var l: Int = 1


    while (l < max_lifCycles) {


      var g: Double = 1 * Random.nextDouble()


      population.foreach(physarum => {

        //        println("before: " + physarum.f)
        val cF = matchTest(physarum.x, casef)
        //        println("after: " + cF)

        physarum.energy(max_lifCycles, l)
        physarum.w = weight(physarum.x, population(0).y)
        g = Random.nextDouble()

        //  Updating memory
        if (cF < physarum.fBest) {
          physarum.y = physarum.x.clone()
          physarum.fBest = cF
          // migrate closer to best physarum
          physarum.migrate(g)
        } else {
          //          create Spores
          if (Random.nextBoolean()) {
            //            ASR Spores
            physarum.crossOver()
          } else {
            //            SR Spores
            physarum.crossOver(population(0).y)
          }

        }


        physarum.f = cF

      })


      val selected = population.sortWith(_.fBest < _.fBest).clone().take((((selection.toDouble * (population.length).toDouble / (100).toDouble))).toInt)
      println("fbest:" + selected(0).fBest)
      population = (selected ++ population.take(population.length - selected.length)).clone()

      l += 1
    }

    val sortedPop = population.sortWith(_.fBest < _.fBest).clone()
    val topPhysarum = sortedPop(0)

    println("Fitness: " + topPhysarum.fBest)
    println("Position: " + topPhysarum.y.toList)

  }

  def weight(x_i: Array[Double], x_m: Array[Double]): Double = {
    (((x_i, x_m).zipped.map((a, b) => b - a).sum) / x_i.length)
    //    (ed(x_i, x_m) / x_i.length) + 0.00000000001

  }

  def ed(x: Array[Double], x_m: Array[Double]): Double = {
    var sum = 0.0
    var i = 0
    while (i < x.length) {
      val diff = x(i) - x_m(i)
      sum += diff * diff
      i += 1
    }
    math.sqrt(sum)
  }

}
