package CMAes


/**
 * This class is responsible for [describe what the class does].
 *
 * @project PhysarumSearchAlgorithm 
 * @author KASHAN ASIM
 * @created 16/10/2024
 * @version 1.0
 * @lastModified 16/10/2024 by [Name of the person who last modified it]
 * @see [References to other classes, interfaces, or methods, if any]
 */


import scala.util.Random
import scala.math._

object CMAESOptimizer {

  case class Individual(position: Array[Double], fitness: Double)

  // Rastrigin function (Objective function)
  def rastrigin(x: Array[Double]): Double = {
    val A = 10.0
    val n = x.length
    A * n + x.map(xi => xi * xi - A * cos(2 * Pi * xi)).sum
  }

  // Main CMA-ES implementation
  def cmaes(
             objectiveFunction: Array[Double] => Double,
             dimension: Int,
             populationSize: Int,
             maxGenerations: Int,
             sigma: Double
           ): Individual = {

    // Initial population mean and covariance matrix
    var mean = Array.fill(dimension)(0.0)
    var covarianceMatrix = Array.fill(dimension, dimension)(0.0)
    for (i <- 0 until dimension) covarianceMatrix(i)(i) = 1.0 // Identity matrix

    val random = new Random()

    // Evolution paths
    var pathC = Array.fill(dimension)(0.0)
    var pathSigma = Array.fill(dimension)(0.0)

    // Other parameters
    val mu = populationSize / 2
    val weights = (0 until mu).map(i => log(mu + 0.5) - log(i + 1)).toArray
    val weightsSum = weights.sum
    val weightsNormalized = weights.map(_ / weightsSum)
    val muEff = 1.0 / weightsNormalized.map(w => w * w).sum

    val cs = (muEff + 2.0) / (dimension + muEff + 5.0)
    val ds = 1.0 + 2.0 * max(0.0, sqrt((muEff - 1.0) / (dimension + 1.0)) - 1.0) + cs
    val cc = (4.0 + muEff / dimension) / (dimension + 4.0 + 2.0 * muEff / dimension)
    val c1 = 2.0 / (pow(dimension + 1.3, 2) + muEff)
    val cmu = min(1.0 - c1, 2.0 * (muEff - 2.0 + 1.0 / muEff) / (pow(dimension + 2.0, 2) + muEff))

    var covarianceMatrixDecomp = covarianceMatrix
    var eigenvalues = Array.fill(dimension)(1.0)

    var sigmaGlobal = sigma

    // Utility functions
    def randomNormal(mean: Double, stddev: Double): Double = {
      mean + random.nextGaussian() * stddev
    }

    def generateOffspring(): Array[Double] = {
      val z = Array.fill(dimension)(randomNormal(0.0, 1.0))
      (0 until dimension).map { i =>
        (0 until dimension).map(j => covarianceMatrix(i)(j) * z(j)).sum * sigmaGlobal + mean(i)
      }.toArray
    }

    def recombination(offspring: Seq[Individual]): Unit = {
      val zMean = (0 until dimension).map { i =>
        offspring.zip(weightsNormalized).map {
          case (ind, w) => (ind.position(i) - mean(i)) / sigmaGlobal * w
        }.sum
      }.toArray

      // Update the mean
      mean = (0 until dimension).map { i =>
        offspring.zip(weightsNormalized).map {
          case (ind, w) => ind.position(i) * w
        }.sum
      }.toArray

      // Update paths
      val hSigma = if (norm(pathSigma) / sqrt(1.0 - pow(1.0 - cs, 2.0 * maxGenerations.toDouble)) < 1.4 + 2.0 / (dimension + 1.0)) 1.0 else 0.0

      pathSigma = pathSigma.map(_ * (1.0 - cs)) ++ (0 until dimension).map { i =>
        sqrt(cs * (2.0 - cs) * muEff) * zMean(i)
      }

      pathC = pathC.map(_ * (1.0 - cc)) ++ (0 until dimension).map { i =>
        hSigma * sqrt(cc * (2.0 - cc) * muEff) * zMean(i)
      }

      // Update covariance matrix
      val covarianceUpdate = (0 until dimension).map { i =>
        (0 until dimension).map { j =>
          c1 * (pathC(i) * pathC(j)) + cmu * offspring.zip(weightsNormalized).map {
            case (ind, w) => w * (ind.position(i) - mean(i)) * (ind.position(j) - mean(j)) / (sigmaGlobal * sigmaGlobal)
          }.sum
        }.toArray
      }.toArray

      for (i <- 0 until dimension; j <- 0 until dimension) {
        covarianceMatrix(i)(j) *= (1.0 - c1 - cmu)
        covarianceMatrix(i)(j) += covarianceUpdate(i)(j)
      }

      // Adapt sigma
      sigmaGlobal *= exp((norm(pathSigma) / sqrt(dimension) - 1.0) * cs / ds)
    }

    def norm(vec: Array[Double]): Double = sqrt(vec.map(x => x * x).sum)

    // Main loop
    for (_ <- 0 until maxGenerations) {
      // Generate offspring and evaluate
      val offspring = (0 until populationSize).map { _ =>
        val pos = generateOffspring()
        val fit = objectiveFunction(pos)
        Individual(pos, fit)
      }.sortBy(_.fitness)

      recombination(offspring.take(mu))

      // Check for convergence (optional termination condition)
      if (offspring.head.fitness <= 1e-10) return offspring.head
    }

    Individual(mean, objectiveFunction(mean))
  }

  def main(args: Array[String]): Unit = {
    val dimension = 1000
    val populationSize = 23
    val maxGenerations = 1000
    val sigma = 0.3

    val result = cmaes(rastrigin, dimension, populationSize, maxGenerations, sigma)
    println(s"Best solution: ${result.position.mkString(", ")} with fitness: ${result.fitness}")
  }
}





//
//import breeze.numerics.sin
//
//import scala.util.Random
//import scala.math.{ cos, exp, log, max, min, sqrt}
//import scala.collection.mutable.ArrayBuffer
//
//object CMAESOptimizer {
//
//  def main(args: Array[String]): Unit = {
//    val dimension = 1000 // Higher dimension example
//    val maxIterations = 1000
//    val lowerBounds = Array.fill(dimension)(-5.12)
//    val upperBounds = Array.fill(dimension)(5.12)
//
//    val cmaes = new CMAES(dimension, lowerBounds, upperBounds)
//    val result = cmaes.optimize(new RastriginFunction, maxIterations)
//
//    println("Best solution found:")
//    println(result.bestSolution.mkString(", "))
//    println(s"Best fitness: ${result.bestFitness}")
//    println(s"Verified Result: ${new RastriginFunction().evaluate(result.bestSolution)}")
//  }
//}
//
//// Trait for optimization functions
//trait ObjectiveFunction {
//  def evaluate(x: Array[Double]): Double
//}
//
//// Implementation of the Rastrigin function
//class RastriginFunction extends ObjectiveFunction {
//  override def evaluate(x: Array[Double]): Double = {
//    val A = 10.0
//    val sum = x.foldLeft(A * x.length)((acc, xi) => acc + (xi * xi - A * cos(2 * Math.PI * xi)))
//    sum
//  }
//}
//
//// Case class to hold the optimization result
//case class CMAESResult(bestSolution: Array[Double], bestFitness: Double)
//
//// CMA-ES optimizer implementation
//class CMAES(val dimension: Int, val lowerBounds: Array[Double], val upperBounds: Array[Double]) {
//
//  // Strategy parameters
//  private val lambda: Int = Math.floor(4 + Math.floor(3 * log(dimension))).toInt // Population size
//  private val mu: Int = lambda / 2
//  private val weights: Array[Double] = Array.ofDim[Double](mu)
//  private val mueff: Double = {
//    var sum = 0.0
//    for (i <- 0 until mu) {
//      weights(i) = log(mu + 0.5) - log(i + 1)
//      sum += weights(i) * weights(i)
//    }
//    1.0 / sum
//  }
//
//  // Normalize weights
//  for (i <- 0 until mu) {
//    weights(i) /= weights.map(w => w * w).sum
//  }
//
//  // Step size parameters
//  private val cs: Double = (mueff + 2) / (dimension + mueff + 5)
//  private val ds: Double = 1 + 2 * max(0.0, sqrt(Math.abs((mueff - 1) / (dimension + 1))) - 1) + cs
//  private val cc: Double = (4 + mueff / dimension) / (dimension + 4 + 2 * mueff / dimension)
//  private val c1: Double = 2 / (((dimension + 1.3) * (dimension + 1.3)) + mueff)
//  private val cmu: Double = min(1 - c1, 2 * (mueff - 2 + 1 / mueff) / (((dimension + 2) * (dimension + 2)) + mueff))
//
//  // Initialize dynamic (internal) strategy parameters and constants
//  private var pc: Array[Double] = Array.fill(dimension)(0.0)
//  private var ps: Array[Double] = Array.fill(dimension)(0.0)
//  private var C: Array[Array[Double]] = Array.fill(dimension, dimension)(0.0)
//  for (i <- 0 until dimension) {
//    C(i)(i) = 1.0
//  }
//  private var B: Array[Array[Double]] = identityMatrix(dimension)
//  private var D: Array[Double] = Array.fill(dimension)(1.0)
//  private var BD: Array[Array[Double]] = Array.ofDim[Double](dimension, dimension)
//  for (i <- 0 until dimension) {
//    BD(i)(i) = B(i)(i) * D(i)
//  }
//
//  private var sigma: Double = 0.3 * (upperBounds(0) - lowerBounds(0)) // Initial step size
//
//  private val rand: Random = new Random()
//
//  def optimize(f: ObjectiveFunction, maxIterations: Int): CMAESResult = {
//    var m: Array[Double] = Array.fill(dimension)(0.0) // Initial mean
//    val chiN: Double = sqrt(dimension) * (1.0 - 1.0 / (4.0 * dimension) + 1.0 / (21.0 * dimension * dimension))
//
//    var bestFitness: Double = Double.PositiveInfinity
//    var bestSolution: Array[Double] = Array.empty[Double]
//
//    for (iteration <- 0 until maxIterations) {
//      // Sample population
//      val arz: Array[Array[Double]] = Array.ofDim[Double](lambda, dimension)
//      val arx: Array[Array[Double]] = Array.ofDim[Double](lambda, dimension)
//      for (k <- 0 until lambda) {
//        val z = sampleStandardNormalVector(dimension)
//        arz(k) = z
//        val x = Array.ofDim[Double](dimension)
//        for (i <- 0 until dimension) {
//          var sum = 0.0
//          for (j <- 0 until dimension) {
//            sum += B(i)(j) * D(j) * z(j)
//          }
//          x(i) = m(i) + sum * sigma
//
//          // Handle bounds
//          x(i) = max(x(i), lowerBounds(i))
//          x(i) = min(x(i), upperBounds(i))
//        }
//        arx(k) = x.clone()
//      }
//
//      // Evaluate fitness
//      val fitness: Array[Double] = Array.ofDim[Double](lambda)
//      for (k <- 0 until lambda) {
//        fitness(k) = f.evaluate(arx(k))
//        if (fitness(k) < bestFitness) {
//          bestFitness = fitness(k)
//          bestSolution = arx(k).clone()
//        }
//      }
//
//      // Sort by fitness
//      val sortedIndices: Array[Int] = fitness.indices.toArray.sortWith((i, j) => fitness(i) < fitness(j))
//
//      // Update mean
//      val oldm: Array[Double] = m.clone()
//      for (i <- 0 until dimension) {
//        m(i) = 0.0
//        for (j <- 0 until mu) {
//          val idx = sortedIndices(j)
//          m(i) += weights(j) * arx(idx)(i)
//        }
//      }
//
//      // Update evolution paths
//      val y: Array[Double] = Array.ofDim[Double](dimension)
//      for (i <- 0 until dimension) {
//        y(i) = (m(i) - oldm(i)) / sigma
//      }
//
//      // ps update (simplified)
//      for (i <- 0 until dimension) {
//        // Note: The original Java code only updates the first three components, which is incorrect.
//        // Here, we update all components properly.
//        ps(i) = (1 - cs) * ps(i) + sqrt(cs * (2 - cs) * mueff) * y(i)
//      }
//      val normPs: Double = vectorNorm(ps)
//      val hsig: Int = if ((normPs / sqrt(1 - Math.pow(1 - cs, 2 * (iteration + 1))) / chiN) < (1.4 + 2.0 / (dimension + 1))) 1 else 0
//
//      // pc update
//      for (i <- 0 until dimension) {
//        pc(i) = (1 - cc) * pc(i) + hsig * sqrt(cc * (2 - cc) * mueff) * y(i)
//      }
//
//      // Adapt covariance matrix
//      for (i <- 0 until dimension) {
//        for (j <- 0 to i) { // Symmetric matrix
//          var delta = 0.0
//          for (k <- 0 until mu) {
//            val idx = sortedIndices(k)
//            delta += weights(k) * arz(idx)(i) * arz(idx)(j)
//          }
//          C(i)(j) = (1 - c1 - cmu) * C(i)(j) +
//            c1 * (pc(i) * pc(j) + (1 - hsig) * cc * (2 - cc) * C(i)(j)) +
//            cmu * delta
//          C(j)(i) = C(i)(j) // Maintain symmetry
//        }
//      }
//
//      // Adapt step size
//      val newPsNorm: Double = vectorNorm(ps)
//      sigma = sigma * exp((cs / ds) * (newPsNorm / chiN - 1))
//
//      // Decomposition of C into B and D
//      // For simplicity, skipping eigen decomposition and assuming C remains identity
//      // In a full implementation, perform eigen decomposition here
//      // This is a placeholder and not a correct CMA-ES implementation
//      // To keep the implementation manageable, we'll skip updating B and D
//      // which means we won't adapt the covariance matrix properly.
//
//      // This simplification will degrade performance but keeps the code concise.
//
//      // End of iteration
//      if (iteration % 100 == 0) {
//        println(s"Iteration $iteration: Best fitness = $bestFitness")
//      }
//    }
//
//    CMAESResult(bestSolution, bestFitness)
//  }
//
//  // Utility methods
//
//  // Generates a standard normal random vector using Box-Muller transform
//  private def sampleStandardNormalVector(size: Int): Array[Double] = {
//    val z = Array.ofDim[Double](size)
//    var i = 0
//    while (i < size) {
//      val u1 = rand.nextDouble()
//      val u2 = rand.nextDouble()
//      val r = sqrt(-2.0 * log(u1))
//      val theta = 2.0 * Math.PI * u2
//      z(i) = r * cos(theta)
//      if (i + 1 < size) {
//        z(i + 1) = r * sin(theta)
//      }
//      i += 2
//    }
//    z
//  }
//
//  // Computes the Euclidean norm of a vector
//  private def vectorNorm(v: Array[Double]): Double = {
//    sqrt(v.map(x => x * x).sum)
//  }
//
//  // Generates an identity matrix of size n
//  private def identityMatrix(n: Int): Array[Array[Double]] = {
//    Array.tabulate(n, n) { (i, j) =>
//      if (i == j) 1.0 else 0.0
//    }
//  }
//}
//
