package com.psa.test.functions

import com.psa.test.functions.OptimizationFunctions._


object FunctionHandler {


  // for selecting cases for optimization functions
  def matchTest(x: Array[Double], c: String): Double = {

    var res: Double = 0.0

    c match {

      case "0" => res = x.map(xi => xi).product
      case "1" => res = ShiftedElliptic(x)
      case "2" => res = ShiftedRastrigin(x)
      case "3" => res = ShiftedAckleyFunction(x)
      case "4" => res = Ackley(x)
      case "5" => res = ShiftedRotatedEllipticFunction(x)
      case "6" => res = ShiftedRotatedRastriginFunction(x)
      case "7" => res = ShiftedRotatedAckleyFunction(x)
      case "8" => res = ShiftedRotatedSchwefelFunction(x)
      case "9" => res = NonSeparableShiftedRotatedEllipticFunction(x)
      case "10" => res = NonSeparableShiftedRotatedRastriginFunction(x)
      case "11" => res = NonSeparableShiftedRotatedAckleyFunction(x)
      case "12" => res = NonSeparableShiftedSchwefelFunction(x)
      case "13" => res = ShiftedRosenbrockFunction(x)
      case "14" => res = ConformingOverlappingShiftedSchwefelFunction(x)
      case "15" => res = ConflictingOverlappingShiftedSchwefelFunction(x)
      case "16" => res = ShiftedSchwefelFunction(x)
      case _ => res
    }
    //    math.abs(res)
    res
  }
}