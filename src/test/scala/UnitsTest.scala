package transients

import org.scalatest.funsuite.AnyFunSuite
import transients.Quantities._
import breeze.math._

class UnitsTest extends AnyFunSuite {
  
      test("Test math operation on Quantities"){
        assert(Power(4 + 1*i) == Power(4 + 1*i))
        assert(Power(4 + 1*i) + Power(4 + 1*i) == Power(8 + 2*i))
      }
}
