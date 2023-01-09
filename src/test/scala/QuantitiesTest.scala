

import org.scalatest.funsuite.AnyFunSuite
import transients.Quantities.*
import breeze.math.*

class QuantitiesTest extends AnyFunSuite {
  
      test("Test equals() on Units"){
        assert(Power(4 + 1 * i) == Power(4 + 1 * i), "Exact values")
        assert(Power(4 + 1 * i) != Voltage(4 + 1 * i), "Comparing of different units")
        assert(Power(1 + 0 * i) != Power(i), "Different values")
      }
      test("Test math operations"){
        assert(Voltage(1) + Voltage(i) == Voltage(1 + i))
        assert(Current(2 + 5 * i) - Current(2 - 5 * i) == Current(10 * i))
        assert(Power(3 + 2 * i) * Power(3 - 2 * i) == Power(13))
        assert(Power(1) / Power(1 + 2 * i) == Power(0.2 - 0.4 * i))
        assert(Impedance(3 + 2 * i) * 3 == Impedance(9 + 6 * i))
        assert(Power(4 + 2 * i) / 2 == Power(2 + 1 * i))
        assert((Impedance(4) || Impedance(4)) == Impedance(2))
      }
}
