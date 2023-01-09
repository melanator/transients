package transients

import breeze.math._
import breeze.linalg.support.CanTraverseKeyValuePairs.OpArraySS

object Quantities{
    abstract class Quantity {
        type QType <: QuantityType
        abstract class QuantityType(val value: Complex){
            val unit_sign: String

            def + (that: QType): QType = 
                make(this.value + that.value)
            def - (that: QType): QType = 
                make(this.value - that.value)
            def * (that: QType): QType = 
                make(this.value * that.value)
            def / (that: QType): QType = 
                make(this.value / that.value)
            def * (that: Double) = 
                make(this.value * Complex(that, 0))
            def / (that: Double) = 
                make(this.value / Complex(that, 0))

            override def toString(): String = s"${unit_sign} = ${value}"
        }

        def make(value: Complex): QType
    }

    final class Voltage extends Quantity{
        class V(override val value: Complex) extends QuantityType(value){
            override val unit_sign: String = "V"
        }
        type QType = V
        override def make(value: Complex): QType = {
            new V(value)
        }
        
    }
    final class Current extends Quantity{
        class I(override val value: Complex) extends QuantityType(value){
            override val unit_sign: String = "I"
        }
        type QType = I
        override def make(value: Complex): QType = 
            new I(value)
    }

    final class Power extends Quantity{
        class S(override val value: Complex) extends QuantityType(value){
            override val unit_sign: String = "S"
        }
        type QType = S
        override def make(value: Complex): S = {
            new S(value)
        }
    }

    object Voltage {
        def apply(value: Complex): Voltage#V = new Voltage make value
    }

    object Current {
        def apply(value: Complex): Current#I = new Current make value
    }

    object Power {
        def apply(value: Complex): Power#S = new Power make value
    }

}

