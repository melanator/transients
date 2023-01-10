package transients

import breeze.math._

object Quantities {
    abstract class Quantity {
        type QType <: QuantityType
        def make(value: Complex): QType

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
            def * (that: Double): QType =
                make(this.value * Complex(that, 0))
            def / (that: Double): QType =
                make(this.value / Complex(that, 0))

            def canEqual(that: Any): Boolean = that.isInstanceOf[QuantityType]

            override def equals(that: Any): Boolean =  that match {
                case that: QuantityType => this.value == that.value && this.getClass == that.getClass
                case _ => false
            }

            override def toString: String = s"$unit_sign = $value"
        }
    }

    object Voltage extends Quantity{
        final class V(override val value: Complex) extends QuantityType(value){
            override val unit_sign: String = "V"
        }
        type QType = V
        override def make(value: Complex): QType =
            new V(value)

        def apply(value: Complex): Voltage.V = make(value)
        def apply(value: Int): Voltage.V = make(Complex(value, 0))
    }

    object Current extends Quantity{
        final class I(override val value: Complex) extends QuantityType(value){
            override val unit_sign: String = "I"
        }
        type QType = I
        override def make(value: Complex): QType = 
            new I(value)

        def apply(value: Complex): Current.I = make(value)
        def apply(value: Int): Current.I = make(Complex(value, 0))
    }

    object Power extends Quantity{
        final class S(override val value: Complex) extends QuantityType(value){
            override val unit_sign: String = "S"
        }
        type QType = S
        override def make(value: Complex): S =
            new S(value)

        def apply(value: Complex): Power.S = make(value)
        def apply(value: Int): Power.S = make(Complex(value, 0))
    }

    object Impedance extends Quantity{
        final class Z(override val value: Complex) extends QuantityType(value){
            override val unit_sign: String = "Z"

            // Parallel connection of impedances
            def ||(that: QType): QType = 
                make(this.value * that.value / (this.value + that.value))
        }
        type QType = Z
        override def make(value: Complex): Z =
            new Z(value)

        def apply(value: Complex): Impedance.Z = make(value)
        def apply(value: Int): Impedance.Z = make(Complex(value, 0))
    }
}

