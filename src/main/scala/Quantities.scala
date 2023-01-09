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
            def * (that: Double) = 
                make(this.value * Complex(that, 0))
            def / (that: Double) = 
                make(this.value / Complex(that, 0))

            def canEqual(that: Any) = that.isInstanceOf[QuantityType]

            override def equals(that: Any) =  that match {
                case that: QType => this.value == that.value && this.getClass == that.getClass
                case _ => false
            }

            override def toString: String = s"$unit_sign = $value"
        }
    }

    object Voltage extends Quantity{
        class V(override val value: Complex) extends QuantityType(value){
            override val unit_sign: String = "V"
        }
        type QType = V
        override def make(value: Complex): QType =
            new V(value)

        def apply(value: Complex): Voltage.V = make(value)
        def apply(value: Int): Voltage.V = make(Complex(value, 0))
    }

    object Current extends Quantity{
        class I(override val value: Complex) extends QuantityType(value){
            override val unit_sign: String = "I"
        }
        type QType = I
        override def make(value: Complex): QType = 
            new I(value)

        def apply(value: Complex): Current.I = make(value)
        def apply(value: Int): Current.I = make(Complex(value, 0))
    }

    object Power extends Quantity{
        class S(override val value: Complex) extends QuantityType(value){
            override val unit_sign: String = "S"
        }
        type QType = S
        override def make(value: Complex): S =
            new S(value)

        def apply(value: Complex): Power.S = make(value)
        def apply(value: Int): Power.S = make(Complex(value, 0))
    }
}

