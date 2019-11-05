
package com.terminal

case class Price(quantity: Int, value: Double)

case class PriceWrapper(value: Double) {
  def forPackOf(quantity: Int): Price = Price(quantity, value)
}

case object Price {
  implicit def toPriceWrapper(value: Double): PriceWrapper = PriceWrapper(value)

  implicit def toPrice(value: Double): Price = Price(1, value)
}
