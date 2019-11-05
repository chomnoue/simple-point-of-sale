package com.terminal

import scala.collection.mutable
import scala.util.{Failure, Success, Try}


case class Terminal(pricing: Map[Code, Set[Price]], items: List[Code]) {

  def scan(code: Code): Terminal = copy(items = code :: items)

  val priceOrdering: (Price, Price) => Boolean = (price1, price2) =>
    if (price1.quantity == price2.quantity) {
      //Assuming lower value in case of duplicate price entry
      price1.value < price2.value
    } else {
      price1.quantity > price2.quantity
    }

  def computeItemTotal(code: Code, quantity: Int): Try[Double] = {
    @scala.annotation.tailrec
    def computeItemTotal(current: Try[Double], quantity: Int, prices: List[Price]): Try[Double] = {
      if (quantity == 0) {
        current
      } else {
        prices match {
          case Nil => Failure(new IllegalArgumentException(s"No matching price for code $code and quantity $quantity"))
          case price :: nextPrices =>
            val number = quantity / price.quantity
            val rest = quantity % price.quantity
            computeItemTotal(Success(current.get + number * price.value), rest, nextPrices)
        }
      }
    }

    pricing.get(code) match {
      case None => Failure(new IllegalArgumentException(s"No pricing defined for item $code"))
      case Some(prices) =>
        val orderedPrices = prices.toList.sortWith(priceOrdering)
        computeItemTotal(Success(0), quantity, orderedPrices)
    }
  }

  def computeTotal(codesAndQuantities: List[(Code, Int)]): Try[Double] = {
    @scala.annotation.tailrec
    def computeTotal(current: Try[Double], codesAndQuantities: List[(Code, Int)]): Try[Double] =
      codesAndQuantities match {
        case Nil => current
        case (code, quantity) :: next => computeItemTotal(code, quantity) match {
          case failure: Failure[Double] => failure
          case Success(value) => computeTotal(Success(value + current.get), next)
        }
      }

    computeTotal(Success(0), codesAndQuantities)
  }

  lazy val total: Try[Double] = {
    val itemCounts = items.foldLeft(mutable.HashMap.empty[Code, Int].withDefaultValue(0))((counts, code) => {
      counts(code) += 1
      counts
    }).toMap

    computeTotal(itemCounts.toList)
  }
}

case object Terminal {
  def apply(pricing: Map[Code, Set[Price]]): Terminal = Terminal(pricing, List.empty)
}
