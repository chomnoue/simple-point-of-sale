package com.terminal

import org.scalatest.FunSuite
import org.scalatest.prop.TableFor2

import scala.util.{Failure, Try}


class TerminalTest extends FunSuite {

  import Price._
  import org.scalatest.Inside._
  import org.scalatest.Matchers._
  import org.scalatest.prop.TableDrivenPropertyChecks._

  import scala.util.Success


  def scanAndGetTotal(prices: Map[Code, Set[Price]], items: List[Code]): Try[Double] = {
    items.foldLeft(Terminal(prices))((terminal, code) => terminal.scan(code)).total
  }

  private def scanItemsAndCheckTotals(prices: Map[Code, Set[Price]], itemsAndTotals: TableFor2[List[Code], Double]) = {
    forAll(itemsAndTotals) { (items: List[Code], total: Double) =>
      scanAndGetTotal(prices, items) shouldBe Success(total)
    }
  }

  test("Should Return total after scanning items") {
    val prices: Map[Code, Set[Price]] = Map("A" -> Set(2, 7 forPackOf 4),
      "B" -> Set(12),
      "C" -> Set(1.25, 6 forPackOf 6),
      "D" -> Set(.15)
    )

    val itemsAndTotals = Table(
      ("Items", "Total"),
      (List("A", "B", "C", "D", "A", "B", "A", "A"), 32.40),
      (List("C", "C", "C", "C", "C", "C", "C"), 7.25),
      (List("A", "B", "C", "D"), 15.40),
      (List.empty, 0.0)
    )

    scanItemsAndCheckTotals(prices, itemsAndTotals)
  }

  test("Should Return total after scanning items: complex pricing") {
    val prices: Map[Code, Set[Price]] = Map("A" -> Set(2, 20 forPackOf 12, 7 forPackOf 4),
      "B" -> Set(12, 11),
      "C" -> Set(1.25, 6 forPackOf 6, 5 forPackOf 6),
      "D" -> Set(.15)
    )

    val itemsAndTotals = Table(
      ("Items", "Total"),
      (List("A", "B", "C", "D", "A", "B", "A", "A", "A",
        "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
        "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
        "A", "A", "A", "A", "A", "A", "A", "A", "A", "A"), 83.40),
      (List("C", "C", "C", "C", "C", "C", "C"), 6.25),
      (List("A", "B", "C", "D"), 14.40)
    )

    scanItemsAndCheckTotals(prices, itemsAndTotals)
  }

  test("Should Return error when price not defined") {
    val prices: Map[Code, Set[Price]] = Map("A" -> Set(2, 7 forPackOf 4),
      "B" -> Set(12 forPackOf 3),
      "C" -> Set(1.25, 6 forPackOf 6),
      "D" -> Set(.15)
    )

    val itemsAndTotals = Table(
      ("Items", "Total"),
      (List("A", "B", "C", "D", "A", "B", "A", "A"), "No matching price for code B and quantity 2"),
      (List("C", "C", "C", "E", "C", "C", "C"), "No pricing defined for item E")
    )

    forAll(itemsAndTotals) { (items: List[Code], message: String) =>
      inside(scanAndGetTotal(prices, items)) {
        case Failure(exception: IllegalArgumentException) => exception.getMessage shouldBe message
      }
    }
  }
}
