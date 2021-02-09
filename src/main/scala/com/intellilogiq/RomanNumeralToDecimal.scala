package com.intellilogiq

import scala.annotation.tailrec

/**
 * This is a challenge I got in a coding interview with Standard AI.
 *
 * Given a Roman numeral string return it's decimal representation.
 * The solution I came up with is as follows
 *
 * 1. Reverse the string so that we work from right to left.
 *    This should make it easier to work with since you only need to look at the next character to determine the rule to apply on the current roman numeral
 * 2. Reverse the order of comparison for Roman numerals.
 *    e.g. XIV when reversed becomes VIX. So when we check 'V' and 'I' we subtract 'I' from, instead adding it to V. 5 - 1 = 4
 * 3. Iterate through the string and add the decimals together.
 *
 * https://literacy.kent.edu/Minigrants/Cinci/romanchart.htm
 */
object RomanNumeralToDecimal extends App {

  def romanNumeral(roman: String): Int = {

    val numerals = Map('I' -> 1
      , 'V' -> 5
      , 'X' -> 10
      , 'L' -> 50
      , 'C' -> 100
      , 'D' -> 500
      , 'M' -> 1000)

    @tailrec
    def rec(roman: List[Char], number: Int) : Int =
      roman match {
        case List() => number
        case fst :: Nil => number + numerals.getOrElse(fst, 0)
        case fst :: snd :: rst => {
          val count = numerals
            .get(fst)
            .flatMap(f => numerals
              .get(snd)
              .map(s => (f, s)))
            .map{ case (f, s) => if (f > s) f - s else f + s }
          rec(rst, number + count.getOrElse(0))
        }
      }
    rec(roman.reverse.toList, 0)
  }

    println(romanNumeral("XIV"))
    println(romanNumeral("LXIII"))
    println(romanNumeral("XCVIII"))
    println(romanNumeral("LXVIII"))
    println(romanNumeral("XXXIII"))
    println(romanNumeral("DXXX"))
    println(romanNumeral("DCCVII"))
    println(romanNumeral("MDCCC"))
    println(romanNumeral("LXXXVIII"))

}
