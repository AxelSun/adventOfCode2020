package adventofcode.puzzles

import adventofcode.SantasLittleHelper

object Day4 extends SantasLittleHelper {

  def run(): Unit = {
    val input = readInputAsStrig("/day4.txt")
      .split("\\n\\n")
      .map(_.replace('\n', ' '))
      .map(_.split(" ").toList)
      .toList

    val passportsWithValidKeys = input
      .map(toPassportMap(_))
      .filter(validPassportKeys(_))

    val nbrValidKeys = passportsWithValidKeys.length

    val validPassport = passportsWithValidKeys.filter(isValidPassport(_))

    val nbrValidPassports = validPassport.length

    printPuzzleAnswer(nbrValidKeys.toString, nbrValidPassports.toString)
  }

  def toPassportMap(l: List[String]): Map[String, String] =
    l.map{ e =>
        val key = e.substring(0, 3)
        val value = e.substring(4, e.length)
        (key, value)
      }.toMap

  def validPassportKeys(m: Map[String, String]): Boolean = {
    m.size == 8 || (m.size == 7 && m.contains("cid") == false)
  }

  def isValidPassport(m: Map[String, String]): Boolean = {
    m("byr") >= "1920" && m("byr") <= "2002" &&
    m("iyr") >= "2010" && m("iyr") <= "2020" &&
    m("eyr") >= "2020" && m("eyr") <= "2030" &&
    ((m("hgt").contains("cm") && m("hgt") >= "150cm" && m("hgt") <= "193cm") || (m("hgt").contains("in") && m("hgt") >= "59" && m("hgt") <= "76")) &&
    m("hcl").startsWith("#") && m("hcl").substring(1).matches("^[a-f0-9]*$") && m("hcl").length == 7 &&
    List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").exists(m("ecl").contains) &&
    m("pid").forall(_.isDigit) && m("pid").length == 9
  }

}