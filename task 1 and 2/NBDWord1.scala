import scala.annotation.tailrec
import scala.collection.convert.ImplicitConversions.`seq AsJavaList`

object NBDWord1 {

  def main(args: Array[String]) = {
    val days = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    val products = Map("Product1" -> 1, "Product2" -> 2, "Product3" -> 3)
    val productsSale = task5(products)
    val intList = List(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
    println(task1a(days))
    println(task1b(days))
    println(task1c(days))
    println(task2a(days))
    println(task2b(days))
    println(task3(days))
    println(task4c(days))
    println(productsSale)
    println(task6('a', 1, 1.2))
    task7("Mr", "Tomasz", Some("Pietia"))
    println(task8(intList))
    println(task9(intList))
    println(task10(intList))
  }

  def task1a(days: List[String]) = {
    val retval = new StringBuilder("")
    for (i <- 0 until days.length) {
      if (i == days.length - 1) {
        retval ++= days.get(i)
      } else {
        retval ++= days.get(i) + ", "
      }
    }
    retval
  }

  def task1b(days: List[String]) = {
    val retval = new StringBuilder("")
    for (i <- 0 until days.length) {
      {
        if (days.get(i).startsWith("T")) {
          retval ++= days.get(i) + ", "
        }
      }
    }
    retval.subSequence(0, retval.length() - 2)
  }

  def task1c(days: List[String]) = {
    var i: Int = 0
    val retval = new StringBuilder("")
    while (i < days.length) {
      if (i == days.length - 1) {
        retval ++= days.get(i)
      } else {
        retval ++= days.get(i) + ", "
      }
      i += 1;
    }
    retval
  }

  def task2a(days: List[String]) = {
    val retval = new StringBuilder("")

    def inner(list: List[String], i: Int) {
      if (i < list.length) {
        retval ++= days.get(i) + ", "
        inner(list, i + 1)
      }
    }

    inner(days, 0)
    retval.subSequence(0, retval.length() - 2)
  }

  def task2b(list: List[String]) = {
    val retval = new StringBuilder("")

    def inner(list: List[String], i: Int) {
      if (i > -1) {
        inner(list, i - 1)
        retval ++= (list.get(list.length - i - 1) + ", ")
      }
    }

    inner(list, list.length - 1)
    retval.subSequence(0, retval.length() - 2)
  }


  def task3(days: List[String]) = {
    @tailrec
    def add(i: Int, retVal: String): String = {
      if (i == days.length) return retVal

      val currentDay = if (i == days.length - 1) days.get(i) else days.get(i) + ", "

      add(i + 1, retVal + currentDay)
    }
    add(0, "")
  }

  def task4a(days: List[String]) : String = {
    var retVal = days.fold("") {(sum, curr) => {
      sum + curr + ", "
    }}
    retVal.substring(0, retVal.length() - 2)
  }

  def task4b(days: List[String]) : String = {
    var retVal = days.foldRight("") {(sum, curr) => {
      sum + ", " + curr
    }}
    retVal.substring(0, retVal.length() - 2)
  }

  def task4c(days: List[String]) : String = {
    var retVal = days.fold("") {(sum, curr) => {
      if (curr.startsWith("T"))
        sum + curr + ", "
      else sum

    }}
    retVal.substring(0, retVal.length() - 2)
  }

  def task5(products: Map[String, Int]):  Map[String, Double] = {
    products map { case (key, value) => (key, value * 0.9) }
  }

  def task6(tup: (Char, Int, Double)) = {
    println(tup._1)
    println(tup._2)
    println(tup._3)
  }

  def task7(title: String, firstName: String, lastNameOpt: Option[String]) = {
    lastNameOpt match {
      case Some(lastName) => println(s"Hello $title. $lastName")
      case None => println(s"Hello $firstName")
    }
  }

  def task8(intList: List[Int]): List[Int] = {
    def iter(index: Int, currentList: List[Int]): List[Int] = {
      if (index >= currentList.length) return currentList;

      val (part1, part2) = currentList.splitAt(index)

      if (currentList.get(index) == 0)
        iter(index + 1, part1 ++ part2.tail)
      else
        iter(index + 1, currentList)
    }

    iter(0, intList)
  }

  def task9(list: List[Int]): List[Int] = {
    list map (item => item + 1)
  }

  def task10(list: List[Int]): List[Int] = {
    list filter (el => el >= -5 && el <= 12) map (el => el.abs)
  }
}