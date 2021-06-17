import scala.annotation.tailrec

//zad 1,2,3,4 ==========================================================================================================
val daysOfAWeek = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

def concatWithForLoop(list: List[String], delimiter: String, filter: String => Boolean): String = {
  var concatenatedElements = ""
  for (i <- 0 until list.length) {
    val element = list(i)
    if (filter(element)) {
      concatenatedElements += delimiter + element
    }
  }
  concatenatedElements
}

def concatWithWhileLoop(list: List[String], delimiter: String, filter: String => Boolean): String = {
  var i = 0
  var concatenatedElements = ""
  while (i < list.length) {
    val element = list(i)
    if (filter(element)) {
      concatenatedElements += delimiter + element
    }
    i += 1
  }
  concatenatedElements
}

def concatWithRecursion(list: List[String], delimiter: String, filter: String => Boolean): String = {
  var head = list.head
  if (!filter(head)) head = ""

  if (list.tail.isEmpty) {
    head
  } else {
    head + delimiter + concatWithRecursion(list.tail, delimiter, filter)
  }
}

def concatWithInverseRecursion(list: List[String], delimiter: String, filter: String => Boolean): String = {
  var head = list.head
  if (!filter(head)) head = ""

  if (list.tail.isEmpty) {
    head
  } else {
    concatWithInverseRecursion(list.tail, delimiter, filter) + delimiter + head
  }

}

def concatWithTailRecursion(list: List[String], delimiter: String, filter: String => Boolean): String = {
  @tailrec
  def concatIter(list: List[String], delimiter: String, filter: String => Boolean, result: String) : String = {
    var head = list.head
    if (!filter(head)) head = ""
    if (list.tail.isEmpty) {
      result + delimiter + head
    } else {
      concatIter(list.tail, delimiter, filter,  result + delimiter + head)
    }
  }
  concatIter(list, delimiter, filter, "")
}

def concatWithFoldLeft(list: List[String], delimiter: String, filter: String => Boolean) : String = {
  list.filter(filter).foldLeft("")(_ + delimiter + _)
}

def concatWithFoldRight(list: List[String], delimiter: String, filter: String => Boolean) : String = {
  list.filter(filter).foldRight("")(delimiter + _ + _)
}

def printMatchingDelimited(list: List[String], delimiter: String, loopThrough: (List[String], String, String => Boolean) => String, filter: String => Boolean): Unit = {
  println(loopThrough(list, delimiter, filter).stripPrefix(delimiter))
}

//zad 1a
printMatchingDelimited(daysOfAWeek, ",", concatWithForLoop, (element: String) => !element.isBlank)
//zad 1b
printMatchingDelimited(daysOfAWeek, ",", concatWithForLoop, (element: String) => element.startsWith("P"))
//zad 1c
printMatchingDelimited(daysOfAWeek, ",", concatWithWhileLoop, (element: String) => !element.isBlank)
//zad 2a
printMatchingDelimited(daysOfAWeek, ",", concatWithRecursion, (element: String) => !element.isBlank)
//zad 2b
printMatchingDelimited(daysOfAWeek, ",", concatWithInverseRecursion, (element: String) => !element.isBlank)
//zad 3
printMatchingDelimited(daysOfAWeek, ",", concatWithTailRecursion, (element: String) => !element.isBlank)
//zad 4a
printMatchingDelimited(daysOfAWeek, ",", concatWithFoldLeft, (element: String) => !element.isBlank)
//zad 4b
printMatchingDelimited(daysOfAWeek, ",", concatWithFoldRight, (element: String) => !element.isBlank)
//zad 4c
printMatchingDelimited(daysOfAWeek, ",", concatWithFoldLeft, (element: String) => element.startsWith("P"))

//======================================================================================================================

// zad 5
val products = Map("Milk" -> 10.0, "Bread" -> 4.0, "Meat" -> 12.0)

def discountProducts(products: Map[String, Double], discount: Double) = {
  products map { case (key, value) => (key, value - (value * discount)) }
}

val discountedProducts = discountProducts(products, 0.1)
discountedProducts.keys.foreach(key => {
  println(key + ": " + discountedProducts.get(key).get)
})

//======================================================================================================================

// zad 6
def printTuple(tuple: (String, Int, Double)) = {
  println(tuple._1 + ", " + tuple._2 + ", " + tuple._3)
}

printTuple(("value1", 1, 100.234))

//======================================================================================================================

// zad 7
val optionTestMap = Map("value1" -> 20.0, "value2" -> 30.0)

println("Key present in a map option returns some(20.0) : " + optionTestMap.get("value1"))
println("Key missing in a map option returns None : " + optionTestMap.get("value100"))

//======================================================================================================================

//zad 8
val numbers = List(0, 2, 3, 5, 0, 2, 0, 12, 15, -6)

def removeValue(list: List[Int], value: Int): List[Int] = {
  val head = list.head
  val tail = list.tail
  var resultList = List[Int]()
  if(head != 0) {
    resultList = resultList.appended(head)
  }
  if(tail.isEmpty){
    resultList
  } else {
    removeValue(tail, value) ::: resultList
  }
}

val newList = removeValue(numbers, 0)
println(newList)

//======================================================================================================================

//zad 9
val numbers2 = List(0, 2, 3, 5)

def addToEach(list: List[Int], value: Int): List[Int] = {
  list map (element => element + value)
}

val increasedNumbers = addToEach(numbers2, 1)
println(increasedNumbers)

//======================================================================================================================

// zad 10
val realNumbers = List(0.0, 2.0, 3.4, -5.00, 0.9, 2.0, 0.0, 12.23, 15.34, -6.0)

def transformIntoAbsoluteValues(list: List[Double]): List[Double] = {
  list.filter(element => element >= -5 && element <= 12).map(element => element.abs)
}

val transformedNumbers = transformIntoAbsoluteValues(realNumbers)
println(transformedNumbers)

//======================================================================================================================