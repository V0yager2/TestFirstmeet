import scala.math._

object Knn{

case class Struct(id: Int, lat: Double, long: Double, gold: Boolean,distance : Double)

def printArray[T](arr : Array[T],start : Int): Unit = {
  if (start >= arr.length) ()
  else {
    println(arr(start))
    printArray(arr,start+1)
  }
}

def updateMine(mines : Array[Struct],tlat: Double, tlong: Double): Array[Struct] = {
  if (mines.isEmpty) Array.empty[Struct]
  else {
    val dis = sqrt(pow((mines.head.lat - tlat), 2) + pow((mines.head.long - tlong), 2))
    val newmine = mines.head.copy(distance = dis)
    Array(newmine) ++ updateMine(mines.tail,tlat,tlong)
  }
}

def mergeSort(arr: Array[Struct]): Array[Struct] = {
  if (arr.length <= 1) arr
  else {
    val (left, right) = arr.splitAt(arr.length / 2)
    merge(mergeSort(left), mergeSort(right))
  }
}

def merge(left: Array[Struct], right: Array[Struct]): Array[Struct] = {
  if (left.isEmpty) right
  else if (right.isEmpty) left
  else if (left.head.distance < right.head.distance) left.head +: merge(left.tail, right)
  else right.head +: merge(left, right.tail)
}

def vote(mines : Array[Struct],range : Int,count : Int): Boolean = {
  if(range < 0) count >= 0
  else{
    if(mines.head.gold) count + 1
    else count - 1
    vote(mines.tail,range - 1,count)
  }
}

def main(args: Array[String]) = {

  val target_lat: Double = 52
  val target_long: Double = 25
  val defult: Double = 0
  val count: Int = 0

  val minesData : Array[Struct] = Array[Struct](
    Struct(1, 54, 97, true,defult),
    Struct(2, 99, 91, false,defult),
    Struct(3, 61, 53, true,defult),
    Struct(4, 43, 95, false,defult),
    Struct(5, 60, 10, true,defult),
    Struct(6, 40, 74, false,defult),
    Struct(7, 16, 92, true,defult),
    Struct(8, 8, 8, false,defult),
    Struct(9, 90, 37, true,defult),
    Struct(10, 13, 32, false,defult),
    Struct(11, 85, 75, true,defult),
    Struct(12, 98, 23, false,defult),
    Struct(13, 94, 74, true,defult),
    Struct(14, 54, 38, false,defult),
    Struct(15, 36, 42, true,defult))

  if(vote(mergeSort(updateMine(minesData,target_lat,target_long)),3,count)) println("Congratulation,Your mine have GOLD!!!")
  else println("Unfortunately,Your mine doesn't have gold :(")
}
}