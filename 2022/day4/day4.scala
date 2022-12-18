import scala.io.StdIn.readLine

object Day4 extends App {

  def readPairs(line: String): List[(Int, Int)] = {
      List((line.split('-').head.toInt, 
                    line.split('-').tail.head.toInt))
  }
  def read(line: String): List[(Int, Int)] = line match {

    case "" => List()
    case _ => readPairs(line.split(",").head) ++ readPairs(line.split(",").tail.head) ++ read(readLine())
  }

  def group(intervals: List[(Int, Int)]): List[((Int, Int), (Int, Int))] = intervals match {
    
    case a :: b :: tail => List((a, b)) ++ group(tail)
    case _ => List()
  }

  def intersectionFull(pair: ((Int, Int), (Int, Int))): Int = {

    val check = (pair._1._1 <= pair._2._1 && pair._1._2 >= pair._2._2) || (pair._2._1 <= pair._1._1 && pair._2._2 >= pair._1._2)

    if (check) return 1
    0
  }

  def intersection(pair: ((Int, Int), (Int, Int))): Int = {
    
    val list = List(pair._1, pair._2).sorted
    if (list.take(2).head._2 >= list.take(2).tail.head._1) return 1
    0
  }

  def one() = {
    val intervals = read(readLine())
    val pairs = group(intervals)
    println(pairs.map(intersectionFull).sum)
  }

  def two() = {
    val intervals = read(readLine())
    val pairs = group(intervals)
    println(pairs.map(intersection).sum)
  }

  if (args.head == "--1") one()
  if (args.head == "--2") two()
}
