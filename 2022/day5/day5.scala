import scala.io.StdIn.readLine
import scala.collection.immutable.ArraySeq


object Day5 extends App {

  def readTuple(line: String): (Int, Int, Int) = {

     val splits = line.split(" ").toList
     (splits.take(2).last.toInt, splits.take(4).last.toInt, splits.take(6).last.toInt)
  }

  def read(line: String): List[(Int, Int, Int)] = line match {
    
    case "" => List()
    case _ => List(readTuple(line)) ++ read(readLine())
  }

  def operation(stacks: ArraySeq[List[Char]], op: (Int, Int, Int)) = {

     val added = stacks(op._2 - 1).take(op._1).reverse ++ stacks(op._3 - 1)
     val remain = stacks(op._2 - 1).drop(op._1)

     stacks.updated(op._2 - 1, remain).updated(op._3 - 1, added)
  }
  
  def operationv2(stacks: ArraySeq[List[Char]], op: (Int, Int, Int)) = {

     val added = stacks(op._2 - 1).take(op._1) ++ stacks(op._3 - 1)
     val remain = stacks(op._2 - 1).drop(op._1)

     stacks.updated(op._2 - 1, remain).updated(op._3 - 1, added)
  }


  val l1 = List('W', 'B', 'G', 'Z', 'R', 'D', 'C', 'V')
  val l2 = List('V', 'T', 'S', 'B', 'C', 'F', 'W', 'G')
  val l3 = List('W', 'N', 'S', 'B', 'C')
  val l4 = List('P', 'C', 'V', 'J', 'N', 'M', 'G', 'Q')
  val l5 = List('B', 'H', 'D', 'F', 'L', 'S', 'T')
  val l6 = List('N', 'M', 'W', 'T', 'V', 'J')
  val l7 = List('G', 'T', 'S', 'C', 'L', 'F', 'P')
  val l8 = List('Z', 'D', 'B')
  val l9 = List('W', 'Z', 'N', 'B')

  val e1 = List('N', 'Z')
  val e2 = List('D', 'C', 'M')
  val e3 = List('P')

  val stacks = ArraySeq(l1, l2, l3, l4, l5, l6, l7, l8, l9)
  val operations = read(readLine())

  if (args.head == "--1") println(operations.foldLeft(stacks)(operation).map(x => x.head))
  if (args.head == "--2") println(operations.foldLeft(stacks)(operationv2).map(x => x.head))

}



