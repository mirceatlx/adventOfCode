import scala.io.StdIn.readLine
import scala.collection.immutable.Set

object Day9 extends App {


    def read(line: String): List[(String, Int)] = line match {

        case "" => List()
        case _ => List((line.split(" ").head, line.split(" ").tail.head.toInt)) ++ read(readLine())
    }

    def unpack(commands: List[(String, Int)]): List[String] = commands match {
        
        case Nil => List()
        case h :: t => List.fill(h._2)(h._1) ++ unpack(t)
    }
    
    def move(chain: List[(Int, Int)], hx: Int, hy: Int, tx: Int, ty: Int): List[(Int, Int)] = chain match {

        case Nil => List()
        case h :: t => if ((hy - h._2).abs < 2 && (hx - h._1).abs < 2) List((h._1, h._2)) ++ move(t, h._1, h._2, h._1, h._2)
                       else { val a = (h._1 + (hx - h._1).sign, h._2 + (hy - h._2).sign)
                         List(a) ++ move(t, a._1, a._2, h._1, h._2)}
    }

    def sim(commands: List[String], positions: Set[(Int, Int)], tail: List[(Int, Int)]): Set[(Int, Int)] = {
      commands match {

        case Nil => positions
        case h :: t => h match {
            
            case "U" => { val n = List((tail.head._1, tail.head._2 + 1)) ++ move(tail.drop(1), tail.head._1, tail.head._2 + 1, tail.head._1, 
              tail.head._2)
            sim(t, positions ++ List(n.last), n)}
            case "D" => { val n = List((tail.head._1, tail.head._2 - 1)) ++ move(tail.drop(1), tail.head._1, tail.head._2 - 1, tail.head._1, 
              tail.head._2)
            sim(t, positions ++ List(n.last), n)}
            case "L" => { val n = List((tail.head._1 - 1, tail.head._2)) ++ move(tail.drop(1), tail.head._1 - 1, tail.head._2, tail.head._1, 
              tail.head._2)
            sim(t, positions ++ List(n.last), n)}
            case "R" => { val n = List((tail.head._1 + 1, tail.head._2)) ++ move(tail.drop(1), tail.head._1 + 1, tail.head._2, tail.head._1, 
              tail.head._2)
            sim(t, positions ++ List(n.last), n)}
        }
    }}

    val input = read(readLine())
    val cc = List((0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0))
    val ccc = List((0, 0), (0, 0))
    if (args.head == "--1") println(sim(unpack(input), Set((0, 0)), ccc).size)
    if (args.head == "--2") println(sim(unpack(input), Set((0, 0)), cc).size)

}
