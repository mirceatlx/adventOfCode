import scala.io.StdIn.readLine


object Day3 extends App {
  
  def read(line: String): List[String] = line match {

    case "" => List()
    case _ => List(line) ++ read(readLine())
  }

  def divideGroups(elfs: List[String]): List[(String, String, String)] = elfs match {

    case a :: b :: c :: tail => List((a, b, c)) ++ divideGroups(tail)
    case _ => List()
  }

  val rucks = read(readLine())
  
  if (args.head == "--1") println(rucks.map(x => (x.substring(0, x.length / 2), 
                                      x.substring(x.length / 2, x.length)))
                                      .map(x => x._1.foldLeft("")
                                      ((a, b) => if (x._2.contains(b)) a ++ b.toString else a))
                                      .map(x => if (x.head.isUpper) x.head - 'A'.toInt + 27
                                                else x.head - 'a'.toInt + 1)
                                      .sum
                                      )

  if (args.head == "--2") println(divideGroups(rucks).map(x => x._1.foldLeft("")
                                        ((a, b) => if (x._2.contains(b) && x._3.contains(b)) a ++ b.toString else a))
                                        .map(x => if (x.head.isUpper) x.head - 'A'.toInt + 27
                                                  else x.head - 'a'.toInt + 1)
                                        .sum)

}


