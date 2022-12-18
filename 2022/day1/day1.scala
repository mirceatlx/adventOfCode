import scala.io.StdIn.readLine

object Day1 extends App {

  def readelf(line: String): List[String] = line match {
    case "" => List()
    case _ => List(line) ++ readelf(readLine())
  }

  def read(line: String): List[List[String]] = line match {
    case "" => List()
    case _ => List(readelf(line)) ++ read(readLine()) 
  }

  val elfs = read(readLine())
  
  // part 1
  if (args.head == "--1") println(elfs.map(x => x.map(y => y.toInt)).map(x => x.sum).max)  
  // part 2
  if (args.head == "--2") println(elfs.map(x => x.map(y => y.toInt)).map(x => x.sum).sorted.reverse.take(3).sum)
}


