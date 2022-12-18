import scala.io.StdIn.readLine

object Day2 extends App {

  def readgame(line: String): List[(Char, Char)] = line match {
    case "" => List()
    case _ => List((line.split(" ").head.charAt(0), 
                  line.split(" ").tail.head.charAt(0))) ++ readgame(readLine())
  }

  def game(move: (Int, Int)): Int = {

    val x = ((move._2 - move._1): Int, move: (Int, Int)) 
    x match {
      case (0, _) => x._2._2 + 3
      case (1, _) => x._2._2 + 6
      case (2, _) => x._2._2
      case (-2, _) => x._2._2 + 6
      case _ => x._2._2
    }
  }

  def strategy(move: (Int, Int)): Int = move match {
   
    case (1, 1) => 3
    case (3, 3) => 7
    case (_, 1) => move._1 - 1
    case (_, 2) => move._1 + 3
    case (_, 3) => move._1 + 7
  }
  val games = readgame(readLine())
  
  // part 1
  if (args.head == "--1") println(games.map(x => (x._1.toInt - 'A'.toInt + 1, x._2.toInt - 'X'.toInt + 1))
              .map(game).sum)  
  // part 2
  if (args.head == "--2") println(games.map(x => (x._1.toInt - 'A'.toInt + 1, x._2.toInt - 'X'.toInt + 1))
              .map(strategy).sum)  
}
