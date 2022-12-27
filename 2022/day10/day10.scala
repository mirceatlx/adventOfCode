import scala.io.StdIn.readLine
import scala.io.Source

object Day10 extends App {
    def sim(line: String, register: Int, cycle: Int, query: List[Int]): List[Int] = line.split(" ").head match {

        case "" => if (query.contains(cycle)) List(register * (cycle + 1)) else List()
        case "noop" => if (query.contains(cycle)) List(register * (cycle + 1)) ++ sim(readLine(), register, cycle + 1, query) 
                       else sim(readLine(), register, cycle + 1, query)

        case "inter" => if (query.contains(cycle)) List(register * (cycle + 1)) ++ sim(readLine(), line.split(" ").last.toInt, cycle + 1, query)
                        else sim(readLine(), line.split(" ").last.toInt, cycle + 1, query)

        case "addx" => if (query.contains(cycle)) List(register * (cycle + 1)) ++ 
                                  sim("inter " + (register + line.split(" ").last.toInt).toString, register, cycle + 1, query)
                       else sim("inter " + (register + line.split(" ").last.toInt).toString, register, cycle + 1, query)

    }
    def sim2(line: String, register: Int, cycle: Int, position: Int): List[Char] = line.split(" ").head match {

        case "" =>  List('\n')
        case "noop" => if (List(register - 1, register, register + 1).contains(position)) 
                            List('#') ++ sim2(readLine(), register, cycle + 1, (position + 1) % 40) 
                       else List('.') ++ sim2(readLine(), register, cycle + 1, (position + 1) % 40)

        case "inter" => if (List(register - 1, register, register + 1).contains(position)) 
                        List('#') ++ sim2(readLine(), line.split(" ").last.toInt, cycle + 1, (position + 1) % 40)
                        else List('.') ++ sim2(readLine(), line.split(" ").last.toInt, cycle + 1, (position + 1) % 40)

        case "addx" => if (List(register - 1, register, register + 1).contains(position)) 
                        List('#') ++ sim2("inter " + (register + line.split(" ").last.toInt).toString, register, cycle + 1, (position + 1) % 40)
                       else List('.') ++ sim2("inter " + (register + line.split(" ").last.toInt).toString, register, cycle + 1, (position + 1) % 40)

    }
    if (args.head == "--1") println(sim(readLine(), 1, 0, List(19, 59, 99, 139, 179, 219)).sum)
    if (args.head == "--2") println(sim2(readLine(), 1, 0, 0).take(240).grouped(40).toList.map(x => (x ++ List('\n'))))
    
}
