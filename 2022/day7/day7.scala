import scala.io.StdIn.readLine
import scala.collection.immutable.Map
class File(val size: Int, val name: String)
class Directory(val name: String, val files: List[File], val dirs: List[String])

object Day7 extends App {

  def commandLine(dir: String, line: String, m: Map[String, Directory]): Map[String, Directory] = if (line == "") m else line.charAt(0) match {
      
      case '$' => line.charAt(2) match {
                      case 'c' => if (line.charAt(5) != '/' && line.length > 6 && line.substring(5, 7) == "..") {
                                     commandLine(dir.split("#").reverse.drop(1).reverse.reduceLeft((a, b) => a + "#" + b), 
                                                  readLine(), m)
                                  } else 
                      commandLine(dir + "#" + line.split(" ").last, 
                            readLine(), m.updated(dir + "#" + line.split(" ").last, new Directory(dir + "#" + line.split(" ").last, List(), List()))) 
                      case 'l' => processLS(dir, readLine(), m)
                 }
      case _ => Map() 
  }

  def processLS(dir: String, line: String, m: Map[String, Directory]): Map[String, Directory] = if (line == "") m else line.charAt(0) match {
      case '$' => commandLine(dir, line, m)
      case _ => line.charAt(0) match {
                   case 'd' => processLS(dir, readLine(), m.updated(dir, new Directory(dir, m.get(dir).get.files, 
                                  m.get(dir).get.dirs ++ List(dir + "#" + line.split(" ").last)))) 
                   case _ => processLS(dir, readLine(), 
                                  m.updated(dir, new Directory(dir, 
                                      m.get(dir).get.files ++ List(new File(line.split(" ").head.toInt, line.split(" ").last)), m.get(dir).get.dirs))) 
                }
  }

  def traverse(dir: String, filesys: Map[String, Directory]): Int = filesys.get(dir).get.files.map(x => x.size).sum 
                                                                      + filesys.get(dir).get.dirs.map(x => traverse(x, filesys)).sum
  
  val m = commandLine("", readLine(), Map())
  val totalspace = traverse("#/", m)
  if (args.head == "--1") println(m.map(x => traverse(x._1, m)).filter(_ <= 100000).sum)
  if (args.head == "--2") println(m.map(x => traverse(x._1, m)).filter(_ >= 30000000 - (70000000 - totalspace)).toSeq.sorted.head)
}

