import scala.io.StdIn.readLine
import scala.io.Source

object Day6 extends App {

  
   def createMarkers(list: List[Char]): List[List[Char]] = list match {
      
      case a :: b :: c :: d :: tail => List(List(a, b, c, d)) ++ createMarkers(b :: c :: d :: tail)
      case _ => List()
   }

   def createMarkersv2(list: List[Char]): List[List[Char]] = {
      
      if (list.length < 14) return List()
      List(list.take(14)) ++ createMarkersv2(list.drop(1))
   }
   val initial = Source.fromFile("input.txt").mkString
   val seq = List(initial).flatten

   if (args.head == "--1") {
     val pattern = createMarkers(seq).filter(_.distinct.length == 4).head.foldLeft("")((a, b) => a ++ b.toString)
     println(initial.indexOf(pattern) + 4)
   }
   else {
     val pattern = createMarkersv2(seq).filter(_.distinct.length == 14).head.foldLeft("")((a, b) => a ++ b.toString)
     println(initial.indexOf(pattern) + 14)

   }


}




