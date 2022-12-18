import scala.io.StdIn.readLine



object Day8 extends App {

    def read(line: String): List[List[(Int, Int)]] = line match {
        case "" => List()
        case _ => List(List(line).flatten.map(x => x.toString.toInt).zipWithIndex) ++ read(readLine())
    }

    def transpose(cols: Int, index: Int, f: List[List[(Int, Int)]]): List[List[(Int, Int)]] = index match {
        case `cols` => List()
        case _ => List(f.map(x => x.take(index + 1).last._1).zipWithIndex) ++ transpose(cols, index + 1, f)
    }

    def isHidden(r: Int, c: Int, height: Int, row1: List[Int], row2: List[Int], col1: List[Int], col2: List[Int]): Int = {
        if (r == 1 || c == 1 || r == row.length || c == col.length) return 1
        if ((row.filter(_ < height).length + col.filter(_ < height).length) == (row.length + col.length)) return 1
        0
    }
    def forestPath(r: Int, c: Int, rows: Int, cols: Int, f: List[List[(Int, Int)]], ft: List[List[(Int, Int)]]): Int = {

        (r, c) match {
            case (`rows`, _) => 0
            case (_, `cols`) => forestPath(r + 1, 0, rows, cols, f, ft)
            case (_, _) => {println((r, c), isHidden(r + 1, c + 1, f.take(r + 1).last.take(c + 1).last._1, f.take(r + 1).last.map(_._1), ft.take(c + 1).last.map(_._1)))
              forestPath(r, c + 1, rows, cols, f, ft) + 
                        isHidden(r + 1, c + 1, f.take(r + 1).last.take(c + 1).last._1, f.take(r + 1).last.map(_._1), ft.take(c + 1).last.map(_._1))}
        }
    }

    val forest = read(readLine())
    val forestTransposed = transpose(forest.head.length, 0, forest)
    println(forest)
    println(forestTransposed)

    println(forestPath(0, 0, forest.length, forestTransposed.length, forest, forestTransposed))


}
