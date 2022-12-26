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

    def isVisible(height: Int, row: List[Int], col: List[Int]): Boolean = {
        row.filter(_ < height).length == row.length || col.filter(_ < height).length == col.length
    }

    def score(height: Int, l: List[Int]): Int = l match {
      case Nil => 0
      case h :: t => if (height <= h) 1 else 1 + score(height, t) 
    }

    def forestPath(r: Int, c: Int, rows: Int, cols: Int, f: List[List[(Int, Int)]], ft: List[List[(Int, Int)]]): Int = {

        (r, c) match {
            case (`rows`, _) => 0
            case (_, `cols`) => forestPath(r + 1, 0, rows, cols, f, ft)
            case (_, _) => { 
                        if (isVisible(f(r)(c)._1, 
                            f(r).map(_._1).take(c), ft(c).map(_._1).take(r))
                         || isVisible(f(r)(c)._1, 
                            f(r).map(_._1).reverse.take(cols - c - 1), ft(c).map(_._1).reverse.take(rows - r - 1))
                        ) 1 + forestPath(r, c + 1, rows, cols, f, ft) else forestPath(r, c + 1, rows, cols, f, ft)}
        }
    }

    def treeScore(r: Int, c: Int, rows: Int, cols: Int, f: List[List[(Int, Int)]], ft: List[List[(Int, Int)]], mval: Int): Int = {
        (r, c) match {
            case (`rows`, _) => mval
            case (_, `cols`) => treeScore(r + 1, 0, rows, cols, f, ft, mval)
            case (_, _) => {val temp = (score(f(r)(c)._1, f(r).map(_._1).take(c).reverse) * score(ft(c)(r)._1, ft(c).map(_._1).take(r).reverse)
                            * score(f(r)(c)._1, f(r).map(_._1).drop(c + 1))
                            * score(ft(c)(r)._1, ft(c).map(_._1).drop(r + 1)))
                            treeScore(r, c + 1, rows, cols, f, ft, temp.max(mval))
                            }
        }
    }

    val forest = read(readLine())
    val forestTransposed = transpose(forest.head.length, 0, forest)

    if (args.head == "--1") println(forestPath(0, 0, forest.length, forestTransposed.length, forest, forestTransposed))
    if (args.head == "--2") println(treeScore(0, 0, forest.length, forestTransposed.length, forest, forestTransposed, 0))


}
