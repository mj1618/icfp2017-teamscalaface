import scalax.collection.Graph // or scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

object Main extends App {
  println("Hello, wold!")
  val dg = Graph(0~>1, 2~>0, 2~>3, 3~>2, 3~>5, 4~>2,
    4~>3, 5~>4, 6~>0, 6~>4, 6~>9, 7~>6, 7~>8, 8~>7,
    8~>9, 9~>10, 9~>11, 10~>12, 11~>12, 12~>9)
  println(dg)
}