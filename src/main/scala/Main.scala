import Model._

@main def hello: Unit = 
  println(Position(1,1).toString + " -> " + Position(7,2).toString)
  println(Knight(Position(3,3),White).validateMove(Position(4,5)))
  println(Knight(Position(7,7),White).validateMove(Position(8,9)))
  println(Position(1,1).passed(Position(5,5)))
  println(Position(1,1).passed(Position(1,5)))
  println(Position(1,1).passed(Position(1,3)))