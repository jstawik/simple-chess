package Model

case class Position(column: Int, row: Int){
    override def toString = s"${('A'-1+column).toChar}${row}"
    def inBoard = List(row, column).forall(i => 0 < i && i < 9)
    def distance(that: Position) = ((column - that.column).abs, (row - that.row).abs)
    def passed(that: Position): Seq[Position] = this.distance(that) match {
        case (0, _) => for {i <- this.row to that.row by (that.row-this.row).sign} yield Position(this.column, i)
        case (_, 0) => for {i <- this.column to that.column by (that.column-this.column).sign} yield Position(i, this.row)
        case (i, j) => if(i != j) Seq.empty else for{i<- this.row to that.row by (that.row-this.row).sign} yield Position(i, i)
    }
    def contested(pieces: List[Piece]): Boolean = pieces.exists(_.validateMove(this))
}

sealed trait PieceColor
case object White extends PieceColor
case object Black extends PieceColor

sealed trait Piece(pos: Position, color: PieceColor){
    def moveLogic(next: Position): Boolean
    def validateMove(next: Position) = moveLogic(next) 
        && next.inBoard 
        && pos.distance(next) != (0,0)
        //TODO: Add line-of-sight check
}

case class King(pos: Position, color: PieceColor) extends Piece(pos, color) {
    def moveLogic(next: Position) = pos.distance(next) match {
        case (0, 1) => true
        case (1, 0) => true
        case (1, 1) => true
        // TODO: add castle
        case _ => false
    }
}
case class Queen(pos: Position, color: PieceColor) extends Piece(pos, color) {
    def moveLogic(next: Position) = pos.distance(next) match {
        case (0, _) => true
        case (_, 0) => true
        case (i, j) => i == j
    } 
}
case class Rook(pos: Position, color: PieceColor) extends Piece(pos, color) {
    def moveLogic(next: Position) = pos.distance(next) match {
        case (0, _) => true
        case (_, 0) => true
        case _ => false
    }
}
case class Bishop(pos: Position, color: PieceColor) extends Piece(pos, color) {
    def moveLogic(next: Position) = pos.distance(next) match {
        case (i, j) => i == j
    }
}
case class Knight(pos: Position, color: PieceColor) extends Piece(pos, color){
    def moveLogic(next: Position): Boolean = pos.distance(next).match {
        case (2, 1) => true
        case (1, 2) => true
        case (_, _) => false
    }
}
case class Pawn(pos: Position, color: PieceColor) extends Piece(pos, color){
    def moveLogic(next: Position) = ???
}