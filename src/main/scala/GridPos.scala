case class GridPos(val x: Int, val y: Int):
  def moveX(moveBy: Int): GridPos =
    GridPos(this.x + moveBy, this.y)

  def moveY(moveBy: Int): GridPos = GridPos(this.x, this.y + moveBy)

  def moveInDirection(direction: Direction, moveBy: Int): GridPos =
    direction match
      case Direction.North  => moveY(-moveBy)
      case Direction.East   => moveX(moveBy)
      case Direction.South  => moveY(moveBy)
      case Direction.West   => moveX(-moveBy)
      case Direction.NonDirectional => this

  override def toString: String = "(" + this.x + ", " + this.y + ")"
  override def equals(another: Any): Boolean  =
    another match
      case anotherGrid: GridPos => this.x == anotherGrid.x && this.y == anotherGrid.y
      case _ => false
end GridPos

enum Direction:
  case North, East, South, West, NonDirectional
