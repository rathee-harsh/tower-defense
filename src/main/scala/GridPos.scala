case class GridPos(val x: Double, val y: Double):
  def moveX(moveBy: Double): GridPos =
    GridPos(round2Dec(this.x + moveBy), this.y)

  def moveY(moveBy: Double): GridPos = GridPos(this.x, round2Dec(this.y + moveBy))

  def moveInDirection(direction: Direction, moveBy: Double): GridPos =
    direction match
      case Direction.North  => moveY(-moveBy)
      case Direction.East   => moveX(moveBy)
      case Direction.South  => moveY(moveBy)
      case Direction.West   => moveX(-moveBy)
      case _ => this

  def getDistance(another: GridPos) = math.sqrt(math.hypot(this.x - another.x, this.y - another.y))

  override def toString: String = "(" + this.x + ", " + this.y + ")"
  override def equals(another: Any): Boolean  =
    another match
      case anotherGrid: GridPos => this.x == anotherGrid.x && this.y == anotherGrid.y
      case _ => false
end GridPos

enum Direction:
  case North, East, South, West, NonDirectional, Forest, Placable

def round2Dec(num: Double) = (num * 100).round.toDouble/100