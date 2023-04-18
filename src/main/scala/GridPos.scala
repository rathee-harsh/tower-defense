// A GridPos is a point with x and y coordianted on a grid
case class GridPos(val x: Double, val y: Double):
  def moveX(moveBy: Double): GridPos =
    GridPos(round2Dec(this.x + moveBy), this.y)
    
  def moveY(moveBy: Double): GridPos = 
    GridPos(this.x, round2Dec(this.y + moveBy))

  def moveInDirection(direction: Tile, moveBy: Double): GridPos =
    direction match
      case Tile.North  => moveY(-moveBy)
      case Tile.East   => moveX(moveBy)
      case Tile.South  => moveY(moveBy)
      case Tile.West   => moveX(-moveBy)
      case _ => this

  def getDistance(another: GridPos) = math.hypot(this.x - another.x, this.y - another.y)

  override def toString: String = "(" + this.x + ", " + this.y + ")"
  
  override def equals(another: Any): Boolean  =
    another match
      case anotherGrid: GridPos => this.x == anotherGrid.x && this.y == anotherGrid.y
      case _ => false
end GridPos

enum Tile:
  case North, East, South, West, NonDirectional, Forest, Placable

// Rounds up to 2 decimal places
def round2Dec(num: Double) = (num * 100).round.toDouble/100