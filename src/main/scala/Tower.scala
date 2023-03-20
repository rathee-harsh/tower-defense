import scala.collection.mutable.Buffer

trait Tower(val image: String, val game: Game, startLevel: Int, startLocation: GridPos):
  var currentLevel: Int = 1

  private var currentLocation: GridPos = startLocation
  private var level = startLevel

  def upgrade(): Unit = this.level += 1

  def takeTurn(): Unit

  def move(newLocation: GridPos) = this.currentLocation = newLocation
  def location = this.currentLocation
end Tower

class Ranged(image: String, game: Game, startLevel: Int = 1, startLocation: GridPos, val directionFacing: Direction)
  extends Tower(image, game, startLevel, startLocation):
  val enemiesInRange: Buffer[Enemy] = Buffer()

  def takeTurn(): Unit = this.game.projectiles += new CannonBall("assets/cannon-ball.png", 10, this.location, this.directionFacing)
end Ranged

class Collector(image: String, game: Game, startLevel: Int = 1, startLocation: GridPos) extends Tower(image, game, startLevel, startLocation):
  def takeTurn(): Unit = ???
end Collector

//class KingTower(game: Game, level: Int, location: GridPos) extends Passive with Tower("", "test.png", 1, game, level, location)