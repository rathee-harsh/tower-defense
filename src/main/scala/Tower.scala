import scala.collection.mutable.Buffer

val MINING_WAIT_TIME = 10

trait Tower(val image: String, val game: Game, startLevel: Int, startLocation: GridPos, val waitTime: Int):
  var currentLevel: Int = 1
  private var waitCounter = 0

  private var currentLocation: GridPos = startLocation
  private var level = startLevel

  def upgrade(): Unit = this.level += 1

  def action(): Unit
  def takeTurn(): Unit =
    if this.waitCounter == waitTime then
      this.action()
      this.waitCounter = 0
    else
      this.waitCounter += 1

  def move(newLocation: GridPos) = this.currentLocation = newLocation
  def location = this.currentLocation
end Tower

class Cannon(game: Game, startLevel: Int = 1, startLocation: GridPos, val directionFacing: Direction, waitBetweenShots: Int)
  extends Tower(CANNON_IMAGE_PATH, game, startLevel, startLocation, waitBetweenShots):
  def action(): Unit =
    this.game.projectiles +=
      new CannonBall("assets/cannon-ball.png", 10, this.location.moveInDirection(this.directionFacing, 0.5), this.game, this.directionFacing)
end Cannon

class Bomber(game: Game, startLevel: Int = 1, startLocation: GridPos, val directionFacing: Direction, waitBetweenShots: Int) extends Tower(BOMBER_IMAGE_PATH, game, startLevel, startLocation, waitBetweenShots):
  def action(): Unit =
    this.game.projectiles += new Bomb("assets/cannon-ball.png", 10, this.location.moveInDirection(this.directionFacing, 0.6), this.game, this.directionFacing, 1)
end Bomber


class Collector(game: Game, startLevel: Int = 1, startLocation: GridPos, private var miningPower: Int) extends Tower(COLLECTOR_IMAGE_PATH, game, startLevel, startLocation, MINING_WAIT_TIME):
  def action(): Unit =
      this.game.resourcesToAdd += this.miningPower
end Collector