import scala.collection.mutable.Buffer

trait Tower(val image: String, val game: Game, startLocation: GridPos, val waitTime: Int, var directionFacing: Tile):

  private var waitCounter = 0
  private var currentLocation: GridPos = startLocation

  // The action that the tower performs
  def action(): Unit

  // Takes turn(calls action) after some intervals
  def takeTurn(): Unit =
    if this.waitCounter == waitTime then
      this.action()
      this.waitCounter = 0
    else
      this.waitCounter += 1

  def move(newLocation: GridPos) = this.currentLocation = newLocation
  def location = this.currentLocation
end Tower

// Shoots cannon-balls. Targets Land Enemies
class Cannon(game: Game, startLocation: GridPos, dir: Tile, waitBetweenShots: Int)
  extends Tower(CANNON_IMAGE_PATH, game, startLocation, waitBetweenShots, dir):
  // Create a new cannon ball every time the method is called
  def action(): Unit =
    this.game.projectiles +=
      new CannonBall("assets/cannon-ball.png", 10, this.location.moveInDirection(this.directionFacing, 0.5), this.game, this.directionFacing)
end Cannon

class Archer(game: Game, startLocation: GridPos, dir: Tile, waitBetweenShots: Int)
  extends Tower(ARCHER_IAMGE_PATH, game, startLocation, waitBetweenShots, dir):
  // Create a new arrow every time the method is called. Targest Air Enemies
  def action(): Unit =
    this.game.projectiles +=
      new Arrow("assets/arrow.png", 10, this.location.moveInDirection(this.directionFacing, 0.5), this.game, this.directionFacing)

class Bomber(game: Game, startLocation: GridPos, dir: Tile, waitBetweenShots: Int)
  extends Tower(BOMBER_IMAGE_PATH, game, startLocation, waitBetweenShots, dir):
  // Create a new bomb every time the method is called. Area damage. Targets land enemies
  def action(): Unit =
    this.game.projectiles += new Bomb("assets/bomb.png", 10, this.location.moveInDirection(this.directionFacing, 0.6), this.game, this.directionFacing, 1)
end Bomber


class Collector(game: Game, startLocation: GridPos, private var miningPower: Int, dir: Tile)
  extends Tower(COLLECTOR_IMAGE_PATH, game, startLocation, MINING_WAIT_TIME, dir):
  // Increase resources every time the method is called
  def action(): Unit =
      this.game.resourcesToAdd += this.miningPower
end Collector