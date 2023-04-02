trait Enemy(
      val image: String,
      val game: Game,
      val maxHP: Double,
      var startLocation: GridPos,
      val movementSpeed: Int):

  var health: Double = maxHP
  private var currentLocation = startLocation
  var moveDirection = Direction.East

  def move(): Unit =
    if this.game.gridMap.contains(this.location) then
      this.moveDirection = this.game.gridMap(this.location)
    this.currentLocation = this.currentLocation.moveInDirection(this.moveDirection, this.movementSpeed.toDouble/10)
  def location: GridPos = this.currentLocation
  def takeDamage(damage: Double) = this.health = math.max(this.health - damage, 0)
  def isDead: Boolean = this.health == 0
end Enemy

class LandEnemy(image: String, game: Game, maxHP: Int, startingLocation: GridPos, movementSpeed: Int)
  extends Enemy(image, game, maxHP, startingLocation, movementSpeed)


