trait Enemy(
      val name: String,
      val image: String,
      val enemyType: Int,
      val game: Game,
      val maxHP: Double,
      var startLocation: GridPos):

  var health: Double = maxHP
  private var currentLocation = startLocation
  var moveDirection = Direction.East

  def move(): Unit =
    if this.game.gridMap.contains(this.location) then
      this.moveDirection = this.game.gridMap(this.location)
    this.currentLocation = this.currentLocation.moveInDirection(this.moveDirection, 0.4)
    println(this.currentLocation)
  def location: GridPos = this.currentLocation
  def takeDamage(damage: Double) = this.health = math.max(this.health - damage, 0)
  def isDead: Boolean = this.health == 0
end Enemy

class LandEnemy(name: String, image: String, game: Game, maxHP: Int, dps: Double, startingLocation: GridPos)
  extends Enemy(name, image, 1, game, maxHP, startingLocation)


