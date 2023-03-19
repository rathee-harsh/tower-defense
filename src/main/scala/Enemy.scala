trait Enemy(
      val name: String,
      val image: String,
      val enemyType: Int,
      val game: Game,
      val maxHP: Double,
      var dps: Double,
      var startLocation: GridPos):

  var health: Double = maxHP
  private var currentLocation = startLocation

  def takeTurn(): Unit
  def move(): Unit =
    println(this.game.gridMap(this.location))
    this.currentLocation = this.game.gridMap(this.location)
  def location: GridPos = this.currentLocation
  def takeDamage(damage: Double) = this.health = math.max(this.health - damage, 0)
  def isDead: Boolean = this.health == 0
end Enemy

class LandEnemy(name: String, image: String, game: Game, maxHP: Int, dps: Double, startingLocation: GridPos)
  extends Enemy(name, image, 1, game, maxHP, dps, startingLocation):
  def takeTurn() = this.move()



