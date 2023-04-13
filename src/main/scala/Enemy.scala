trait Enemy(
      val image: String,
      val game: Game,
      val maxHP: Double,
      var startLocation: GridPos,
      val movementSpeed: Int):

  var health: Double = maxHP
  private var currentLocation = startLocation
  var moveDirection = Tile.East
  
  // Get the direction in which the enemy would move next
  def getMoveDirection: Tile = this.game.gridMap(this.location)

  // Move the enemy in the move direction
  def move(): Unit =
    if this.game.gridMap.contains(this.location) then
      this.moveDirection = this.getMoveDirection
    this.currentLocation = this.currentLocation.moveInDirection(this.moveDirection, this.movementSpeed.toDouble/100)
    
  def location: GridPos = this.currentLocation
  
  // Reduce health and if the enemy is dead, increase the kill counter
  def takeDamage(damage: Double) =
    this.health = math.max(this.health - damage, 0)
    if this.health == 0 then
      game.enemiesKilled += 1
  def isDead: Boolean = this.health == 0
end Enemy

class LandEnemy(image: String, game: Game, maxHP: Int, startingLocation: GridPos, movementSpeed: Int)
  extends Enemy(image, game, maxHP, startingLocation, movementSpeed)

class AirEnemy(image: String, game: Game, maxHP: Int, startingLocation: GridPos, movementSpeed: Int)
  extends Enemy(image, game, maxHP, startingLocation, movementSpeed)

