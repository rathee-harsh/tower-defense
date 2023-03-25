val PROJECTILE_MOVE_SPEED = 0.1

trait Projectile(val image: String, val damage: Int, startingLocation: GridPos, val game: Game, val moveDirection: Direction):
  var location = startingLocation
  var isActive: Boolean = true
  def move(): Unit
end Projectile

class CannonBall(image: String, damage: Int, startingLocation: GridPos, game: Game, dir: Direction)
  extends Projectile(image, damage, startingLocation, game, dir):

  private def enemyLiesInPath(finalLocation: GridPos): Option[Enemy] =
    while this.location != finalLocation do
      if game.getEnemyLocations.contains(this.location) then
        return Some(game.getEnemyLocations(this.location))
      this.location = this.location.moveInDirection(dir, GRID_STEP)
    None

  def move() =
    val finalLocation = this.location.moveInDirection(dir, PROJECTILE_MOVE_SPEED)
    this.enemyLiesInPath(finalLocation) match
      case Some(enemy) => enemy.takeDamage(this.damage); this.isActive = false
      case None => this.location = finalLocation
  end move

end CannonBall

class Bomb(image: String, damage: Int, startingLocation: GridPos, game: Game, dir: Direction, range: Int, damageArea: (Double, Double))
  extends Projectile(image, damage, startingLocation, game, dir):

  val finalLocation = startingLocation.moveInDirection(dir, range * PROJECTILE_MOVE_SPEED)

  private def enemiesInArea: Vector[Enemy] = ???

  private def explode(): Unit =
    enemiesInArea.foreach(_.takeDamage(damage))

  def move(): Unit =
    if this.location == finalLocation then
      this.explode()
      this.isActive = false
    else
      this.location = this.location.moveInDirection(dir, PROJECTILE_MOVE_SPEED)
  end move

end Bomb