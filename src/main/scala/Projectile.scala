val PROJECTILE_MOVE_SPEED = 0.1

trait Projectile(val image: String, val damage: Int, startingLocation: GridPos, moveDirection: Direction):
  var location = startingLocation
  var isActive: Boolean = true
  def move(): Unit
end Projectile

class CannonBall(image: String, damage: Int, startingLocation: GridPos, dir: Direction)
  extends Projectile(image, damage, startingLocation, dir):

  private def enemyLiesInPath: Option[Enemy] = None

  def move() =
    val finalLocation = this.location.moveInDirection(dir, PROJECTILE_MOVE_SPEED)
    this.enemyLiesInPath match
      case Some(enemy) => enemy.takeDamage(this.damage); this.isActive = false
      case None => this.location = finalLocation
  end move

end CannonBall

class Bomb(image: String, damage: Int, startingLocation: GridPos, dir: Direction, range: Int, damageArea: (Double, Double))
  extends Projectile(image, damage, startingLocation, dir):

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