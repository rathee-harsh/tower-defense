val PROJECTILE_MOVE_SPEED = 0.2

trait Projectile(val image: String, val damage: Int, startingLocation: GridPos, val game: Game, val moveDirection: Direction):
  var location = startingLocation
  var isActive: Boolean = true
  def move(): Unit
end Projectile

class CannonBall(image: String, damage: Int, startingLocation: GridPos, game: Game, dir: Direction)
  extends Projectile(image, damage, startingLocation, game, dir):

  def enemyLiesInPath(finalLocation: GridPos): Option[Enemy] =
    if game.enemies.nonEmpty then
      while this.location != finalLocation do

        val distances: Map[Enemy, Double] = game.getEnemyLocations.map((location, enemy) => (enemy, location.getDistance(this.location)))
        val closest: (Enemy, Double) = distances.minBy((_, distance) => distance)
        if closest._2 < 0.5 then
          return Some(closest._1)
        this.location = this.location.moveInDirection(moveDirection, GRID_STEP)

      val distances: Map[Enemy, Double] = game.getEnemyLocations.map((location, enemy) => (enemy, location.getDistance(this.location)))
        val closest: (Enemy, Double) = distances.minBy((_, distance) => distance)
        if closest._2 < 0.2 then
          return Some(closest._1)
    None

  def move() =
    val finalLocation = this.location.moveInDirection(dir, PROJECTILE_MOVE_SPEED)
    this.enemyLiesInPath(finalLocation) match
      case Some(enemy) => enemy.takeDamage(this.damage); this.isActive = false
      case None => this.location = finalLocation
  end move

end CannonBall

class Bomb(image: String, damage: Int, startingLocation: GridPos, game: Game, dir: Direction, damageArea: Double)
  extends Projectile(image, damage, startingLocation, game, dir):

  private def enemiesInArea: Vector[Enemy] =
    game.getEnemyLocations.map((location, enemy) => (enemy, location.getDistance(this.location))).filter(_._2 < damageArea).map(_._1).toVector

  private def explode(): Unit =
    enemiesInArea.foreach(_.takeDamage(damage))
    this.isActive = false

  def move(): Unit =
    val finalLocation = this.location.moveInDirection(dir, PROJECTILE_MOVE_SPEED)
    val pathLocations: Vector[GridPos] =
      game.gridMap.filter((_, direction) => direction == Direction.North || direction == Direction.South || direction == Direction.East || direction == Direction.West).map(_._1).toVector
    val nearest = pathLocations.minBy(_.getDistance(finalLocation))
    println(nearest)
    println(finalLocation)
    if nearest.getDistance(finalLocation) < PROJECTILE_MOVE_SPEED then
      this.location = nearest
      this.explode()
    else
      this.location = finalLocation
  end move

end Bomb