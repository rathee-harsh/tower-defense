val PROJECTILE_MOVE_SPEED = 0.1

def enemyInPath(game: Game, startLocation: GridPos, finalLocation: GridPos, moveDirection: Direction): Option[Enemy] =
  var start = startLocation
  if game.enemies.nonEmpty then
    while start != finalLocation do

      val distances: Map[Enemy, Double] = game.getEnemyLocations.map((location, enemy) => (enemy, location.getDistance(start)))
      val closest: (Enemy, Double) = distances.minBy((_, distance) => distance)
      if closest._2 < 0.5 then
        return Some(closest._1)
      start = start.moveInDirection(moveDirection, GRID_STEP)

    val distances: Map[Enemy, Double] = game.getEnemyLocations.map((location, enemy) => (enemy, location.getDistance(start)))
      val closest: (Enemy, Double) = distances.minBy((_, distance) => distance)
      if closest._2 < 0.2 then
        return Some(closest._1)
  None

trait Projectile(val image: String, val damage: Int, startingLocation: GridPos, val game: Game, val moveDirection: Direction):
  var location = startingLocation
  var isActive: Boolean = true
  def move(): Unit
end Projectile

class CannonBall(image: String, damage: Int, startingLocation: GridPos, game: Game, dir: Direction)
  extends Projectile(image, damage, startingLocation, game, dir):
  def move() =
    val finalLocation = this.location.moveInDirection(dir, PROJECTILE_MOVE_SPEED)
    enemyInPath(game, this.location, finalLocation, this.moveDirection) match
      case Some(enemy) if enemy.getClass.getName == "LandEnemy" => enemy.takeDamage(this.damage); this.isActive = false
      case _ => this.location = finalLocation
  end move

end CannonBall

class Arrow(image: String, damage: Int, startingLocation: GridPos, game: Game, dir: Direction) 
  extends Projectile(image, damage, startingLocation, game, dir):
  def move() =
    val finalLocation = this.location.moveInDirection(dir, PROJECTILE_MOVE_SPEED)
    enemyInPath(game, this.location, finalLocation, this.moveDirection) match
      case Some(enemy) if enemy.getClass.getName == "AirEnemy" => enemy.takeDamage(this.damage); this.isActive = false
      case _ => this.location = finalLocation

class Bomb(image: String, damage: Int, startingLocation: GridPos, game: Game, dir: Direction, damageArea: Double)
  extends Projectile(image, damage, startingLocation, game, dir):

  private def landEnemiesInArea: Vector[Enemy] =
    game.getEnemyLocations.filter((_, enemy) => enemy match
      case e: LandEnemy => true
      case _ => false
    ).
    map((location, enemy) => (enemy, location.getDistance(this.location))).filter(_._2 < damageArea).map(_._1).toVector

  private def explode(): Unit =
    landEnemiesInArea.foreach(_.takeDamage(damage))
    this.isActive = false

  def move(): Unit =
    val finalLocation = this.location.moveInDirection(dir, PROJECTILE_MOVE_SPEED)
    val pathLocations: Vector[GridPos] =
      game.gridMap.filter((_, direction) => direction == Direction.North || direction == Direction.South || direction == Direction.East || direction == Direction.West).map(_._1).toVector
    val nearest = pathLocations.minBy(_.getDistance(finalLocation))
    if nearest.getDistance(finalLocation) < PROJECTILE_MOVE_SPEED then
      this.location = nearest
      this.explode()
    else
      this.location = finalLocation
  end move

end Bomb