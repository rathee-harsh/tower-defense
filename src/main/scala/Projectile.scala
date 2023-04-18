// Checks if there is any enemy, Forest or Placable tile between the current location and final location.
// The function returns a pair where the first element contains the enemy in an Option container if some enemy was there.
// The second element of the pair is true if there was a forest or placable tile.
// For enemies, the functions checks if the distance between the location and enemyLocation is less than a certain number
def blockagesInPath(game: Game, startLocation: GridPos, finalLocation: GridPos, moveDirection: Tile): (Option[Enemy], Boolean) =
  var start = startLocation
  if game.enemies.nonEmpty then
    while start != finalLocation do
      val distances: Map[Enemy, Double] = game.getEnemyLocations.filter((location, _) => math.abs((location.x - start.x)) <= 1 && math.abs((location.y - start.y)) <= 1)
        .map((location, enemy) => (enemy, location.getDistance(start)))
      if distances.nonEmpty then
        val closest: (Enemy, Double) = distances.minBy((_, distance) => distance)
        if closest._2 < 0.25 then
          return (Some(closest._1), false)
      else if game.gridMap.contains(start) && (game.gridMap(start) == Tile.Forest || game.gridMap(start) == Tile.Placable) then
        return (None, true)
      start = start.moveInDirection(moveDirection, GRID_STEP)

    val distances: Map[Enemy, Double] = game.getEnemyLocations.map((location, enemy) => (enemy, location.getDistance(start)))
      val closest: (Enemy, Double) = distances.minBy((_, distance) => distance)
      if closest._2 < 0.25 then
        return (Some(closest._1), false)
      else if game.gridMap.contains(start) && (game.gridMap(start) == Tile.Forest || game.gridMap(start) == Tile.Placable) then
        return(None, true)
  (None, false)

// A projectile
trait Projectile(val image: String, val damage: Int, startingLocation: GridPos, val game: Game, val moveDirection: Tile):
  var location = startingLocation
  var isActive: Boolean = true // The projectile moves, only if this is true
  def move(): Unit
end Projectile

// Moves if there is nothing in the path.
// Deals a fixed amount of damage to a land enemy if it finds one
class CannonBall(image: String, damage: Int, startingLocation: GridPos, game: Game, dir: Tile)
  extends Projectile(image, damage, startingLocation, game, dir):
  def move() =
    val finalLocation = this.location.moveInDirection(dir, PROJECTILE_MOVE_SPEED)
    blockagesInPath(game, this.location, finalLocation, this.moveDirection) match
      case (Some(enemy), _) if enemy.getClass.getName == "LandEnemy" => enemy.takeDamage(this.damage); this.isActive = false
      case (_, true) => this.isActive = false
      case _ => this.location = finalLocation
  end move

end CannonBall

// Moves if there is nothing in the path.
// Deals a fixed amount of damage to an air enemy if it finds one
class Arrow(image: String, damage: Int, startingLocation: GridPos, game: Game, dir: Tile)
  extends Projectile(image, damage, startingLocation, game, dir):
  def move() =
    val finalLocation = this.location.moveInDirection(dir, PROJECTILE_MOVE_SPEED)
    blockagesInPath(game, this.location, finalLocation, this.moveDirection) match
      case (Some(enemy), _) if enemy.getClass.getName == "AirEnemy" => enemy.takeDamage(this.damage); this.isActive = false
      case (_, true) => this.isActive = false
      case _ => this.location = finalLocation

// Moves till it encounters a path.
// Explodes on the path, dealing a fixed amount of damage to all enemies that are closer than a certain distance
class Bomb(image: String, damage: Int, startingLocation: GridPos, game: Game, dir: Tile, damageArea: Double)
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
      game.gridMap.filter((_, direction) => direction == Tile.North || direction == Tile.South || direction == Tile.East || direction == Tile.West).map(_._1).toVector
    val nearest = pathLocations.minBy(_.getDistance(finalLocation))
    if nearest.getDistance(finalLocation) < PROJECTILE_MOVE_SPEED then
      this.location = nearest
      this.explode()
    else
      this.location = finalLocation
  end move

end Bomb