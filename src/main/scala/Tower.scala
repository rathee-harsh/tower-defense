import scala.collection.mutable.Buffer

trait Tower(val name: String, val image: String, val id: Int, val game: Game, startLevel: Int, startLocation: GridPos):
  var currentLevel: Int = 1

  var location: GridPos = startLocation
  private var level = startLevel

  def upgrade(): Unit = this.level += 1
  def takeTurn(): Unit
end Tower

trait Active extends Tower:
  val enemiesInRange: Buffer[Enemy]
end Active

trait Passive extends Tower


class Melee(name: String, image: String, id: Int, game: Game, startLevel: Int = 1, startLocation: GridPos) extends Active with Tower(name, image, id, game, startLevel, startLocation):
  val enemiesInRange: Buffer[Enemy] = Buffer()

  def takeTurn(): Unit = ???
end Melee

class Ranged(name: String, image: String, id: Int, game: Game, startLevel: Int = 1, startLocation: GridPos) extends Active with Tower(name, image, id, game, startLevel, startLocation):
  val enemiesInRange: Buffer[Enemy] = Buffer()

  def takeTurn(): Unit = ???
end Ranged

class Collector(name: String, image: String, id: Int, game: Game, startLevel: Int = 1, startLocation: GridPos) extends Passive with Tower(name, image, id, game, startLevel, startLocation):
  def takeTurn(): Unit = ???
end Collector

//class KingTower(game: Game, level: Int, location: GridPos) extends Passive with Tower("", "test.png", 1, game, level, location)