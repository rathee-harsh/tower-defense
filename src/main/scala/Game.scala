import AppGUI.game

import scala.collection.mutable.{Buffer, Map}

class Game(val worldMap: Vector[Vector[String]]):
  var enemyCount = 0
  var towerCount = 0

  val towers = Buffer[Tower]()
  var enemies = Buffer[Enemy]()

  val projectiles = Buffer[Projectile]()
  private var totalResources: Int = 0
  var resourcesToAdd = 0

  private var gameWon = false
  private var gameLost = false

  val gridMap = createGirdPosMap(this.worldMap)
  println(gridMap)

  def resources = this.totalResources

  def addEnemy(enemy: Enemy) =
    this.enemies += enemy 
  end addEnemy

  def addTower(tower: Tower) =
    this.towers += tower

  def upgrade(tower: Tower): Unit =
    tower.upgrade()

  def removeUnwantedProjectiles(): Unit =
    val toRemove = Buffer[Projectile]()
    for projectile <- projectiles do
      if this.positionOutOfBounds(projectile.location) || !projectile.isActive then
        toRemove += projectile
    toRemove.foreach(projectile => this.projectiles -= projectile)

  def advance(): Unit =
    this.enemies.foreach(_.move())
    this.towers.foreach(_.takeTurn())
    this.projectiles.foreach(_.move())

    this.removeUnwantedProjectiles()
    this.enemies = this.enemies.filterNot(_.isDead)

    this.totalResources += resourcesToAdd
    resourcesToAdd = 0

  def positionOutOfBounds(location: GridPos) = location.x < 0 || location.x > COLS || location.y < 0 || location.y > ROWS

  def isOver: Boolean = this.gameWon || this.gameLost

  def updateTowerLocation(tower: Tower, newLocation: GridPos) =
    tower.move(newLocation)



  def getEnemyLocations: scala.collection.immutable.Map[GridPos, Enemy] =
    this.enemies.map( (enemy: Enemy) => (enemy.location, enemy) ).toMap

  def getTowerLocations: scala.collection.immutable.Map[GridPos, Tower] =
    this.towers.map( (tower: Tower) => (tower.location, tower) ).toMap


end Game



def createGirdPosMap(arrMap: Vector[Vector[String]]) =
  val gridMap = Map[GridPos, Direction]()
  for i <- 0 until COLS do
    for j <- 0 until ROWS do
      val matchString = arrMap(i)(j)
      val matchSplit = matchString.split(",")
      val pos = GridPos(i, j)
      if matchSplit.length == 2 then
        gridMap(pos) =  matchSplit(1).toLowerCase match
          case "north"    => Direction.North
          case "east"     => Direction.East
          case "south"    => Direction.South
          case "west"     => Direction.West
          case "forest"   => Direction.Forest
          case "placable" => Direction.Placable
    end for
  end for
  gridMap
end createGirdPosMap