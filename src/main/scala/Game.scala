import scala.collection.mutable.{Buffer, Map}
import scala.io.Source

// A game object represents a level of the game
// enemiesPassings is the number of lives or the number of enemies that can cross before the game is over
class Game(level: Int, var enemiesPassings: Int):
  val worldMap: Vector[Vector[String]] = FileOperations.loadMap("" + level) // level map loaded from the map-file
  var enemyCount = 0
  var towerCount = 0
  
  var enemiesKilled = 0

  val towers = Buffer[Tower]()
  var enemies = Buffer[Enemy]()
  val projectiles = Buffer[Projectile]()
  
  var totalResources: Int = 0 // Total coins
  var resourcesToAdd = 0 // This gets added to the total resources every single time the game state is updated
  
  val gridMap = createGirdPosMap(this.worldMap) //  A 2D grid map having Grid Positions and Tiles

  LoadLevel.game = Some(this)

  // True if there are no enemies left or if # lives left > # all enemies that have crossed the map and there are no more enemies
  def gameWon =
    val enemiesLeft = LoadLevel.totalEnemies - enemiesKilled
    (enemiesKilled == LoadLevel.totalEnemies) || ((enemiesLeft == this.enemies.count(_.location.x >= COLS)) && enemiesLeft < enemiesPassings)
  
  // If # enemies that have crossed the map >= # total lives
  def gameLost = this.enemies.count(_.location.x >= COLS) >= enemiesPassings

  def addEnemy(enemy: Enemy) =
    this.enemies += enemy 
  end addEnemy

  def addTower(tower: Tower) =
    this.towers += tower

  // Remove projectiles that are not active or are out of the map
  def removeUnwantedProjectiles(): Unit =
    val toRemove = Buffer[Projectile]()
    for projectile <- projectiles do
      if this.positionOutOfBounds(projectile.location) || !projectile.isActive then
        toRemove += projectile
    toRemove.foreach(projectile => this.projectiles -= projectile)

  // Update the game state
  def advance(): Unit =
    if !this.isOver then
      if !LoadLevel.allRead then
        LoadLevel.execute()
      this.enemies.foreach(_.move())
      this.towers.foreach(_.takeTurn())
      this.projectiles.foreach(_.move())

      this.removeUnwantedProjectiles()

      this.enemies = this.enemies.filterNot(_.isDead)


      this.totalResources += resourcesToAdd
      resourcesToAdd = 0

  def positionOutOfBounds(location: GridPos) = location.x < 0 || location.x > COLS || location.y < 0 || location.y > ROWS

  def isOver: Boolean = this.gameWon || this.gameLost

  def getEnemyLocations: scala.collection.immutable.Map[GridPos, Enemy] =
    this.enemies.map( (enemy: Enemy) => (enemy.location, enemy) ).toMap

  def getTowerLocations: scala.collection.immutable.Map[GridPos, Tower] =
    this.towers.map( (tower: Tower) => (tower.location, tower) ).toMap

  // Loads a level from the level save file
  object LoadLevel:
    val reader = Source.fromFile(LEVEL_STORAGE_PATH + level.toString)
    val lines = reader.getLines()
    var game: Option[Game] = None
    val firstLine = lines.next()
    if firstLine.toIntOption.isEmpty then throw Exception("File Corrupt")
    val totalEnemies = firstLine.toInt
    var enemiesDeployed = 0
    var lastInstruction: String = ""
    var currentEnemiesLeft: Int = 0
    var waveOngoing = false
    var waitTillNextWave = 0
    var allRead = false
    var waitTillNextEnemy = 0
    def execute() =
      if !waveOngoing && waitTillNextWave != 0 then
          waitTillNextWave -= 1
      else if waveOngoing && currentEnemiesLeft != 0 then
        val insSplit = lastInstruction.split(",")
        if insSplit.length != 4 then
          throw Exception("Fille Corrupt")
        if waitTillNextEnemy == 0 then
          waitTillNextEnemy = scala.util.Random.between(5, 20)
          insSplit.head match
            case "landEnemy" =>
              addEnemy(new LandEnemy(LAND_ENEMY_IMAGE, game.get, insSplit(1).toInt, GridPos(0, 2), insSplit(2).toInt))
              currentEnemiesLeft -= 1
              enemiesDeployed += 1
            case "airEnemy"  =>
              addEnemy(new AirEnemy(AIR_ENEMY_IMAGE, game.get, insSplit(1).toInt, GridPos(0, 2), insSplit(2).toInt))
              currentEnemiesLeft -= 1
              enemiesDeployed += 1
            case _ => throw Exception("Fille Corrupt")
        else
          waitTillNextEnemy-= 1
      else
        val instruction = lines.next()
        if (instruction == "wave begin" && !waveOngoing) || (instruction == "wave end" && waveOngoing) then
          waveOngoing = !waveOngoing
        else if (instruction.toIntOption.isDefined) then
          waitTillNextWave = instruction.toInt
        else if instruction.split(",").length == 4 && instruction.split(",")(3).toIntOption.isDefined then
          currentEnemiesLeft = instruction.split(",")(3).toInt
        else if instruction == "end" then
          allRead = true
          reader.close()
        else
          throw Exception("File Corrupt")
        lastInstruction = instruction
    end execute
  end LoadLevel

end Game


//  Creates a 2D grid map having Grid Positions and Tiles
def createGirdPosMap(arrMap: Vector[Vector[String]]) =
  val gridMap = Map[GridPos, Tile]()
  for i <- 0 until ROWS do
    for j <- 0 until COLS do
      val matchString = arrMap(i)(j)
      val matchSplit = matchString.split(",")
      val pos = GridPos(j, i)
      if matchSplit.length == 2 then
        gridMap(pos) =  matchSplit(1).toLowerCase match
          case "north"    => Tile.North
          case "east"     => Tile.East
          case "south"    => Tile.South
          case "west"     => Tile.West
          case "forest"   => Tile.Forest
          case "placable" => Tile.Placable
    end for
  end for
  gridMap
end createGirdPosMap
