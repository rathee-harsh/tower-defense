import scala.collection.mutable.{Buffer, Map}

class Game(val worldMap: Buffer[Buffer[String]]):
  var enemyCount = 0
  val towers = Map[Int, Tower]()
  val enemies = Map[Int, Enemy]()
  val towerLocations = Map[Int, GridPos]()
  val enemyLocaitons = Map[Int, GridPos]()
  var totalResources: Double = 0
  private var gameWon = false
  private var gameLost = false
  val gridMap = createGirdPosMap(this.worldMap)

  def addEnemy() =
    val startingLocation = new GridPos(0, 3)
    val enemy = new LandEnemy("Test", "assets/test.png", this, 100, 10, startingLocation)
    enemies(enemyCount) = enemy
    enemyLocaitons(enemyCount) = startingLocation
    this.enemyCount += 1


  def upgrade(): Unit = ???
  def attackEnemy(): Unit = ???
  def advance(): Unit =
    this.enemies.foreach(_._2.takeTurn())
    this.updateEnemyLocations()

  def moveTower(id: Int, newLocation: GridPos) = ???
  def isOver: Boolean = this.gameWon || this.gameLost

  def updateEnemyLocations(): Unit =
    for id <- enemies.keys do
      enemyLocaitons(id) = enemies(id).location

  def getEnemyLocations: Map[GridPos, Int] =
    this.enemyLocaitons.map( (count: Int, location: GridPos) => (location, enemies(count).enemyType) )


end Game



def createGirdPosMap(arrMap: Buffer[Buffer[String]]) =
  val gridMap = Map[GridPos, Direction]()
  for i <- 0 until COLS do
    for j <- 0 until ROWS do
      val matchString = arrMap(i)(j)
      val matchSplit = matchString.split(",")
      val pos = GridPos(i, j)
      if matchSplit.length == 2 then
        gridMap(pos) =  matchSplit(1).toLowerCase match
          case "north"  => Direction.North
          case "east"   => Direction.East
          case "south"  => Direction.South
          case "west"   => Direction.West
      end if
    end for
  end for
  gridMap
end createGirdPosMap