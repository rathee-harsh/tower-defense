// Levels have different waves. Each wave has land enemies and air enemies and there is a waiting time after each wave.
object CreateLevel:
  var levelString = ""
  var totalEnemies = 0
  def addWave(landEnemy: (Int, Int, Int), airEnemy: (Int, Int, Int), timeToNextWave: Int) =
    val (lHP, lSpeed, lCount) = landEnemy
    val (aHP, aSpeed, aCount) = airEnemy
    if aCount != 0 || lCount != 0 then
      this.levelString += s"wave begin\n"
      if lCount != 0 then
        this.levelString += s"landEnemy,${lHP},${lSpeed},${lCount}\n"
      if aCount != 0 then
        this.levelString += s"airEnemy,${aHP},${aSpeed},${aCount}\n"
      this.levelString += "wave end\n"
      this.levelString += timeToNextWave + "\n"
      this.totalEnemies += lCount
      this.totalEnemies += aCount
  end addWave
  def finishLevel() =
    this.levelString = totalEnemies.toString + "\n" + this.levelString
    this.levelString += "end"
end CreateLevel


@main def levelMaker() =
  CreateLevel.addWave((10, 10, 10), (5, 5, 3), 20)
  CreateLevel.addWave((20, 10, 10), (10, 10, 5), 20)
  CreateLevel.addWave((20, 20, 15), (20, 20, 10), 20)
  CreateLevel.finishLevel()
  FileOperations.fileWrite(CreateLevel.levelString, "2", false)



