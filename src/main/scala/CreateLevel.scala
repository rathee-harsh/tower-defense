object CreateLevel:
  var levelString = ""
  def addWave(landEnemy: (Int, Int, Int), airEnemy: (Int, Int, Int), timeToNextWave: Int) =
    val (lHP, lSpeed, lCount) = landEnemy
    val (aHP, aSpeed, aCount) = airEnemy
    if lCount != 0 then
      this.levelString += s"landEnemy,${lHP},${lSpeed},${lCount}\n"
    if aCount != 0 then
      this.levelString += s"airEnemy,${aHP},${aSpeed},${aCount}\n"
    if aCount != 0 || lCount != 0 then
      this.levelString += timeToNextWave + "\n"
  end addWave
  def finishLevel() =
    this.levelString += "end"
end CreateLevel


@main def levelMaker() =
  CreateLevel.addWave((10, 10, 10), (5, 3, 1), 20)
  CreateLevel.addWave((15, 15, 10), (10, 3, 1), 20)
  CreateLevel.addWave((10, 10, 20), (5, 3, 3), 20)
  CreateLevel.finishLevel()
  FileOperations.fileWrite(CreateLevel.levelString, "level1")
