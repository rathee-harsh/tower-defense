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
  CreateLevel.addWave((10, 1, 10), (5, 3, 1), 20)
  CreateLevel.addWave((15, 2, 10), (10, 3, 1), 20)
  CreateLevel.addWave((10, 3, 20), (5, 3, 3), 20)
  CreateLevel.finishLevel()
  FileOperations.fileWrite(CreateLevel.levelString, "2", false)
//  for i <- 0 until ROWS do
//    for j <- 0 until COLS do
//      if i == 3  then
//        testMap(i)(j) = "2,east"
//      else if i == 2 || i == 4  then
//        testMap(i)(j) = "1,placable"
//      else
//        testMap(i)(j) = "0,forest"
//  end for
//  FileOperations.saveMap(testMap.map(_.toVector).toVector, "1")


