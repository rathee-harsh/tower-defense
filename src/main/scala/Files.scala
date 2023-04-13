import java.io.{File, FileWriter}
import scala.collection.mutable.Buffer
import scala.io.Source


object FileOperations:
  // Saves a map to a file
  def saveMap(map: Vector[Vector[String]], fileName: String) =
    def mapSizedCorrectly: Boolean =
      if map.isEmpty then
        false
      else
        val length = map.head.length
        if length == 0 then
          false
        else
          for i <- map do
            if i.length != length then
              return false
          true
    if !mapSizedCorrectly then
      println("Error saving. Map corrupted")
    else
      var writeString = ""
      for i <- 0 until ROWS do
        for j <- 0 until COLS do
          writeString += map(i)(j) + " "
        writeString += "\n"
      writeString = writeString.dropRight(1)
      fileWrite(writeString, fileName, true)
  end saveMap

  // Loads a map from a file and returns it
  def loadMap(fileName: String): Vector[Vector[String]] =
    val gameMap: Buffer[Buffer[String]] = Buffer()
    val reader = Source.fromFile(MAP_STORAGE_PATH + fileName)
    for line <- reader.getLines() do
      val blocks = line.split(" ")
      val rowBlocks = Buffer[String]()
      for block <- blocks do
        rowBlocks += block
      gameMap += rowBlocks
    reader.close()
    gameMap.map(_.toVector).toVector

  // Writes str to 'fileName'. Used for creating maps and levels
  def fileWrite(str: String, fileName: String, isMap: Boolean) =
    val storagePath = if isMap then MAP_STORAGE_PATH else LEVEL_STORAGE_PATH
    val fileWriter = new FileWriter(new File(storagePath + fileName))
    fileWriter.write(str)
    fileWriter.close()
  end fileWrite



end FileOperations
