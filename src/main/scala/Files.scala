import java.io.{File, FileWriter}
import scala.collection.mutable.Buffer
import scala.io.Source

val FILE_STORAGE_PATH = "assets/maps/"
val GAME_FILE_IDENTIFIER = "Tower Defence Map-file"

object FileOperations:
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
      val fileWriter = new FileWriter(new File(FILE_STORAGE_PATH + fileName))
      val COLS = map.length
      val ROWS = map.head.length
      var writeString = ""
      writeString += COLS + " " + ROWS + "\n"
      for i <- 0 until COLS do
        for j <- 0 until ROWS do
          writeString += map(i)(j) + " "
        writeString += "\n"
      writeString = writeString.dropRight(1)
      fileWriter.write(writeString)
      fileWriter.close()
  end saveMap

  def loadMap(fileName: String): Vector[Vector[String]] =
    val reader = Source.fromFile(FILE_STORAGE_PATH + fileName)
    val gameMap: Buffer[Buffer[String]] = Buffer()
    for line <- reader.getLines() do
      val blocks = line.split(" ")
      val rowBlocks = Buffer[String]()
      for block <- blocks do
        rowBlocks += block
      gameMap += rowBlocks
    gameMap.map(_.toVector).toVector


end FileOperations
