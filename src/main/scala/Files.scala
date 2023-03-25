import java.io.{FileWriter, File}

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

end FileOperations
