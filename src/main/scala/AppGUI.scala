import scala.swing.*

object AppGUI extends SimpleSwingApplication:
  // Main components:
  val allPartsTogether = BoxPanel(Orientation.Vertical)
  val frame = MainFrame()
  frame.contents = allPartsTogether
  frame.title = "Tower Defence"

  // A method that returns the app’s main window:
  def top = this.frame

end AppGUI