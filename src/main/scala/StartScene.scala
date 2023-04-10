import java.awt.{Color, Dimension}
import java.io.{File, FileWriter}
import javax.imageio.ImageIO
import javax.swing.ImageIcon
import scala.io.Source
import scala.swing.{BorderPanel, BoxPanel, Button, ComboBox, FlowPanel, Graphics2D, Label, Orientation}
import scala.swing.event.ButtonClicked

class SettingsAndInstructions

def startScene =
  val reader = Source.fromFile(SETTINGS)
    var enemiesPassingValue = 1
    var speedValue = "slow"
    var startingResourcesValue = 75
    for line <- reader.getLines() do
      val property = line.split(":")(0)
      val value = line.split(":")(1)
      property match
        case "enemiesPassing"    => enemiesPassingValue = value.toInt
        case "speed"             => speedValue = value
        case "startingResources" => startingResourcesValue = value.toInt
  val enemiesPassingLabel = new Label("Total enemies that can pass before the game ends: "):
    foreground = Color.white
  val enemiesPassingOptions = new ComboBox[Int](Seq(3, 2, 1)):
    selection.item = enemiesPassingValue
  val speedLabel = new Label("Game Speed: "):
    foreground = Color.white
  val speedOptions = new ComboBox[String](Seq("slow", "normal", "fast")):
    selection.item = speedValue
  val themeLabel = new Label("Coins at the starting of a level: "):
    foreground = Color.white
  val themeOptions = new ComboBox[Int](Seq(75, 100, 150)):
    selection.item = startingResourcesValue

  val settings = new PauseScreen("Settings", Vector(15, 15, 0, 0)):
    val settingsOptions = new BoxPanel(Orientation.Vertical):
      maximumSize = Dimension(200, 50)
      minimumSize = Dimension(200, 50)
      opaque = false
      val enemiesPassing = new FlowPanel():
        opaque = false
        contents ++= Seq(enemiesPassingLabel, enemiesPassingOptions)
      val speed = new FlowPanel():
        opaque = false
        contents ++= Seq(speedLabel, speedOptions)
      val theme = new FlowPanel():
        opaque = false
        contents ++= Seq(themeLabel, themeOptions)
      contents ++= Seq(enemiesPassing, speed, theme)
    preferredSize = Dimension(PAUSE_SCREEN_WIDTH, PAUSE_SCREEN_HEIGHT)
    layout(settingsOptions) = BorderPanel.Position.Center
    visible = false
    opaque = false
  end settings

  val settingsButton = new Button():
      name = "settings"
      icon = new ImageIcon(ImageIO.read(new File(SETTINGS_IMAGE_PATH)).getScaledInstance(50, 50, 2))
      preferredSize = new Dimension(icon.getIconWidth, icon.getIconHeight)
      focusable = false

  val startButton = new Button():
    name = "start"
    icon = new ImageIcon(ImageIO.read(new File(RUN_IMAGE_PATH)).getScaledInstance(50, 50, 2))
    preferredSize = new Dimension(icon.getIconWidth, icon.getIconHeight)
    focusable = false

  val topMenu = new BoxPanel(Orientation.Vertical):
    val buttonPanel = new BoxPanel(Orientation.Horizontal):
      contents ++= Seq(settingsButton, startButton)
    contents ++= Seq(buttonPanel)
    preferredSize = new Dimension(WIDTH, (0.07 * HEIGHT).toInt)

  val settingsAndInstructions = new FlowPanel:
    preferredSize = new Dimension(WIDTH, (0.93 * HEIGHT).toInt)
    override def paintComponent(g: Graphics2D): Unit =
      val image = ImageIO.read(new File(INSTRUCTIONS_IMAGE_PATH))
      g.drawImage(image, 0, 0, WIDTH, (0.93 * HEIGHT).toInt, null)
    contents ++= Seq(settings)

  def writeSettings() =
    val options = Vector(enemiesPassingOptions.selection.item, speedOptions.selection.item, themeOptions.selection.item)
    val str = "enemiesPassing:" + options(0) + "\nspeed:" + options(1) + "\nstartingResources:" + options(2)
    val fileWriter = new FileWriter(new File(SETTINGS))
    fileWriter.write(str)
    fileWriter.close()

  val root = new BoxPanel(Orientation.Vertical):
    contents ++= Seq(topMenu, settingsAndInstructions)
    listenTo(settingsButton)
    listenTo(startButton)
    reactions += {
      case buttonClicked: ButtonClicked =>
        if buttonClicked.source.name == "settings" then
          if settings.visible then
            writeSettings()
          settings.visible = !settings.visible
        else
          writeSettings()
          AppGUI.startGame()
    }
  root