import scala.swing.*
import javax.swing.JFrame
import javax.swing.JPanel
import java.awt.BorderLayout
import java.awt.Color
import java.awt.Graphics
import javax.imageio.ImageIO
import scala.collection.mutable.Buffer
import java.io.File

 val WIDTH = 1400
 val HEIGHT = 900

class TopBar(levelProgress: Double) extends Panel:
  override def paintComponent(g: Graphics2D): Unit =
    val levelBanner = ImageIO.read(new File("assets/level_banner.png"))
    val enemy = ImageIO.read(new File("assets/enemy.png"))
    g.drawImage( levelBanner, 0, 0, WIDTH, 80, null)
    g.setPaint(Color.black)
    g.fillRect(10, 30, WIDTH - 40, 10)
    g.setPaint(Color.green)
    g.fillRect(10, 30, 10 + ((WIDTH - 50) * levelProgress).toInt, 10)
    g.drawImage( enemy, ((WIDTH - 50) * levelProgress).toInt, 20, 50, 50, null)

class TowerPanel() extends Panel:
  override def paintComponent(g : Graphics2D) =
    val image = ImageIO.read(new File("assets/lockedItem.png"))
    g.drawImage(image, 0, 0, 100, 100, null)
end TowerPanel


class StatusPanel() extends Panel:
  override def paintComponent(g : Graphics2D) =
    val img = ImageIO.read(new File("assets/test.png") )
    g.drawImage(img, 0, 15, 200, 70, null)

    val health = ImageIO.read(new File("assets/health.png"))
    g.drawImage(health, 5, 20, 20, 20, null)
    g.setColor(Color.red)
    g.fillRect(30, 25, 100, 8)

    val coins = ImageIO.read(new File("assets/coins.png"))
    g.drawImage(coins, 5, 50, 25, 25, null)
    g.setColor(Color.yellow)
    g.drawString("550", 40, 67)

end StatusPanel


object AppGUI extends SimpleSwingApplication:
  val firstButton  = Button("Press me, please")( () )
  val secondButton = Button("No, press ME!")( () )
  val prompt = Label("Press one of the buttons.")

  val spell = new TowerPanel
  val towers= Buffer[TowerPanel](new TowerPanel, new TowerPanel, new TowerPanel, new TowerPanel)

  val status = new StatusPanel
  val towersAndSpells = BoxPanel(Orientation.Horizontal)
  towersAndSpells.contents ++= towers
  towersAndSpells.contents += spell

  val topBar = BoxPanel(Orientation.Horizontal)
  topBar.contents += new TopBar(0.5)

  val bottomMenu = BoxPanel(Orientation.Horizontal)
  bottomMenu.contents += status
  bottomMenu.contents += towersAndSpells

  val root = BoxPanel(Orientation.Vertical)
  root.contents += topBar
  root.contents += prompt
  root.contents += bottomMenu

  val gameWindow = new MainFrame:
    title = "Level 1"
    contents = root
    size = new Dimension(WIDTH, HEIGHT)
    resizable = false


  def top = this.gameWindow

end AppGUI




