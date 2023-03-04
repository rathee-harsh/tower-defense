import scala.swing.*
import javax.swing.JFrame
import javax.swing.JPanel
import java.awt.BorderLayout
import java.awt.Color
import java.awt.Graphics
import java.awt.Dimension
import javax.imageio.ImageIO
import scala.collection.mutable.Buffer
import java.io.File

 val WIDTH = 1400
 val HEIGHT = 900

val testMap = Buffer.fill(8)(Buffer.fill(20)(0))

//80 -> h - 40
//h - 120
//0 -> W

class TopBar(levelProgress: Double) extends Panel:
  override def paintComponent(g: Graphics2D): Unit =
    val levelBanner = ImageIO.read(new File("assets/level_banner.png"))
    val enemy = ImageIO.read(new File("assets/enemy.png"))
    g.drawImage( levelBanner, 0, 0, WIDTH, 80, null)
    g.setPaint(Color.black)
    g.fillRect(10, 30, WIDTH - 20, 10)
    g.setPaint(Color.green)
    g.fillRect(10, 30, 10 + ((WIDTH - 50) * levelProgress).toInt, 10)
    g.drawImage( enemy, ((WIDTH - 50) * levelProgress).toInt, 20, 50, 50, null)

class mainMap extends Panel:
  override def paintComponent(g: Graphics2D): Unit =
    val heightOfSquare = (HEIGHT - 110).toDouble/8
    val widthOfSquare = WIDTH.toDouble/20
    val offset = 0

    val image = ImageIO.read(new File("assets/tree.png"))

    val imageMap = Map(
      0 -> ImageIO.read(new File("assets/tree.png")),
      1 -> ImageIO.read(new File("assets/placable.png")),
      2 -> ImageIO.read(new File("assets/path.png"))
    )

    for i <- 0 until 7 do
      for j <- 0 until 20 do
        g.drawImage(imageMap(testMap(i)(j)), j * widthOfSquare.toInt, offset + (i * heightOfSquare).toInt, widthOfSquare.toInt, heightOfSquare.toInt, null)


class BottomPanel(towerStatus: Buffer[Int]) extends Panel:
  override def paintComponent(g : Graphics2D) =
    g.setColor(Color.gray)
    g.fillRect(0, 0, WIDTH, 100)

    val health = ImageIO.read(new File("assets/health.png"))
    g.drawImage(health, 5, 5, 30, 30, null)
    g.setColor(Color.red)
    g.fillRect(40, 15, 170, 14)

    val coins = ImageIO.read(new File("assets/coins.png"))
    g.drawImage(coins, 5, 30, 35, 35, null)
    g.setColor(Color.yellow)
    g.drawString("550", 50, 47)
    val image = ImageIO.read(new File("assets/lockedItem.png"))
    for i <- towerStatus.indices do
      g.drawImage(image, 250 + i * 100, -17, 100, 100, null)
end BottomPanel


object AppGUI extends SimpleSwingApplication:

  for i <- 0 until 8 do
    if i == 3 then
      for j <- 0 until 20 do
        testMap(i)(j) = 2
    else if i == 2 || i == 4 then
      for j <- 0 until 20 do
        testMap(i)(j) = 1
    else
      for j <- 0 until 20 do
        testMap(i)(j) = 0
  end for



  val bottomMenu = new BottomPanel(Buffer.fill(5)(0))
  bottomMenu.minimumSize = Dimension(WIDTH * 100, 4000)
  bottomMenu.maximumSize = Dimension(WIDTH * 100, 4000)

  val topBar = new TopBar(0.5):
    maximumSize = Dimension(WIDTH, 3350)
    minimumSize = Dimension(WIDTH, 3350)


  val mainGame = new mainMap()


  val root = BoxPanel(Orientation.Vertical)
  root.contents += topBar
  root.contents += mainGame
  root.contents += bottomMenu



  val gameWindow = new MainFrame:
    title = "Level 1"
    contents = root
    size = new Dimension(WIDTH, HEIGHT)
    resizable = false


  def top = this.gameWindow

end AppGUI




