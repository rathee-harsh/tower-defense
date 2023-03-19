import scala.swing.*
import javax.swing.{BoxLayout, JFrame, JPanel}
import java.awt.BorderLayout
import java.awt.Color
import java.awt.Graphics
import java.awt.Dimension
import javax.imageio.ImageIO
import scala.collection.mutable.Buffer
import java.io.File
import scala.swing.event.*
import java.awt.event.*
import scala.collection.mutable.*

 val WIDTH = 1200
 val HEIGHT = 900

val COLS = 20
val ROWS = 7

val testMap = Buffer.fill(COLS)(Buffer.fill(ROWS)("0"))

class TopBar(levelProgress: Double) extends Panel:
  override def paintComponent(g: Graphics2D): Unit =
    val levelBanner = ImageIO.read(new File("assets/level_banner.png"))
    val enemy = ImageIO.read(new File("assets/enemy.png"))
    g.drawImage( levelBanner, 0, 0, WIDTH, 100, null)
    g.setPaint(Color.black)
    g.fillRect(10, 30, WIDTH - 20, 10)
    g.setPaint(Color.green)
    g.fillRect(10, 30, 10 + ((WIDTH - 50) * levelProgress).toInt, 10)
    g.drawImage( enemy, ((WIDTH - 50) * levelProgress).toInt, 20, 50, 50, null)
end TopBar



class mainMap(var enemyPositions: Map[GridPos, Int]) extends Panel:
  override def paintComponent(g: Graphics2D): Unit =
    val heightOfSquare = (HEIGHT - 200).toDouble/ROWS
    val widthOfSquare = WIDTH.toDouble/COLS
    val image = ImageIO.read(new File("assets/tree.png"))

    val imageMap = Map(
      0 -> ImageIO.read(new File("assets/tree.png")),
      1 -> ImageIO.read(new File("assets/placable.png")),
      2 -> ImageIO.read(new File("assets/path.png"))
    )

    val enemy = ImageIO.read(new File("assets/enemy.png"))

    for i <- 0 until COLS do
      for j <- 0 until ROWS do
        val img = imageMap(testMap(i)(j).split(",")(0).toInt)
        g.drawImage(img, (i * widthOfSquare).toInt, (j * heightOfSquare).toInt, widthOfSquare.toInt, heightOfSquare.toInt, null)
    for i <- 0 until COLS do
      for j <- 0 until ROWS do
        if enemyPositions.keys.toVector.contains(GridPos(i, j)) then
          val xOff = (i * widthOfSquare + widthOfSquare/4).toInt
          val yOff = (j * heightOfSquare + heightOfSquare/4).toInt
          g.drawImage(enemy, xOff, yOff, widthOfSquare.toInt/2, heightOfSquare.toInt/2, null)

end mainMap


class BottomPanel(towerStatus: Buffer[Int]) extends Panel:
  override def paintComponent(g : Graphics2D) =
    g.setColor(Color.gray)
    g.fillRect(0, 0, WIDTH, 60)

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
      g.drawImage(image, 250 + i * 60, 0, 60, 60, null)
end BottomPanel


object AppGUI extends SimpleSwingApplication:

  for i <- 0 until COLS do
    for j <- 0 until ROWS do
      if j == 3 && i != COLS - 1 && i != COLS - 2 then
        testMap(i)(j) = "2,East"
      else if j == 2 || j == 4 || (j == 3 && (i == COLS - 1 || i == COLS - 2)) then
        testMap(i)(j) = "1"
      else
        testMap(i)(j) = "0"
  end for

  val game = Game(testMap)



  val bottomMenu = new BottomPanel(Buffer.fill(5)(0))
  bottomMenu.preferredSize = Dimension(WIDTH, 60)

  val mainGame = new mainMap(Map())

  val topBar = new TopBar(0.5)
  topBar.preferredSize = Dimension(WIDTH, 100)

  val root = new BorderPanel:
    add(topBar,BorderPanel.Position.North)
    add(mainGame,BorderPanel.Position.Center)
    add(bottomMenu,BorderPanel.Position.South)
    focusable = true
    listenTo(this.keys)
    reactions += {
      case KeyPressed(_,  Key.P, _, _) =>
        println("HI")
    }

  val gameWindow = new MainFrame:
    title = "Level 1"
    contents = root
    size = new Dimension(WIDTH, HEIGHT)
    resizable = false


  def top = this.gameWindow

  game.addEnemy()

  val listener = new ActionListener():
      def actionPerformed(e: java.awt.event.ActionEvent) =
        game.advance()
        mainGame.enemyPositions = game.getEnemyLocations
        topBar.repaint()
        mainGame.repaint()
        bottomMenu.repaint()


  val timer = new javax.swing.Timer(800, listener)
  timer.start()

end AppGUI




