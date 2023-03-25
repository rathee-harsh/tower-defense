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
import javax.swing.ImageIcon


 val WIDTH = 1200
 val HEIGHT = 900

val COLS = 15
val ROWS = 7

val GRID_STEP = 0.1

val CANNON_IMAGE_PATH = "assets/cannon.png"
val COLLECTOR_IMAGE_PATH = "assets/cannon.png"

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



class mainMap(game: Game) extends Panel:

  val heightOfSquare = (HEIGHT - 200).toDouble/ROWS
  val widthOfSquare = WIDTH.toDouble/COLS

  private def drawTopEntities(g: Graphics2D, imagePath: String, location: GridPos) =
    val image = ImageIO.read(new File(imagePath))
    val xOff = (location.x * widthOfSquare + widthOfSquare/4).toInt
    val yOff = (location.y * heightOfSquare + heightOfSquare/4).toInt
    g.drawImage(image, xOff, yOff, widthOfSquare.toInt/2, heightOfSquare.toInt/2, null)

  private def drawMap(g: Graphics2D): Unit =
    val imageMap = Map(
      0 -> ImageIO.read(new File("assets/tree.png")),
      1 -> ImageIO.read(new File("assets/placable.png")),
      2 -> ImageIO.read(new File("assets/path.jpg"))
    )
    for i <- 0 until COLS do
      for j <- 0 until ROWS do
        val img = imageMap(testMap(i)(j).split(",")(0).toInt)
        g.drawImage(img, (i * widthOfSquare).toInt, (j * heightOfSquare).toInt, widthOfSquare.toInt, heightOfSquare.toInt, null)
  end drawMap

  private def drawEnemies(g: Graphics2D): Unit =
    for i <- BigDecimal(0.0) to BigDecimal(COLS.toDouble) by GRID_STEP do
      for j <- BigDecimal(0.0) to BigDecimal(ROWS.toDouble) by GRID_STEP do
        val pos = GridPos(i.toDouble, j.toDouble)
        val enemies = game.getEnemyLocations
        if enemies.keys.toVector.contains(pos) then
          this.drawTopEntities(g, enemies(pos).image, pos)
    end for
  end drawEnemies


  private def drawTowers(g: Graphics2D): Unit =
    val towers = game.getTowerLocations
    for (location, tower) <- towers do
      this.drawTopEntities(g, tower.image, location)
    end for
  end drawTowers

  private def drawProjectiles(g: Graphics2D): Unit =
    for projectile <- game.projectiles do
      this.drawTopEntities(g, projectile.image, projectile.location)
  end drawProjectiles

  private def drawSelectedTower(g: Graphics2D): Unit =
    PickedUpTower.tower match
      case Some(tower) =>
        this.drawTopEntities(g, tower.image, PickedUpTower.location)
      case None => ()


  override def paintComponent(g: Graphics2D): Unit =
    this.drawMap(g)
    this.drawEnemies(g)
    this.drawTowers(g)
    this.drawSelectedTower(g)
    this.drawProjectiles(g)
  end paintComponent

end mainMap


class BottomPanel(game: Game) extends Panel:
  override def paintComponent(g : Graphics2D) =
    g.setColor(Color.gray)
    g.fillRect(0, 0, WIDTH, 60)

    val coins = ImageIO.read(new File("assets/coins.png"))
    val totalResources = game.resources.toString
    g.drawImage(coins, 5, 15, 35, 35, null)
    g.setColor(Color.yellow)
    g.setFont(new Font("TimesRoman", 3, 18))
    g.drawString(totalResources, 60, 35)

end BottomPanel


object AppGUI extends SimpleSwingApplication:

  private def formGridCoordinates(point: Point): GridPos =
    val x = point.getX
    val y = point.getY
    val relativeX = x
    val relativeY = y - topBar.size.getHeight
    GridPos(math.floor(relativeX/root.size.getWidth * COLS), math.floor(relativeY/mainGame.size.getHeight * ROWS))
  end formGridCoordinates

  private def createButtons(towerButtons: Map[String, String]) =
    val buttons = Buffer[Button]()
    for (buttonName, imagePath) <- towerButtons do
      val image = ImageIO.read(new File(imagePath))
      buttons += new Button("") {
        this.name = buttonName
        icon = new ImageIcon(image.getScaledInstance(50, 50, 2))
        preferredSize = new Dimension(icon.getIconWidth, icon.getIconHeight)
      }
    buttons.toSeq

  for i <- 0 until COLS do
    for j <- 0 until ROWS do
      if j == 3 && i != COLS - 1 && i != COLS - 2 then
        testMap(i)(j) = "2,east"
      else if j == 2 || j == 4 || (j == 3 && (i == COLS - 1 || i == COLS - 2)) then
        testMap(i)(j) = "1,placable"
      else
        testMap(i)(j) = "0,forest"
  end for

  FileOperations.saveMap(testMap.map(_.toVector).toVector, "test.txt")

  val towerButtons = Map[String, String]("cannon" -> CANNON_IMAGE_PATH, "collector" -> COLLECTOR_IMAGE_PATH)

  val game = Game(testMap.map(_.toVector).toVector)
  val buttons = this.createButtons(towerButtons)

  val buttonPanel = new FlowPanel
  buttonPanel.contents ++= this.buttons

  val bottomLeft = new BottomPanel(game):
    preferredSize = new Dimension(100, 60)

  val bottomMenu = new BorderPanel:
    layout(buttonPanel) = BorderPanel.Position.East
    layout(bottomLeft) = BorderPanel.Position.West
  bottomMenu.preferredSize = Dimension(WIDTH, 60)


  val mainGame = new mainMap(game)

  val topBar = new TopBar(0.5)
  topBar.preferredSize = Dimension(WIDTH, 100)


  val root = new BorderPanel:
    add(topBar,BorderPanel.Position.North)
    add(mainGame,BorderPanel.Position.Center)
    add(bottomMenu,BorderPanel.Position.South)
    focusable = true
    listenTo(this.keys)
    listenTo(this.mouse.moves)
    listenTo(this.mouse.clicks)
    buttons.foreach( this.listenTo(_) )
    reactions += {
      case KeyPressed(_,  Key.P, _, _) =>
        println("HI")
      case MouseMoved(_, point, _) =>
        if point.getY > topBar.size.getHeight && point.getY < (this.size.getHeight - bottomMenu.size.getHeight) then
          PickedUpTower.cursorOnGame = true
          PickedUpTower.location = formGridCoordinates(point)
        else
          PickedUpTower.cursorOnGame = false
      case MouseClicked(component, point, _, _, _) =>
        if PickedUpTower.tower.isDefined then
          val location = formGridCoordinates(point)

          game.gridMap.get(location) match
            case Some(direction) if !game.getTowerLocations.contains(location) =>
              if direction == Direction.Placable && !PickedUpTower.isCollector then
                PickedUpTower.tower.get.move(location)
                game.addTower(PickedUpTower.tower.get)
                println(game.towers)
              else if direction == Direction.Forest && PickedUpTower.isCollector then
                PickedUpTower.tower.get.move(location)
                game.addTower(PickedUpTower.tower.get)
            case _ => ()
          PickedUpTower.tower = None
          PickedUpTower.cursorOnGame = false


      case buttonClicked: ButtonClicked =>
        buttonClicked.source.name match
          case "cannon" =>
            PickedUpTower.tower = Some(new Cannon( game, 1, GridPos(10, 4), Direction.North, 15))
            PickedUpTower.isCollector = false
          case "collector" =>
            PickedUpTower.tower = Some(new Collector(game, 1, GridPos(10, 4), 15))
            PickedUpTower.isCollector = true
          case _ => ()

    }

  val gameWindow = new MainFrame:
    title = "Level 1"
    contents = root
    preferredSize = new Dimension(WIDTH, HEIGHT)
    resizable = false
  this.gameWindow.pack()



  def top = this.gameWindow

  game.addEnemy(new LandEnemy("Test", "assets/enemy.png", game, 100, 10, GridPos(0, 3)))
  game.addTower(new Cannon( game, 1, GridPos(10, 4), Direction.North, 15))


  val listener = new ActionListener():
      def actionPerformed(e: java.awt.event.ActionEvent) =
        game.advance()
        topBar.repaint()
        mainGame.repaint()
        bottomMenu.repaint()
        game.removeUnwantedProjectiles()
  end listener

  val timer = new javax.swing.Timer(200, listener)
  timer.start()

end AppGUI


object PickedUpTower:
  var tower: Option[Tower] = None
  var cursorOnGame = false
  def toBeDrawn = cursorOnGame && this.tower.isDefined
  var location = GridPos(0, 0)
  var isCollector = false
end PickedUpTower




