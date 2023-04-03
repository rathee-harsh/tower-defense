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
val COLLECTOR_IMAGE_PATH = "assets/gold_mine.png"
val BOMBER_IMAGE_PATH = "assets/bomber.png"

val testMap = FileOperations.loadMap("test.txt")


class TopBar(game: Game) extends Panel:
  override def paintComponent(g: Graphics2D): Unit =
    val levelProgress = game.LoadLevel.enemiesDeployed.toDouble/game.LoadLevel.totalEnemies
    val levelBanner = ImageIO.read(new File("assets/level_banner.png"))
    val enemy = ImageIO.read(new File("assets/enemy.png"))
    g.drawImage( levelBanner, 0, 0, WIDTH, 100, null)
    g.setPaint(Color.black)
    g.fillRect(10, 30, WIDTH - 20, 10)
    g.setPaint(Color.green)
    g.fillRect(10, 30, 10 + ((WIDTH - 50) * levelProgress).toInt, 10)
    g.drawImage( enemy, ((WIDTH - 50) * levelProgress).toInt, 20, 50, 50, null)
end TopBar


class PauseScreen(game: Game) extends BorderPanel:
  override def paintComponent(g: Graphics2D): Unit =
    g.setPaint(Color.black)
    g.setFont(new Font("TimesRoman", 3, 35))
    g.fillRect(0, 0, 500, 500)
    g.setColor(Color.white)
    g.drawString("Game Paused", 125, 50)
end PauseScreen


class mainMap(game: Game) extends FlowPanel:


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
    if !game.isPaused then
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
        focusable = false
      }
    buttons.toSeq

  val towerButtons = Map[String, String]("cannon" -> CANNON_IMAGE_PATH, "bomber" -> BOMBER_IMAGE_PATH, "collector" -> COLLECTOR_IMAGE_PATH)


  val game = Game(1, testMap)
  val buttons = this.createButtons(towerButtons)


  val buttonPanel = new FlowPanel
  buttonPanel.contents ++= this.buttons

  val bottomLeft = new BottomPanel(game):
    preferredSize = new Dimension(100, 60)

  val bottomMenu = new BorderPanel:
    layout(buttonPanel) = BorderPanel.Position.East
    layout(bottomLeft) = BorderPanel.Position.West
  bottomMenu.preferredSize = Dimension(WIDTH, 60)

  val topMargin = new FlowPanel:
    preferredSize = Dimension(500, 225)
    opaque = false

  val bottomMargin = new FlowPanel:
    preferredSize = Dimension(500, 225)
    opaque = false

  val options = new BoxPanel(Orientation.Vertical):
    maximumSize = Dimension(200, 50)
    minimumSize = Dimension(200, 50)
    opaque = false
    val volume = new Slider():
        labels = Map(0 -> new Label("0") ,1 -> new Label("1"), 2 -> new Label("2"))
        opaque = false
    val difficulty = new Slider():
      labels = Map(0 -> new Label("Easy"), 1 -> new Label("Moderate"), 2 -> new Label("Hard"))
      opaque = false
    val theme = new Slider():
      opaque = false
    this.contents ++= Seq(volume, difficulty, theme)


  val pauseScreen = new PauseScreen(game):
    preferredSize = Dimension(500, 500)
    layout(topMargin) = BorderPanel.Position.North
    layout(options) = BorderPanel.Position.Center
    layout(bottomMargin) = BorderPanel.Position.South

  val mainGame = new mainMap(game)
  mainGame.contents ++= Seq(pauseScreen)

  val topBar = new TopBar(game)
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
        game.isPaused = !game.isPaused
      case MouseMoved(_, point, _) if !game.isPaused =>
        if point.getY > topBar.size.getHeight && point.getY < (this.size.getHeight - bottomMenu.size.getHeight) then
          PickedUpTower.cursorOnGame = true
          PickedUpTower.location = formGridCoordinates(point)
        else
          PickedUpTower.cursorOnGame = false
      case MouseClicked(component, point, _, _, _) if !game.isPaused =>
        if PickedUpTower.tower.isDefined then
          val location = formGridCoordinates(point)

          game.gridMap.get(location) match
            case Some(direction) if !game.getTowerLocations.contains(location) =>
              if direction == Direction.Placable && !PickedUpTower.isCollector then
                PickedUpTower.tower.get.move(location)
                game.addTower(PickedUpTower.tower.get)
              else if direction == Direction.Forest && PickedUpTower.isCollector then
                PickedUpTower.tower.get.move(location)
                game.addTower(PickedUpTower.tower.get)
            case _ => ()
          PickedUpTower.tower = None
          PickedUpTower.cursorOnGame = false


      case buttonClicked: ButtonClicked =>
        buttonClicked.source.name match
          case "cannon" =>
            PickedUpTower.tower = Some(new Cannon( game, 1, GridPos(10, 4), Direction.North, 10))
            PickedUpTower.isCollector = false
          case "bomber" =>
            PickedUpTower.tower = Some(new Bomber( game, 1, GridPos(10, 4), Direction.North, 25))
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

  val listener = new ActionListener():
      def actionPerformed(e: java.awt.event.ActionEvent) =
        if !game.isPaused then
          game.advance()
          topBar.repaint()
          mainGame.repaint()
          bottomMenu.repaint()
          pauseScreen.visible = false
        else
          pauseScreen.visible =  true
          pauseScreen.repaint()
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