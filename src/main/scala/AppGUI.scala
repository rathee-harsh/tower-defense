import AppGUI.{buttons, level, pause}

import scala.swing.*
import javax.swing.{BorderFactory, BoxLayout, ImageIcon, JFrame, JPanel}
import java.awt.BorderLayout
import java.awt.Color
import java.awt.Graphics
import java.awt.Dimension
import javax.imageio.ImageIO
import scala.collection.mutable.Buffer
import java.io.{File, FileWriter}
import scala.swing.event.*
import java.awt.event.*
import scala.collection.mutable.*

object AppGUI extends SimpleSwingApplication:

  var level = 1
  var exitGame = false
  val towerButtons = Map[String, String]("cannon" -> CANNON_IMAGE_PATH, "bomber" -> BOMBER_IMAGE_PATH, "collector" -> COLLECTOR_IMAGE_PATH, "archer" -> ARCHER_IAMGE_PATH)
  var gameLevel = Game(level)
  val buttons = this.createButtons(towerButtons)
  var nextLevelStarted = false
  var gameStarted = false

  def startGame(): Unit =
    gameStarted = true
    gameWindow.contents = root

  val restart = new Button():
    name = "restart"
    icon = new ImageIcon(ImageIO.read(new File(RESTART_IMAGE_PATH)).getScaledInstance(50, 50, 2))
    preferredSize = new Dimension(icon.getIconWidth, icon.getIconHeight)
    focusable = false
    opaque = false

  val next = new Button():
    name = "next"
    icon = new ImageIcon(ImageIO.read(new File(NEXT_IAMGE_PATH)).getScaledInstance(50, 50, 2))
    preferredSize = new Dimension(icon.getIconWidth, icon.getIconHeight)
    focusable = false
    opaque = false

  val exit = new Button():
      name = "exit"
      icon = new ImageIcon(ImageIO.read(new File(EXIT_IMAGE_PATH)).getScaledInstance(50, 50, 2))
      preferredSize = new Dimension(icon.getIconWidth, icon.getIconHeight)
      focusable = false

  val pause = new PauseScreen("Game Paused", Vector(45, 45, 20, 20)):
    val pauseOptions = new BoxPanel(Orientation.Vertical):
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
      contents ++= Seq(volume, difficulty, theme)
    preferredSize = Dimension(PAUSE_SCREEN_WIDTH, PAUSE_SCREEN_HEIGHT)
    layout(pauseOptions) = BorderPanel.Position.Center
    visible = false

  val gameOverOptions = new BoxPanel(Orientation.Vertical):
    val margin1 = new FlowPanel:
      preferredSize = Dimension(PAUSE_SCREEN_WIDTH, (PAUSE_SCREEN_HEIGHT * 0.05).toInt)
      opaque = false
    val margin2 = new FlowPanel:
      preferredSize = Dimension(PAUSE_SCREEN_WIDTH, (PAUSE_SCREEN_HEIGHT * 0.05).toInt)
      opaque = false
    opaque = false
    contents ++= Seq(restart, margin1, next, margin2, exit)
    opaque = false
  end gameOverOptions


  val gameOver = new PauseScreen("Game Over", Vector(20, 20, 30, 30)):
    preferredSize = Dimension(500, 500)
    layout(gameOverOptions) = BorderPanel.Position.Center
    visible = false

  private def startNewLevel(level: Int): Unit =
    gameLevel = new Game(level)
    topBar = createTopMenu
    mainGame = createMainGame
    bottomMenu = createBottomMenu(buttons.toVector)
    root = newRoot
    gameOver.visible = false
    nextLevelStarted = true
    gameWindow.contents = root
    if level == TOTAL_LEVELS then
      next.visible = false

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
  end createButtons

  private def createTopMenu =
    new TopBar(gameLevel):
      preferredSize = Dimension(WIDTH, 100)

  private def createBottomMenu(buttons: Vector[Button]) =
    val buttonPanel = new FlowPanel
    buttonPanel.contents ++= buttons
    val bottomLeft = new BottomPanel(gameLevel):
      preferredSize = new Dimension(100, 60)
    val bottomMenu = new BorderPanel:
      layout(buttonPanel) = BorderPanel.Position.East
      layout(bottomLeft) = BorderPanel.Position.West
      preferredSize = Dimension(WIDTH, 60)
    bottomMenu
  end createBottomMenu

  private def createMainGame =
    new mainMap(gameLevel):
      contents ++= Seq(pause, gameOver)
  end createMainGame

  var topBar = createTopMenu
  var mainGame= createMainGame
  var bottomMenu = createBottomMenu(buttons.toVector)

  if TOTAL_LEVELS == 1 then
    next.visible = false

  private def newRoot = new BorderPanel:
    add(topBar,BorderPanel.Position.North)
    add(mainGame,BorderPanel.Position.Center)
    add(bottomMenu,BorderPanel.Position.South)
    focusable = true
    listenTo(restart)
    listenTo(next)
    listenTo(exit)
    listenTo(this.keys)
    listenTo(this.mouse.moves)
    listenTo(this.mouse.clicks)
    buttons.foreach( listenTo(_) )
    reactions += {
      case KeyPressed(_,  Key.P, _, _)  if !gameLevel.isOver=>
        gameLevel.isPaused = !gameLevel.isPaused

      case MouseMoved(_, point, _) if !gameLevel.isPaused && !gameLevel.isOver =>
        if point.getY > topBar.size.getHeight && point.getY < (this.size.getHeight - bottomMenu.size.getHeight) then
          PickedUpTower.cursorOnGame = true
          PickedUpTower.location = formGridCoordinates(point)
        else
          PickedUpTower.cursorOnGame = false

      case MouseClicked(component, point, _, _, _) if !gameLevel.isPaused && !gameLevel.isOver =>
        if PickedUpTower.tower.isDefined then
          val location = formGridCoordinates(point)
          gameLevel.gridMap.get(location) match
            case Some(direction) if !gameLevel.getTowerLocations.contains(location) =>
              if direction == Direction.Placable && !PickedUpTower.isCollector then
                PickedUpTower.tower.get.move(location)
                gameLevel.addTower(PickedUpTower.tower.get)
              else if direction == Direction.Forest && PickedUpTower.isCollector then
                PickedUpTower.tower.get.move(location)
                gameLevel.addTower(PickedUpTower.tower.get)
            case _ => ()
          PickedUpTower.tower = None
          PickedUpTower.cursorOnGame = false

      case buttonClicked: ButtonClicked if !gameLevel.isPaused =>
        buttonClicked.source.name match
          case "cannon" if !gameLevel.isOver =>
            PickedUpTower.tower = Some(new Cannon( gameLevel, 1, GridPos(10, 4), Direction.North, 10))
            PickedUpTower.isCollector = false
          case "archer" if !gameLevel.isOver =>
            PickedUpTower.tower = Some(new Archer( gameLevel, 1, GridPos(10, 4), Direction.North, 10))
            PickedUpTower.isCollector = false
          case "bomber" if !gameLevel.isOver =>
            PickedUpTower.tower = Some(new Bomber( gameLevel, 1, GridPos(10, 4), Direction.North, 25))
            PickedUpTower.isCollector = false
          case "collector" if !gameLevel.isOver =>
            PickedUpTower.tower = Some(new Collector(gameLevel, 1, GridPos(10, 4), 15))
            PickedUpTower.isCollector = true
          case "restart" =>
            startNewLevel(level)
          case "next" =>
            level += 1
            startNewLevel(level)
          case "exit" => exitGame = true
    }

  var root = newRoot

  var gameWindow = new MainFrame:
    title = "Tower Defence"
    contents = startScene
    preferredSize = new Dimension(WIDTH, HEIGHT)
    resizable = false
    pack()

  def top = this.gameWindow

  val listener = new ActionListener():
      def actionPerformed(e: java.awt.event.ActionEvent) =
        if exitGame then
          gameWindow.dispose()
        else if nextLevelStarted then
          root.repaint()
          nextLevelStarted = false
        else if gameLevel.isOver then
          gameOver.visible = true
        else if gameLevel.isPaused then
          pause.visible =  true
          pause.repaint()
        else if gameStarted then
          gameLevel.advance()
          topBar.repaint()
          mainGame.repaint()
          bottomMenu.repaint()
          pause.visible = false
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