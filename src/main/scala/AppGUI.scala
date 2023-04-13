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
import scala.io.Source

object AppGUI extends SimpleSwingApplication:
  var startingResources = 75
  var enemeisPassing = 3
  var speed = "slow"
  var level = 1
  val towerButtons = Map[String, String]("cannon" -> CANNON_IMAGE_PATH, "bomber" -> BOMBER_IMAGE_PATH, "collector" -> COLLECTOR_IMAGE_PATH, "archer" -> ARCHER_IAMGE_PATH)
  var gameLevel = Game(level, 1)
  val buttons = this.createButtonsPanel(towerButtons)
  var nextLevelStarted = false
  var gameStarted = false

  var gameSpeedCounter = 0

  // Load user settings from the settings file
  def setUserSettings(): Unit =
    val reader = Source.fromFile(SETTINGS)
    for line <- reader.getLines() do
      val property = line.split(":")(0)
      val value = line.split(":")(1)
      property match
        case "enemiesPassing"             => enemeisPassing = value.toInt
        case "speed"                      => speed = value
        case "startingResources"          => startingResources = value.toInt

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
  val margin1 = new FlowPanel:
      preferredSize = Dimension(PAUSE_SCREEN_WIDTH, (PAUSE_SCREEN_HEIGHT * 0.05).toInt)
      opaque = false
  val margin2 = new FlowPanel:
    preferredSize = Dimension(PAUSE_SCREEN_WIDTH, (PAUSE_SCREEN_HEIGHT * 0.05).toInt)
    opaque = false
  val gameOverOptions = new BoxPanel(Orientation.Vertical):
    opaque = false
    contents ++= Seq(restart, margin1, next, margin2, exit)
    opaque = false
  end gameOverOptions

  //Level complete screen
  val gameOver = new PauseScreen("Game Over", Vector(20, 20, 30, 30)):
    preferredSize = Dimension(500, 500)
    layout(gameOverOptions) = BorderPanel.Position.Center
    visible = false

  // starts a level
  private def startNewLevel(level: Int): Unit =
    if level <= TOTAL_LEVELS then
      gameLevel = new Game(level, enemeisPassing)
      gameLevel.totalResources = startingResources
      topBar = createTopMenu
      mainGame = createMainGame
      bottomMenu = createBottomMenu(buttons.toVector)
      root = newRoot
      gameOver.visible = false
      nextLevelStarted = true
      gameWindow.contents = root

  // form grid coordinates relative to the main game screen
  private def formGridCoordinates(point: Point): GridPos =
    val x = point.getX
    val y = point.getY
    val relativeX = x
    val relativeY = y - topBar.size.getHeight
    GridPos(math.floor(relativeX/root.size.getWidth * COLS), math.floor(relativeY/mainGame.size.getHeight * ROWS))
  end formGridCoordinates

  // The bottom buttons panel
  private def createButtonsPanel(towerButtons: Map[String, String]) =
    val buttons = Buffer[BoxPanel]()
    for (buttonName, imagePath) <- towerButtons do
      val image = ImageIO.read(new File(imagePath.dropRight(4) + "_north.png"))
      val label = new Label("Cost: " + PRICES_MAP(buttonName))
      val button = new Button("") {
        this.name = buttonName
        icon = new ImageIcon(image.getScaledInstance(50, 50, 2))
        preferredSize = new Dimension(icon.getIconWidth, icon.getIconHeight)
        focusable = false
      }
      val combined = new BoxPanel(Orientation.Vertical):
        contents ++= Seq(label, button)
      buttons += combined
    buttons.toSeq
  end createButtonsPanel

  private def createTopMenu =
    new TopBar(gameLevel):
      preferredSize = Dimension(WIDTH, 100)

  private def createBottomMenu(buttons: Vector[BoxPanel]) =
    val combinedButtonPanel = new FlowPanel
    combinedButtonPanel.contents ++= buttons
    val bottomLeft = new BottomPanel(gameLevel):
      preferredSize = new Dimension(100, 60)
    val bottomMenu = new BorderPanel:
      layout(combinedButtonPanel) = BorderPanel.Position.East
      layout(bottomLeft) = BorderPanel.Position.West
      preferredSize = Dimension(WIDTH, 60)
    bottomMenu
  end createBottomMenu

  private def createMainGame =
    new mainMap(gameLevel):
      contents ++= Seq(gameOver)
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
    listenTo(this.keys)
    listenTo(restart)
    listenTo(next)
    listenTo(exit)
    listenTo(this.mouse.moves)
    listenTo(this.mouse.clicks)
    buttons.map(_.contents(1)).foreach( listenTo(_) )
    reactions += {
      case KeyPressed(_, Key.Up, _, _) if PickedUpTower.tower.isDefined =>
        PickedUpTower.tower.get.directionFacing = Tile.North
      case KeyPressed(_, Key.Down, _, _) if PickedUpTower.tower.isDefined =>
          PickedUpTower.tower.get.directionFacing = Tile.South
      case KeyPressed(_, Key.Left, _, _) if PickedUpTower.tower.isDefined =>
          PickedUpTower.tower.get.directionFacing = Tile.West
      case KeyPressed(_, Key.Right, _, _) if PickedUpTower.tower.isDefined =>
          PickedUpTower.tower.get.directionFacing = Tile.East
      case KeyPressed(_, Key.Key1, _, _) if !gameLevel.isOver && gameLevel.totalResources >= PRICES_MAP("cannon") =>
            PickedUpTower.tower = Some(new Cannon( gameLevel, GridPos(10, 4), Tile.North, 10))
            PickedUpTower.isCollector = false
            PickedUpTower.cost = PRICES_MAP("cannon")
      case KeyPressed(_, Key.Key4, _, _) if !gameLevel.isOver && gameLevel.totalResources >= PRICES_MAP("archer") =>
        PickedUpTower.tower = Some(new Archer( gameLevel, GridPos(10, 4), Tile.North, 10))
        PickedUpTower.cost = PRICES_MAP("archer")
        PickedUpTower.isCollector = false
      case KeyPressed(_, Key.Key3, _, _) if !gameLevel.isOver && gameLevel.totalResources >= PRICES_MAP("bomber") =>
        PickedUpTower.tower = Some(new Bomber( gameLevel, GridPos(10, 4), Tile.North, 25))
        PickedUpTower.isCollector = false
        PickedUpTower.cost = PRICES_MAP("bomber")
      case KeyPressed(_, Key.Key2, _, _) if !gameLevel.isOver && gameLevel.totalResources >= PRICES_MAP("collector") =>
        PickedUpTower.tower = Some(new Collector(gameLevel, GridPos(10, 4), 15, Tile.East))
        PickedUpTower.isCollector = true
        PickedUpTower.cost = PRICES_MAP("collector")

      case MouseMoved(_, point, _) if !gameLevel.isOver =>
        if point.getY > topBar.size.getHeight && point.getY < (this.size.getHeight - bottomMenu.size.getHeight) then
          PickedUpTower.cursorOnGame = true
          PickedUpTower.location = formGridCoordinates(point)
        else
          PickedUpTower.cursorOnGame = false

      case MouseClicked(component, point, _, _, _) if!gameLevel.isOver =>
        if PickedUpTower.tower.isDefined then
          val location = formGridCoordinates(point)
          gameLevel.gridMap.get(location) match
            case Some(direction) if !gameLevel.getTowerLocations.contains(location) =>
              if direction == Tile.Placable && !PickedUpTower.isCollector then
                PickedUpTower.tower.get.move(location)
                gameLevel.addTower(PickedUpTower.tower.get)
                gameLevel.totalResources -= PickedUpTower.cost
              else if direction == Tile.Forest && PickedUpTower.isCollector then
                PickedUpTower.tower.get.move(location)
                gameLevel.addTower(PickedUpTower.tower.get)
                gameLevel.totalResources -= PickedUpTower.cost
            case _ => ()
          PickedUpTower.tower = None
          PickedUpTower.cursorOnGame = false

      case buttonClicked: ButtonClicked =>
        buttonClicked.source.name match
          case "cannon" if !gameLevel.isOver && gameLevel.totalResources >= PRICES_MAP("cannon") =>
            PickedUpTower.tower = Some(new Cannon( gameLevel, GridPos(10, 4), Tile.North, 10))
            PickedUpTower.isCollector = false
            PickedUpTower.cost = PRICES_MAP("cannon")
          case "archer" if !gameLevel.isOver && gameLevel.totalResources >= PRICES_MAP("archer") =>
            PickedUpTower.tower = Some(new Archer( gameLevel, GridPos(10, 4), Tile.North, 10))
            PickedUpTower.cost = PRICES_MAP("archer")
            PickedUpTower.isCollector = false
          case "bomber" if !gameLevel.isOver && gameLevel.totalResources >= PRICES_MAP("bomber") =>
            PickedUpTower.tower = Some(new Bomber( gameLevel, GridPos(10, 4), Tile.North, 25))
            PickedUpTower.isCollector = false
            PickedUpTower.cost = PRICES_MAP("bomber")
          case "collector" if !gameLevel.isOver && gameLevel.totalResources >= PRICES_MAP("collector") =>
            PickedUpTower.tower = Some(new Collector(gameLevel, GridPos(10, 4), 15, Tile.East))
            PickedUpTower.isCollector = true
            PickedUpTower.cost = PRICES_MAP("collector")
          case "restart" =>
            startNewLevel(level)
          case "next" =>
            level += 1
            startNewLevel(level)
          case "exit" =>
            System.exit(0)
          case _ => ()
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
        root.requestFocus()
        def advanceState() =
          gameLevel.advance()
          topBar.repaint()
          mainGame.repaint()
          bottomMenu.repaint()
        if gameLevel.isOver then
          if gameLevel.gameLost then
              gameOver.text = "Level failed."
          else
            gameOver.text = "Level Completed."
          gameOver.repaint()
          if level == TOTAL_LEVELS || gameLevel.gameLost then
            next.visible = false
            margin2.visible = false
          else
            next.visible = true
            margin2.visible = true
          gameOver.visible = true
        else if nextLevelStarted then
          root.repaint()
          nextLevelStarted = false
        else if gameStarted then
          if speed == "slow" then
            gameSpeedCounter += 1
            if gameSpeedCounter == 2 then
              gameSpeedCounter = 0
            else
              advanceState()
          else if speed == "normal" then
            advanceState()
          else if speed == "fast" then
            advanceState()
            advanceState()

  end listener

  val timer = new javax.swing.Timer(200, listener)
  timer.start()

end AppGUI


object PickedUpTower:
  var tower: Option[Tower] = None
  var cost = 0
  var cursorOnGame = false
  def toBeDrawn = cursorOnGame && this.tower.isDefined
  var location = GridPos(0, 0)
  var isCollector = false
end PickedUpTower

val testMap = Buffer.fill(ROWS)(Buffer.fill(COLS)("0"))
