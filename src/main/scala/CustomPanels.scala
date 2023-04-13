import java.awt.{Color, Dimension}
import java.io.File
import javax.imageio.ImageIO
import scala.swing.{BorderPanel, FlowPanel, Font, Graphics2D, Panel}

class TopBar(game: Game) extends Panel:
  override def paintComponent(g: Graphics2D): Unit =
    val levelProgress = game.LoadLevel.enemiesDeployed.toDouble/game.LoadLevel.totalEnemies
    val levelBanner = ImageIO.read(new File("assets/level_banner.png"))
    val enemy = ImageIO.read(new File("assets/enemy_south.png"))
    g.drawImage( levelBanner, 0, 0, WIDTH, 100, null)
    g.setPaint(Color.black)
    g.fillRect(10, 30, WIDTH - 20, 10)
    g.setPaint(Color.green)
    g.fillRect(10, 30, 10 + ((WIDTH - 50) * levelProgress).toInt, 10)
    g.drawImage( enemy, ((WIDTH - 50) * levelProgress).toInt, 20, 50, 50, null)
end TopBar


class PauseScreen(var text: String, margins: Vector[Double]) extends BorderPanel:
  val topMargin = new FlowPanel:
    preferredSize = Dimension(PAUSE_SCREEN_WIDTH, (PAUSE_SCREEN_HEIGHT * margins(0)/100).toInt)
    opaque = false
  val bottomMargin = new FlowPanel:
    preferredSize = Dimension(PAUSE_SCREEN_WIDTH, (PAUSE_SCREEN_HEIGHT * margins(1)/100).toInt)
    opaque = false
  val eastMargin = new FlowPanel:
    preferredSize = Dimension((PAUSE_SCREEN_WIDTH * margins(2)/100).toInt, PAUSE_SCREEN_HEIGHT)
    opaque = false
  val westMargin = new FlowPanel:
    preferredSize = Dimension((PAUSE_SCREEN_WIDTH * margins(3)/100).toInt, PAUSE_SCREEN_HEIGHT)
    opaque = false

  layout(topMargin) = BorderPanel.Position.North
  layout(bottomMargin)= BorderPanel.Position.South
  layout(eastMargin)= BorderPanel.Position.East
  layout(westMargin)= BorderPanel.Position.West
  override def paintComponent(g: Graphics2D): Unit =
    g.setPaint(Color.black)
    g.setFont(new Font("TimesRoman", 3, 35))
    g.fillRect(0, 0, 500, 500)
    g.setColor(Color.white)
    g.drawString(text, 175 - text.length, 50)
end PauseScreen


class mainMap(game: Game) extends FlowPanel:


  val heightOfSquare = (HEIGHT - 200).toDouble/ROWS
  val widthOfSquare = WIDTH.toDouble/COLS

  private def drawTopEntities(g: Graphics2D, imagePath: String, location: GridPos, direction: Tile) =
    val xOff = (location.x * widthOfSquare + widthOfSquare/4).toInt
    val yOff = (location.y * heightOfSquare + heightOfSquare/4).toInt
    val finalImagePath = imagePath.dropRight(4) + "_" +
      (direction match
        case Tile.North => "north"
        case Tile.South => "south"
        case Tile.East  => "east"
        case Tile.West  => "west") + ".png"
    val image = ImageIO.read(new File(finalImagePath))
    g.drawImage(image, xOff, yOff, widthOfSquare.toInt/2, heightOfSquare.toInt/2, null)

  private def drawMap(g: Graphics2D): Unit =
    val imageMap = Map(
      0 -> ImageIO.read(new File("assets/tree.png")),
      1 -> ImageIO.read(new File("assets/placable.png")),
      2 -> ImageIO.read(new File("assets/path.jpg"))
    )
    for i <- 0 until ROWS do
      for j <- 0 until COLS do
        var img = imageMap(game.worldMap(i)(j).split(",")(0).toInt)
        if (game.gridMap(GridPos(j, i)) == Tile.Forest && game.getTowerLocations.keys.toSeq.contains(GridPos(j,i))) then
          img = imageMap(1)
        g.drawImage(img, (j * widthOfSquare).toInt, (i * heightOfSquare).toInt, widthOfSquare.toInt, heightOfSquare.toInt, null)

  end drawMap

  private def drawEnemies(g: Graphics2D): Unit =
    for i <- BigDecimal(0.0) to BigDecimal(COLS.toDouble) by GRID_STEP do
      for j <- BigDecimal(0.0) to BigDecimal(ROWS.toDouble) by GRID_STEP do
        val pos = GridPos(i.toDouble, j.toDouble)
        val enemies = game.getEnemyLocations
        if enemies.keys.toVector.contains(pos) then
          this.drawTopEntities(g, enemies(pos).image, pos, enemies(pos).moveDirection)
    end for
  end drawEnemies


  private def drawTowers(g: Graphics2D): Unit =
    val towers = game.getTowerLocations
    for (location, tower) <- towers do
      this.drawTopEntities(g, tower.image, location, tower.directionFacing)
    end for
  end drawTowers

  private def drawProjectiles(g: Graphics2D): Unit =
    for projectile <- game.projectiles do
      this.drawTopEntities(g, projectile.image, projectile.location, projectile.moveDirection)
  end drawProjectiles

  private def drawSelectedTower(g: Graphics2D): Unit =
    PickedUpTower.tower match
      case Some(tower) =>
        this.drawTopEntities(g, tower.image, PickedUpTower.location, tower.directionFacing)
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
    val totalResources = game.totalResources.toString
    g.drawImage(coins, 5, 15, 35, 35, null)
    g.setColor(Color.yellow)
    g.setFont(new Font("TimesRoman", 3, 18))
    g.drawString(totalResources, 60, 35)

end BottomPanel