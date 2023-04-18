import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class MainTest extends AnyFlatSpec with Matchers:

  val game = new Game(1, 3)
  val enemy = new LandEnemy(LAND_ENEMY_IMAGE, game, 10, GridPos(0, 2), 10)
  val secondEnemy = new LandEnemy(LAND_ENEMY_IMAGE, game, 10, GridPos(0.1,2), 10)
  val tower = new Cannon(game, GridPos(0, 1), Tile.South, 10)
  game.addEnemy(enemy)
  game.addEnemy(secondEnemy)
  game.addTower(tower)

  "Bomb" should "behave correctly" in {
    val bomb = new Bomb("assets/bomb.png", 10, GridPos(0, 1.9), this.game, Tile.South, 1)
    (1 to 100).foreach(_ => secondEnemy.move())
    bomb.move()
    assert(!bomb.isActive)
    assert(enemy.isDead)
    assert(!secondEnemy.isDead)
  }
  "GridPos" should "calculate distance correctly" in {
    assert(GridPos(3,4).getDistance(GridPos(0, 0)) === 5.0)
    assert(GridPos(3,4).getDistance(GridPos(3, 4)) === 0)
    assert(GridPos(3,4).getDistance(GridPos(3, 5)) === 1.0)
    assert(GridPos(3,4).getDistance(GridPos(7, 7)) === 5.0)
  }
  "blockagesInPath" should "work correctly" in {
    assert(blockagesInPath(game, GridPos(0,2), GridPos(0, 6), Tile.East) === (Some(enemy), false))
    assert(blockagesInPath(game, GridPos(0,4), GridPos(0, 3), Tile.East) === (None, true))
    assert(blockagesInPath(game, GridPos(1,2), GridPos(2, 2), Tile.East) === (None, false))
  }
  "Enemy.move" should "behave correctly" in {
    enemy.move()
    assert(enemy.location === GridPos(0.1, 2))
    (1 to 19).foreach(_ => enemy.move())
    assert(enemy.location === GridPos(2, 2))
    enemy.move()
    assert(enemy.location === GridPos(2, 2.1))
  }
  "enemy.takeDamage()" should "behave correctly" in {
    assume(enemy.health === 10)
    enemy.takeDamage(10)
    assert(enemy.health === 0)
    assert(enemy.isDead)
    assume(secondEnemy.health === 10)
    secondEnemy.takeDamage(5)
    assert(secondEnemy.health === 5)
    assert(!secondEnemy.isDead)
    secondEnemy.takeDamage(100)
    assert(secondEnemy.health === 0)
    assert(secondEnemy.isDead)
  }
  "Tower.action" should "behave correctly" in {
    tower.action()
    assert(game.projectiles.length === 1)
    assert(game.projectiles.head.location === GridPos(0, 1.5))
  }
  "Projectile.move" should "behave correctly" in {
    tower.action()
    game.projectiles.head.move()
    assert(game.projectiles.length === 1)
    assert(game.projectiles.head.isActive)
    assert(game.projectiles.head.location === GridPos(0, 1.6))
  }
  "Projectile hit mechanism" should "behave correctly" in {
    tower.action()
    (1 to 3).foreach(_ => game.projectiles.head.move())
    assert(!game.projectiles.head.isActive)
    assert(enemy.isDead)
  }

end MainTest
