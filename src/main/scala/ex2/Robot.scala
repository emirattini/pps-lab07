package ex2

import scala.util.Random

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot) extends Robot:
  export robot.{position, direction}
  private var battery = 100
  override def act(): Unit =
    if (battery - 20 >= 0) then battery = battery - 20; robot.act()
  override def turn(dir: Direction): Unit =
    if (battery - 10 >= 0) then battery = battery - 10; robot.turn(dir)
  override def toString: String = s"${robot.toString} (With Battery: $battery%)"

class RobotCanFail(val robot: Robot, percentage: Int) extends Robot:
  export robot.{position, direction}
  override def act(): Unit =
    if Random().nextInt(100) > percentage then robot.act() else println("Failed")
  override def turn(dir: Direction): Unit =
    if Random().nextInt(100) > percentage then robot.turn(dir) else println("Failed")

class RobotRepeated(val robot: Robot, reps: Int) extends Robot:
  export robot.{position, direction}
  if reps < 0 then throw IllegalStateException()
  override def act(): Unit =
    for _ <- 0 to reps do robot.act()
  override def turn(dir: Direction): Unit =
    0 to reps foreach (i => robot.turn(dir))

@main def testRobot(): Unit =
  val simple = SimpleRobot((0, 0), Direction.North)
  val logging = LoggingRobot(simple)
  logging.act() // robot at (0, 1) facing North
  logging.turn(logging.direction.turnRight) // robot at (0, 1) facing East
  logging.act() // robot at (1, 1) facing East
  logging.act() // robot at (2, 1) facing East
  val battery = LoggingRobot(RobotWithBattery(simple))
  battery.act()
  battery.turn(battery.direction.turnLeft)
  battery.act()
  battery.act()
  val canFail = RobotCanFail(LoggingRobot(simple), 80)
  canFail.act()
  canFail.turn(canFail.direction.turnLeft)
  canFail.act()
  canFail.act()
  val repeated = RobotRepeated(LoggingRobot(simple), 3)
  repeated.act()
  repeated.turn(repeated.direction.turnLeft)
  repeated.act()
  repeated.act()
