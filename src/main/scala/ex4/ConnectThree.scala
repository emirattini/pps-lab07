package ex4

import java.util.OptionalInt
import scala.annotation.{tailrec, targetName}
import scala.util.Random

// Optional!
object GameUtils:
  type Point = (Int, Int)

  enum Direction:
    case East, North, NorthEast, NorthWest

  import Direction.*
  given Conversion[Direction, Point] =
    case East => (1, 0)
    case North => (0, 1)
    case NorthEast => (1, 1)
    case NorthWest => (-1, 1)

  type Mask = Set[Point]

  def generateWinningMasks(gridSize: Int, numberOfPoints: Int): IndexedSeq[Mask] =
    for
      col <- 0 until gridSize
      row <- 0 until gridSize
      dir <- Set(East, North, NorthEast, NorthWest)
      mask <- generateMask((col, row), dir, gridSize - 1, numberOfPoints)
    yield
      mask

  extension (p1: Point)
    @targetName("sum")
    def +(p2: Point): Point = (p1._1 + p2._1, p1._2 + p2._2)
    def inBound(bound: Int): Boolean = p1._1 >= 0 && p1._2 >= 0 && p1._1 <= bound && p1._2 <= bound

  def generateMask(starting: Point, direction: Direction, bound: Int, numberOfPoints: Int): Option[Mask] =
    @tailrec
    def iter(point: Point, pointsLeft: Int, acc: Mask): Option[Mask] = point + direction match
      case next if !next.inBound(bound) => None
      case next if pointsLeft > 1 => iter(next, pointsLeft - 1, acc + next)
      case _ => Some(acc)
    iter(starting, numberOfPoints, Set(starting))

object ConnectThree extends App:
  val size = 4
  val bound = size - 1

  enum Player:
    case X, O

    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   * 0 1 2 3 <-- x
   */
  type Board = Seq[Disk]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] =
    board.find(d => d.x == x && d.y == y).map(d => d.player)

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    given ordering: Ordering[Int] = (x, y) => x - y
    board.filter(d => d.x == x).map(d => d.y).maxOption match
      case Some(x) if x < bound => Some(x + 1)
      case None => Some(0)
      case _ => None

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      col <- 0 to bound
      row <- firstAvailableRow(board, col)
    yield board :+ Disk(col, row, player)

  type Game = Seq[Board]
  type Win = Boolean
  def computeAnyGame(player: Player, moves: Int): LazyList[(Game, Win)] = moves match
    case 1 => LazyList.from(placeAnyDisk(Seq.empty[Disk], player)
      .map(initialBoard => (Seq(initialBoard), false)))
    case _ =>
      for
        game <- computeAnyGame(player.other, moves - 1)
        move <- placeAnyDisk(game._1.last, player)
      yield
        if !isAWin(game._1) then
          if !isAWin(game._1 :+ move) then (game._1 :+ move, false)
          else (game._1 :+ move, true)
        else (game._1, true)

  val consecutiveForWin = 3
  import GameUtils.{Mask, generateWinningMasks}
  val winningMasks: Set[Mask] = generateWinningMasks(size, consecutiveForWin).toSet
  def isAWin(game: Game): Boolean =
    val xDisks = game.last.filter(d => d.player == X).map(d => (d.x, d.y)).toSet
    val oDisks = game.last.filter(d => d.player == O).map(d => (d.x, d.y)).toSet
    winningMasks.map(mask => xDisks & mask).exists(s => s.size == consecutiveForWin)
      || winningMasks.map(mask => oDisks & mask).exists(s => s.size == consecutiveForWin)

  trait AI:
    def move(board: Board): Disk
    def player: Player

  case class RandomAI(player: Player) extends AI:
    def move(board: Board): Disk =
      val col = Random().nextInt(size)
      firstAvailableRow(board, col) match
        case Some(row) => Disk(col, row, player)
        case _ => move(board)

  import GameUtils.Point
  object Disk:
    def apply(p: Point, player: Player): Disk = Disk(p._1, p._2, player)
  case class SmartAI(player: Player) extends AI:
    def move(board: Board): Disk =
      val possibleMoves = for
        row <- 0 to bound
        col <- 0 to bound
        if !board.map(d => (d.x, d.y)).contains((col, row))
      yield (col, row)
      given Ordering[(Int, Int)] = (p1, p2) => p1._2 - p2._2
      Disk(possibleMoves.min, player)

  def printBoards(game: Seq[Board], b: Boolean = false, i: Int = 0): Unit =
    println(s"Game: $i Won: $b")
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  @main def run(): Unit =
    // Exercise 1: implement find such that..
    println("EX 1: ")

    println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
    println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
    println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

    // Exercise 2: implement firstAvailableRow such that..
    println("EX 2: ")
    println(firstAvailableRow(List(), 0)) // Some(0)
    println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
    println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
    println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
    println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
    // Exercise 2: implement placeAnyDisk such that..
    printBoards(placeAnyDisk(List(), X))
    // .... .... .... ....
    // .... .... .... ....
    // .... .... .... ....
    // ...X ..X. .X.. X...
    printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
    // .... .... .... ....
    // .... .... .... ....
    // ...X .... .... ....
    // ...O ..XO .X.O X..O
    println("EX 4: ")
    // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
    import GameUtils.*
    import Direction.East
    println(generateMask((0, 0), East, bound, 3)) //Some(Set((0,0), (1,0), (2,0)))
    println(generateMask((2, 2), East, bound, 3)) //None cause out of bound
    println(generateWinningMasks(gridSize = 4, numberOfPoints = 3).toSet)
    println(winningMasks)
    println(isAWin(Seq(Seq(Disk(3, 0, O), Disk(2, 0, X), Disk(3, 1, O), Disk(1, 0, X), Disk(3, 2, O))))) //true
    val games = computeAnyGame(O, 5)
    games.zipWithIndex.collect({
      case p if p._1._2 => p
    }).foreach { (g, i) =>
      printBoards(g._1.reverse, g._2, i)
      println()
    }
    val wonMap = games.groupMapReduce(p => p._2)(p => 1)(_ + _)
    println(s"Total Won: $wonMap")
    //  .... .... .... .... ...O
    //  .... .... .... ...X ...X
    //  .... .... ...O ...O ...O
    //  .... ...X ...X ...X ...X
    //
    //
    // .... .... .... .... O...
    // .... .... .... X... X...
    // .... .... O... O... O...
    // .... X... X... X... X...
    // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
    // Exercise 6 -- Implement ai
    val board = Seq(Disk(1, 0, O), Disk(2, 0, X), Disk(1, 1, O))
    println("RandomAI:")
    printBoards(Seq(board, board :+ RandomAI(X).move(board)).reverse)
    println("SmartAI:")
    val first = board :+ SmartAI(X).move(board)
    val second = first :+ SmartAI(X).move(first)
    val third = second :+ SmartAI(X).move(second)
    printBoards(Seq(first, second, third).reverse)
