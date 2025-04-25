package ex4

import scala.annotation.{tailrec, targetName}

object GameUtils:
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

  type Point = (Int, Int)

  object Disk:
    def apply(p: Point, player: Player): Disk = Disk(p._1, p._2, player)

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
    def apply(x: Int, y: Int): Point = (x, y)
    @targetName("sum")
    def +(p2: Point): Point = (p1._1 + p2._1, p1._2 + p2._2)
    def inBound(bound: Int): Boolean = p1._1 >= 0 && p1._2 >= 0 && p1._1 <= bound && p1._2 <= bound

  def generateMask(starting: Point, direction: Direction, bound: Int, numberOfPoints: Int): Option[Mask] =
    @tailrec
    def iter(point: Point, pointsLeft: Int, acc: Mask): Option[Mask] = point + direction match
      case _ if pointsLeft == 0 => Some(acc)
      case next if !next.inBound(bound) => None
      case next => iter(next, pointsLeft - 1, acc + next)
    iter(starting, numberOfPoints - 1, Set(starting))

  import Player.*
  def getWinner(board: Board, winningMasks: Set[Mask], consecutiveForWin: Int): Option[Player] =
    if winningMasks.map(mask => getPlayerPoints(board, X) & mask).exists(s => s.size == consecutiveForWin)
    then Some(X)
    else if winningMasks.map(mask => getPlayerPoints(board, O) & mask).exists(s => s.size == consecutiveForWin)
      then Some(O)
      else None

  private type Rating = Int
  def getBestMove(board: Board, player: Player, possibleMoves: Board => Seq[Point],
                  winningMasks: Set[Mask]): (Point, Rating) =
    val points = getPlayerPoints(board, player)
    possibleMoves(board)
      .map(move => (move, bestWinningMaskMatch(points + move, winningMasks)))
      .maxBy((p, rating) => rating)

  private def getPlayerPoints(b: Board, p: Player): Set[Point] =
    b.collect({ case d if d.player == p => (d.x, d.y) }).toSet

  private def bestWinningMaskMatch(points: Set[Point], masks: Set[Mask]): Rating =
    masks.map(mask => (points & mask).size).max

@main def testGameUtils(): Unit =
  import GameUtils.*
  import Direction.East

  println(generateMask((0, 0), East, bound = 2, 3)) //Some(Set((0,0), (1,0), (2,0)))
  println(generateMask((2, 2), East, bound = 2, 3)) //None cause out of bound
  println(generateWinningMasks(gridSize = 3, numberOfPoints = 3).toSet)

