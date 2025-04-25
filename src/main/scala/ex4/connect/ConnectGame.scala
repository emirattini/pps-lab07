package ex4.connect

import scala.annotation.{tailrec, targetName}
import scala.util.Random

// Optional!
object ConnectGame extends App:
  val size = 4
  val bound = size - 1

  import ex4.GameUtils.{Player, Point, Disk, Board}
  def find(board: Board, x: Int, y: Int): Option[Player] =
    board.find(d => d.x == x && d.y == y).map(d => d.player)

  def firstAvailableRow(board: Board, col: Int): Option[Int] =
    given ordering: Ordering[Int] = (x, y) => x - y

    board.filter(d => d.x == col).map(d => d.y).maxOption match
      case Some(row) if row < bound => Some(row + 1)
      case None if col <= bound => Some(0)
      case _ => None

  def possibleMoves(board: Board): Seq[Point] =
    for
      col <- 0 to bound
      row <- firstAvailableRow(board, col)
    yield (col, row)

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    possibleMoves(board).map(point => board :+ Disk(point, player))

  type Game = Seq[Board]
  type Win = Boolean

  def computeAnyGame(player: Player, moves: Int): LazyList[(Game, Win)] = moves match
    case 1 => LazyList.from(placeAnyDisk(Seq.empty[Disk], player)
      .map(initialBoard => (Seq(initialBoard), false)))
    case _ =>
      for
        game <- computeAnyGame(player.other, moves - 1)
        newBoard <- placeAnyDisk(game._1.last, player)
      yield
        if !isAWin(game._1.last) then
          if !isAWin(newBoard) then (game._1 :+ newBoard, false)
          else (game._1 :+ newBoard, true)
        else (game._1, true)

  import ex4.GameUtils.{Mask, generateWinningMasks, getWinner}
  val consecutiveForWin = 3
  val winningMasks: Set[Mask] = generateWinningMasks(size, consecutiveForWin).toSet

  def isAWin(board: Board): Boolean =
    getWinner(board, winningMasks, consecutiveForWin).isDefined

  trait AI:
    def move(board: Board): Disk
    def player: Player

  case class RandomAI(player: Player) extends AI:
    def move(board: Board): Disk =
      val col = Random().nextInt(size)
      firstAvailableRow(board, col) match
        case Some(row) => Disk(col, row, player)
        case _ => move(board)

  case class SmartAI(player: Player) extends AI:
    import ex4.GameUtils.getBestMove
    def move(board: Board): Disk =
      val (move, rating) = getBestMove(board, player, possibleMoves, winningMasks)
      if rating == consecutiveForWin
      then Disk(move, player)
      else getBestMove(board, player.other, possibleMoves, winningMasks) match
        case (counterMove, 3) => Disk(counterMove, player)
        case _ => Disk(move, player)

  def printBoards(game: Seq[Board], bound: Int = bound): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  def printBoard(board: Board, bound: Int = bound): Unit =
    for
      y <- bound to 0 by -1
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then print("\n")

  @main def run(): Unit =
    // Exercise 1: implement find such that..
    println("EX 1: ")

    import Player.*
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
    println(winningMasks)
    println(isAWin(Seq(Disk(3, 0, O), Disk(2, 0, X), Disk(3, 1, O), Disk(1, 0, X), Disk(3, 2, O)))) //true
    val games = computeAnyGame(O, 8)
    games.zipWithIndex.collect({
      case p if true => p
    }).foreach { (g, i) =>
      println(s"Game: $i Won: ${g._2}")
      printBoards(g._1.reverse)
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
