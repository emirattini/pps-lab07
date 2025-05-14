package ex4.tictactoe

import ex4.connect.ConnectGame
import ex4.GameUtils.*

object TicTacToe extends App:
  val size = 3
  val bound = size - 1

  import ex4.connect.ConnectGame.{placeAnyDisk as _, computeAnyGame as _, *}

  def possibleMoves(board: Board): Seq[Point] =
    for
      col <- 0 to bound
      row <- 0 to bound
      if !board.map(d => (d.x, d.y)).contains((col, row))
    yield (col, row)

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    possibleMoves(board).map(move => board :+ Disk(move, player))

  def computeAnyGame(player: Player, moves: Int): LazyList[(Game, Win)] = moves match
    case 1 => LazyList.from(placeAnyDisk(Seq.empty[Disk], player)
      .map(initialBoard => (Seq(initialBoard), false)))
    case _ =>
      for
        game <- computeAnyGame(player.other, moves - 1)
        newBoard <- placeAnyDisk(game._1.last, player)
      yield
        if isAWin(game._1.last) then (game._1, true)
        else (game._1 :+ newBoard, isAWin(newBoard))

  val consecutiveForWin = 3

  import ex4.GameUtils.{Mask, generateWinningMasks}

  val winningMasks: Set[Mask] = generateWinningMasks(size, consecutiveForWin).toSet

  import Player.*
  def isAWin(board: Board): Boolean =
    val xDisks = board.filter(d => d.player == X).map(d => (d.x, d.y)).toSet
    val oDisks = board.filter(d => d.player == O).map(d => (d.x, d.y)).toSet
    winningMasks.map(mask => xDisks & mask).exists(s => s.size == consecutiveForWin)
      || winningMasks.map(mask => oDisks & mask).exists(s => s.size == consecutiveForWin)

  def printBoards(game: Seq[Board]): Unit =
    ConnectGame.printBoards(game, bound = bound)

  import scala.util.Random
  case class RandomAI(player: Player) extends AI:
    def move(board: Board): Disk =
      val disk = Disk(Random.nextInt(size), Random.nextInt(size), player)
      if !board.contains(disk) then disk else move(board)

  case class SmartAI(player: Player) extends AI:
    def move(board: Board): Disk =
      val (move, rating) = getBestMove(board, player, possibleMoves, winningMasks)
      if rating == consecutiveForWin
      then Disk(move, player)
      else getBestMove(board, player.other, possibleMoves, winningMasks) match
        case (counterMove, rating) if rating == consecutiveForWin => Disk(counterMove, player)
        case _ => Disk(move, player)

  @main def runTicTacToe(): Unit =
    val games = computeAnyGame(O, 9)
    games.zipWithIndex.collect({
      case p if p._1._2 => p
    }).foreach { (g, i) =>
      println(s"Game: $i Won: ${g._2}")
      printBoards(g._1.reverse)
      println()
    }
    val wonMap = games.groupMapReduce(p => p._2)(p => 1)(_ + _)
    println(s"Total: ${games.size}, Won: $wonMap")