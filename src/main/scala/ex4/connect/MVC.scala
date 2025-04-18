package ex4.connect

import ex4.connect.ConnectGame.Player.O
import ex4.connect.ConnectGame.{AI, Board, Disk, Player, RandomAI, firstAvailableRow, isAWin, printBoard, printBoards}

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.sys.exit

@tailrec
def askUserDisk(board: Board, player: Player): Disk =
  val col = readLine("Enter column (1-4): ").toInt - 1
  firstAvailableRow(board, col) match
    case Some(row) => Disk(col, row, player)
    case None => print("Column full or out of bound. "); askUserDisk(board, player)

@tailrec
def gameLoop(board: Board, player: Player, ai: Option[AI]): Unit =
  val next = nextMove(board, player, ai)
  gameLoop(next, player.other, ai)

def nextMove(board: Board, player: Player, ai: Option[AI]): Board = ai match
  case Some(ai: RandomAI) if player != O =>
    println("Processing AI outstanding move...")
    Thread.sleep(1000)
    val move = board :+ ai.move(board)
    printBoard(move)
    if isAWin(move) then {println("Sorry, AI won..."); exit}
    move
  case _ =>
    val move = board :+ askUserDisk(board, player)
    printBoard(move)
    if isAWin(move) then {println(s"Player $player won!!"); exit}
    move

@main def runGame(): Unit =
  import ConnectGame.*
  import Player.*
  import scala.io.StdIn.readLine
  val players = readLine("Enter number of players: ").toInt
  val ai = if players == 2 then None else
    if readLine("Which AI? (Random or Smart): ").equalsIgnoreCase("random")
    then Some(RandomAI(X)) else Some(SmartAI(X))
  printBoard(Seq.empty)
  gameLoop(Seq.empty, O, ai)
