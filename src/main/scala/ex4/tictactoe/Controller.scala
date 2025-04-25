package ex4.tictactoe

import TicTacToe.*
import ex4.GameUtils.*
import Player.*
import ex4.connect.ConnectGame.{printBoard, AI}

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.sys.exit

val size = 3
val bound = size - 1

@tailrec
def askUserDisk(board: Board, player: Player): Disk =
  val input = readLine("Enter point (x y): ").split(" ")
  val point: Point = (input(0).toInt, input(1).toInt)
  if point.inBound(bound) && !board.map(d => (d.x, d.y)).contains(point)
  then
    Disk(point, player)
  else
    println("Point not valid")
    askUserDisk(board, player)

@tailrec
def gameLoop(board: Board, player: Player, ai: Option[AI]): Unit =
  val next = nextMove(board, player, ai)
  if next.size == size * size
  then { println("It's a draw!"); exit }
  gameLoop(next, player.other, ai)

def nextMove(board: Board, player: Player, ai: Option[AI]): Board = ai match
  case Some(ai) if player != O =>
    println("Processing AI outstanding move...")
    Thread.sleep(1000)
    val move = board :+ ai.move(board)
    printBoard(move, bound)
    if isAWin(move) then {
      println("Sorry, AI won..."); exit
    }
    move
  case _ =>
    val move = board :+ askUserDisk(board, player)
    printBoard(move, bound)
    if isAWin(move) then {
      println(s"Player $player won!!"); exit
    }
    move

@main def runGame(): Unit =
  val players = readLine("Enter number of players: ").toInt
  val ai = if players == 2 then None else
    if readLine("Which AI? (r -> Random, s -> Smart): ").equalsIgnoreCase("r")
    then Some(RandomAI(X)) else Some(SmartAI(X))
  printBoard(Seq.empty, bound)
  gameLoop(Seq.empty, O, ai)