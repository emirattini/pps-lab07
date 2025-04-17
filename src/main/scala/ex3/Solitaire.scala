package ex3

object Solitaire extends App:
  type Cell = (Int, Int)
  type Solution = Iterable[Cell]
  type IterableFactory = Solution => Iterable[Solution]
  val width = 6
  val height = 6
  given IterableFactory = LazyList(_)

  val numbers = 4
  @main def run(): Unit = placeNumbers(numbers)
    .zipWithIndex
    .foreach((sol, i) => println(render(sol, i, width, height)))

  def placeNumbers(n: Int, width: Int = width, height: Int = height)(using factory: IterableFactory): Iterable[Solution] = n match
    case 1 => factory(List((width / 2, height / 2)))
    case _ =>
      for
        numbers <- placeNumbers(n - 1)
        x <- 0 to width
        y <- 0 to height
        number = (x, y)
        if !numbers.toList.contains(number)
          && isReachable(number, numbers.last, width, height)
      yield numbers.toSeq :+ number

  def isReachable(to: Cell, from: Cell, width: Int, height: Int): Boolean =
    straightMoves(from) concat obliqueMoves(from) filter((x, y) => x < width && y < height) contains to

  private def obliqueMoves(from: Cell): List[Cell] =
    for
      x <- List(2, -2)
      y <- List(2, -2)
    yield (from._1 + x, from._2 + y)

  private def straightMoves(from: Cell): List[Cell] =
    for
      x <- List(0, 3, -3)
      y <- List(0, 3, -3)
      if x == 0 || y == 0 && !(x == 0 && y == 0)
    yield (from._1 + x, from._2 + y)

  def render(solution: Solution, index: Int, width: Int, height: Int): String =
    println(s"Solution $index -> $solution")
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = solution.toList.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")
