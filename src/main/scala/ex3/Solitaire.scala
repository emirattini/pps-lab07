package ex3

object Solitaire extends App:
  type Cell = (Int, Int)
  type Solution = Iterable[Cell]
  type IterableFactory = Solution => Iterable[Solution]
  val width = 6
  val height = 6
  given IterableFactory = LazyList(_)

  val numbers = 36
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

  private def isReachable(to: Cell, from: Cell, width: Int, height: Int): Boolean =
    possibleMoves(from) filter((x, y) => x < width && y < height) contains to

  private def possibleMoves(from: Cell): List[Cell] =
    for
      (x, y) <- List((2, 2), (3, 0), (0, 3))
      xi <- if x == 0 then List(0) else List(x, -x)
      yi <- if y == 0 then List(0) else List(y, -y)
    yield (from._1 + xi, from._2 + yi)

  def render(solution: Solution, index: Int, width: Int, height: Int): String =
    println(s"Solution $index -> $solution")
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = solution.toList.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")
