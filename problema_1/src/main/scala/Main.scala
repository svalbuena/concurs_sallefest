object Main {
  def main(args: Array[String]): Unit = {
    val fileId = "1d"
    val fileHelper = new FileHelper(fileId)

    val tests = fileHelper.readFile()
    var solutions: List[Int] = List()

    for (test <- tests) {
      solutions = solve(test.people, test.numOfXurros) :: solutions
    }

    fileHelper.writeFile(solutions)
  }

  def solve(people: Int, numOfXurros: Int): Int = {
    if (numOfXurros % people == 0 && numOfXurros % 2 == 0) numOfXurros
    else solve(people, numOfXurros + 1)
  }
}
