object Main {
  def main(args: Array[String]): Unit = {
    val fileId = "2d"
    val fileHelper = new FileHelper(fileId)

    val tests = fileHelper.readFile()
    var solutions: List[Int] = List()

    for (test <- tests) {
      solutions = solve(test.degrees) :: solutions
    }

    fileHelper.writeFile(solutions)
  }

  def solve(degrees: List[Int]): Int = {
    val degreesAppearances = new Array[Int](4)

    for (degree <- degrees) {
      degreesAppearances(degree) += 1
    }

    val numOfGroups = degreesAppearances.min

    numOfGroups
  }
}
