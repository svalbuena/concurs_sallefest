object MainDificil {
  def main(args: Array[String]): Unit = {
    val fileId = "3d"
    val fileHelper = new FileHelper(fileId)

    val tests = fileHelper.readFile()
    var solutions: List[Int] = List()

    for (test <- tests) {
      solutions = solve(test.waiters, test.position) :: solutions
      println("Test -> " + solutions.head)
    }

    fileHelper.writeFile(solutions.reverse)
  }

  def solve(waiters: List[Int], position: Int): Int = {
    val currentSeconds = new Array[Int](waiters.length)
    var curPosition = position

    if (position <= waiters.length) position
    else {
      val newPosition = position - waiters.length
      -1
    }
  }
}
