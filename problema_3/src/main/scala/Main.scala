object Main {
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

    while (position > 0) {
      for (i <- waiters.indices) {
        if (currentSeconds(i) == 0) {
          curPosition -= 1
          currentSeconds(i) = waiters(i)
          if (curPosition == 0) return i + 1
        }
      }
      val minSec = currentSeconds.min

      for (i <- currentSeconds.indices) {
        currentSeconds(i) -= minSec
      }
    }

    -1
  }
}
