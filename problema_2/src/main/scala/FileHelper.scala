import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

class FileHelper(fileId: String) {
  private val path = "src/main/files/"
  private val inputFilename = path + "input/fitxer_entrada_" + fileId + ".txt"
  private val outputFilename = path + "output/fitxer_sortida_" + fileId + ".txt"

  def readFile(): List[Test] = {
    val bufferedSource = Source.fromFile(inputFilename)
    val linesIterator = bufferedSource.getLines()

    val numOfTests = linesIterator.next().toInt

    var tests: List[Test] = List()

    for (_ <- 0 until numOfTests) {
      val numOfDegrees = linesIterator.next().toInt

      val fields = linesIterator.next().split(" ")
      var degrees: List[Int] = List()
      for (field <- fields) {
        val degree = field match {
          case "ENG" => 0
          case "ADE" => 1
          case "ARQ" => 2
          case "ANI" => 3
          case _ => -1
        }

        degrees = degree :: degrees
      }

      val test = new Test(degrees)

      tests = test :: tests
    }

    bufferedSource.close()

    tests
  }

  def writeFile(solutions: List[Int]): Unit = {
    val outputFile = new File(outputFilename)
    val bw = new BufferedWriter(new FileWriter(outputFile))

    for (i <- solutions.indices) {
      val testNumber = i + 1

      bw.write(s"Test #$testNumber: ${solutions(i)}")
      bw.newLine()
    }

    bw.close()
  }
}
