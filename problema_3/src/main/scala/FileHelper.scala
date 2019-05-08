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
      val firstLineFields = linesIterator.next().split(" ")
      val secondLineFields = linesIterator.next().split(" ")

      val numOfWaiters = firstLineFields(0).toInt
      val position = firstLineFields(1).toInt

      val waiters = secondLineFields.toList.map(_.toInt)

      val test = new Test(waiters, position)

      tests = test :: tests
    }

    bufferedSource.close()

    tests.reverse
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
