package easy

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
      val numOfProgrammers = linesIterator.next().toInt

      val programmers = linesIterator.next().split(" ").map(_.toInt)

      val test = new Test(programmers)

      tests = test :: tests
    }

    bufferedSource.close()

    tests.reverse
  }

  def writeFile(solutions: List[(Int, Int)]): Unit = {
    val outputFile = new File(outputFilename)
    val bw = new BufferedWriter(new FileWriter(outputFile))

    for (i <- solutions.indices) {
      val testNumber = i + 1

      bw.write(s"Test #$testNumber: ${solutions(i)._1} ${solutions(i)._2} ")
      bw.newLine()
    }

    bw.close()
  }
}