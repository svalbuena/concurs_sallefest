package difficult

object MainDifficult {
  val multiplesOf: Map[BigInt, BigInt] = Map()

  def main(args: Array[String]): Unit = {
    val fileId = "3d"
    val fileHelper = new FileHelper(fileId)

    val tests = fileHelper.readFile()
    var solutions: List[BigInt] = List()

    for ((test, i) <- tests.zipWithIndex) {
      println(s"Test -> ${i + 1}")
      solutions = solve(test.waiters, test.position) :: solutions
      println("Sol -> " + solutions.head)
      println()
    }

    fileHelper.writeFile(solutions.reverse)
  }

  def solve(waiters: Array[BigInt], position: BigInt): BigInt = {
    if (position <= waiters.length) position
    else {
      val newPosition = position - BigInt.int2bigInt(waiters.length)

      val secondsLcm = calculateLcm(waiters)
      val numOfShifts: BigInt = waiters.map(s => secondsLcm / s).sum
      val myShift = ((newPosition - 1) % numOfShifts) + 1

      //if (waiters.forall(s => s == waiters.head)) return myShift
      //if (waiters.head == 1 && myShift == 1) return 1

      //if (myShift < numOfShifts && myShift >= (numOfShifts - waiters.length)) return myShift - (numOfShifts - waiters.length)

      val mySecond = findTheSecond(1, secondsLcm, myShift, waiters)

      println(myShift)

      val myRelativeShift = myShift - howManyShiftsTill(mySecond - 1, waiters)

      val myWaiter = {
        var count = 0
        for (w <- waiters.indices) {
          if (mySecond % waiters(w) == 0) {
            count += 1
            if (count == myRelativeShift) return w + 1
          }
        }

        -1
      }

      myWaiter
    }
  }

  private def findTheSecond(firstSec: BigInt, lastSec: BigInt, myShift: BigInt, waiters: Array[BigInt]): BigInt = {
    //println(s"Trying -> ($firstSec, $lastSec)")
    if (firstSec == lastSec) {
      if (isMyShiftOnThatSecond(myShift, firstSec, waiters)) firstSec
      else -1
    } else {
      val middleSec = (firstSec + lastSec) / 2

      if (myShift <= howManyShiftsTill(middleSec, waiters)) findTheSecond(firstSec, middleSec, myShift, waiters)
      else findTheSecond(middleSec + 1, lastSec, myShift, waiters)
    }
  }

  private def isMyShiftOnThatSecond(myShift: BigInt, second: BigInt, waiters: Array[BigInt]): Boolean = {
    val shiftsTillNow = howManyShiftsTill(second, waiters)
    val shiftsOfNow = howManyShiftsOn(second, waiters)

    if (myShift > (shiftsOfNow - shiftsOfNow) && myShift <= shiftsTillNow) true
    else false
  }

  private def howManyShiftsTill(second: BigInt, waiters: Array[BigInt]): BigInt = {
    var numOfShifts: BigInt = 0

    for (s <- waiters) {
      numOfShifts += second / s
    }

    numOfShifts
  }

  private def howManyShiftsOn(second: BigInt, waiters: Array[BigInt]): BigInt = {
    waiters.count(s => second % s == 0)
  }


  private def calculateLcm(ints: Array[BigInt]): BigInt = {
    var lcm: BigInt = ints.head

    for (int <- ints) {
      lcm = lcm * int / myGcd(lcm, int)
    }

    lcm
  }

  private def calculateMyWaiter(waiters: Array[BigInt], userShift: BigInt, secondsLcm: BigInt): BigInt = {
    var curUserShift = userShift
    var curSecond = 0

    while (curSecond <= secondsLcm) {
      curSecond += 1
      for (w <- waiters.indices) {
        if (curSecond % waiters(w) == 0) {
          curUserShift -= 1

          if (curUserShift == 0) return w + 1
        }
      }
    }

    -1
  }

  /*private def calculateWaitersShifts(waiters: List[BigInt], secondsLcm: BigInt): List[BigInt] = {
    var waitersShifts: List[BigInt] = List()

    for (shift <- 1 to secondsLcm) {
      for (w <- waiters.indices) {
        if (shift % waiters(w) == 0) waitersShifts = (w + 1) :: waitersShifts
      }
    }

    waitersShifts.reverse
  }*/

  private def myGcd(a: BigInt, b: BigInt): BigInt = {
    /*val r = b % a
    if (r == 0) a
    else myGcd(r, a)*/
    if (b == 0) a.abs
    else myGcd(b, a % b)
  }

  private def getMultiples(n: BigInt): List[BigInt] = {
    var i: BigInt = 0
    var multiples: List[BigInt] = List()

    while (i <= n) {
      i += 1
      if (n % i == 0) multiples = i :: multiples
    }

    multiples
  }
}
