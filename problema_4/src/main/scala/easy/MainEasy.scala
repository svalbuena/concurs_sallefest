package easy

object MainEasy {
  var bestNumOfTeams = 0
  var bestExpertise = 0

  def main(args: Array[String]): Unit = {
    val fileId = "4f"
    val fileHelper = new FileHelper(fileId)

    val tests = fileHelper.readFile()
    var solutions: List[(Int, Int)] = List()

    for ((test, i) <- tests.zipWithIndex) {
      solutions = solve(test.programmer) :: solutions
      println(s"Test #${i + 1}: -> ${solutions.head._1} ${solutions.head._2}")
    }

    fileHelper.writeFile(solutions.reverse)
  }

  def solve(programmers: Array[Int]): (Int, Int) = {
    bestNumOfTeams = 0
    bestExpertise = 0

    exec(programmers, List(), 0, List())

    (bestExpertise, bestNumOfTeams)
  }

  private def printTeams(teams: List[List[Int]]): Unit = {
    println("-- Printing --")
    if (areTeamsOk(teams)) {
      val teamsExpertise = teams.head.sum
      println(s"\t OKEY -> expertise = $teamsExpertise numOfTeams = ${teams.length}")
    } else {
      println(s"\t BAD -> numOfTeams = ${teams.length}")
    }

    for ((t, i) <- teams.zipWithIndex) {
      println("\t\t- Team " + i)
      print("\t\t")
      for (e <- t) {
        print(e + ", ")
      }
      println()
      println()
    }
  }

  private def computeResults(teams: List[List[Int]]): Unit = {
    //printTeams(teams)
    if (teams.nonEmpty && areTeamsOk(teams)) {
      val teamsExpertise = teams.head.sum

      if (teams.length == bestNumOfTeams && teamsExpertise > bestExpertise) {
        bestExpertise = teamsExpertise
      } else if (teams.length > bestNumOfTeams) {
        bestNumOfTeams = teams.length
        bestExpertise = teamsExpertise
      }
    }
  }

  private def exec(programmers: Array[Int], teams: List[List[Int]], currentIndex: Int, currentTeam: List[Int]): Unit = {
    if (currentIndex == programmers.length) {
      if (currentTeam.nonEmpty) computeResults(currentTeam :: teams)
      else computeResults(teams)
    } else {
      //Case 2, putting the current programmer on the next team, only add the current team if its not empty
      if (isNextTeamViable(teams, List(programmers(currentIndex)))) {
        if (currentTeam.nonEmpty) {
          exec(programmers, currentTeam :: teams, currentIndex + 1, List(programmers(currentIndex)))
        } else {
          exec(programmers, teams, currentIndex + 1, List(programmers(currentIndex)))
        }
      }

      //Case 1, keeping the current programmer on the current team
      if (currentTeam.nonEmpty && isNextTeamViable(teams, programmers(currentIndex)  :: currentTeam)) {
        exec(programmers, teams, currentIndex + 1, programmers(currentIndex) :: currentTeam)
      }

      //Case 3, discarding the current programmer, the creation of the current team stops, only adding the team if it is not empty
      if (currentTeam.nonEmpty) {
        exec(programmers, currentTeam :: teams, currentIndex + 1, List())
      } else {
        exec(programmers, teams, currentIndex + 1, List())
      }
    }
  }

  private def areTeamsOk(teams: List[List[Int]]): Boolean = {
    if (teams.isEmpty) false
    else {
      val expertise = teams.head.sum

      teams.forall(t => t.sum == expertise)
    }
  }

  private def isNextTeamViable(teams: List[List[Int]], nextTeam: List[Int]): Boolean = {
    if (teams.isEmpty) true
    else {
      val expertise = teams.head.sum

      nextTeam.sum <= expertise
    }
  }
}