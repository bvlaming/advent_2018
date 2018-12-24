import java.time.LocalDate
import java.time.format.DateTimeFormatter

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source


case class Guard(id: Int)

case class Point(coords: (Int, Int))
case class Velocity(vel: (Int, Int))

case class Node(nrChildren: Int, nrMetadata: Int, label: Int)
case class Tag(tag: String) // header, metadata

case class Elf(id: Int)

abstract class stateOfPlay {
  val marbles: Array[Int]
  val currentMarble: Int
  val score: Map[Elf, Int]
}

object advent_2018 {

  def main(args: Array[String]): Unit = {
    doDay11()
  }


  def doDay11(): Unit = {
    val path11 = "/Users/basvlaming/Documents/input_11.csv"
    val testpath11 = "/Users/basvlaming/Documents/input_11_test.csv"

    val data: List[String] = io.Source.fromFile(path11).getLines().toList


  }

  //// DAY 10 /////

  def doDay10(): Unit = {
    val path10 = "/Users/basvlaming/Documents/input_10.csv"
    val testpath10 = "/Users/basvlaming/Documents/input_10_test.csv"

    val data: List[String] = io.Source.fromFile(path10).getLines().toList

    val parsedData = data.map(parseDay10)

    val max_horizons = for (t <- Range(0, 100000) )
    yield {(positions(t, parsedData), maxHorizontalRow(positions(t, parsedData)), t)}

    val message: (List[Point], Int, Int) = max_horizons.maxBy(_._2)

    println(message._3)

    println(box_message(message._1))

    visualise2(message._1)

  }


  def visualise2(message: List[Point]): Unit = {
    // take the message; print each
    val box = box_message(message)
    val x0 = box._1 - 3
    val xl = box._2 - box._1 + 10
    val points = message.map(p => p.coords)
    val points_by_y: Map[Int, List[Int]] = points.groupBy(_._2).map{case (y, lxy) => (y, lxy.map(_._1))}
    val all_y = Range(box._3 - 2, box._4 + 2)
    val outputToVis = all_y.toList.map(y => (y, points_by_y.getOrElse(y, List[Int]())))
    val output = outputToVis.map{case (y, lx) => (y, visualiseX(lx, x0, xl))}
    output.foreach{case (y, lx) => println(lx.mkString)}
  }

  def visualiseX(input: List[Int], x0: Int, length: Int): ArrayBuffer[Char] = {
    // make an array of length Int with e.g. '.'; make X for all the x's of the input
    val arr: ArrayBuffer[Char] = ArrayBuffer.fill(length)('.')
    for (x <- input) {
      arr(x - x0) = 'O'
    }
    arr
  }

  def box_message(message: List[Point]): (Int, Int, Int, Int) = {
    val xs = message.map(_.coords._1)
    val ys = message.map(_.coords._2)
    (xs.min, xs.max, ys.min, ys.max)
  }


  def bool(c: Char): Boolean = {
    c.isDigit || c == '-'
  }

  def maxHorizontalRow(points: List[Point]): Int = {
      points.map(p => p.coords._1).groupBy(identity).map(_._2.length).max
  }

  def positions(time: Int, initial: List[(Point, Int, Int)]): List[Point] = {
    initial.map { case (p, vx, vy) => Point((p.coords._1 + time * vx, p.coords._2 + time * vy))
    }
  }

  def parseDay10(row: String): (Point, Int, Int) = {
    val row2 = row.replace("> velocity=<", ",")
    val splitRow = row2.split(",").map(_.filter(p => bool(p)).toInt)
    (Point(splitRow(0), splitRow(1)), splitRow(2), splitRow(3))
  }



  def doDay9(): Unit = {

    val (nr_players, max_marble_value) = (477, 70851)
//    val (nr_players, max_marble_value) = (21, 6111)

    val initState = ArrayBuffer(0, 2, 1, 3)

    val results = updateState(initState, 3, 4, List(), nr_players, max_marble_value)

    val maxScore = results.groupBy(_._1).map{case (elf, scores) => (elf, scores.map(_._2).sum)}.maxBy(_._2)
    println(maxScore)
  }


  def insertAt[T](arr: ArrayBuffer[T], pos: Int, newVal: T): ArrayBuffer[T] = {
    arr.insert(pos, newVal)
    arr
  }

  def removedAt[T](arr: ArrayBuffer[T], pos: Int): (ArrayBuffer[T], T) = {
    val el = arr(pos)
    arr.remove(pos)
    (arr, el)
  }

  @tailrec
def updateState(marbles: ArrayBuffer[Int],
                indexMarble: Int,
                nextMarble: Int,
                score: List[(Elf, Long)],
                nrPlayers: Int,
                maxSteps: Int): List[(Elf, Long)] = {
  if (nextMarble == maxSteps) {
    score
  } else {
    if (nextMarble % 23 == 0) {
      val elfPlaying = Elf(nextMarble % nrPlayers)
      val marbleTakenAt = if (indexMarble < 7) indexMarble - 7 + marbles.length else indexMarble - 7
      val (newMarbles, marbleTaken) = removedAt(marbles, marbleTakenAt)
      val newScore = (elfPlaying, (nextMarble + marbleTaken).toLong) :: score
      updateState(newMarbles, marbleTakenAt, nextMarble + 1, newScore, nrPlayers, maxSteps)
    }
    else
    {
      if (nextMarble % 1000 == 0) {println(nextMarble)}
//      println(marbles)
      val placeAt = if (indexMarble > (marbles.length - 2)) indexMarble + 2 - marbles.length else indexMarble + 2
      val newMarbles = insertAt(marbles, placeAt, nextMarble)
      updateState(newMarbles, placeAt, nextMarble + 1, score, nrPlayers, maxSteps)
    }
  }
}


  def doDay8(): Unit = {
    val path8 = "/Users/basvlaming/Documents/input_08.csv"

    val data: List[String] = io.Source.fromFile(path8).getLines().toList

    val parsedData = data.flatMap(s => s.split(" ").map(_.toInt))

    val testTree: List[Int] = List(2,3 ,0 ,3 ,10, 11 ,12 ,1, 1, 0 ,1 ,99 ,2, 1, 1 ,2)

//    val taggedData = splitIntoNodes(parsedData)
    val taggedData = splitIntoNodes(testTree)
    taggedData.foreach(println)
    val answer = taggedData.filter(_._4 == Tag("metadata")).map(_._1).sum

    println(answer)

    // for part 2, it's just a matter of tracking the child nodes and their values. Can't be bothered for now, will
    // get back to it. Maybe.
  }

  def splitIntoNodes(x: List[Int]): List[(Int, Int, Node, Tag)] = {
    val firstNode = Node(x.head, x.tail.head, 1)

    val idx: List[(Int, Int)] = x.zipWithIndex // (value, index)

    val labelsSoFar = List((idx.head._1, idx.head._2, firstNode, Tag("header")),
      (idx.tail.head._1, idx.tail.head._2, firstNode, Tag("header"))).reverse
    // node: always list them with childNodesLeft, metadataLeft
    @tailrec
    def tagPoints(currentNode: (Node, Int, Int),
                  unfinishedNodes: List[(Node, Int, Int)],
                  pointsToDo: List[(Int, Int)],
                  nodeCounter: Int,
                  pointsLabeled: List[(Int, Int, Node, Tag)]): List[(Int, Int, Node, Tag)] = {
      // pointsLabeled: value, index, node it's part of, tag (=header or metadata)
      if (pointsToDo.isEmpty) pointsLabeled.reverse
      else {
        // OPTIONS
        // header complete, and we need to create a new node;
        if (currentNode._2 > 0) {
          // create a new node; take the first two next points as header
          // tag these points, and add them to the accumulator
          // reset the various variables
          val node = Node(pointsToDo.head._1, pointsToDo.tail.head._1, nodeCounter + 1)
//          println(s"newly created node: $node")
//          println(pointsToDo)
          val updateUnfinished = (currentNode._1, currentNode._2 - 1, currentNode._3)
          val newUnfinished = updateUnfinished :: unfinishedNodes
          val newPointsToDo = pointsToDo.drop(2)
          val newLabels = List((pointsToDo.head._1, pointsToDo.head._2, node, Tag("header")),
            (pointsToDo.tail.head._1, pointsToDo.tail.head._2, node, Tag("header"))).reverse
          tagPoints((node, node.nrChildren, node.nrMetadata), newUnfinished, newPointsToDo, nodeCounter + 1,
            newLabels ::: pointsLabeled)
        } else { //  now, we don't have to create a new child node
          //      if (currentNode._3 > 0)
          // option 1: metadata left --> tag these
          //      {
          val pointsToTag = pointsToDo.take(currentNode._3)
          val taggedPoints = pointsToTag.map { case (n, index) => (n, index, currentNode._1, Tag("metadata")) }
          val newPointsToDo = pointsToDo.drop(currentNode._3)
          if (unfinishedNodes.isEmpty) {
            println(s"tagged points $taggedPoints")
            println(s"points still to do $newPointsToDo")
            println(s"current node $currentNode")
            println(s"pointsLabeled $pointsLabeled")
          }

          val newCurrentNode = if (unfinishedNodes.nonEmpty) unfinishedNodes.head else currentNode
          val newUnfinishedNodes = if (unfinishedNodes.nonEmpty) unfinishedNodes.tail else List()
          //          println(newCurrentNode)
          val newPointsLabeled = taggedPoints.reverse ::: pointsLabeled
          tagPoints(newCurrentNode, newUnfinishedNodes, newPointsToDo, nodeCounter, newPointsLabeled)
          //      }
        }
        // option 2: no metadata left --> continue with the previous unfinished node
      }
    }

    tagPoints((firstNode, firstNode.nrChildren, firstNode.nrMetadata),
      List[(Node, Int, Int)](), idx.drop(2), 1, labelsSoFar)
  }





  def doDay7(): Unit = {
    val path7 = "c:/Users/basvl/Documents/advent/input_07.csv"

    val data: List[String] = io.Source.fromFile(path7).getLines().toList

    val parsedData = data.map(row => parseLine7(row))

    val prereqMap: Map[Char, Set[Char]] = parsedData.groupBy(_._2).map{case (c, l) => (c, l.map(_._1).toSet)}

    //    val answer3 = goThroughPrereqs(prereqMap, List[Char]()).mkString
    ///
    //    println(answer3)
    val charList = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val taskDuration: Map[Char, Int] = charList.zipWithIndex.map{case (ch, idx) => (ch, idx + 59)}.toMap

    val answer = goThroughPrereqsWithTime(prereqMap, List[Char](), List[(Char, Int)](), 0, taskDuration)
    println(answer)
  }

  //  @tailrec
  def goThroughPrereqsWithTime(prereqMap: Map[Char, Set[Char]],
                               instructionsCompleted: List[Char], tasksInProgress: List[(Char, Int)],
                               time: Int, durationMap: Map[Char, Int]): (List[Char], Int) = {
    // For each time step: check possible tasks that can be started, and check if we have free workers
    // Update progress; if a task is completed, add it to the completed collection, and remove it from prereqs
    // The way it is constructed somehow leads to a off-by-one error if not careful, that's why the task duration has
    // 59 seconds instead of 60

    val charList = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toList
    val charSet = charList.toSet
    if (charSet.diff(instructionsCompleted.toSet).isEmpty) {
      val unaccountedInstructions = charList.filter(p => !instructionsCompleted.contains(p))
      ((unaccountedInstructions.sorted ::: instructionsCompleted).reverse, time)
    }
    else {
      val newTime = time + 1
      val tasksNewlyCompleted = tasksInProgress.filter(_._2 == 0).map(_._1)
      val updatedCurrentTasks = tasksInProgress.filter(_._2 > 0).map{case (c, dur) => (c, dur - 1)}

      val newInstructionsCompleted = tasksNewlyCompleted ::: instructionsCompleted

      // simply a set of all completed tasks; these can be removed from the prereq map
      val completeTaskSet = newInstructionsCompleted.toSet

      val tasksProgress = tasksInProgress.map(_._1).toSet
      // all chars in prereqMap have uncompleted prerequisites; take all characters that are not in there. Then, clean
      // the prereqmap of the latest instructions
      val possibleInstructions = charList.filter(p => (!newInstructionsCompleted.contains(p)) &
        (!prereqMap.keySet.contains(p)) & (!tasksProgress.contains(p)))

      val freeWorkers = 5 - tasksInProgress.count(_._2 > 0)
      val startJobNr = List(possibleInstructions.length, freeWorkers).min
      val newJobsStarted: List[(Char, Int)] = possibleInstructions.sorted.take(startJobNr).map(c => (c, durationMap(c)))

      val newTasksInProgress = newJobsStarted ::: updatedCurrentTasks
      println(s"current time $time")
      println(s"Workers active: ${newTasksInProgress.length}")
      println(newTasksInProgress)
      println(newInstructionsCompleted)

      val filteredPrereqMap: Map[Char, Set[Char]] = prereqMap.filterKeys(p => !completeTaskSet.contains(p)).map{
        case (c, st) => (c, st -- completeTaskSet)}.filter(_._2.nonEmpty)

      goThroughPrereqsWithTime(filteredPrereqMap, newInstructionsCompleted, newTasksInProgress, newTime, durationMap)
    }
  }


  @tailrec
  def goThroughPrereqs(prereqMap: Map[Char, Set[Char]], instructionsCompleted: List[Char]): List[Char] = {
    // For each instruction, we now what the uncompleted prerequisites are (prereqMap). Find the alphabetically first
    // doable instruction; complete it, and remove it from the uncompleted prerequisites Map. Repeat until all have
    // been completed.
    val charList = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toList

    if (prereqMap.isEmpty) {
      val unaccountedInstructions = charList.filter(p => !instructionsCompleted.contains(p))
      println(s"We haven't performed $unaccountedInstructions")
      (unaccountedInstructions.sorted ::: instructionsCompleted).reverse
    }
    else {
      val keysWithPrereqs =prereqMap.keys
      // all chars in prereqMap have uncompleted prerequisites; take all characters that are not in there. Then, clean
      // the prereqmap of the latest instructions
      val possibleInstructions = charList.filter(p => (!instructionsCompleted.contains(p)) & (!prereqMap.keySet.contains(p)))

      val nextInstruction = possibleInstructions.min

      val filteredPrereqMap = prereqMap.filter(_._1 != nextInstruction).map{case (c, st) => (c, st - nextInstruction)}.filter(_._2.nonEmpty)
      goThroughPrereqs(filteredPrereqMap, nextInstruction :: instructionsCompleted)
    }
  }

  def parseLine7(row: String): (Char, Char) = {
    val filteredRow = row.filter(_.isUpper)
    (filteredRow.tail.head, filteredRow.last)
  }

  /// DAY 6 ///

  def doDay6(): Unit = {
    val path6 = "c:/Users/basvl/Documents/advent/input_06.csv"

    val data: List[String] = io.Source.fromFile(path6).getLines().toList

    val coords = data.map(s => parseCoord(s))

    println(coords.take(3))

    val grid1 = (for (x <- Range(1,500);
                      y <- Range(1,500)) yield Point(x, y)).toList

    val grid2 = (for (x <- Range(0,501);
                      y <- Range(0,501)) yield Point(x, y)).toList

    // all points in grid: check closest point, save that. List(Point, Point)
    val answer = findSolutionDay6_1(grid1, grid2, coords)
    println(answer)

    val answer2 = findSolutionDay6_2(grid1, coords, 10000)

    println(answer2)
    // scan through grid; for each point in grid, sum the manhattan distances to the refpoints
  }

  def findSolutionDay6_2(grid: List[Point], refPoints: List[Point], maxDist: Int): Int = {
    // find how many points in the grid have a total summed distance to the refPoints that is less than maxDist
    val pointsDists = grid.map(p => (p, findTotalDist(p, refPoints)))

    pointsDists.count(_._2 < maxDist)
  }

  def findTotalDist(point: Point, refPoints: List[Point]): Int = {
    /// for a point, find the sum over all distances to all refPoints
    refPoints.map(q => r(point, q)).sum
  }

  def findSolutionDay6_1(grid1: List[Point], grid2: List[Point], refPoints: List[Point]): (Point, Int, Int) = {
    // Simply calculate all distances and pick the closest point for each (if it exists). Do this over a grid. Then,
    // expand the grid in all directions by 1; all finite sized areas will remain constant in size, the others will
    // not; pick the largest finite one.
    val voronoi1: List[(Point, Int)] = voronoiSize(grid1, refPoints)
    val voronoi2: Map[Point, Int] = voronoiSize(grid2, refPoints).toMap

    val bothSizes: List[(Point, Int, Int)] = voronoi1.map{case (p, ct) => (p, ct, voronoi2(p))}

    val finites = bothSizes.filter(x => x._2 == x._3)

    finites.maxBy(_._2)
  }

  def voronoiSize(grid: List[Point], ptc: List[Point]): List[(Point, Int)] = {
    // find how many points in the grid are closest to each point in ptc
    findClosestPoints(grid, ptc).groupBy(_._2).map{case (p, l) => (p, l.length)}.toList
  }

  def findClosestPoints(grid: List[Point], pointsToCheck: List[Point]): List[(Point, Point)] = {
    // for each point in grid, find the point in pointsToCheck that is closest (unique)
    grid.map(p => (p, findPoint(p, pointsToCheck)))
  }

  def findPoint(p: Point, pointsToCheck: List[Point]): Point = {
    // find closest point in pointsToCheck; return Point(0,0) if no unique
    val pointsAndDistances: List[(Point, Int)] = pointsToCheck.map(q => (q, r(q, p)))
    val minDist = pointsAndDistances.minBy(_._2)
    val mult = pointsAndDistances.count(_._2 == minDist._2)
    if (mult == 1) minDist._1
    else Point(0, 0)
  }

  def parseCoord(row: String): Point = {
    val splitString = row.split(",").map(_.trim.toInt)
    Point((splitString.head, splitString.last))
  }

  def manhattan(p1: (Int, Int), p2: (Int, Int)): Int = {
    math.abs(p1._1 - p2._1) + math.abs(p1._2 - p2._2)
  }

  def r(p1: Point, p2: Point): Int = {
    manhattan(p1.coords, p2.coords)
  }

  /// DAY 5 ///

  def doDay5(): Unit = {
    val path5 = "c:/Users/basvl/Documents/advent/input_05.csv"

    val data: List[String] = io.Source.fromFile(path5).getLines().toList

    val data5 = data.head

    val testString = "abdcFeEfg"
    //    println(day5(testString))
    val testString2 = "BxXbD"
    //    println(day5(testString2))

    val answer = day5(data5)
    println(s"Length of code ${answer.length}")
    println(s"Answer is $answer")

    val lengths = day5_2(data5)

    val answer5_2 = lengths.minBy(_._2)

    println(answer5_2)
  }

  def day5(code: String): String = {

    @tailrec
    def looper(left: List[Char], right: List[Char]): List[Char] = {
      if (right.isEmpty) left
      else {
        if (left.isEmpty) {
          looper(List(right.head), right.tail)
        }
        else {
          val currChar = left.last
          val nextChar = right.head
          if (annihilate(currChar, nextChar)) looper(left.init, right.tail)
          else looper((nextChar :: left.reverse).reverse, right.tail)

        }
      }
    }

    looper(List(code.head), code.tail.toList).mkString
  }

  def day5_2(code: String): List[(Char, Int)] = {

    val allChars = "abcdefghijklmnopqrstuvwxyz"

    val filteredStrings: Seq[(Char, String)] = for (c <- allChars)
      yield (c, code.filter(p => (p != c) & (p != c.toUpper)))

    filteredStrings.map{case (c, s) => (c, day5(s).length)}.toList
  }


  def annihilate(x: Char, y: Char): Boolean = {
    (x != y) & (x.toLower == y.toLower)
  }

  /// DAY 4 ///

  def doDay4(): Unit = {


    val path: String = "/Users/basvlaming/Documents/input_04.csv"
    val inputData = Source.fromFile(path).getLines.toList
    val format = new java.text.SimpleDateFormat("yyyy-MM-dd")
    //    val result = day1(inputData)
    //    val result2 = dayd1_2(inputData)
    //    println(result2)
    //    val test: String = "test"
    //    val result = groupChar(test)
    //    println(result)
    //    val result = day2_2(inputData)
    //    val readableResult = result.map(_._1).mkString
    //    println(readableResult

    val testString = inputData.head
    println(parseRow(testString).toString())
    val orderedLog: List[(LocalDate, Option[Guard], Int, Char)] = inputData.map(row => parseRow(row))

    val groupedLog: Map[LocalDate, List[(LocalDate, Option[Guard], Int, Char)]] = orderedLog.groupBy(_._1)

    val listSleeps: List[(Guard, Int, List[Int])] = groupedLog.toList.flatMap{case (dt, data) => parseData(data)}

    val totalSleep: List[(Guard, Int)] = listSleeps.groupBy(_._1).map{case (g, ls) => (g, ls.map(_._2).sum)}.toList

    val minuteMostSlept: Map[Guard, Int] = listSleeps.groupBy(_._1).map{
      case (g, ls) => (g, findMostFrequentMinute(ls))}
    val orderedSleep = totalSleep.sortBy(-_._2)
    println(orderedSleep.take(4))

    val guard = orderedSleep.head._1

    val minuteSlept = minuteMostSlept(guard)

    println(guard)
    println(minuteSlept)

    val answer = guard.id * minuteSlept

    println(answer)

    val minuteMostSleptAndFreq: Map[Guard, (Int, Int)] = listSleeps.groupBy(_._1).map{
      case (g, ls) => (g, findMostFrequentMinuteAndFrequency(ls))
    }


    val answer2 = minuteMostSleptAndFreq.toList.minBy(-_._2._2)

    println(answer2)
    println(answer2._1.id * answer2._2._1)
  }

  def findMostFrequentMinuteAndFrequency(ls: List[(Guard, Int, List[Int])]): (Int, Int) = {
    if (ls.isEmpty || ls.flatMap(_._3).isEmpty) (-1, 0)
    else {
      val allMinutes = ls.flatMap(_._3)

      allMinutes.groupBy(identity).toList.sortBy(-_._2.length).map{case (m, ls) => (m, ls.length)}.head
    }
  }

  def findMostFrequentMinute(ls: List[(Guard, Int, List[Int])]): Int = {
    if (ls.isEmpty || ls.flatMap(_._3).isEmpty) -1
    else {
      val allMinutes = ls.flatMap(_._3)

      allMinutes.groupBy(identity).toList.minBy(-_._2.length)._1
    }
  }

  def parseData(contents: List[(LocalDate, Option[Guard], Int, Char)]): List[(Guard, Int, List[Int])] = {
    // We can have multiple guards

    val orderedContents: List[(LocalDate, Option[Guard], Int, Char)] = contents.sortBy(_._3)


    def addGuard(currentGuard: Guard,
                 acc: List[(LocalDate, Option[Guard], Int, Char)],
                 listToDo: List[(LocalDate, Option[Guard], Int, Char)]): List[(LocalDate, Option[Guard], Int, Char)] = {
      if (listToDo.isEmpty) acc
      else {
        val row = listToDo.head
        if (row._4 == 'G') addGuard(row._2.get, row :: acc, listToDo.tail)
        else {
          val newRow = (row._1, Some(currentGuard), row._3, row._4)
          addGuard(currentGuard, newRow :: acc, listToDo.tail)
        }
      }
    }

    val enriched = addGuard(orderedContents.head._2.get, List(), orderedContents)

    val byGuard = enriched.groupBy(_._2).map{case (og, ls) => (og.get, ls)}.toList

    byGuard.map{case (g, ls) => parseGuard(ls)}
  }

  def parseGuard(data: List[(LocalDate, Option[Guard], Int, Char)]): (Guard, Int, List[Int]) = {
    val guard = data.flatMap(_._2).head
    val actSet = Set('w', 'f')
    val activities = data.filter(x => actSet.contains(x._4)).map(x => (x._3, x._4)).sortBy(_._1)
    val sleeps = activities.filter(x => x._2 == 'f').map(_._1)
    val wakes = activities.filter(x => x._2 == 'w').map(_._1)

    val timeSlept = sleeps.zip(wakes).map { case (s, w) => w - s }.sum
    val minutesSlept: List[Int] = sleeps.zip(wakes).flatMap{case (s, w) => Range(s, w).toList}
    (guard, timeSlept, minutesSlept)
  }

  def parseRow(row: String): (LocalDate, Option[Guard], Int, Char) = {
    val format = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val cleanedRow = row.replace("[", "")
    val parts = cleanedRow.split("]")
    val dt = parts.head
    val text = parts.last

    val splitDt = dt.split(" ")
    val shift = splitDt.last.take(2).toInt > 1

    val datumRaw: LocalDate = LocalDate.parse(dt.take(10), format)
    val datum = if (shift) {datumRaw.plusDays(1)} else datumRaw
    val minute: Int = if (shift) -1 else dt.takeRight(2).toInt


    val guardNr = text.filter(_.isDigit)
    val guard = if (guardNr.nonEmpty) Some(Guard(guardNr.toInt)) else None

    val action = text.strip().head

    (datum, guard, minute, action)
  }


  /// DAY 3 ///

  def day3(input: List[String]): Int = {
    // for each patch: add the filled fields to a list
    // eventually just groupBy and count which fields occur more than once
    val parsedPatch: Seq[Array[Int]] = input.map(parser)
    val allPatches: Seq[(Int, Int)] = parsedPatch.flatMap(fillPatch)

    val fieldsWithFreq: List[((Int, Int), Int)] = allPatches.groupBy(identity).mapValues(_.size).toList

    fieldsWithFreq.count(_._2 > 1)
  }

  def day3_2(input: List[String]): String = {
    // find patch Id that has no overlap
    val parsedPatches = input.map(parser)
    val overlappingFields = fieldsWithOverlap(parsedPatches)
    val x = parsedPatches.map(p => patchHasNoOverlap(p, overlappingFields)).filter(_._2)
    println("Number of solutions: " + x.length)
    x.head._1.toString
  }

  def patchHasNoOverlap(patch: Array[Int], overlappingFields: Set[(Int, Int)]): (Int, Boolean) = {
    val patchFill = fillPatch(patch)
    val hasOverlap = patchFill.toSet.intersect(overlappingFields).isEmpty
    (patch.head, hasOverlap)
  }

  def fieldsWithOverlap(parsedPatch: Seq[Array[Int]]): Set[(Int, Int)] = {
    //    val parsedPatch: Seq[Array[Int]] = input.map(parser)
    val allPatches: Seq[(Int, Int)] = parsedPatch.flatMap(fillPatch)

    val fieldsWithFreq: List[((Int, Int), Int)] = allPatches.groupBy(identity).mapValues(_.size).toList

    fieldsWithFreq.filter(_._2 > 1).map(_._1).toSet
  }

  def parser(code: String): Array[Int] = {
    // Separate the string by # @ , : x
    // result: patch_id, x_0, y_0, x_length, y_length
    val result = code.split(Array('#','@',',', ':', 'x', ' ')).filter(x => x!="")

    result.map(_.toInt)
  }

  def fillPatch(patch: Array[Int]): List[(Int, Int)] = {
    val patchId = patch.head
    val x0 = patch(1)
    val y0 = patch(2)
    val xn = patch(3)
    val yn = patch(4)
    val xrange = Range(x0, x0 + xn)
    val yrange = Range(y0, y0 + yn)

    val patchList = for {
      x <- xrange
      y <- yrange
    } yield (x, y)
    patchList.toList
  }

  /// DAY 2 ///

  def day2_2(input: List[String]): List[(Char, Int)] = {
    // all words -> map to List[(Char, Int)] with the second the position
    // go through all word combinations, take the set of the two appended Lists, require that
    // set size is len(list) + 1
    val wordTransform: List[List[(Char, Int)]] = input.map(w => w.zipWithIndex.toList)

    val combinations: List[(List[(Char, Int)], List[(Char, Int)])] = for {
      word1 <- wordTransform
      word2 <- wordTransform
    }
      yield (word1, word2)

    val sols = combinations.filter{case (word1, word2) => (word1 ::: word2).toSet.size == word1.length + 1}
    println(f"number of solutions (should be 2): " + sols.length)
    // The 21 in the next line is hardcoded; simply inspect the result of the next line and look what index is
    // repeated. Can't be arsed to detect this properly but it's easy enough.
    sols.map(w => (w._1 ::: w._2).toSet).head.toList.filter(_._2 != 21).sortBy(_._2)
  }

  def day2(input: List[String]): Int = {
    // for each word: group by character and count double, triple occurrences
    // add that word to a set of double/triple occurrence words
    // mulitply the two set sizes
    val doubles = input.count(word => {
      findTuplesTriples(groupChar(word))._1
    })
    val triples = input.count(word => {
      findTuplesTriples(groupChar(word))._2
    })

    doubles * triples
  }

  def findTuplesTriples(groups: List[(Char, Int)]): (Boolean, Boolean) = {
    val double = groups.exists(p => p._2 == 2)
    val triple = groups.exists(p => p._2 == 3)

    (double, triple)
  }

  def groupChar(input: String): List[(Char, Int)] = {
    // This is really just a groupby --> we can use that directly
    @tailrec
    def addCharToList(c: Char,
                      listChecked: List[(Char, Int)],
                      listToCheck: List[(Char, Int)]): List[(Char, Int)] = {
      if (listToCheck.isEmpty) (c, 1) :: listChecked
      else if (c == listToCheck.head._1) {
        (c, listToCheck.head._2 + 1) :: listChecked ::: listToCheck.tail
      }
      else {
        val newListChecked = listToCheck.head :: listChecked
        val newListToCheck = listToCheck.tail
        addCharToList(c, newListChecked, newListToCheck)
      }
    }

    @tailrec
    def goThroughWord(wordLeft: String, acc: List[(Char, Int)]): List[(Char, Int)] = {
      if (wordLeft.isEmpty) acc
      else {
        goThroughWord(wordLeft.tail, addCharToList(wordLeft.head, List(), acc))
      }
    }
    //    input.foldLeft(List[(Char, Int)](begin)){case (acc, el) => addCharToList(el, List(), acc)}
    goThroughWord(input, List[(Char, Int)]())
  }


  /// DAY 1 ///

  def day1(input: List[String]): Int = {
    val listNumbers: List[Int] = input.map { n: String => n.toInt }
    listNumbers.sum
  }

  def day1_2(input: List[String]): Int = {
    val listNumbers: List[Int] = input.map { n: String => n.toInt }

    // make cumsum function; list of cumsums; fold through

    val listWithIndex: List[(Int, Int)] = listIndex(listNumbers)
    // cumsum
    val z: List[Int] = listWithIndex.map { case (a, b) => cumsum(a, listNumbers) }


    @tailrec
    def findDuplicate(k: Int, ls: List[Int], acc: Set[Int], fullList: List[Int]): Int = {
      if (acc.contains(k)) k
      else if (ls.isEmpty) {
        // repeat for the list
        println("Go through the list again")


        val numbersList: List[Int] = fullList ::: listNumbers
        val lsNew: List[Int] = listIndex(numbersList).map { case (a, _) => cumsum(a, numbersList) }
        findDuplicate(lsNew.head, lsNew.tail, Set[Int](), numbersList)
      }
      else {
        val newSet = acc + k
        val newVal = ls.head
        val newList = ls.tail
        findDuplicate(newVal, newList, newSet, fullList)
      }
    }

    findDuplicate(z.head, z.tail, Set[Int](), listNumbers)
  }

  def cumsum(n: Int, data: List[Int]): Int = {
    data.take(n).sum
  }

  // This is really zipWithIndex reimplemented. D'oh.
  def listIndex[T](ls: List[T]): List[(Int, T)] = {
    // I'm sure there's some built in functionality for indices, but this works
    ls.tail.foldLeft(List[(Int, T)]((1, ls.head))) { (acc, el) =>
      (acc.head._1 + 1, el) :: acc
    }.reverse
  }


}
