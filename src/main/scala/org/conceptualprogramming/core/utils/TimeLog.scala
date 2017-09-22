package org.conceptualprogramming.core.utils

/**
  * Created by oleksii.voropai on 9/12/2017.
  */
object TimeLog {

  val points = scala.collection.mutable.Map.empty[Int, (String, Long, Long)]
  var curPointId = 0

  def start(pointName: String): Int = {
    curPointId = curPointId + 1
    val startTime = System.nanoTime
    points.put(curPointId, (pointName, startTime, startTime))
    curPointId
  }

  def stop(pointId: Int) = {
    val endTime = System.nanoTime
    val start = points.get(pointId)
    if(start.isDefined) {
      points.put(pointId, (start.get._1, start.get._2, endTime))
    }
  }

  def measureTime[R](pointName: String, block: => R): R = {
    val id = start(pointName)
    val result = block
    stop(id)
    result
  }

  def aggregatedResults(): String = {
    val aggregated = points.values.foldLeft(Map[String, (Long, Int)]()) {(z, f) => {
      val curStat: (Long, Int) = z.getOrElse(f._1, (0, 0))
      val newTime: Long = curStat._1 + f._3 - f._2
      val newStat = (newTime, curStat._2 + 1)
      z ++ Map(f._1 -> newStat)
    }}
    val res = aggregated.map(item => {
      item._1 + ": " + (item._2._1 / item._2._2) + " (" + item._2._2 + ")"
    }).mkString("\n")
    res
  }

}
