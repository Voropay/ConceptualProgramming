package org.concepualprogramming.core.utils

/**
 * Created by oleksii.voropai on 8/28/2016.
 */
object Utils {
  def compareList[A](list1: List[A], list2: List[A]): Boolean = {
    if (list1 == list2) {
      return true
    }
    if(list1.size != list2.size) {
      return false
    }
    for(entry <- list1) {
      val found = list2.find(_.equals(entry))
      if(found.isEmpty) {
        return false
      }
    }
    return true
  }

  def compareMap[A,B](map1: Map[A,B], map2: Map[A, B]): Boolean = {
    if (map1 == map2) {
      return true
    }
    if(map1.size != map2.size) {
      return false
    }
    for(entry <- map1) {
      if(!map2.contains(entry._1) || !map2.get(entry._1).get.equals(entry._2)) {
        return false
      }
    }
    return true
  }

}
