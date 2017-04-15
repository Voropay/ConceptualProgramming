package org.concepualprogramming.core.datatypes.composite

import org.conceptualprogramming.core.datatypes.composite.{CPObjectValue, CPMap}
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 12/30/2016.
 */
abstract trait CPCompositeType extends CPValue {
  def getListValues: Option[CPList]
  def getMapValues: Option[CPMap]
  def add(value: CPValue): Option[CPCompositeType]
  def add(value: CPValue, position: CPValue): Option[CPCompositeType]
  def get(position: CPValue): Option[CPValue]
}
