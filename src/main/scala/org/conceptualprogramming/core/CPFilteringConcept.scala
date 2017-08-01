package org.conceptualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core._
import org.concepualprogramming.core.dependencies.CPDependency

/**
  * Created by oleksii.voropai on 8/1/2017.
  */
case class CPFilteringConcept(name: String, childConcept: Tuple2[String, String], attributesDependencies: List[CPDependency]) extends CPAbstractConcept with CPConcept{
  override val childConcepts: List[(String, String)] = List(childConcept)

  override def inferValues(query: CPSubstitutions, context: CPExecutionContext): Option[Map[CPAttributeName, CPValue]] = inferValuesFromDependencies(query, attributesDependencies, context)

  override def prepareObjectFromAttributesValues(attributesValues: CPSubstitutions): Option[CPObject] = {
    attributesValues.objects.get(childConcept._2)
  }
}
