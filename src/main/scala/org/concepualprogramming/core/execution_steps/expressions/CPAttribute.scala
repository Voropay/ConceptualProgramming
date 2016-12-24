package org.concepualprogramming.core.execution_steps.expressions

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}

/**
 * Created by oleksii.voropai on 12/22/2016.
 */
class CPAttribute(attrName: CPAttributeName) extends CPExpression {
  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val subst = context.getSubstitutions
    if(subst.isEmpty) {
      return None
    }
    subst.get.attributesValues.get(attrName)
  }
}
