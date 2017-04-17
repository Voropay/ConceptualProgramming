package org.conceptualprogramming.core.statements

import org.conceptualprogramming.core.RunPreferences
import org.conceptualprogramming.core.datatypes.composite.{CPObjectValue, CPMap}
import org.conceptualprogramming.core.statements.expressions.functions.ConsoleFunctions
import org.conceptualprogramming.parser.{StatementsParser, ProgramParser}
import org.concepualprogramming.core.statements.CPStatement
import org.concepualprogramming.core.{CPDecisionNode, CPObject, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.statements.expressions.functions.{ObjectsFunctions, GroupingFunctions}

import scala.collection.mutable

/**
 * Created by oleksii.voropai on 3/26/2017.
 */
class ProgramExecutor {
  def execute(programText: String, preferences: RunPreferences): String = {
    val programCode = ProgramParser(programText)
    if(programCode.isEmpty) {
println("parsing failed")
      return ""
    }
    val context = initContext
    val resolveType = preferences.getResolveType
    val res = resolveType match {
      case RunPreferences.RECURSIVE_RESOLVE_TYPE => programCode.get.execute(context); context.getValueResult
      case RunPreferences.DECISION_TREE_RESOLVE_TYPE => resolveDecisionTree(programCode.get, context); context.getValueResult
      case _ => None
    }
    if(res.isEmpty || res.get.getStringValue.isEmpty) {
      return ""
    } else {
      return res.get.getStringValue.get
    }
  }

  def initContext(): CPExecutionContext = {
    val context = new CPExecutionContext
    GroupingFunctions.register(context)
    ConsoleFunctions.register(context)
    ObjectsFunctions.register(context)
    CPList.register(context)
    CPMap.register(context)
    CPObjectValue.register(context)
    context
  }

  def resolveDecisionTree(statement: CPStatement, context: CPExecutionContext): Unit = {
    var currentNode: CPDecisionNode = statement.createDecisionNode(context)
    currentNode.init()
    val stack = mutable.Stack[CPDecisionNode]()
    while(currentNode.hasNextBranch || !stack.isEmpty) {
      if(currentNode.hasNextBranch) {
        val nextBranch = currentNode.nextBranch
        nextBranch.init()
        stack.push(currentNode)
        currentNode = nextBranch
      } else {
        if(!stack.isEmpty) {
          val prevNode = stack.pop
          prevNode.setCurrentNodeResolvingResult(currentNode.getAllResults)
          currentNode = prevNode
        }
      }
    }
  }

  def stepByStepExecute(preferences: RunPreferences) = {
    val context = initContext
    val resolveType = preferences.getResolveType
    val stmtParser = new StatementsParser {
      def apply(code: String): Option[CPStatement] = {
        parseAll(statement, code) match {
          case Success(res, _) => Some(res)
          case _ => None
        }
      }
    }

    while(!context.isStopped) {
      print(">")
      val statementStr = scala.io.StdIn.readLine
      val statementCode = stmtParser(statementStr)
      if(statementCode.isDefined) {
        if(resolveType == RunPreferences.RECURSIVE_RESOLVE_TYPE) {
          statementCode.get.execute(context)
        } else {
          resolveDecisionTree(statementCode.get, context)
        }
        //TODO: print execution results
      } else {
        //TODO: show parse error
      }
      println
    }
  }
}

object ProgramExecutor {
  def main(args: Array[String]): Unit = {
    val preferences = RunPreferences(args)
    val executor = new ProgramExecutor
    if(preferences.getSourceFile.isEmpty) {
      executor.stepByStepExecute(preferences)
    } else {
      val source = scala.io.Source.fromFile(preferences.getSourceFile)
      val sourceCode = try source.mkString finally source.close()
      val res = executor.execute(sourceCode, preferences)
      println(res)
    }
  }
}
