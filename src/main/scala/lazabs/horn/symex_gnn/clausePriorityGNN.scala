package lazabs.horn.symex_gnn

import lazabs.GlobalParameters
import lazabs.horn.bottomup.NormClause
import lazabs.horn.symex.UnitClause
import scala.collection.mutable.{Queue => MQueue}
import java.io.{File, PrintWriter}
import play.api.libs.json.{JsSuccess, JsValue, Json}

object HornGraphType extends Enumeration {
  val CDHG, CG = Value
}

object clausePriorityGNN {


  def prioritizeQueue(choiceQueue: MQueue[(NormClause, Seq[UnitClause])],normClauseToScore: Map[NormClause, Double]): Unit= {
    //extract elements from choiceQueue
    //for (e<-choiceQueue) println(Console.BLUE + e._1 + " " + e._2)
    val queueSeq=(for (i <- 1 to choiceQueue.length) yield choiceQueue.dequeue()).toSeq

    //sort elements by score
    val queueSeqToScore = for (nc <- queueSeq) yield (nc, normClauseToScore(nc._1))
    val sortedQueueSeqToScore= queueSeqToScore.sortBy(_._2).reverse

    //for (e<-queueSeqToScore) println(Console.YELLOW + e._1 + " " + e._2)
    //for (e<-sortedQueueSeqToScore) println(Console.YELLOW_B + e._1 + " " + e._2)

    //enqueue sorted elements to choiceQueue
    for (s<-sortedQueueSeqToScore) choiceQueue.enqueue(s._1)
    //for (e<-choiceQueue) println(Console.RED + e._1 + " " + e._2)

    println(Console.BLUE + "choiceQueue length:" + choiceQueue.size)
    println(Console.BLUE + "queueSeq length:" + queueSeq.size)
    println(Console.BLUE + "queueSeqToScore length:" + queueSeqToScore.size)
    println(Console.BLUE + "sortedQueueSeqToScore length:" + sortedQueueSeqToScore.size)
    println(Console.BLUE + "choiceQueue length:" + choiceQueue.size)
    //sys.exit(0)


  }
  def prioritizeClauses(normClauses: Seq[NormClause], normClauseToScore: Map[NormClause, Double]): Seq[NormClause] = {

    val currentNormClauseToScore = for (nc <- normClauses) yield (nc, normClauseToScore(nc))
    //val sortedCurrentNormClauseToScore = currentNormClauseToScore.sortBy(_._2)
    val sortedCurrentNormClauseToScore = currentNormClauseToScore.sortBy(_._2).reverse

    // print middle data for debug
    println(Console.BLUE + "normClauseToScore length:" + normClauseToScore.size)
    println(Console.BLUE + "currentNormClauseToScore length:" + currentNormClauseToScore.size)
    //for (c<-currentNormClauseToScore) println(Console.BLUE + c._1 + " " + c._2)
    //for (c<-sortedCurrentNormClauseToScore) println(Console.YELLOW + c._1 + " " + c._2)


    sortedCurrentNormClauseToScore.map(_._1)
    //normClauses
  }


  def readClauseScores[CC](clauses: Iterable[CC]): Map[CC, Double] = {
    //get graph file name
    val graphFileName =
      if (GlobalParameters.get.fileName.contains("simplified")) GlobalParameters.get.fileName.stripSuffix(".simplified.smt2") + "." + graphFileNameMap(GlobalParameters.get.hornGraphType) + ".JSON"
      else GlobalParameters.get.fileName + "." + graphFileNameMap(GlobalParameters.get.hornGraphType) + ".JSON"
    //read logit values from graph file
    val predictedLogitsFromGraph = readJsonFieldDouble(graphFileName, readLabelName = "predictedLabelLogit")
    //for CDHG map predicted (read) Logits to correct clause number, for CG just return predictedLogitsFromGraph
    val predictedLogits = GlobalParameters.get.hornGraphType match {
      case HornGraphType.CDHG => {
        val labelMask = readJsonFieldInt(graphFileName, readLabelName = "labelMask")
        val originalClausesIndex = labelMask.distinct
        val separatedPredictedLabels = for (i <- originalClausesIndex) yield {
          for (ii <- (0 until labelMask.count(_ == i))) yield predictedLogitsFromGraph(i + ii)
        }
        val logitsForOriginalClauses = for (sl <- separatedPredictedLabels) yield {
          sl.max
        }
        logitsForOriginalClauses
      }
      case HornGraphType.CG => {
        predictedLogitsFromGraph
      }
    }

    println(Console.BLUE + "predictedLogits length:" + predictedLogits.length)
    (for ((c, s) <- clauses.zip(predictedLogits)) yield (c, s)).toMap
  }

  def graphFileNameMap(hgt: HornGraphType.Value): String = hgt match {
    case HornGraphType.CDHG => "hyperEdgeGraph"
    case HornGraphType.CG => "monoDirectionLayerGraph"
  }

  def readJsonFieldDouble(fileName: String, readLabelName: String): Array[Double] = {
    val json_data = readJSONFile(fileName)
    val readLabel = (json_data \ readLabelName).validate[Array[Double]] match {
      case JsSuccess(templateLabel, _) => templateLabel
    }
    readLabel
  }

  def readJsonFieldInt(fileName: String, readLabelName: String): Array[Int] = {
    val json_data = readJSONFile(fileName)
    val readLabel = (json_data \ readLabelName).validate[Array[Int]] match {
      case JsSuccess(templateLabel, _) => templateLabel
    }
    readLabel
  }

  def readJSONFile(fileName: String): JsValue = {
    val json_content = scala.io.Source.fromFile(fileName).mkString
    Json.parse(json_content)
  }


}
