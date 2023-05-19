package lazabs.horn.symex_gnn

import ap.terfor.conjunctions.Conjunction
import lazabs.GlobalParameters
import lazabs.horn.bottomup.{AbstractState, HornClauses, NormClause, RelationSymbol}
import lazabs.horn.symex.UnitClause

import scala.collection.mutable.{PriorityQueue, Queue => MQueue}
import java.io.{File, PrintWriter}
import play.api.libs.json.{JsSuccess, JsValue, Json}

object HornGraphType extends Enumeration {
  val CDHG, CG = Value
}

trait StateQueue {
  type TimeType
  type ChoiceQueueElement = (NormClause, Seq[UnitClause], TimeType)

  def isEmpty: Boolean

  def size: Int

  def enqueue(e: (NormClause, Seq[UnitClause])): Unit

  def dequeue(): (NormClause, Seq[UnitClause])

  def incTime: Unit = {}
}

class PriorityChoiceQueue(normClauseToScore: Map[NormClause, Double]) extends StateQueue {
  type TimeType = Int
  private var time = 0

  //type ChoiceQueueElement = (NormClause, Seq[UnitClause])
  val coefClauseScoreFromGNN = 100

  //println(Console.BLUE+"ChoiceQueue:PriorityChoiceQueue")
  private def priority(s: ChoiceQueueElement) = {
    val (nc, ucs, birthTime) = s
    val normclauseSocre = normClauseToScore(nc)
    //val unitClauseSeqScore = ucs.map(_.constraint.size).sum //+ nc._2.map(_.rs.arity).sum
    val queueElementScore = normclauseSocre //by rank
    //val queueElementScore = normclauseSocre * coefClauseScoreFromGNN
    //val queueElementScore = normclauseSocre * coefClauseScoreFromGNN + unitClauseSeqScore
    //val queueElementScore =  birthTime
    //val queueElementScore = normclauseSocre * coefClauseScoreFromGNN + birthTime
    //val queueElementScore = normclauseSocre * coefClauseScoreFromGNN + unitClauseSeqScore + birthTime
    //println(Console.RED_B+"priority",normclauseSocre,unitClauseSeqScore,queueElementScore.toInt)

    -queueElementScore.toInt
  }

  private implicit val ord = new Ordering[ChoiceQueueElement] {
    def compare(s: ChoiceQueueElement, t: ChoiceQueueElement) =
      priority(t) - priority(s)
  }

  private val states = new PriorityQueue[ChoiceQueueElement]

  def isEmpty: Boolean =
    states.isEmpty

  def size: Int =
    states.size

  def enqueue(e: (NormClause, Seq[UnitClause])): Unit = {
    //println(Console.BLUE+"enqueue",e._1,e._2)
    states += ((e._1, e._2, time))
  }

  def dequeue(): (NormClause, Seq[UnitClause]) = {
    val (nc, ucs,birthTime) = states.dequeue
    (nc, ucs)
  }
  override def incTime: Unit =
    time = time + 1
}

class OriginalPriorityChoiceQueue() extends StateQueue {
  type TimeType = Int
  private var time = 0
  private def priority(s: ChoiceQueueElement) = {
    val (nc, ucs,_) = s
    val unitClauseSeqScore = 1
    //val unitClauseSeqScore = ucs.map(_.constraint.size).sum //+ nc._2.map(_.rs.arity).sum
    val queueElementScore = unitClauseSeqScore
    -queueElementScore.toInt
  }

  private implicit val ord = new Ordering[ChoiceQueueElement] {
    def compare(s: ChoiceQueueElement, t: ChoiceQueueElement) =
      priority(t) - priority(s)
  }
  private val states = new PriorityQueue[ChoiceQueueElement]

  def isEmpty: Boolean =
    states.isEmpty

  def size: Int =
    states.size

  def enqueue(e: (NormClause, Seq[UnitClause])): Unit = {
    //println(Console.BLUE+"enqueue",e._1,e._2)
    states += ((e._1, e._2,time))
  }

  def dequeue(): (NormClause, Seq[UnitClause]) = {
    val (nc, ucs,_) = states.dequeue
    (nc, ucs)
  }

  override def incTime: Unit =
    time = time + 1
}

object clausePriorityGNN {


  def readClauseScores[CC](clauses: Iterable[CC]): Map[CC, Double] = {
    //get graph file name
    val graphFileName =
      if (GlobalParameters.get.fileName.contains("simplified"))
        GlobalParameters.get.fileName.stripSuffix(".simplified.smt2") + "." + graphFileNameMap(GlobalParameters.get.hornGraphType) + ".JSON"
      else
        GlobalParameters.get.fileName + "." + graphFileNameMap(GlobalParameters.get.hornGraphType) + ".JSON"
    //read logit values from graph file
    val predictedLogitsFromGraph = readJsonFieldDouble(graphFileName, readLabelName = "predictedLabelLogit")
    //normalize scores
    val normalizedLogits = predictedLogitsFromGraph.map(x => (x - predictedLogitsFromGraph.min) / (predictedLogitsFromGraph.max - predictedLogitsFromGraph.min))
    val (ranks,stableRanks)= rankFloatList(normalizedLogits)

    //for CDHG map predicted (read) Logits to correct clause number, for CG just return normalizedLogits
    val predictedLogits = GlobalParameters.get.hornGraphType match {
      case HornGraphType.CDHG => {
        val labelMask = readJsonFieldInt(graphFileName, readLabelName = "labelMask")
        val originalClausesIndex = labelMask.distinct
        val separatedPredictedLabels = for (i <- originalClausesIndex) yield {
          for (ii <- (0 until labelMask.count(_ == i))) yield ranks(i + ii)
        }
        val logitsForOriginalClauses = for (sl <- separatedPredictedLabels) yield {
          sl.max
        }
        logitsForOriginalClauses
      }
      case HornGraphType.CG => {
        ranks
      }
    }

    //println(Console.BLUE + "predictedLogits length:" + predictedLogits.length)
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

  def rankFloatList(values: Array[Double]): (Array[Double], Array[Double]) = {
    val valuesWithIndex = for ((v, i) <- values.zipWithIndex) yield (i, v)
    val rankTuple = (for (((i, v), r) <- valuesWithIndex.sortBy(_._2).reverse.zipWithIndex) yield (i, v, r)).sortBy(_._1)
    val ranks = rankTuple.map(_._3 + 1.toDouble)

    val stableRankMap = values.toSet.toList.sorted.reverse.zipWithIndex.toMap
    val StableRanks = for (v <- values) yield stableRankMap(v).toDouble
    (ranks, StableRanks)
  }


}
