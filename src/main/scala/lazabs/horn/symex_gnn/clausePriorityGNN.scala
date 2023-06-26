package lazabs.horn.symex_gnn

import ap.terfor.conjunctions.Conjunction
import ap.terfor.preds.Predicate
import lazabs.GlobalParameters
import lazabs.horn.bottomup.{AbstractState, HornClauses, HornTranslator, NormClause, RelationSymbol}
import lazabs.horn.symex.UnitClause
import lazabs.horn.parser.HornReader.fromSMT

import scala.collection.mutable.{PriorityQueue, HashSet => MHashSet, Map => MMap, Queue => MQueue}
import java.io.{File, PrintWriter}
import play.api.libs.json.{JsSuccess, JsValue, Json}

import scala.util.Random
import lazabs.horn.preprocessor.HornPreprocessor.Clauses

import scala.collection.mutable

/*
-sym:1 -hornGraphType:CDHG/CG -abstract:off -prioritizeClauses:label/constant/random/score/rank/SEHPlus/SEHMinus/REHPlus/REHMinus
*/
object HornGraphType extends Enumeration {
  val CDHG, CG = Value
}

object PrioritizeOption extends Enumeration {
  val label, constant, random, score, rank, SEHPlus, SEHMinus, REHPlus, REHMinus, twoQueue02, twoQueue05,twoQueue08 = Value
}

class ControlledChoiceQueue(normClauseToScore: Map[NormClause, Double]) extends StateQueue {
  Random.setSeed(42)
  //val processedMap : MMap[(NormClause, Seq[UnitClause]), Boolean]=MMap()
  val processedHashSet = new MHashSet[((NormClause, Seq[UnitClause]), TimeType)]()

  val priFunc: (Double, Int, Int) => Double =
    GlobalParameters.get.prioritizeClauseOption match {
      case PrioritizeOption.label => PriorotyQueueFunc.label
      case PrioritizeOption.constant => PriorotyQueueFunc.constant
      case PrioritizeOption.random => PriorotyQueueFunc.random
      case PrioritizeOption.score => PriorotyQueueFunc.score
      case PrioritizeOption.rank => PriorotyQueueFunc.rank
      case PrioritizeOption.SEHPlus => PriorotyQueueFunc.SEHPlus
      case PrioritizeOption.SEHMinus => PriorotyQueueFunc.SEHMinus
      case PrioritizeOption.REHPlus => PriorotyQueueFunc.REHPlus
      case PrioritizeOption.REHMinus => PriorotyQueueFunc.REHMinus
      case PrioritizeOption.twoQueue02 => PriorotyQueueFunc.twoQueue02
      case PrioritizeOption.twoQueue05 => PriorotyQueueFunc.twoQueue05
      case PrioritizeOption.twoQueue08 => PriorotyQueueFunc.twoQueue08
    }

  val scoreQueue = new PriorityChoiceQueue(normClauseToScore,priFunc)

  val secondQueue = new PriorityChoiceQueue(normClauseToScore,PriorotyQueueFunc.random)

  def data: PriorityQueue[ChoiceQueueElement] = {
    secondQueue.data
  }

  def isEmpty: Boolean = {
    //processedMap.count(_._2 == false) == 0
    //if all element already in processedMap, then return true
    scoreQueue.isEmpty || secondQueue.isEmpty //|| (originalQueue.data.forall(processedHashSet.contains) && scoreQueue.data.forall(processedHashSet.contains))
  }

  def size: Int = {
    scoreQueue.size.max(secondQueue.size)
  }

  def enqueue(e: (NormClause, Seq[UnitClause])): Unit = {
    // processedMap += (e -> false)
    scoreQueue.enqueue(e)
    secondQueue.enqueue(e)
  }

  def dequeue(): ((NormClause, Seq[UnitClause]), TimeType) = {
    val exploration = if(GlobalParameters.get.prioritizeClauseOption == PrioritizeOption.twoQueue02) Random.nextDouble() > 0.2
    else if(GlobalParameters.get.prioritizeClauseOption == PrioritizeOption.twoQueue05) Random.nextDouble() > 0.5
    else if(GlobalParameters.get.prioritizeClauseOption == PrioritizeOption.twoQueue08) Random.nextDouble() > 0.8
    else Random.nextDouble() > -1

    //println("-" * 10)
    //println(Console.BLUE + "processedMap", processedMap.size, "false", processedMap.count(_._2 == false))
    val queue = if (exploration) scoreQueue else secondQueue // when both queue have the same last element stack overflow


    if (queue.isEmpty) {
      dequeue()
    } else {
      val e = queue.dequeue()
      if (processedHashSet.contains(e)) {
        if (queue.isEmpty) {
          e
        } else {
          dequeue() //do nothing and go to next iteration
        }
      } else {
        processedHashSet.add(e)
        e
      }
    }

    //        if (exploration == true) {
    //          //println("dequeue scoreQueue")
    //          while (true) {
    //            if (scoreQueue.isEmpty) {
    //              return originalQueue.dequeue()
    //            }
    //            else {
    //              val e = scoreQueue.dequeue()
    //              if (processedMap(e) == false) {
    //                processedMap(e) = true
    //                return e
    //              }
    //            }
    //          }
    //          scoreQueue.dequeue()
    //        } else {
    //          //println("dequeue originalQueue")
    //          while (true) {
    //            if (originalQueue.isEmpty) {
    //              return scoreQueue.dequeue()
    //            }
    //            else {
    //              val e = originalQueue.dequeue()
    //              if (processedMap(e) == false) {
    //                processedMap(e) = true
    //                return e
    //              }
    //            }
    //          }
    //          originalQueue.dequeue()
    //        }
    //
  }


}

trait StateQueue {
  type TimeType = Int
  type ChoiceQueueElement = ((NormClause, Seq[UnitClause]), TimeType)

  def data: PriorityQueue[ChoiceQueueElement]

  def isEmpty: Boolean

  def size: Int

  def enqueue(e: (NormClause, Seq[UnitClause])): Unit

  def dequeue(): ((NormClause, Seq[UnitClause]), TimeType)

  def incTime: Unit = {}
}

object PriorotyQueueFunc {
  val coefClauseScoreFromGNN = 1000
  def label(normclauseSocre: Double, birthTime: Int, unitClauseSeqScore: Int): Double = {
    normclauseSocre
  }
  def constant(normclauseSocre: Double, birthTime: Int, unitClauseSeqScore: Int): Double = {
    1.0
  }
  def random(normclauseSocre: Double, birthTime: Int, unitClauseSeqScore: Int): Double = {
    Random.nextInt(1000).toDouble
  }

  def score(normclauseSocre: Double, birthTime: Int, unitClauseSeqScore: Int): Double = {
    normclauseSocre*coefClauseScoreFromGNN
  }
  def rank = label _

  def SEHPlus(normclauseSocre: Double, birthTime: Int, unitClauseSeqScore: Int): Double = {
    normclauseSocre*coefClauseScoreFromGNN + birthTime + unitClauseSeqScore
  }
  def SEHMinus(normclauseSocre: Double, birthTime: Int, unitClauseSeqScore: Int): Double = {
    normclauseSocre*coefClauseScoreFromGNN - birthTime - unitClauseSeqScore
  }
  def REHPlus(normclauseSocre: Double, birthTime: Int, unitClauseSeqScore: Int): Double = {
    normclauseSocre + birthTime + unitClauseSeqScore
  }
  def REHMinus(normclauseSocre: Double, birthTime: Int, unitClauseSeqScore: Int): Double = {
    normclauseSocre - birthTime - unitClauseSeqScore
  }

  def twoQueue02 = REHMinus _
  def twoQueue05 = REHMinus _
  def twoQueue08 = REHMinus _
}

class PriorityChoiceQueue(normClauseToScore: Map[NormClause, Double],priFunc:(Double, Int, Int)=>Double) extends StateQueue {
  private var time = 0
  Random.setSeed(42)
  //type ChoiceQueueElement = (NormClause, Seq[UnitClause])

  //println(Console.BLUE+"ChoiceQueue:PriorityChoiceQueue")
  private def priority(s: ChoiceQueueElement) = {
    val ((nc, ucs), birthTime) = s
    val normclauseSocre = normClauseToScore(nc)
    val unitClauseSeqScore = ucs.map(_.constraint.size).sum //+ nc._2.map(_.rs.arity).sum
    val queueElementScore = priFunc(normclauseSocre, birthTime,unitClauseSeqScore)

    //println("normclauseSocre",normclauseSocre,ucs.size,unitClauseSeqScore,birthTime)
    //println(Console.RED_B+"priority",normclauseSocre,unitClauseSeqScore,queueElementScore.toInt)

    -queueElementScore.toInt
  }

  private implicit val ord = new Ordering[ChoiceQueueElement] {
    def compare(s: ChoiceQueueElement, t: ChoiceQueueElement) =
      priority(t) - priority(s)
  }

  private val states = new PriorityQueue[ChoiceQueueElement]

  def data: PriorityQueue[ChoiceQueueElement] = {
    states
  }

  def isEmpty: Boolean =
    states.isEmpty

  def size: Int =
    states.size

  def enqueue(e: (NormClause, Seq[UnitClause])): Unit = {
    incTime
    states += (((e._1, e._2), time))
  }

  def dequeue(): ((NormClause, Seq[UnitClause]), TimeType) = {
    val ((nc, ucs), birthTime) = states.dequeue
    //println(Console.BLUE + "dequeue", "birthTime", birthTime)
    ((nc, ucs), birthTime)
  }

  override def incTime: Unit =
    time = time + 1
}


object clausePriorityGNN {

  def readClauseLabel[CC](clauses: Iterable[CC]): Map[CC, Double] = {
    val labelFileName = GlobalParameters.get.fileName + ".counterExampleIndex.JSON"
    val labels = readJsonFieldInt(labelFileName, readLabelName = "counterExampleLabels", dataLength = clauses.size)
    (for ((c, s) <- clauses.zip(labels)) yield (c, s.toDouble)).toMap
  }

  def readClauseScores[CC](clauses: Iterable[CC]): Map[CC, Double] = {
    //get graph file name
    val graphFileName =
      if (GlobalParameters.get.fileName.contains("simplified"))
        GlobalParameters.get.fileName.stripSuffix(".simplified.smt2") + "." + graphFileNameMap(GlobalParameters.get.hornGraphType) + ".JSON"
      else
        GlobalParameters.get.fileName + "." + graphFileNameMap(GlobalParameters.get.hornGraphType) + ".JSON"
    //read logit values from graph file
    val predictedLogitsFromGraph = readJsonFieldDouble(graphFileName, readLabelName = "predictedLabelLogit", dataLength = clauses.size)
    //normalize scores
    val normalizedLogits = if(predictedLogitsFromGraph.forall(_==0.0)) predictedLogitsFromGraph else predictedLogitsFromGraph.map(x => (x - predictedLogitsFromGraph.min) / (predictedLogitsFromGraph.max - predictedLogitsFromGraph.min))
    val (ranks, stableRanks) = rankFloatList(normalizedLogits)

    val scores =
      if (GlobalParameters.get.prioritizeClauseOption == PrioritizeOption.score || GlobalParameters.get.prioritizeClauseOption == PrioritizeOption.SEHPlus || GlobalParameters.get.prioritizeClauseOption == PrioritizeOption.SEHMinus)
        normalizedLogits
      else
        stableRanks

    //for CDHG map predicted (read) Logits to correct clause number, for CG just return normalized Logits
    val predictedLogits = GlobalParameters.get.hornGraphType match {
      case HornGraphType.CDHG => {
        val labelMask = readJsonFieldInt(graphFileName, readLabelName = "labelMask", dataLength = clauses.size)
        val originalClausesIndex = labelMask.distinct
        val separatedPredictedLabels = for (i <- originalClausesIndex) yield {
          for (ii <- (0 until labelMask.count(_ == i))) yield scores(i + ii)
        }
        val logitsForOriginalClauses = for (sl <- separatedPredictedLabels) yield {
          sl.max
        }
        logitsForOriginalClauses
      }
      case HornGraphType.CG => {
        scores
      }
    }

    //println(Console.BLUE + "predictedLogits length:" + predictedLogits.length)
    (for ((c, s) <- clauses.zip(predictedLogits)) yield (c, s)).toMap
  }

  def graphFileNameMap(hgt: HornGraphType.Value): String = hgt match {
    case HornGraphType.CDHG => "hyperEdgeGraph"
    case HornGraphType.CG => "monoDirectionLayerGraph"
  }

  def readJsonFieldInt(fileName: String, readLabelName: String, dataLength: Int = 0): Array[Int] = {
    try {
      val json_data = readJSONFile(fileName)
      val readLabel = (json_data \ readLabelName).validate[Array[Int]] match {
        case JsSuccess(templateLabel, _) => templateLabel
      }
      readLabel
    } catch {
      case _ => {
        if(GlobalParameters.get.log)
          println(Console.RED + "read " + fileName + " failed")
        Seq.fill(dataLength)(0).toArray
      }
    }
  }

  def readJsonFieldDouble(fileName: String, readLabelName: String, dataLength: Int = 0): Array[Double] = {
    try {
      val json_data = readJSONFile(fileName)
      val readLabel = (json_data \ readLabelName).validate[Array[Double]] match {
        case JsSuccess(templateLabel, _) => templateLabel
      }
      readLabel
    } catch {
      case _ => {
        if(GlobalParameters.get.log)
          println(Console.RED + "read " + fileName + " failed")
        Seq.fill(dataLength)(0.0).toArray
      }
    }

  }

  def readJSONFile(fileName: String): JsValue = {
    val json_content = scala.io.Source.fromFile(fileName).mkString
    Json.parse(json_content)
  }

  def rankFloatList(values: Array[Double]): (Array[Double], Array[Double]) = {
    val valuesWithIndex = for ((v, i) <- values.zipWithIndex) yield (i, v)
    val rankTuple = (for (((i, v), r) <- valuesWithIndex.sortBy(_._2).reverse.zipWithIndex) yield (i, v, r)).sortBy(_._1)
    val ranks = rankTuple.map(_._3 + 1.toDouble)

    val stableRankMap = if (values.forall(_==0.0)) values.zipWithIndex.toMap else values.toSet.toList.sorted.reverse.zipWithIndex.toMap
    val StableRanks = for (v <- values) yield stableRankMap(v).toDouble

    (ranks, StableRanks)
  }

  def readSMTFormatFromFile(fileName: String): Clauses = {
    if (GlobalParameters.get.log)
      println(Console.BLUE + "-" * 10 + "read CHCs from file" + fileName + "-" * 10)
    val _hornTranslator = new HornTranslator
    fromSMT(fileName) map ((_hornTranslator).transform(_))
  }


}
