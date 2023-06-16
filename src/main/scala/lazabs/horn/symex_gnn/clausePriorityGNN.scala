package lazabs.horn.symex_gnn

import ap.terfor.conjunctions.Conjunction
import lazabs.GlobalParameters
import lazabs.horn.bottomup.{AbstractState, HornClauses, HornTranslator, NormClause, RelationSymbol}
import lazabs.horn.symex.UnitClause
import lazabs.horn.parser.HornReader.fromSMT
import scala.collection.mutable.{PriorityQueue, Queue => MQueue, Map => MMap, HashSet => MHashSet}
import java.io.{File, PrintWriter}
import play.api.libs.json.{JsSuccess, JsValue, Json}
import scala.util.Random
import lazabs.horn.preprocessor.HornPreprocessor.Clauses

object HornGraphType extends Enumeration {
  val CDHG, CG = Value
}

class ControlledChoiceQueue(normClauseToScore: Map[NormClause, Double]) extends StateQueue {
  Random.setSeed(42)
  //val processedMap : MMap[(NormClause, Seq[UnitClause]), Boolean]=MMap()
  val processedHashSet = new MHashSet[((NormClause, Seq[UnitClause]),TimeType)]()
  val scoreQueue =
    if (GlobalParameters.get.useGNN)
      new PriorityChoiceQueue(normClauseToScore)
    else
      new RandomPriorityChoiceQueue()

  val originalQueue = new RandomPriorityChoiceQueue()


  def isEmpty: Boolean = {
    //processedMap.count(_._2 == false) == 0
    scoreQueue.isEmpty || originalQueue.isEmpty
  }

  def size: Int = {
    scoreQueue.size.max(originalQueue.size)
  }

  def enqueue(e: (NormClause, Seq[UnitClause])): Unit = {
    // processedMap += (e -> false)
    scoreQueue.enqueue(e)
    originalQueue.enqueue(e)
  }

  def dequeue(): ((NormClause, Seq[UnitClause]),TimeType) = {
    val exploration = Random.nextDouble() > -1 //only use score queue
    //val exploration = Random.nextDouble() < -1 //only use original/random queue
    //val exploration = Random.nextDouble() > 0.5 // more than 0.5 means use more random/original queue
    //println("-" * 10)
    //println(Console.BLUE + "processedMap", processedMap.size, "false", processedMap.count(_._2 == false))
    //    println(Console.BLUE + "processedHashSet.size: " + processedHashSet.size)
    //    println(Console.BLUE + "scoreQueue.size: " + scoreQueue.size,scoreQueue.isEmpty)
    //    println(Console.BLUE + "originalQueue.size: " + originalQueue.size,originalQueue.isEmpty)
    //    println(Console.BLUE + "exploration: " + exploration)
    val queue = if (exploration) scoreQueue else originalQueue
    if (queue.isEmpty) {
      dequeue()
    } else {
      val e = queue.dequeue()
      if (processedHashSet.contains(e)) {
        dequeue() //do nothing and go to next iteration
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
  type ChoiceQueueElement = (NormClause, Seq[UnitClause], TimeType)


  def isEmpty: Boolean

  def size: Int

  def enqueue(e: (NormClause, Seq[UnitClause])): Unit

  def dequeue(): ((NormClause, Seq[UnitClause]),TimeType)

  def incTime: Unit = {}
}

class PriorityChoiceQueue(normClauseToScore: Map[NormClause, Double]) extends StateQueue {
  private var time = 0

  //type ChoiceQueueElement = (NormClause, Seq[UnitClause])
  val coefClauseScoreFromGNN = 1000

  //println(Console.BLUE+"ChoiceQueue:PriorityChoiceQueue")
  private def priority(s: ChoiceQueueElement) = {
    val (nc, ucs, birthTime) = s
    val normclauseSocre = normClauseToScore(nc)
    val unitClauseSeqScore = ucs.map(_.constraint.size).sum //+ nc._2.map(_.rs.arity).sum

    //println("normclauseSocre",normclauseSocre,ucs.size,unitClauseSeqScore,birthTime)
    //by rank, need to shift val scores=
    //val queueElementScore = normclauseSocre //rank
    //val queueElementScore = normclauseSocre - birthTime //rank + birthTime
    //val queueElementScore = normclauseSocre - unitClauseSeqScore //rank + unitClauseSeqScore
    val queueElementScore = normclauseSocre - birthTime - unitClauseSeqScore //rank + birthTime + unitClauseSeqScore
    //by score, need to shift val scores=
    //val queueElementScore = normclauseSocre * coefClauseScoreFromGNN //score
    //val queueElementScore = normclauseSocre * coefClauseScoreFromGNN - birthTime // score + birthTime
    //val queueElementScore = normclauseSocre * coefClauseScoreFromGNN + unitClauseSeqScore // score + unitClauseSeqScore
    //val queueElementScore = normclauseSocre * coefClauseScoreFromGNN - unitClauseSeqScore - birthTime// score + birthTime + unitClauseSeqScore
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
    incTime
    //println(Console.BLUE+"enqueue",e._1,e._2,e._2.map(_.constraint.size).sum ,time)
    states += ((e._1, e._2, time))
  }

  def dequeue(): ((NormClause, Seq[UnitClause]),TimeType) = {
    val (nc, ucs, birthTime) = states.dequeue
    //println(Console.BLUE + "dequeue", "birthTime", birthTime)
    ((nc, ucs),birthTime)
  }

  override def incTime: Unit =
    time = time + 1
}

class OriginalPriorityChoiceQueue() extends StateQueue {
  private var time = 0


  private def priority(s: ChoiceQueueElement) = {
    val queueElementScore = 1 //constant
    -queueElementScore.toInt
  }

  private implicit val ord = new Ordering[ChoiceQueueElement] {
    def compare(s: ChoiceQueueElement, t: ChoiceQueueElement) = {
      priority(t) - priority(s)
    }
  }
  private val states = new PriorityQueue[ChoiceQueueElement]

  def isEmpty: Boolean =
    states.isEmpty

  def size: Int =
    states.size

  def enqueue(e: (NormClause, Seq[UnitClause])): Unit = {
    //println(Console.BLUE+"enqueue",e._1,e._2)
    incTime
    states += ((e._1, e._2, time))
  }

  def dequeue(): ((NormClause, Seq[UnitClause]),TimeType) = {
    val (nc, ucs, birthTime) = states.dequeue
    ((nc, ucs),birthTime)
  }

  override def incTime: Unit =
    time = time + 1
}

class RandomPriorityChoiceQueue() extends StateQueue {
  private var time = 0
  Random.setSeed(42)

  private def priority(s: ChoiceQueueElement) = {
    val queueElementScore = Random.nextInt(1000) //random
    -queueElementScore.toInt
  }

  private implicit val ord = new Ordering[ChoiceQueueElement] {
    def compare(s: ChoiceQueueElement, t: ChoiceQueueElement) = {
      priority(t) - priority(s)
    }
  }
  private val states = new PriorityQueue[ChoiceQueueElement]

  def isEmpty: Boolean =
    states.isEmpty

  def size: Int =
    states.size

  def enqueue(e: (NormClause, Seq[UnitClause])): Unit = {
    //println(Console.BLUE+"enqueue",e._1,e._2)
    incTime
    states += ((e._1, e._2, time))
  }

  def dequeue(): ((NormClause, Seq[UnitClause]), TimeType) = {
    val (nc, ucs, birthTime) = states.dequeue
    ((nc, ucs), birthTime)
  }

  override def incTime: Unit =
    time = time + 1
}

object clausePriorityGNN {

  def readClauseLabel[CC](clauses: Iterable[CC]): Map[CC, Double] = {
    val labelFileName = GlobalParameters.get.fileName + ".counterExampleIndex.JSON"
    val labels = readJsonFieldInt(labelFileName, readLabelName = "counterExampleLabels")
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
    val predictedLogitsFromGraph = readJsonFieldDouble(graphFileName, readLabelName = "predictedLabelLogit")
    //normalize scores
    val normalizedLogits = predictedLogitsFromGraph.map(x => (x - predictedLogitsFromGraph.min) / (predictedLogitsFromGraph.max - predictedLogitsFromGraph.min))
    val (ranks, stableRanks) = rankFloatList(normalizedLogits)
    val scores = stableRanks

    //for CDHG map predicted (read) Logits to correct clause number, for CG just return normalized Logits
    val predictedLogits = GlobalParameters.get.hornGraphType match {
      case HornGraphType.CDHG => {
        val labelMask = readJsonFieldInt(graphFileName, readLabelName = "labelMask")
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

  def readSMTFormatFromFile(fileName: String): Clauses = {
    if (GlobalParameters.get.log)
      println(Console.BLUE + "-" * 10 + "read CHCs from file" + fileName + "-" * 10)
    val _hornTranslator = new HornTranslator
    fromSMT(fileName) map ((_hornTranslator).transform(_))
  }


}
