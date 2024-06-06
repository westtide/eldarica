package lazabs.horn.graphs

import ap.parser.{EquivExpander, IAtom, PartialEvaluator, SMTLineariser, Transform2Prenex}
import ap.terfor.conjunctions.Conjunction
import lazabs.GlobalParameters
import ap.terfor.preds.Predicate
import lazabs.horn.abstractions.VerificationHints
import lazabs.horn.bottomup.DisjInterpolator.AndOrNode
import lazabs.horn.bottomup.Util.Dag
import lazabs.horn.bottomup.{CEGAR, HornClauses, HornPredAbs, HornTranslator, NormClause}
import lazabs.horn.graphs.GraphUtils.{seqToString, simplifyClauses}
import lazabs.horn.parser.HornReader.fromSMT
import lazabs.horn.preprocessor.HornPreprocessor.Clauses
import play.api.libs.json.{JsSuccess, JsValue, Json}

import java.io.{File, PrintWriter}

object Utils {


  def readSMTFormatFromFile(fileName: String): Clauses = {
    if(GlobalParameters.get.log)
      println(Console.BLUE + "-" * 10 + "read CHCs from file"+ fileName + "-" * 10)
    val _hornTranslator = new HornTranslator
    fromSMT(fileName) map ((_hornTranslator).transform(_))
  }

  def writeSMTFormatToFile(simpClauses: Clauses, suffix: String): Unit = {
    val fileName = GlobalParameters.get.fileName + "." + suffix
    println("write " + fileName + " to file")
    val out = new java.io.FileOutputStream(fileName)
    Console.withOut(out) {
      val clauseFors =
        for (c <- simpClauses) yield {
          val f = c.toFormula
          // eliminate remaining operators like eps
          Transform2Prenex(EquivExpander(PartialEvaluator(f)))
        }

      val allPredicates =
        HornClauses allPredicates simpClauses

      SMTLineariser.printWithDecls(benchmarkName  = "C_VC",
        logic          = "HORN",
        predsToDeclare =
          allPredicates.toSeq.sortBy(_.name),
        formulas       = clauseFors)
    }
    out.close

  }

  def writePrologFormatToFile(clause: Clauses, suffix: String): Unit = {
    val _suffix = if (suffix.isEmpty) "" else "." + suffix
    val fileName = GlobalParameters.get.fileName + _suffix + "." + "prolog"
    println("write " + fileName + " to file")
    val writerGraph = new PrintWriter(new File(fileName))
    for (c <- clause) {
      writerGraph.write(c.toPrologString + "\n")
    }
    writerGraph.close()
  }

  def printListMap[A, B](m: Map[A, Seq[B]], title: String = ""): Unit = {
    println("-" * 10 + title + "-" * 10)
    for ((k, v) <- m) {
      println(k)
      for (vv <- v)
        println(vv)
    }
  }

  def getPredAbs(simplifiedClauses: Clauses, simpHints: VerificationHints, disjunctive: Boolean,
                 predGenerator: Dag[AndOrNode[NormClause, Unit]] =>
                   Either[Seq[(Predicate, Seq[Conjunction])],
                     Dag[(IAtom, NormClause)]]):
  (HornPredAbs[HornClauses.Clause]) = {
    val counterexampleMethod =
      if (disjunctive)
        CEGAR.CounterexampleMethod.AllShortest
      else
        CEGAR.CounterexampleMethod.FirstBestShortest
    val predAbs =
      new HornPredAbs(simplifiedClauses,
        simpHints.toInitialPredicates, predGenerator,
        counterexampleMethod)
    predAbs
  }


  def writeOneLineJson(head: String, body: String, writer: PrintWriter, changeLine: Boolean = true, lastEntry: Boolean = false): Unit = {
    if (lastEntry == false) {
      if (changeLine == true)
        writer.write("\"" + head + "\"" + ":\n" + seqToString(body) + "," + "\n")
      else
        writer.write("\"" + head + "\"" + ":" + seqToString(body) + "," + "\n")
    } else {
      writer.write("\"" + head + "\"" + ":\n" + seqToString(body) + "\n")
    }

  }

  def readJSONFile(fileName: String): JsValue = {
    val json_content = scala.io.Source.fromFile(fileName).mkString
    Json.parse(json_content)
  }

  def readJsonFieldInt(fileName: String, readLabelName: String,dataLength:Int=0): Array[Int] = {
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

  def readJsonFieldDouble(fileName: String, readLabelName: String,dataLength:Int=0): Array[Double] = {
    try{
      val json_data = readJSONFile(fileName)
      val readLabel = (json_data \ readLabelName).validate[Array[Double]] match {
        case JsSuccess(templateLabel, _) => templateLabel
      }
      readLabel
    }catch {
      case _ => {
        if(GlobalParameters.get.log)
          println(Console.RED+"read "+fileName+" failed")
        Seq.fill(dataLength)(0.0).toArray
      }
    }

  }

  def outputClauses(simplifiedClauses: Clauses, unsimplifiedClauses: Clauses): Unit = {
    writeSMTFormatToFile(simplifiedClauses, suffix = "simplified")
    writePrologFormatToFile(simplifiedClauses, suffix = "simplified")
    writePrologFormatToFile(unsimplifiedClauses, suffix = "")
  }
  def outputSimplifiedClauses(perprocessedClauses: Clauses): Unit = {
    val furtherSimplifiedClauses=simplifyClauses(perprocessedClauses,VerificationHints(Map()))
    println(Console.BLUE + "-" * 10 + "write further simplified CHCs to file" + "-" * 10)
    writeSMTFormatToFile(furtherSimplifiedClauses, "simplified")
  }

  def getFloatSeqRank(inputSeq: Seq[Double], inverse: Boolean = true): Seq[Int] = {
    val sortedSeq = inputSeq.sorted
    val rankSeq = inputSeq.map(value => sortedSeq.indexOf(value) + 1) //The lower value the higher rank
    //The higher value the lower rank
    var currentRank = 1
    //todo get pair and sort them

    val inverseRankSeq = rankSeq.map(rank => inputSeq.length - rank + 1) //The higher value the higher rank
    if (inverse == true)
      inverseRankSeq
    else
      rankSeq
  }

  def roundByDigit(number: Double, digit: Int) = {
    BigDecimal(number).setScale(digit, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  def roundByDigit(number: Float, digit: Int) = {
    BigDecimal(number).setScale(digit, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

}
