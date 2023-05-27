/**
 * Copyright (c) 2022 Zafer Esen, Philipp Ruemmer. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * * Neither the name of the authors nor the names of their
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package lazabs.horn.symex

import ap.util.Combinatorics
import lazabs.GlobalParameters
import lazabs.horn.bottomup.HornClauses.ConstraintClause
import lazabs.horn.bottomup.NormClause
import lazabs.horn.symex_gnn.{OriginalPriorityChoiceQueue, PriorityChoiceQueue, ControlledChoiceQueue}

import scala.collection.mutable.{Queue => MQueue}

/**
 * Implements a breadth-first forward symbolic execution using Symex.
 */
class BreadthFirstForwardSymex[CC](clauses: Iterable[CC])(
  implicit clause2ConstraintClause: CC => ConstraintClause)
  extends Symex(clauses)
    with SimpleSubsumptionChecker
    with ConstraintSimplifierUsingConjunctEliminator {

  import Symex._

  printInfo("Starting breadth-first forward symbolic execution (BFS)...\n")

  // Explore the state graph (the derived unit clauses) breadth-first. At
  // each depth there can be multiple choices for execution from a state
  // (the clauses to resolve with). Hence, we have a queue of states to resolve
  // with, and for each state a queue of branches to explore.
  //
  // Nonlinear clauses: there might be some paths that are accessible using more
  // than a single state, if other states we use are in the queue, we remove
  // from those states' queue the path that we are about to take.

  //private val choicesQueue = new MQueue[(NormClause, Seq[UnitClause])]

  //private val choicesQueue = new PriorityChoiceQueue(normClauseToScore)
//  private val choicesQueue =
    if (GlobalParameters.get.useGNN) new PriorityChoiceQueue(normClauseToScore)
    else new OriginalPriorityChoiceQueue()
//    private val choicesQueue =
//      if (GlobalParameters.get.useGNN) new ControlledChoiceQueue(normClauseToScore)
//      else new OriginalPriorityChoiceQueue()

  /*
   * Initialize the search by adding the facts (the initial states).
   * Each fact corresponds to a source in the search DAG.
   */
  for (fact <- facts) {
    unitClauseDB add(fact, parents = (factToNormClause(fact), Nil))
    handleNewUnitClause(fact)
  }

  final override def getClausesForResolution
  : Option[(NormClause, Seq[UnitClause])] = {
    if (unitClauseDB.isEmpty || choicesQueue.isEmpty)
      None
    else
      Some(choicesQueue.dequeue())
  }

  override def handleNewUnitClause(electron: UnitClause): Unit = {
    //choicesQueue.incTime //todo: check where to use incTime

    val possibleChoices = clausesWithRelationInBody(electron.rs)

    // for each possible choice, fix electron.rs, and resolve against
    // all previous derivations of other body literals
    for (nucleus <- possibleChoices) {
      choicesQueue.incTime //todo: check where to use incTime
      // first find out if there are multiple occurrences of electron.rs
      val hasMultipleOccurrences = nucleus.body.count(_._1 == electron.rs) > 1

      val els = for ((rs, _) <- nucleus.body) yield {
        if (rs == electron.rs) {
          if (hasMultipleOccurrences)
            unitClauseDB.inferred(rs).getOrElse(Seq())
          else Seq(electron)
        } else unitClauseDB.inferred(rs).getOrElse(Seq())
      }
      for (choice <- Combinatorics.cartesianProduct(els.toList))
        choicesQueue enqueue ((nucleus, choice))
    }
  }

  override def handleForwardSubsumption(nucleus: NormClause,
                                        electrons: Seq[UnitClause]): Unit = {}

  override def handleBackwardSubsumption(subsumed: Set[UnitClause]): Unit = {
    // todo: future work
  }

  override def handleFalseConstraint(nucleus: NormClause,
                                     electrons: Seq[UnitClause]): Unit = {}

}
