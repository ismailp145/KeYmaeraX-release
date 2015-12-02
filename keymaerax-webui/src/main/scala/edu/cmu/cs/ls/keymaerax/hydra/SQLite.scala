/**
* Copyright (c) Carnegie Mellon University.
* See LICENSE.txt for the conditions of this license.
*/
package edu.cmu.cs.ls.keymaerax.hydra

import java.io.FileOutputStream
import java.nio.channels.Channels
import java.sql.SQLException

import _root_.edu.cmu.cs.ls.keymaerax.bellerophon.BelleExpr
import _root_.edu.cmu.cs.ls.keymaerax.core.{Provable, Sequent}
import edu.cmu.cs.ls.keymaerax.bellerophon.BelleExpr
import edu.cmu.cs.ls.keymaerax.core.{SuccPos, Formula, Provable, Sequent}
import edu.cmu.cs.ls.keymaerax.hydra.ExecutionStepStatus.ExecutionStepStatus
import edu.cmu.cs.ls.keymaerax.tactics.PosInExpr

import scala.collection.immutable.Nil

//import Tables.TacticonproofRow
import edu.cmu.cs.ls.keymaerax.api.KeYmaeraInterface
import scala.slick.jdbc.StaticQuery.interpolation
import scala.slick.driver.SQLiteDriver.simple._
import scala.slick.lifted.{ProvenShape, ForeignKeyQuery}
import edu.cmu.cs.ls.keymaerax.api.KeYmaeraInterface.PositionTacticAutomation

/**
 * Created by nfulton on 4/10/15.
 */
object SQLite {

  import Tables._

  val ProdDB: SQLiteDB = new SQLiteDB(DBAbstractionObj.dblocation)
  val TestDB: SQLiteDB = new SQLiteDB(DBAbstractionObj.testLocation)

  class SQLiteDB(dblocation: String) extends DBAbstraction {

    val sqldb = Database.forURL("jdbc:sqlite:" + dblocation, driver = "org.sqlite.JDBC")
    private var currentSession:Session = null
    private var nUpdates = 0
    private var nInserts = 0
    private var nSelects = 0

    implicit def session:Session = {
      if (currentSession == null || currentSession.conn.isClosed) {
        currentSession = sqldb.createSession()
        /* Enable write-ahead logging for SQLite - significantly improves write performance */
        sqlu"PRAGMA journal_mode = WAL".execute(currentSession)
        /* Note: Setting synchronous = NORMAL introduces some risk of database corruption during power loss. According
         * to official documentation, that risk is less than the risk of the hard drive failing completely, but we
         * should at least be aware that the risk exists. Initial testing showed this to be about 8 times faster, so
         * it seems worth the risk. */
        sqlu"PRAGMA synchronous = NORMAL".execute(currentSession)
        sqlu"VACUUM".execute(currentSession)
      }
      currentSession
    }

    private def ensureExists(location: String): Unit = {
      if (!new java.io.File(location).exists()) {
        cleanup(location)
      }
    }
    ensureExists(DBAbstractionObj.dblocation)
    ensureExists(DBAbstractionObj.testLocation)

    //@TODO
    // Configuration
    override def getAllConfigurations: Set[ConfigurationPOJO] =
      session.withTransaction({
        nSelects = nSelects + 1
        Config.list.filter(_.configname.isDefined).map(_.configname.get).map(getConfiguration(_)).toSet
      })

    override def createConfiguration(configName: String): Boolean =
      session.withTransaction({
        //This is unnecessary?
        true
      })

    private def blankOk(x: Option[String]): String = x match {
      case Some(y) => y
      case None => ""
    }

    override def getModelList(userId: String): List[ModelPOJO] = {
      session.withTransaction({
        nSelects = nSelects + 1
        Models.filter(_.userid === userId).list.map(element => new ModelPOJO(element._Id.get, element.userid.get, element.name.get,
          blankOk(element.date), blankOk(element.filecontents),
          blankOk(element.description), blankOk(element.publink), blankOk(element.title), element.tactic))
      })
    }

    override def createUser(username: String, password: String): Unit = {
      session.withTransaction({
        Users.map(u => (u.email.get, u.password.get))
          .insert((username, password))
        nInserts = nInserts + 1
      })}

    private def idgen(): String = java.util.UUID.randomUUID().toString()

    /**
      * Poorly names -- either update the config, or else insert an existing key.
      * But in Mongo it was just update, because of the nested documents thing.
      * @param config
      */
    override def updateConfiguration(config: ConfigurationPOJO): Unit =
      session.withTransaction({
        config.config.map(kvp => {
          val key = kvp._1
          val value = kvp._2
          nSelects = nSelects + 1
          val configExists = Config.filter(c => c.configname === config.name && c.key === key).list.length != 0

          if (configExists) {
            val q = for {l <- Config if (l.configname === config.name && l.key === key)} yield l.value
            q.update(Some(value))
            nUpdates = nUpdates + 1
          }
          else {
            Config.map(c => (c.configname.get, c.key.get, c.value.get))
              .insert((config.name, key, value))
            nInserts = nInserts + 1
          }
        })
      })

    //Proofs and Proof Nodes
    override def getProofInfo(proofId: Int): ProofPOJO =
      session.withTransaction({
        val stepCount = getProofSteps(proofId).size
        nSelects = nSelects + 1
        val list = Proofs.filter(_._Id.get === proofId)
          .list
          .map(p => new ProofPOJO(p._Id.get, p.modelid.get, blankOk(p.name), blankOk(p.description),
            blankOk(p.date), stepCount, p.closed.getOrElse(0) == 1))
        if (list.length > 1) throw new Exception()
        else if (list.length == 0) throw new Exception()
        else list.head
      })

    // Users
    override def userExists(username: String): Boolean =
      session.withTransaction({
        nSelects = nSelects + 1
        Users.filter(_.email === username).list.length != 0
      })


    override def getProofsForUser(userId: String): List[(ProofPOJO, String)] =
      session.withTransaction({
        val models = getModelList(userId)

        models.map(model => {
          val modelName = model.name
          val proofs = getProofsForModel(model.modelId)
          proofs.map((_, modelName))
        }).flatten
      })

    override def checkPassword(username: String, password: String): Boolean =
      session.withTransaction({
        nSelects = nSelects + 1
        Users.filter(_.email === username).filter(_.password === password).list.length != 0
      })

    override def updateProofInfo(proof: ProofPOJO): Unit =
      session.withTransaction({
        nSelects = nSelects + 1
        Proofs.filter(_._Id.get === proof.proofId).update(proofPojoToRow(proof))
        nUpdates = nUpdates + 1
      })

    override def updateProofName(proofId: Int, newName: String): Unit = {
      session.withTransaction({
        nSelects = nSelects + 1
        Proofs.filter(_._Id.get === proofId).map(_.name).update(Some(newName))
        nUpdates = nUpdates + 1
      })
    }

    //@todo actually these sorts of methods are rather dangerous because any schema change could mess this up.
    private def proofPojoToRow(p: ProofPOJO): ProofsRow = new ProofsRow(Some(p.proofId), Some(p.modelId), Some(p.name), Some(p.description), Some(p.date), Some(if (p.closed) 1 else 0))


    //the string is a model name.
    override def openProofs(userId: String): List[ProofPOJO] =
      session.withTransaction({
        nSelects = nSelects + 1
        getProofsForUser(userId).map(_._1).filter(!_.closed)
      })

    private def sqliteBoolToBoolean(x: Int) = if (x == 0) false else if (x == 1) true else throw new Exception()

    //returns id of create object
    override def getProofsForModel(modelId: Int): List[ProofPOJO] =
      session.withTransaction({
        nSelects = nSelects + 1
        Proofs.filter(_.modelid === modelId).list.map(p => {
          //        val stepCount : Int = Tacticonproof.filter(_.proofid === p.proofid.get).list.count
          val stepCount = 0 //@todo after everything else is done implement this.
          val closed: Boolean = sqliteBoolToBoolean(p.closed.getOrElse(0))
          new ProofPOJO(p._Id.get, p.modelid.get, blankOk(p.name), blankOk(p.description), blankOk(p.date), stepCount, closed)
        })
      })


    //Models
    override def createModel(userId: String, name: String, fileContents: String, date: String,
                             description: Option[String] = None, publink: Option[String] = None,
                             title: Option[String] = None, tactic: Option[String] = None): Option[Int] =
      session.withTransaction({
        nSelects = nSelects + 1
        if (Models.filter(_.userid === userId).filter(_.name === name).list.length == 0) {
          nInserts = nInserts + 1
          Some((Models.map(m => (m.userid.get, m.name.get, m.filecontents.get, m.date.get, m.description, m.publink, m.title, m.tactic))
            returning Models.map(_._Id.get))
            .insert(userId, name, fileContents, date, description, publink, title, tactic))
        }
        else None
      })

    override def createProofForModel(modelId: Int, name: String, description: String, date: String): Int =
      session.withTransaction({
        nInserts = nInserts + 1
        (Proofs.map(p => ( p.modelid.get, p.name.get, p.description.get, p.date.get, p.closed.get))
          returning Proofs.map(_._Id.get))
          .insert(modelId, name, description, date, 0)
      })

    override def getModel(modelId: Int): ModelPOJO =
      session.withTransaction({
        nSelects = nSelects + 1
        val models =
          Models.filter(_._Id.get === modelId)
            .list
            .map(m => new ModelPOJO(
              m._Id.get, m.userid.get, blankOk(m.name), blankOk(m.date), m.filecontents.get, blankOk(m.description), blankOk(m.publink), blankOk(m.title), m.tactic
            ))

        if (models.length < 1) throw new Exception("getModel type should be an Option")
        else if (models.length == 1) models.head
        else throw new Exception("Primary keys aren't unique in models table.")
      })

    override def getUsername(uid: String): String =
      uid

    private def optToString[T](o: Option[T]) = o match {
      case Some(x) => Some(x.toString())
      case None => None
    }

    override def getConfiguration(configName: String): ConfigurationPOJO =
      session.withTransaction({
        nSelects = nSelects + 1
        val kvp = Config.filter(_.configname === configName)
          .filter(_.key.isDefined)
          .list
          .map(conf => (conf.key.get, blankOk(conf.value)))
          .toMap

        new ConfigurationPOJO(configName, kvp)
      })

    /**
      * Initializes a new database.
      */
    override def cleanup (): Unit = { cleanup(DBAbstractionObj.dblocation)}
    def cleanup(which: String): Unit = {
      val dbFile = this.getClass.getResourceAsStream("/keymaerax.sqlite")
      val target = new java.io.File(which)
      val targetStream = new FileOutputStream(target)
      targetStream.getChannel.transferFrom(Channels.newChannel(dbFile), 0, Long.MaxValue)
      targetStream.close()
      dbFile.close()
    }

    /** Deletes an execution from the database */
    override def deleteExecution(executionId: Int): Unit = ???

    /** Creates a new execution and returns the new ID in tacticExecutions */
    override def createExecution(proofId: Int): Int =
      session.withTransaction({
        val executionId =
          (Tacticexecutions.map(te => te.proofid.get)
            returning Tacticexecutions.map(_._Id.get))
            .insert(proofId)
        nInserts = nInserts + 1
        executionId
      })

    /** Deletes a provable and all associated sequents / formulas */
    override def deleteProvable(provableId: Int): Unit = ???

    /**
      * Adds an execution step to an existing execution
      * @note Implementations should enforce additional invarants -- never insert when branches or alt orderings overlap.
      */
    override def addExecutionStep(step: ExecutionStepPOJO): Int = {
      val (branchOrder: Int, branchLabel) = (step.branchOrder, step.branchLabel) match {
        case (None, None) => (null, null)
        case (Some(order), None) => (order, null)
        case (None, Some(label)) => (null, label)
        case (Some(order), Some(label)) =>
          throw new Exception("execution steps cannot have both a branchOrder and a branchLabel")
      }
      session.withTransaction({
        val status = ExecutionStepStatus.toString(step.status)
        val steps =
          Executionsteps.map({case step => (step.executionid.get, step.previousstep, step.parentstep,
            step.branchorder.get, step.branchlabel.get, step.alternativeorder.get, step.status.get, step.executableid.get,
            step.inputprovableid.get, step.resultprovableid, step.userexecuted.get)
          }) returning Executionsteps.map(es => es._Id.get)
        val stepId = steps
            .insert((step.executionId, step.previousStep, step.parentStep, branchOrder, branchLabel,
              step.alternativeOrder, status, step.executableId, step.inputProvableId, step.resultProvableId,
              step.userExecuted.toString))
        nInserts = nInserts + 1
        stepId
      })
    }

    /** Adds a Bellerophon expression as an executable and returns the new executableId */
    override def addBelleExpr(expr: BelleExpr, params: List[ParameterPOJO]): Int =
      session.withTransaction({
        // @TODO Figure out whether to generate ID's here or pass them in through the params
        val executableId =
          (Executables.map({ case exe => (exe.scalatacticid, exe.belleexpr) })
            returning Executables.map(_._Id.get))
          .insert((None, Some(expr.toString)))
        nInserts = nInserts + 1
        val paramTable = Executableparameter.map({ case param => (param.executableid.get, param.idx.get,
          param.valuetype.get, param.value.get)
        })
        for (i <- params.indices) {
          nInserts = nInserts + 1
          paramTable.insert((executableId, i, params(i).valueType.toString, params(i).value))
        }
        executableId
      })

    def serializeSequent(sequent: Sequent, provableId: Int, subgoal: Option[Int]): Unit = {
      val ante = sequent.ante
      val succ = sequent.succ
      val sequentId =
        (Sequents.map({ case sequent => (sequent.provableid.get, sequent.idx) }) returning Sequents.map(_._Id.get))
          .insert(provableId, subgoal)
      nInserts = nInserts + 1
      val formulas = Sequentformulas.map({ case fml => (fml.sequentid.get,
        fml.isante.get, fml.idx.get, fml.formula.get)
      })
      for (i <- ante.indices) {
        nInserts = nInserts + 1
        formulas.insert((sequentId, true.toString, i, ante(i).toString))
      }
      for (i <- succ.indices) {
        nInserts = nInserts + 1
        formulas.insert((sequentId, false.toString, i, succ(i).toString))
      }
    }

    /** Stores a Provable in the database and returns its ID */
    override def serializeProvable(p: Provable): Int = {
      session.withTransaction({
        /* Working around bug in slick: The natural thing to write would be insert() without any arguments, but
        * that generates an ill-formed SQL statement, so let's explicitly insert a row with a null conclusion - it
        * does the same thing but generates SQL that parses.*/
        val provableId =
          (Provables.map({ case provable => provable._Id}) returning Provables.map(_._Id.get))
            .insert(None)
        serializeSequent(p.conclusion, provableId, None)
        for(i <- p.subgoals.indices) {
          serializeSequent(p.subgoals(i), provableId, Some(i))
        }
        provableId
      })
    }

    /** Returns the executable with ID executableId */
    override def getExecutable(executableId: Int): ExecutablePOJO =
      session.withTransaction({
        nSelects = nSelects + 1
        val executables =
          Executables.filter(_._Id === executableId)
            .list
            .map(exe => new ExecutablePOJO(exe._Id.get, exe.scalatacticid, exe.belleexpr))
        if (executables.length < 1) throw new Exception("getExecutable type should be an Option")
        else if (executables.length == 1) executables.head
        else throw new Exception("Primary keys aren't unique in executables table.")
      })

    /** Use escape hatch in prover core to create a new Provable */
    override def loadProvable(provableId: Int): Sequent = ???

    override def getExecutionSteps(executionID: Int): List[ExecutionStepPOJO] = {
      session.withTransaction({
        nSelects = nSelects + 1
        val steps =
          Executionsteps.filter(_.executionid === executionID)
            .list
            .map(step => new ExecutionStepPOJO(step._Id, step.executionid.get, step.previousstep, step.parentstep,
              step.branchorder, step.branchlabel, step.alternativeorder.get, ExecutionStepStatus.fromString(step.status.get),
              step.executableid.get, step.inputprovableid.get, step.resultprovableid, step.userexecuted.get.toBoolean))
        if (steps.length < 1) throw new Exception("No steps found for execution " + executionID)
        else steps
      })
    }

    /** Adds a new scala tactic and returns the resulting id */
    /*@TODO Understand whether to use the ID passed in or generate our own*/
    override def addScalaTactic(scalaTactic: ScalaTacticPOJO): Int = {
      session.withTransaction({
        (Scalatactics.map({ case tactic => tactic.location.get })
          returning Scalatactics.map(_._Id.get))
          .insert(scalaTactic.location)
      })
    }

    /** @TODO Clarify spec for this function. Questions:
      *       Top-level rules only?
      *       Branches?
      *       Alternatives?
      *       Does order matter?
      *       What's in each string? */
    override def getProofSteps(proofId: Int): List[String] = ???

    /** Adds a built-in tactic application using a set of parameters */
    override def addAppliedScalaTactic(scalaTacticId: Int, params: List[ParameterPOJO]): Int = {
      session.withTransaction({
        val executableId =
          (Executables.map({ case exe => ( exe.scalatacticid, exe.belleexpr)})
            returning Executables.map(_._Id.get))
            .insert(Some(scalaTacticId), None)
        val paramTable = Executableparameter.map({ case param => (param.executableid.get, param.idx.get,
          param.valuetype.get, param.value.get)
        })
        for (i <- params.indices) {
          paramTable.insert((executableId, i, params(i).valueType.toString, params(i).value))
        }
        executableId
      })
    }

    /** Updates an executable step's status. @note should not be transitive */
    override def updateExecutionStatus(executionStepId: Int, status: ExecutionStepStatus): Unit = {
      val newStatus = ExecutionStepStatus.toString(status)
      session.withTransaction({
        nSelects = nSelects + 1
        nUpdates = nUpdates + 1
        Executionsteps.filter(_._Id === executionStepId).map(_.status).update(Some(newStatus))
      })
    }

    private def sortFormulas(fromAnte: Boolean, formulas: List[SequentFormulaPOJO]): List[Formula] = {
      import edu.cmu.cs.ls.keymaerax.parser.StringConverter._
      val relevant = formulas.filter({ case formula => fromAnte == formula.isAnte })
      val sorted = relevant.sortWith({ case (f1, f2) => f1.index > f2.index })
      sorted.map({ case formula => formula.formulaStr.asFormula })
    }

    def getSequent(sequentId: Int)(implicit session: Session): Sequent = {
      val formulas =
        Sequentformulas.filter(_.sequentid === sequentId)
          .list
          .map(formula => new SequentFormulaPOJO(formula._Id.get, formula.sequentid.get,
            formula.isante.get.toBoolean, formula.idx.get, formula.formula.get))
      val ante = sortFormulas(fromAnte = true, formulas).toIndexedSeq
      val succ = sortFormulas(fromAnte = false, formulas).toIndexedSeq
      Sequent(null, ante, succ)
    }

    def getSequents(provableId: Int): (List[Sequent], Sequent) = {
      session.withTransaction({
        nSelects = nSelects + 1
        val sequents =
          Sequents.filter(_.provableid === provableId)
            .list
            .map({ case sequent => (sequent._Id.get, sequent.idx) })
        val (conclusions, subgoals) =
          sequents.partition({case (id, idx) => idx.isEmpty})
        if(conclusions.length != 1)
          throw new Exception("Provable should have exactly one conclusion in getSequents")
        val conclusion = conclusions.head
        val conclusionSequent = getSequent(conclusion._1)
        val sortedSubgoals = subgoals.sortWith({case (x, y) => x._2.get < y._2.get}).map({case (x, _) => x})
        var revSequents: List[Sequent] = Nil
        for (i <- sortedSubgoals.indices) {
          revSequents = getSequent(sortedSubgoals(i)) :: revSequents
        }
        (revSequents.reverse, conclusionSequent)
      })
    }

    /** Gets the conclusion of a provable */
    override def getConclusion(provableId: Int): Sequent = {
      getSequents(provableId)._2
    }

    def printStats = {
      println("Updates: " + nUpdates + " Inserts: " + nInserts + " Selects: " + nSelects)
    }

    def proofSteps(executionId: Int): List[ExecutionStepPOJO] = {
      session.withTransaction({
        var steps = Executionsteps.filter(_.executionid === executionId).list
        var prevId: Option[Int] = None
        var revResult: List[ExecutionStepPOJO] = Nil
        while(steps != Nil) {
          val (headSteps, tailSteps) = steps.partition({step => step.previousstep == prevId})
          if (headSteps == Nil)
            return revResult.reverse
          val headsByAlternative =
            headSteps.sortWith({case (x, y) => y.alternativeorder.get < x.alternativeorder.get})
          val head = headsByAlternative.head
          revResult =
            new ExecutionStepPOJO(head._Id, head.executionid.get, head.previousstep, head.parentstep,
              head.branchorder, head.branchlabel, head.alternativeorder.get, ExecutionStepStatus.fromString(head.status.get),
              head.executableid.get, head.inputprovableid.get, head.resultprovableid, head.userexecuted.get.toBoolean)::revResult
          prevId = head._Id
          steps = tailSteps
        }
        revResult.reverse
      })
    }

    var nodeId = 0

    case class TreeNode(sequent: Sequent, parent: Option[TreeNode])  {
      val id = nodeId
      nodeId = nodeId + 1
      var children: List[TreeNode] = Nil
    }

    case class Tree (id: String, nodes: List[TreeNode], root: TreeNode)
    case class AgendaItem (id: String, name: String, proofId: String, goal: TreeNode, path: List[String])

    def proofTree(executionId: Int) = {
      val steps = proofSteps(executionId)
      if (steps.isEmpty) {
        throw new Exception("Tried to get proof tree for empty execution with ID " + executionId)
      }
      val initialProvable = steps.head.inputProvableId
      var openGoals : List[TreeNode] = ???
      ???
      /*
      * {proofTree:
      *  {id: proofId
      *   nodes: List[
      *     {id: nodeId
      *      sequent:
      *        {ante: List[{id: formulaId formula: rec t. {name: string, children: Option[List[t]]}}]
      *         succ: <same>}
      *      children: List[nodeId]
      *      parent: Option[nodeId]}
      *   root: nodeId}
      *  agendaItems:
      *  {<the nodeId>:
      *    {id: itemId
      *     name: String
      *     proofId: proofId
      *     goal: nodeId
      *     path: List[Something??]
      *  */
    }
  }
}