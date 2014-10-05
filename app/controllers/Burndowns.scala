package controllers

import play.api._
import play.api.mvc._
import anorm._
import play.api.db.DB
import play.api.libs.json._
import play.api.Play.current
import org.joda.time.DateTime
import org.joda.time.format._
import utility.AnormExtension._
import securesocial.core.RuntimeEnvironment
import securesocial.core.SecureSocial
import service.PhabUser

import phabricator.Phabricator

import models.CompositeProject
import models.BurndownTask

import scala.util.{ Try, Success, Failure }

object Burndowns {
  case class HistoricTaskEstimates(taskID: String, estimates: List[Long]) {}
  case class DatedEstimate(d: DateTime, e: Option[Long]) {
    override def toString() = {
      val dtfOut = DateTimeFormat.forPattern("MM/dd/yyyy")
      s"${dtfOut.print(d)} # ${e.getOrElse(0)}"
    }
  }

}

class Burndowns(override implicit val env: RuntimeEnvironment[PhabUser]) extends Controller with SecureSocial[PhabUser] {

  implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

  def index = SecuredAction { implicit request =>
    val projects = Phabricator.listAllProjects

    val composites = CompositeProject.listAll()

    Ok(views.html.burndown_index(projects, composites))
  }

  def extractProjectIDs(projectBase: String) = {
    if (projectBase.head == '{' && projectBase.takeRight(1) == "}") {
      val trimmed = projectBase.tail.dropRight(1);
      val result = trimmed.split(",").toList
      result
    } else {
      // Assume it's not a composite project
      List(projectBase)
    }
  }

  def detectCompositeBurndown(projectIDs: List[String]): Option[Long] = {
    val candidateProjects = CompositeProject.listAll

    val matches = candidateProjects.filter(cp => {
      cp.projectIDs.toSet.equals(projectIDs.toSet)
    })

    matches.headOption.fold(CompositeProject.create(projectIDs))(cp => Option(cp.id))
  }

  def burndownByProject(projectBase: String) = SecuredAction { implicit request =>

    val projectIDs = extractProjectIDs(projectBase)

    val existingComposite = detectCompositeBurndown(projectIDs)

    val allProjects = Phabricator.listAllProjects

    val tasks = projectIDs.map(pid => Phabricator.openTasksByProjectID(pid)).toList.flatten

    val (needsTriage, normalTasks) = tasks.partition(t => t.priority == 90 || t.hours.isEmpty)

    val sortedTasks = (needsTriage ++ normalTasks).distinct

    val hoursToBurn = sortedTasks.map(_.hours.getOrElse("0").toInt).sum

    (Play.current.configuration.getString("phabricator.url"), existingComposite) match {
      case (Some(phabricatorUrl), Some(existingCompositeID)) => {
        // TODO: DANGER! get needs to be handled correctly
        val composite = CompositeProject.getByID(existingCompositeID)

        composite match {
          case Some(c) => {
            val title = c.compositeName
            val commitDateAsText = c.targetDateString()

            Ok(views.html.burndown_by_project(existingCompositeID, c, sortedTasks,
              phabricatorUrl, hoursToBurn, c.getBurndownCount() > 0))
          }
          case None => BadRequest("could not find composite project")
        }

      }
      case (None, _) => BadRequest("phabricator.url not configured")
      case (_, None) => BadRequest("could not create project composite")
    }
  }

  def projectsFromComposite(compositeID: Long) = {
    DB.withConnection("default") { implicit c =>

      val sqlQuery = SQL(
        """
          SELECT phid 
          FROM composite_projects
          WHERE composite_id={composite_id}
          ;
        """).on('composite_id -> compositeID)

      val projects = sqlQuery().map(row => {
        row[String]("phid")
      }).toList

      projects
    }
  }

  def saveTasksForSnapshot(snapshotID: Long, tasks: List[Phabricator.Task]) = {
    DB.withConnection("default") { implicit c =>
      val insertQuery = SQL("""
          INSERT INTO burndown_tasks (burndown_id, task_id, remaining_estimate) 
            VALUES ({snapshot_id}, {task_id}, {estimate});
        """)

      tasks.foreach(t => {
        val estimate = t.hours.fold(0L)(_.toLong)

        insertQuery.on('snapshot_id -> snapshotID, 'task_id -> t.taskID, 'estimate -> estimate).executeInsert()
      })
    }
  }

  def saveSnapshotViaAjax(compositeKey: String) = SecuredAction(parse.json) { implicit request =>
    // TODO: this is brittle
    val compositeID = compositeKey.toLong

    val composite = CompositeProject.getByID(compositeID)

    composite match {
      case None => BadRequest(Json.obj("status" -> "KO", "message" -> "invalid composite id"))
      case Some(c) => {
        val snapshot = c.saveBurndownSnapshot

        snapshot match {
          case None => BadRequest(Json.obj("status" -> "KO", "message" -> "couldn't create snapshot"))
          case Some(snapshotID) => {
            val projectIDs = projectsFromComposite(compositeID)

            val tasks = projectIDs.map(pid => Phabricator.openTasksByProjectID(pid)).toList.flatten.distinct

            val estimatedTasks = tasks.filter(_.hours.isDefined)

            saveTasksForSnapshot(snapshotID, estimatedTasks)

            Ok(Json.obj("result" -> "saved"))
          }
        }
      }
    }

  }

  case class CompositeMetadata(name: String, target_date: String) {}
  object CompositeMetadata {
    implicit val format = Json.format[CompositeMetadata]
  }

  def updateMetadata(compositeID: Long, metadata: CompositeMetadata): Try[Int] = {
    val newName = if (metadata.name.trim().length() == 0) {
      None
    } else {
      Option(metadata.name.trim())
    }
    val dtfIn = DateTimeFormat.forPattern("yyyy-MM-dd")

    val newTarget = if (metadata.target_date.trim().length() == 0) {
      Try(None)
    } else {
      // TODO: catch the exception here
      val d = Try(dtfIn.parseDateTime(metadata.target_date))

      d.flatMap(e => Try(Option(e)))
      //      Option(d)
    }

    newTarget.flatMap(t => {

      val result = DB.withConnection("default") { implicit c =>
        val updateQuery = SQL(
          """
            UPDATE composite SET name={name},target_date={targetDate}
            WHERE id={id};
          """).on('name -> newName, 'targetDate -> t, 'id -> compositeID)

        updateQuery.executeUpdate()
      }

      Success(result)
    })
  }

  def updateCompositeMetadataViaAjax(compositeKey: String) = SecuredAction(parse.json) { implicit request =>

    // TODO: this is brittle
    val compositeID = compositeKey.toLong

    val composeMetadataRequest = request.body.validate[CompositeMetadata]
    composeMetadataRequest.fold(
      errors => {
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toFlatJson(errors)))
      },
      metadata => {

        updateMetadata(compositeID, metadata) match {
          case Success(_) => Ok(Json.obj("status" -> "OK", "message" -> (s"Composite ${compositeKey} saved.")))
          case Failure(f) => BadRequest(Json.obj("status" -> "KO", "message" -> f.getMessage()))
        }

      })
  }

  def generateBalancedEstimateMatrix(allDates: List[DateTime],
    allTasks: List[String],
    estimatesByTask: Map[String, Map[DateTime, Long]]) = {

    val minTasks = estimatesByTask.map {
      case (taskID, es) => {
        val minDate = es.reduce((a, b) => if (a._1.isBefore(b._1)) { a } else { b })._1
        (taskID -> minDate)
      }
    }

    def missingReplacement(task: String, date: DateTime) = {
      minTasks.get(task).fold(Option(0L))(minDate =>
        if (minDate.isBefore(date)) {
          Option(0L)
        } else {
          None
        })
    }

    for {
      task <- allTasks
      date <- allDates

      taskEstByDate = estimatesByTask.getOrElse(task, Map())
      finalEstimate = taskEstByDate.get(date).orElse(missingReplacement(task, date))

    } yield (task, Burndowns.DatedEstimate(date, finalEstimate))

  }

  def getBurndownTasks(composite: CompositeProject) = {
    val allEstimates = composite.getTasksWithEstimates()

    val estimatesByTask = allEstimates.groupBy(_.taskID).map(est => {
      val taskID = est._1
      val estMap = est._2.map(x => {
        (x.timestamp -> x.estimate)
      }).toMap

      (taskID -> estMap)
    })

    def mappedDistinct[B](f: BurndownTask => B): List[B] = allEstimates.map(f).distinct.toList

    val allDates = mappedDistinct(_.timestamp)
    val allTasks = mappedDistinct(_.taskID)

    val finalMatrixList = generateBalancedEstimateMatrix(allDates, allTasks, estimatesByTask)

    val nextMatrix = finalMatrixList.groupBy(_._1)

    val mappedMatrix = nextMatrix.map(e => (e._1, e._2.map(_._2).sortBy(_.d))).toList

    def emptyCount(l: List[Burndowns.DatedEstimate]) = l.filter(_.e.isEmpty).length

    def matrixSorter(a: (String, List[Burndowns.DatedEstimate]), b: (String, List[Burndowns.DatedEstimate])) = {

      val aNones = emptyCount(a._2)
      val bNones = emptyCount(b._2)

      if (aNones == bNones) {
        a._1 < b._1
      } else {
        aNones < bNones
      }
    }

    val sortedMatrix = mappedMatrix.sortWith(matrixSorter)

    (sortedMatrix, allDates)
  }

  def generateSummaries(estimateMatrix: List[(String, List[Burndowns.DatedEstimate])]) = {
    val estimatesByDate = estimateMatrix.map(_._2).flatten.groupBy(_.d)

    val sumsByDate = estimatesByDate.map(ed => {
      val sum = ed._2.map(_.e.getOrElse(0L)).sum

      Burndowns.DatedEstimate(ed._1, Option(sum))
    }).toList.sortBy(_.d)

    sumsByDate
  }

  case class TaskWithBoundaries(taskID: String, maximumEstimate: Burndowns.DatedEstimate, latestEstimate: Burndowns.DatedEstimate) {}

  def burndownData(compositeID: String) = SecuredAction { implicit request =>

    val compositeOption = CompositeProject.getByID(compositeID.toLong)

    compositeOption.fold(
      BadRequest(Json.obj("status" -> "KO", "message" -> "invalid composite id")))({
        c =>

          val (estimateMatrix, dates) = getBurndownTasks(c)

          val summaries = generateSummaries(estimateMatrix)

          val trend = if (summaries.isEmpty) {
            Nil
          } else {
            val summariesOffset = summaries.head :: (summaries.dropRight(1))
            summaries.zip(summariesOffset).map {
              case (thisWeekD, lastWeekD) => {
                val thisWeek = thisWeekD.e.getOrElse(0L)
                val lastWeek = lastWeekD.e.getOrElse(0L)

                thisWeek - lastWeek
              }
            }
          }

          val compositeProjectName = c.compositeName
          val featureProgress = c.featureProgress

          val timeSpent = c.timeProgress

          Ok(views.html.burndown_history(compositeProjectName, c.projectCluster, estimateMatrix, dates, summaries, trend, featureProgress, timeSpent))
      })
  }

}