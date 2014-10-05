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

object Burndowns {
  case class ProjectStub(name: String, phid: String)
  case class HistoricTaskEstimates(taskID: String, estimates: List[Long]) {}
  case class DatedEstimate(d: DateTime, e: Option[Long]) {
    override def toString() = {
      val dtfOut = DateTimeFormat.forPattern("MM/dd/yyyy")
      s"${dtfOut.print(d)} # ${e.getOrElse(0)}"
    }
  }
  case class Task(taskID: String, title: String, priority: Long, hours: Option[String], assignee: Option[String])
  case class TaskEntry(taskID: String, estimate: Long, timestamp: DateTime)

  case class CompositeProject(id: Long, projectIDs: List[String], name: Option[String], targetDate: Option[DateTime]) {
    def projectCluster = {
      s"{${projectIDs.mkString(",")}}"
    }

    def compositeName = {
      name match {
        case Some(n) => n
        case None => {
          val allProjects = listAllProjects

          val filteredStubs = allProjects.filter(p => projectIDs.contains(p.phid))

          filteredStubs.map(_.name).mkString(" / ")
        }
      }

    }

    def targetDateString(): Option[String] = {
      targetDate flatMap { d =>
        val dtfOut = DateTimeFormat.forPattern("yyyy-MM-dd")
        Some(dtfOut.print(d))
      }
    }
  }

  def listAllProjects = {
    DB.withConnection("phabricator") { implicit c =>

      val sqlQuery = SQL(
        """
          SELECT name, phid 
          FROM phabricator_project.project
          
          WHERE status=0
          ORDER BY dateCreated DESC;
        """)

      val projects = sqlQuery().map(row =>
        ProjectStub(row[String]("name"), row[String]("phid"))).toList

      projects
    }

  }

}

class Burndowns(override implicit val env: RuntimeEnvironment[PhabUser]) extends Controller with SecureSocial[PhabUser] {

  implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

  def openTasksByProject(projectID: String) = {
    val estimatedHoursKey = Play.current.configuration.getString("phabricator.estimatedHoursKey")

    DB.withConnection("phabricator") { implicit c =>
      val tasksQuery = SQL(
        """
        select concat('T',task.id) taskID, status, priority, title, description, projectPHIDs,
        	 cf.`fieldValue` as estimation, u.username as AssignedTo
        from phabricator_maniphest.maniphest_task task
        	left join phabricator_maniphest.maniphest_customfieldstorage cf 
        		on task.phid=cf.`objectPHID` and fieldIndex={estHours}
        	left join phabricator_user.user u 
        		on u.phid = task.`ownerPHID`
          left join phabricator_maniphest.edge e 
        		on e.type=41 AND e.src=task.phid
        where
        	status IN ('open') 
         	AND priority > 25
        	AND (projectPHIDs LIKE {projectID}
            OR e.dst LIKE {projectID})
        order by task.id asc;
        ;
      """).on('projectID -> s"%$projectID%", 'estHours -> estimatedHoursKey)

      val tasks = tasksQuery().map(row =>
        Burndowns.Task(row[String]("taskID"),
          row[String]("title"),
          row[Long]("priority"),
          row[Option[String]]("maniphest_customfieldstorage.fieldValue"),
          row[Option[String]]("user.userName")))

      tasks.toList
    }

  }

  def index = SecuredAction { implicit request =>
    val projects = Burndowns.listAllProjects

    val composites = listAllComposites

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

  def listAllComposites() = {
    DB.withConnection("default") { implicit c =>

      val sqlQuery = SQL(
        """
          SELECT composite_id, GROUP_CONCAT(phid) AS phids, name, target_date
          FROM composite_projects cp
            LEFT JOIN composite c on c.id=cp.composite_id
          GROUP BY composite_id
          ;
        """)

      val projects = sqlQuery().map(row => {
        val phids = row[String]("phids").split(",").toList
        val name = row[Option[String]]("name")
        val targetDate = row[Option[DateTime]]("target_date")

        Burndowns.CompositeProject(row[Long]("composite_id"), phids, name, targetDate)
      }).toList

      projects
    }
  }

  def compositeByID(compositeID: Long) = {
    DB.withConnection("default") { implicit c =>

      val sqlQuery = SQL(
        """
          SELECT composite_id, GROUP_CONCAT(phid) AS phids, name, target_date
          FROM composite_projects cp
            LEFT JOIN composite c on c.id=cp.composite_id 
          WHERE composite_id={composite_id}
          GROUP BY composite_id
          ;
        """).on('composite_id -> compositeID)

      val project = sqlQuery().map(row => {
        val phids = row[String]("phids").split(",").toList
        val name = row[Option[String]]("name")
        val targetDate = row[Option[DateTime]]("target_date")

        Burndowns.CompositeProject(row[Long]("composite_id"), phids, name, targetDate)
      }).toList.headOption

      project
    }
  }

  def detectCompositeBurndown(projectIDs: List[String]): Option[Long] = {
    val candidateProjects = listAllComposites

    val matches = candidateProjects.filter(cp => {
      cp.projectIDs.toSet.equals(projectIDs.toSet)
    })

    matches.headOption.fold(createNewComposite(projectIDs))(cp => Option(cp.id))
  }

  def createNewComposite(projectIDs: List[String]) = {
    DB.withConnection("default") { implicit c =>

      val newIDopt = SQL(
        """
          INSERT INTO composite VALUES ();
        """).executeInsert()

      // Note, this causes newIDopt to be the return
      // which is intentional but probably could be clearer
      newIDopt.flatMap(newID => {

        val insertQuery = SQL(
          """
            INSERT INTO composite_projects (composite_id, phid) 
            VALUES ({newComposite}, {phid});
          """)

        projectIDs.foreach(pid => {
          insertQuery.on('newComposite -> newID, 'phid -> pid).executeInsert()
        })

        Some(newID)
      })
    }
  }

  def getBurndownCount(compositeID: Long) = {

    DB.withConnection("default") { implicit c =>

      val count = SQL(
        """
          SELECT count(id) as previous_count
          FROM burndowns b
          WHERE b.composite_id={composite_id}
          ;
        """).on('composite_id -> compositeID).as(SqlParser.long("previous_count").single)

      count
    }
  }

  def burndownByProject(projectBase: String) = SecuredAction { implicit request =>

    val projectIDs = extractProjectIDs(projectBase)

    val existingComposite = detectCompositeBurndown(projectIDs)

    val allProjects = Burndowns.listAllProjects

    val tasks = projectIDs.map(pid => openTasksByProject(pid)).toList.flatten

    val (needsTriage, normalTasks) = tasks.partition(t => t.priority == 90 || t.hours.isEmpty)

    val sortedTasks = (needsTriage ++ normalTasks).distinct

    val hoursToBurn = sortedTasks.map(_.hours.getOrElse("0").toInt).sum

    (Play.current.configuration.getString("phabricator.url"), existingComposite) match {
      case (Some(phabricatorUrl), Some(existingCompositeID)) => {
        // TODO: DANGER! get needs to be handled correctly
        val composite = compositeByID(existingCompositeID)

        composite match {
          case Some(c) => {
            val title = c.compositeName
            val commitDateAsText = c.targetDateString()

            Ok(views.html.burndown_by_project(existingCompositeID, c, sortedTasks,
              phabricatorUrl, hoursToBurn, getBurndownCount(existingCompositeID) > 0))
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

  def createNewSnapshot(compositeID: Long) = {
    DB.withConnection("default") { implicit c =>

      SQL(
        """
          INSERT INTO burndowns (timestamp, composite_id) VALUES (NOW(), {composite});
        """).on('composite -> compositeID).executeInsert()
    }
  }

  def saveTasksForSnapshot(snapshotID: Long, tasks: List[Burndowns.Task]) = {
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

    val snapshot = createNewSnapshot(compositeID)

    snapshot match {
      case None => BadRequest(Json.obj("result" -> "couldn't create snapshot"))
      case Some(snapshotID) => {
        val projectIDs = projectsFromComposite(compositeID)

        val tasks = projectIDs.map(pid => openTasksByProject(pid)).toList.flatten.distinct

        val estimatedTasks = tasks.filter(_.hours.isDefined)

        saveTasksForSnapshot(snapshotID, estimatedTasks)

        Ok(Json.obj("result" -> "saved"))
      }
    }
  }

  case class CompositeMetadata(name: String, target_date: String) {}
  object CompositeMetadata {
    implicit val format = Json.format[CompositeMetadata]
  }

  import scala.util.{ Try, Success, Failure }

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

  def getTasksWithEstimates(compositeID: String) = {
    DB.withConnection("default") { implicit c =>

      val sqlQuery = SQL(
        """
          SELECT task_id, remaining_estimate, timestamp
          FROM burndown_tasks bt
            LEFT JOIN  burndowns b on b.id=bt.burndown_id
          WHERE b.composite_id={composite_id}
          ;
        """).on('composite_id -> compositeID)

      val taskEstimates = sqlQuery().map(row => {
        Burndowns.TaskEntry(
          row[String]("task_id"),
          row[Long]("remaining_estimate"),
          row[DateTime]("timestamp"))

      }).toList

      taskEstimates
    }
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

  def getBurndownTasks(compositeID: String) = {
    val allEstimates = getTasksWithEstimates(compositeID)

    val estimatesByTask = allEstimates.groupBy(_.taskID).map(est => {
      val taskID = est._1
      val estMap = est._2.map(x => {
        (x.timestamp -> x.estimate)
      }).toMap

      (taskID -> estMap)
    })

    def mappedDistinct[B](f: Burndowns.TaskEntry => B): List[B] = allEstimates.map(f).distinct.toList

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

  // This should be moved to a configuration parameter
  val WORKDAY_HOURS = 7

  def allDates(current: DateTime, terminus: DateTime): List[DateTime] = {
    if (current.isBefore(terminus)) {
      val next = current.plusDays(1)
      current :: allDates(next, terminus)
    } else {
      Nil
    }
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

  def getTimeSpentOnProject(composite: Burndowns.CompositeProject, earliestEstimate: DateTime): Option[Int] = {
    composite.targetDate.flatMap(td => {
      if (td.isBefore(earliestEstimate)) {
        None
      } else {

        val dateRange = allDates(earliestEstimate, td)
        
        // Sat = 6, Sun = 7
        val workDays = dateRange.filter(_.getDayOfWeek() < 6)
        
        val elapsedDays = workDays.filter(_.isBefore(new DateTime))
        val numer = elapsedDays.map(_ => WORKDAY_HOURS).sum * 100
        val denom = workDays.map(_ => WORKDAY_HOURS).sum

        val asFraction = numer / denom

        Option(asFraction)
      }
    })
  }

  def getProjectProgress(estimateMatrix: List[(String, List[Burndowns.DatedEstimate])]): Int = {

    val maxInit = Burndowns.DatedEstimate(DateTime.now(), None)
    val curInit = Burndowns.DatedEstimate(new DateTime(0), None)

    val detectBoundaries = estimateMatrix.map(entry => {

      val taskID = entry._1
      val estimates = entry._2

      val finalResult = estimates.foldLeft(TaskWithBoundaries(taskID, maxInit, curInit)) {
        (acc, next) =>
          {
            val l = if (acc.latestEstimate.d.isBefore(next.d)) next else acc.latestEstimate

            val m = if (acc.maximumEstimate.e.getOrElse(-1L) < next.e.getOrElse(0L)) next else acc.maximumEstimate

            TaskWithBoundaries(acc.taskID, m, l)
          }
      }
      finalResult
    })

    case class EstiPair(max: Long, latest: Long)

    val summable = detectBoundaries.map(twb => {
      EstiPair(twb.maximumEstimate.e.getOrElse(0L), twb.latestEstimate.e.getOrElse(0L))
    })

    val sums = summable.reduce((a, b) => EstiPair(a.max + b.max, a.latest + b.latest))

    val denominator = sums.max
    val numerator = denominator - sums.latest

    val fraction = (numerator * 100 / denominator)

    fraction.toInt
  }

  def burndownData(compositeID: String) = SecuredAction { implicit request =>

    val (estimateMatrix, dates) = getBurndownTasks(compositeID: String)

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

    val composite = compositeByID(compositeID.toLong)

    val compositeProjectName = composite.fold("unknown")(_.compositeName)

    val progress = getProjectProgress(estimateMatrix)

    val timeSpent = composite.flatMap(c => getTimeSpentOnProject(c, dates.min))

    Ok(views.html.burndown_history(compositeProjectName, composite.get.projectCluster, estimateMatrix, dates, summaries, trend, progress, timeSpent))
  }

}