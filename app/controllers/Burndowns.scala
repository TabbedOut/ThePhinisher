package controllers

import play.api._
import play.api.mvc._

import anorm._
import play.api.db.DB

import play.api.libs.json.Json

import play.api.Play.current
import org.joda.time.DateTime
import utility.AnormExtension._

object Burndowns extends Controller {

  implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

  case class ProjectStub(name: String, phid: String)
  case class HistoricTaskEstimates(taskID: String, estimates: List[Long]) {}
  case class DatedEstimate(d: DateTime, e: Option[Long])
  case class Task(taskID: String, title: String, priority: Long, hours: Option[String], assignee: Option[String])
  case class CompositeProject(id: Long, projectIDs: List[String]) {
    def projectCluster = {
      s"{${projectIDs.mkString(",")}}"
    }
    
    // A composite actually has a name field
    // That should be used in the future
    def name = {
      val allProjects = listAllProjects
      
      val filteredStubs = allProjects.filter(p => projectIDs.contains(p.phid))
      
      filteredStubs.map(_.name).mkString(" / ")
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

  def openTasksByProject(projectID: String) = {
    val estimatedHoursKey = Play.current.configuration.getString("phabricator.estimatedHoursKey")

    DB.withConnection("phabricator") { implicit c =>
      val tasksQuery = SQL(
        """
        select concat('T',task.id) taskID, status, priority, title, description,
        	 cf.`fieldValue` as estimation, u.username as AssignedTo
        from phabricator_maniphest.maniphest_task task
        	left join phabricator_maniphest.maniphest_taskproject tp 
        		on tp.`taskPHID` = task.`phid`
        	left join phabricator_maniphest.maniphest_nameindex p 
        		on p.`indexedObjectPHID`=tp.`projectPHID`
        	left join phabricator_maniphest.maniphest_customfieldstorage cf 
        		on task.phid=cf.`objectPHID` and fieldIndex={estHours}
        	left join phabricator_user.user u 
        		on u.phid = task.`ownerPHID`
        where
        	status NOT IN (1,2,3,4,5) 
         	AND priority > 25
        	AND tp.`projectPHID` = {projectID}
        order by task.id asc
        ;
      """).on('projectID -> projectID, 'estHours -> estimatedHoursKey)

      val tasks = tasksQuery().map(row =>
        Task(row[String]("taskID"),
          row[String]("title"),
          row[Long]("priority"),
          row[Option[String]]("maniphest_customfieldstorage.fieldValue"),
          row[Option[String]]("user.userName")))

      tasks.toList
    }

  }

  def index = Action {
    val projects = listAllProjects

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
          SELECT composite_id, GROUP_CONCAT(phid) AS phids
          FROM composite_projects
          GROUP BY composite_id
          ;
        """)

      val projects = sqlQuery().map(row => {
        val phids = row[String]("phids").split(",").toList

        CompositeProject(row[Long]("composite_id"), phids)
      }).toList

      projects
    }
  }

  def detectCompositeBurndown(projectIDs: List[String]): Option[Long] = {
    val candidateProjects = listAllComposites

    Logger.info(candidateProjects mkString (":"))

    val matches = candidateProjects.filter(cp => {
      cp.projectIDs.toSet.equals(projectIDs.toSet)
    })

    matches.headOption.flatMap(cp => Some(cp.id))

    matches.headOption match {
      case Some(cp) => Some(cp.id)
      case None => createNewComposite(projectIDs)
    }
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
            INSERT INTO composite_projects (composite_id, phid) VALUES ({newComposite}, {phid});
          """)

        projectIDs.foreach(pid => {
          insertQuery.on('newComposite -> newID, 'phid -> pid).executeInsert()
        })

        Some(newID)
      })
    }
  }

  def getHistoricSnapshotCount(compositeID: Long) = {

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

  def burndownByProject(projectBase: String) = Action {

    val projectIDs = extractProjectIDs(projectBase)

    val existingComposite = detectCompositeBurndown(projectIDs)

    val allProjects = listAllProjects

    val title = allProjects.filter(pid => projectIDs.contains(pid.phid)).map(_.name).mkString(" / ")

    val tasks = projectIDs.map(pid => openTasksByProject(pid)).toList.flatten

    val (needsTriage, normalTasks) = tasks.partition(t => t.priority == 90 || t.hours.isEmpty)

    val sortedTasks = (needsTriage ++ normalTasks).distinct

    val hoursToBurn = sortedTasks.map(_.hours.getOrElse("0").toInt).sum

    (Play.current.configuration.getString("phabricator.url"), existingComposite) match {
      case (Some(phabricatorUrl), Some(existingCompositeID)) => {
        Ok(views.html.burndown_by_project(existingCompositeID, title, sortedTasks,
          phabricatorUrl, hoursToBurn, getHistoricSnapshotCount(existingCompositeID) > 0))
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

  def saveTasksForSnapshot(snapshotID: Long, tasks: List[Task]) = {
    DB.withConnection("default") { implicit c =>
      val insertQuery = SQL("""
          INSERT INTO burndown_tasks (burndown_id, task_id, remaining_estimate) VALUES ({snapshot_id}, {task_id}, {estimate});
        """)

      tasks.foreach(t => {
        val estimate = t.hours match {
          case None => 0
          case Some(e) => e.toLong
        }
        insertQuery.on('snapshot_id -> snapshotID, 'task_id -> t.taskID, 'estimate -> estimate).executeInsert()
      })
    }
  }

  def saveSnapshotViaAjax(compositeKey: String) = Action(parse.json) { request =>

    Logger.info(s"Starting to save snapshot for composite #${compositeKey}")

    // TODO: this is brittle
    val compositeID = compositeKey.toLong

    createNewSnapshot(compositeID) match {
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

  def getHistoricTasks(compositeID: String) = {

    case class TaskEntry(taskID: String, estimate: Long, timestamp: DateTime)

    // This function is embarassingly long.. I can do better

    val allEstimates = DB.withConnection("default") { implicit c =>

      val sqlQuery = SQL(
        """
          SELECT task_id, remaining_estimate, timestamp
          FROM burndown_tasks bt
            LEFT JOIN  burndowns b on b.id=bt.burndown_id
          WHERE b.composite_id={composite_id}
          ;
        """).on('composite_id -> compositeID)

      val taskEstimates = sqlQuery().map(row => {
        TaskEntry(
          row[String]("task_id"),
          row[Long]("remaining_estimate"),
          row[DateTime]("timestamp"))

      }).toList

      taskEstimates
    }

    val estimatesByTask = allEstimates.groupBy(_.taskID).map(est => {
      val taskID = est._1
      val estMap = est._2.map(x => {
        (x.timestamp -> x.estimate)
      }).toMap

      (taskID -> estMap)
    })

    val allDates = allEstimates.map(_.timestamp).distinct
    val allTasks = allEstimates.map(_.taskID).distinct

    val minTasks = estimatesByTask.map(x => {
      val taskID = x._1
      val es = x._2
      val minDate = es.reduce((a, b) => if (a._1.isBefore(b._1)) { a } else { b })._1
      (taskID -> minDate)

    })

    val finalMatrixList = for {
      task <- allTasks
      date <- allDates

      e = estimatesByTask.getOrElse(task, Map()).get(date) match {
        case Some(es) => Some(es)
        case None => {
          minTasks.get(task) match {
            case None => Some(0L)
            case Some(minDate) => {
              if (minDate.isBefore(date)) {
                Some(0L)
              } else {
                None
              }
            }
          }
        }
      }
    } yield (task, DatedEstimate(date, e))

    val nextMatrix = finalMatrixList.groupBy(_._1)

    val mappedMatrix = nextMatrix.map(e => (e._1, e._2.map(_._2).sortBy(_.d)))
    (mappedMatrix, allDates)
  }

  def generateSummaries(estimateMatrix: Map[String, List[DatedEstimate]]) = {
    val estimatesByDate = estimateMatrix.map(x => x._2).flatten.groupBy(_.d)

    val sumsByDate = estimatesByDate.map(ed => {
      val sum = ed._2.map(_.e.getOrElse(0L)).sum

      DatedEstimate(ed._1, Some(sum))
    }).toList.sortBy(_.d)

    sumsByDate
  }

  def historicData(compositeID: String) = Action {

    val (estimateMatrix, dates) = getHistoricTasks(compositeID: String)

    val summaries = generateSummaries(estimateMatrix)

    val trend = if (summaries.isEmpty) {
      Nil
    } else {
      val summariesOffset = summaries.head :: (summaries.dropRight(1))
      summaries.zip(summariesOffset).map(x => {
        val thisWeek = x._1.e.getOrElse(0L)
        val lastWeek = x._2.e.getOrElse(0L)

        thisWeek - lastWeek
      })
    }

    Ok(views.html.burndown_history(estimateMatrix, dates, summaries, trend))
  }

}