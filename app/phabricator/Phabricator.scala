package phabricator

import anorm._
import play.api.db.DB

import play.api.Play.current
import play.api._

object Phabricator {

  // Does not contain anything that is not stored in phabricator
  case class Task(taskID: String, title: String, priority: Long, hours: Option[String], assignee: Option[String])

  case class ProjectStub(name: String, phid: String)

  def openTasksByProjectID(projectID: String) = {
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
        Task(row[String]("taskID"),
          row[String]("title"),
          row[Long]("priority"),
          row[Option[String]]("maniphest_customfieldstorage.fieldValue"),
          row[Option[String]]("user.userName")))

      tasks.toList
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