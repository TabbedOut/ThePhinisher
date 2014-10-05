package models

import phabricator.Phabricator

import anorm._
import play.api.db.DB
import play.api.Play.current

import utility.AnormExtension._

import org.joda.time.DateTime
import org.joda.time.format._

case class CompositeProject(id: Long, projectIDs: List[String], name: Option[String], targetDate: Option[DateTime]) {
  def projectCluster = {
    s"{${projectIDs.mkString(",")}}"
  }

  def compositeName = {
    name match {
      case Some(n) => n
      case None => {
        val allProjects = Phabricator.listAllProjects

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

  def getBurndownCount() = {

    DB.withConnection("default") { implicit c =>

      val count = SQL(
        """
          SELECT count(id) as previous_count
          FROM burndowns b
          WHERE b.composite_id={composite_id}
          ;
        """).on('composite_id -> id).as(SqlParser.long("previous_count").single)

      count
    }
  }

  def saveBurndownSnapshot() = {
    DB.withConnection("default") { implicit c =>

      SQL(
        """
          INSERT INTO burndowns (timestamp, composite_id) VALUES (NOW(), {composite});
        """).on('composite -> id).executeInsert()
    }
  }

  def getTasksWithEstimates() = {
    DB.withConnection("default") { implicit c =>

      val sqlQuery = SQL(
        """
          SELECT task_id, remaining_estimate, timestamp
          FROM burndown_tasks bt
            LEFT JOIN  burndowns b on b.id=bt.burndown_id
          WHERE b.composite_id={composite_id}
          ;
        """).on('composite_id -> id)

      val taskEstimates = sqlQuery().map(row => {
        BurndownTask(
          row[String]("task_id"),
          row[Long]("remaining_estimate"),
          row[DateTime]("timestamp"))

      }).toList

      taskEstimates
    }
  }

  def getTasksWithHighestEstimate() = {
    DB.withConnection("default") { implicit c =>

      val sqlQuery = SQL(
        """
          SELECT task_id, max(remaining_estimate) AS max_estimate, min(timestamp) as ts
          FROM burndown_tasks bt
            LEFT JOIN  burndowns b on b.id=bt.burndown_id
          WHERE b.composite_id={composite_id}
          GROUP BY task_id
          ;
        """).on('composite_id -> id)

      val taskEstimates = sqlQuery().map(row => {
        BurndownTask(
          row[String]("task_id"),
          row[Long]("max_estimate"),
          row[DateTime]("ts"))

      }).toList

      taskEstimates
    }
  }

  def getTasksWithLatestEstimate() = {
    DB.withConnection("default") { implicit c =>

      val sqlQuery = SQL(
        """
          SELECT task_id, remaining_estimate, timestamp
          FROM burndown_tasks bt
            LEFT JOIN  burndowns b on b.id=bt.burndown_id
          WHERE b.composite_id={composite_id}
            AND b.id = (SELECT max(id) from burndowns WHERE composite_id={composite_id})
          ;
        """).on('composite_id -> id)

      val taskEstimates = sqlQuery().map(row => {
        BurndownTask(
          row[String]("task_id"),
          row[Long]("remaining_estimate"),
          row[DateTime]("timestamp"))

      }).toList

      taskEstimates
    }
  }

  def getEarliestBurndownDate() = {
    DB.withConnection("default") { implicit c =>

      val sqlQuery = SQL(
        """ SELECT timestamp
          FROM burndowns b 
          WHERE b.composite_id={composite_id}
          ;
        """).on('composite_id -> id)

      sqlQuery().map(row => row[DateTime]("timestamp")).toList.headOption
    }
  }

  lazy val featureProgress = {
    val denom = getTasksWithHighestEstimate.map(t => t.estimate).sum

    if (denom == 0) { 0 } else {
      val numer = denom - getTasksWithLatestEstimate.map(t => t.estimate).sum

      ((numer * 100) / denom).toInt
    }
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

  lazy val timeProgress: Option[Int] = {
    targetDate.flatMap(td => {

      val ee = getEarliestBurndownDate()

      ee flatMap { earliestEstimate =>
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
      }
    })
  }
}

object CompositeProject {
  def listAll() = {
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

        CompositeProject(row[Long]("composite_id"), phids, name, targetDate)
      }).toList

      projects
    }
  }

  def create(projectIDs: List[String]) = {
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

  def getByID(compositeID: Long) = {
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

        CompositeProject(row[Long]("composite_id"), phids, name, targetDate)
      }).toList.headOption

      project
    }
  }
}