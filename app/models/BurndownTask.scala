package models

import org.joda.time.DateTime

case class BurndownTask(taskID: String, estimate: Long, timestamp: DateTime)