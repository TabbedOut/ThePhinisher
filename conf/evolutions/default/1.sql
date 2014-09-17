# Weekly Burndown Schema

# --- !Ups
 

CREATE TABLE burndowns (
    id bigint(20) NOT NULL AUTO_INCREMENT,
    timestamp datetime NOT NULL,
    composite_id bigint(20) NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE composite_projects (
	id bigint(20) NOT NULL AUTO_INCREMENT,
	composite_id bigint(20) NOT NULL,
	phid char(30) NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE composite (
	id bigint(20) NOT NULL AUTO_INCREMENT,
	name varchar(255),
	target_date datetime,
	PRIMARY KEY (id)
);

CREATE TABLE burndown_tasks (
	id bigint(20) NOT NULL AUTO_INCREMENT,
	burndown_id bigint(20) NOT NULL,
	task_id varchar(255) NOT NULL,
	remaining_estimate int NOT NULL,
	PRIMARY KEY (id)
);

# --- !Downs

DROP TABLE historic_burndown_tasks;
DROP TABLE historic_burndowns;
DROP TABLE composite_projects;
DROP TABLE composite;
 

