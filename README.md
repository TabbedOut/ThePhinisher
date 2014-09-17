The Phinisher
=============

Project management for Phabricator (http://phabricator.org/). Phabricator has 
some useful fields when it comes to tracking time and estimation, but there's 
currently no decent way to unify it.

This project exists solely because I refuse to learn PHP to actually add this 
into the main Phabricator ecosystem. 

Usage
-----

Select your projects, burn them down. Compare to previous burndowns. 


Expected Configuration
----------------------

An example config file is included in conf/application.conf.example

Before starting, set up your database connections and your application
secret.

By default the local database is a simplistic H2 local file. There are
no fancy database functions used, so that can safely be changed to the 
DB you prefer for local storage.

For this to work correctly, some assumptions are made.

Set up an Estimated Hours field in Maniphest:

    "phinisher:time-estimation"    : {
        "name"    : "Estimated Hours",
        "type"    : "int",
        "caption" : "Estimated number of hours this will take."
    },
    
After that is set up, these will exist in the 
    `phabricator_maniphest`.`maniphest_customfieldstorage`
table, the fieldIndex will need to be stored in the application.conf
in the `phabricator.estimatedHoursKey` value