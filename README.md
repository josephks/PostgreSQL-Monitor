Postgres monitoring application.  Inspired by [a presentation](http://www.nycpug.org/events/39469192/) at the New York PostgreSQL Meetup Group

Run ```./sbt``` to run sbt.  If you get a failed download message try removing your ```~/.ivy2``` directory.

To get a war file use the "package-war" command, not the "package" command.


This is still in a very raw state.  On the roadmap:

* Pretty up the UI, it's ugly
* Allow connections to more than just the default database
* Get disk level bandwith from /proc files (according to [the docs](http://www.postgresql.org/docs/9.1/interactive/functions-admin.html) the pg file reading functions only work on files in the db cluster, so a workarond is needed)
* In the locks view, figure out how to get relation names for relations in other databases
* One click db schema -> ORM code generation (partially done)

There is no authentication as of now.  Access control must be done by the container.

NOTE: the default config connecets to the "template1" database.  If you try to create a new database while the monitor is running you will get an error like 

```
createdb: database creation failed: ERROR:  source database "template1" is being accessed by other users
DETAIL:  There are 7 other session(s) using the database.
```  

## How to Use

I anticipate that in the future I will publish this to a maven repo. Users will create their own projects that have dependencies on this, with templates customized for their system.

For now do a git checkout, start sbt, and run ```container:start``` at the sbt prompt.  This will start a jetty server on port 8080 running the webapp (with no access control!).  The alternative is to run ```package-war``` to generate a war that you can then run on your container of choice.