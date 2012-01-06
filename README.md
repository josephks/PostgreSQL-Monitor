Postgres monitoring application.  Inspired by [a presentation](http://www.nycpug.org/events/39469192/) at the New York PostgreSQL Meetup Group

Run ```./sbt``` to run sbt.  If you get a failed download message try removing your ```~/.ivy2``` directory.

To get a war file use the "package-war" command, not the "package" command.


This is in a very very raw state.  On the roadmap:

* Pretty up the UI, it's ugly
* Allow connections to more than just the default database
* Put in the size page
* Get disk level bandwith from /proc files

There is no authentication as of now.  Access control must be done by the container.

NOTE: the default config connecets to the "template1" database.  If you try to create a new database while the monitor is running you will get an error like 

```
createdb: database creation failed: ERROR:  source database "template1" is being accessed by other users
DETAIL:  There are 7 other session(s) using the database.
```  
