
set -e

DIR0=`dirname $0`

if ! [ -f $DIR0/sbt-launch.jar ] ; then
  wget http://typesafe.artifactoryonline.com/typesafe/ivy-releases/org.scala-tools.sbt/sbt-launch/0.10.1/sbt-launch.jar
fi

#java -Xmx712M -Xss2M -XX:+CMSClassUnloadingEnabled -jar `dirname $0`/sbt-launch.jar "$@"

java -Dfile.encoding=UTF8  -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=512m -jar  $DIR0/sbt-launch.jar "$@"
