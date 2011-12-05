
installCs(){
   curl https://raw.github.com/n8han/conscript/master/setup.sh | sh
}

DIR0=`dirname $0`

if ! [ -f $DIR0/sbt-launch.jar ] ; then
  if [ ! -f  ~/.conscript/sbt-launch.jar ] ; then
    installCs
# TODO: check for current version of sbt somehow, and reinstall conscript if not found
  fi
  set -e
  if [ -f  ~/.conscript/sbt-launch.jar ] ; then
      ln -s  ~/.conscript/sbt-launch.jar
  else
      wget http://typesafe.artifactoryonline.com/typesafe/ivy-releases/org.scala-tools.sbt/sbt-launch/0.11.2/sbt-launch.jar
  fi
fi

#java -Xmx712M -Xss2M -XX:+CMSClassUnloadingEnabled -jar `dirname $0`/sbt-launch.jar "$@"

java -Dfile.encoding=UTF8  -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=512m -jar  $DIR0/sbt-launch.jar "$@"
