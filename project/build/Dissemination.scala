import xml._
import sbt.{ FileUtilities => FU, _}

/**
 *    @version 0.10, 28-Aug-10
 */
class DisseminationProject( info: ProjectInfo ) extends ProguardProject( info ) {
   val wolkenpumpe   = "de.sciss" %% "wolkenpumpe" % "0.19"

   // for some reason, snapshot release are not propagated properly through
   // the dependancies, we need to explicitly repeat them (or their repos) here...
   val ccstm      = "edu.stanford.ppl" % "ccstm" % "0.2.2-for-scala-2.8.0-SNAPSHOT"
   val ccstmRepo  = "CCSTM Release Repository at PPL" at "http://ppl.stanford.edu/ccstm/repo-releases"
   val ccstmSnap  = "CCSTM Snapshot Repository at PPL" at "http://ppl.stanford.edu/ccstm/repo-snapshots"
   val prefuse    = "prefuse" % "prefuse" % "beta-SNAPSHOT" from "http://github.com/downloads/Sciss/ScalaColliderSwing/prefuse-beta-SNAPSHOT.jar"

   val camelCaseName          = "Dissemination"
   def appBundleName          = camelCaseName + ".app"
   def appBundleContentsPath  = appBundleName / "Contents"
   def appBundleJavaPath      = appBundleContentsPath / "Resources" / "Java"

   private val jarExt                 = ".jar"
   private val jarFilter: FileFilter  = "*" + jarExt

   /**
    *    Note: there have been always problems in the shrinking,
    *    even with the most severe keep options, and anyway the
    *    size reduction was minimal (some 8%), so now we just
    *    use proguard to put everything in one jar, without
    *    shrinking.
    */
   override def proguardOptions = List(
      "-target 1.6",
      "-dontobfuscate",
      "-dontshrink",
      "-dontpreverify",
      "-forceprocessing"
   )

   override def minJarName = camelCaseName + "-full" + jarExt
   override def minJarPath: Path = minJarName

   private def allJarsPath = (publicClasspath +++ buildLibraryJar +++ buildCompilerJar +++ jarPath) ** jarFilter
   override def proguardInJars = allJarsPath --- jarPath // the plugin adds jarPath again!!

   def packageAppTask = task {
      val jarsPath               = allJarsPath
      val javaPath               = appBundleJavaPath
      val cleanPaths             = javaPath * jarFilter
      val quiet                  = false
      val versionedNamePattern   = """([^-_]*)[-_].*.jar""".r

      FU.clean( cleanPaths.get, quiet, log )

      for( fromPath <- jarsPath.get ) {
         val versionedName = fromPath.asFile.getName
         val plainName     = versionedName match {
            case versionedNamePattern( name ) if( name != "scala" ) => name + jarExt
            case n => n
         }
         val toPath = javaPath / plainName
         log.log( if(quiet) Level.Debug else Level.Info, "Copying to file " + toPath.asFile )
         FU.copyFile( fromPath, toPath, log )
      }

// plist is a real shitty format. we will need apache commons configuration
// to parse it. that in turn means we need dependancies for this task...
// will do that in a future version. for now, just let's assume
// the classpath is correctly set in Info.plist
//
//      val infoXML = XML.loadFile( appBundleContentsPath / "Info.plist" )
//      println( infoXML )

      None // what is this for?
   }

   protected def packageAppAction =
      packageAppTask.dependsOn( `package` ) describedAs "Copies all relevant jars into the OS X app bundle."

   lazy val packageApp = packageAppAction 
   lazy val standalone = proguard
}
