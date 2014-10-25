//
// Scaled Ack Mode - integrates Ack with Scaled project services
// http://github.com/scaled/ack-mode/blob/master/LICENSE

package scaled.project

import java.util.regex.Pattern
import scala.annotation.tailrec
import scaled._
import scaled.major.ReadingMode
import scaled.util.SubProcess

/** Provides configuration for [[AckResultsMode]]. */
object AckResultsConfig {

  /** The CSS style applied to file paths. */
  val pathStyle = "ackPathStyle"

  /** The CSS style applied to line numbers. */
  val lineNoStyle = "ackLineNoStyle"

  /** The CSS style applied to matches. */
  val matchStyle = EditorConfig.matchStyle // standard matchStyle
}

@Major(name="ack-results", tags=Array("ack"),
       desc="Displays `ack` results and allows navigation therethrough")
class AckResultsMode (env :Env, opts :AckConfig.Opts) extends ReadingMode(env) {
  import AckConfig._
  import AckResultsConfig._

  val pspace = ProjectSpace(env)

  override def configDefs = AckConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/ack.css") :: super.stylesheets
  override def keymap = super.keymap.
    bind("ENTER", "visit-match");

  // attributes we stuff into the buffer during processing
  case class File (path :String)
  case class LineNo (lineNo :Int)

  // listen for additions to the buffer and style them as they come in
  buffer.edited.onValue { _ match {
    case Buffer.Insert(start, end) =>
      var l = start ; while (l < end) { styleLine(l) ; l = l.nextStart }
    case _ => // ignore
  }}

  @Fn("Visits the match on the current line.")
  def visitMatch () {
    val p0 = view.point().atCol(0)
    @tailrec def findFile (loc :Loc) :Option[String] = buffer.tagAt(classOf[File], loc) match {
      case Some(File(path)) => Some(path)
      case _ => if (loc.row == 0) None else findFile(loc.prevL)
    }
    buffer.tagAt(classOf[LineNo], p0) match {
      case Some(LineNo(lineNo)) => findFile(p0) match {
        case Some(path) => window.focus.visitFile(Store(path)).point() = Loc(lineNo, 0)
        case None       => window.popStatus("Unable to determine file for match.")
      }
      case _ => window.popStatus("No match on the current line.")
    }
  }

  private def refresh () {
    buffer.delete(buffer.start, buffer.end)
    val cmd = Seq("ack") ++ opts.opts ++ Seq("--nocolor", "--nopager", "-x", opts.term)
    env.log.log(cmd.mkString(" "))
    val proc = SubProcess(SubProcess.Config(cmd.toArray), env.exec, buffer)

    // pass the files in the project to ack individually; this allows us to leverage the filtering
    // done by projects which know about which files to ignore
    val ps = opts.scope match {
      case PScope => Seq(pspace.project(buffer))
      case WScope => pspace.allProjects.map(i => pspace.projectIn(i._1))
    }
    ps foreach(_.onFiles(f => proc.send(f.toString)))
    proc.close()
    proc.waitFor()

    // TODO: wait for the process to terminate on a background thread, post results?
  }

  private def tagPath (line :LineV, loc :Loc, end :Int) {
    buffer.addStyle(pathStyle, loc, loc.atCol(end))
    buffer.addTag(File(line.sliceString(loc.col, end)), loc.atCol(0), loc.atCol(1))
  }

  private def tagLineNo (line :LineV, loc :Loc, end :Int) {
    buffer.addStyle(lineNoStyle, loc, loc.atCol(end))
    buffer.addTag(LineNo(line.sliceString(loc.col, end).toInt-1), loc.atCol(0), loc.atCol(1))
  }

  private def styleLine (loc :Loc) {
    val line = buffer.line(loc)
    if (line.length > 0) {
      // if it's neither a 'num:line' or a 'file:num:line' then it must be a 'file'
      if (!styleNumLine(loc, line) && !styleFileNumLine(loc, line)) {
        tagPath(line, loc.atCol(0), line.length)
      }
    }
  }

  private val NumLineP = Pattern.compile("""(\d+):(.*)""")
  private def styleNumLine (loc :Loc, line :LineV) :Boolean = {
    val m = NumLineP.matcher(line)
    if (!m.matches) false
    else {
      tagLineNo(line, loc.atCol(m.start(1)), m.end(1))
      styleMatches(loc, line, m.start(2))
      true
    }
  }

  private val FileNumLineP = Pattern.compile("""([^:]+):(\d+):(.*)""")
  private def styleFileNumLine (loc :Loc, line :LineV) :Boolean = {
    val m = FileNumLineP.matcher(line)
    if (!m.matches) false
    else {
      tagPath(line, loc.atCol(m.start(1)), m.end(1))
      tagLineNo(line, loc.atCol(m.start(2)), m.end(2))
      styleMatches(loc, line, m.start(2))
      true
    }
  }

  private val TermM = Matcher.on(opts.term)
  private def styleMatches (loc :Loc, line :LineV, start :Int) {
    @tailrec @inline def loop (col :Int) :Unit = line.indexOf(TermM, col) match {
      case -1 => // done
      case ii =>
        val end = ii+TermM.matchLength
        buffer.addStyle(matchStyle, loc.atCol(ii), loc.atCol(end))
        loop(end)
    }
    loop(start)
  }

  refresh() // run the search for the first time
}
