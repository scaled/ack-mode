//
// Scaled Ack Mode - integrates Ack with Scaled project services
// http://github.com/scaled/ack-mode/blob/master/LICENSE

package scaled.project

import scaled._
import scaled.util.Chars

/** Provides configuration for [[AckMode]]. */
object AckConfig extends Config.Defs {
  import EditorConfig._

  @Var("Options to pass to `ack`.")
  val ackOpts = key("--heading --smart-case")

  /** The history ring for Ack queries. */
  val searchHistory = fnKey(cfg => new Ring(cfg(historySize)))

  // encapsulates ack arguments
  sealed trait Scope
  case object PScope extends Scope
  case object WScope extends Scope
  case class Opts (term :String, opts :Array[String], scope :Scope)
}

@Minor(name="ack", tags=Array("project"),
       desc="""A minor mode that provides Ack searching of Scaled projects.""")
class AckMode (env :Env) extends MinorMode(env) {
  import AckConfig._
  import Chars._

  val project = ProjectSpace(env).project(buffer)

  override def configDefs = AckConfig :: super.configDefs
  override def keymap = Seq(
    "C-c C-g"     -> "ack-in-project",
    "S-C-c S-C-g" -> "ack-in-workspace"
  )

  @Fn("Requests a query string and invokes `ack` on all project files therewith.")
  def ackInProject () :Unit = ackInScope("Search project for:", PScope)

  @Fn("Requests a query string and invokes `ack` on all workspace files therewith.")
  def ackInWorkspace () :Unit = ackInScope("Search workspace for:", WScope)

  private def ackInScope (prompt :String, scope :Scope) {
    editor.mini.read(prompt, wordAt(view.point()), config(searchHistory),
                     Completer.none) onSuccess { term => if (term.length > 0) {
      val opts = config(ackOpts).split(" ")
      val bc = editor.bufferConfig(s"*ack: $term*").mode("ack-results", Opts(term, opts, scope))
      // if we have project scope, set the results buffer up as a project buffer
      val bv = (scope match {
        case PScope => bc.tags("project").state(project.asState)
        case WScope => bc
      }).create()
      editor.visitBuffer(bv.buffer)
    }}
  }

  /** Returns the "word" at the specified location in the buffer. */
  private def wordAt (loc :Loc) :String = {
    val p = view.point()
    val pstart = buffer.scanBackward(isNotWord, p)
    val start = if (isWord(buffer.charAt(pstart))) pstart else buffer.forward(pstart, 1)
    val end = if (!isWord(buffer.charAt(start))) start
              else buffer.scanForward(isNotWord, p)
    buffer.region(start, end).map(_.asString).mkString
  }
}
