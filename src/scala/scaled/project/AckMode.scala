//
// Scaled Ack Mode - integrates Ack with Scaled project services
// http://github.com/scaled/ack-mode/blob/master/LICENSE

package scaled.project

import scaled._
import scaled.util.Chars

/** Provides configuration for [[AckMode]]. */
object AckConfig extends Config.Defs {

  @Var("Options to pass to `ack`.")
  val ackOpts = key("--heading --smart-case")

  // encapsulates ack arguments
  sealed trait Scope
  case object PScope extends Scope
  case object WScope extends Scope
  case class Opts (term :String, opts :Seq[String], scope :Scope)
}

@Minor(name="ack", tags=Array("project"),
       desc="""A minor mode that provides Ack searching of Scaled projects.""")
class AckMode (env :Env) extends MinorMode(env) {
  import AckConfig._
  import Chars._

  val project = Project(buffer)

  override def configDefs = AckConfig :: super.configDefs
  override def keymap = super.keymap.
    bind("ack-in-project",   "C-c C-g").
    bind("ack-in-workspace", "S-C-c S-C-g");

  @Fn("Requests a query string and invokes `ack` on all project files therewith.")
  def ackInProject () :Unit = ackInScope("Search project for:", PScope)

  @Fn("Requests a query string and invokes `ack` on all workspace files therewith.")
  def ackInWorkspace () :Unit = ackInScope("Search workspace for:", WScope)

  private def searchHistory = Workspace.historyRing(wspace, "ack-search")

  private def ackInScope (prompt :String, scope :Scope) {
    window.mini.read(prompt, wordAt(view.point()), searchHistory,
                     Completer.none) onSuccess { term => if (term.length > 0) {
      val opts = Opts(term, config(ackOpts).split(" ").mkSeq, scope)
      // if we have project scope, set the results buffer up as a project buffer
      val state = scope match {
        case PScope => project.bufferState("ack-results", opts)
        case WScope => State.inits(Mode.Hint("ack-results", opts))
      }
      window.focus.visit(wspace.createBuffer(Store.scratch(s"*ack: $term*", buffer.store), state))
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
