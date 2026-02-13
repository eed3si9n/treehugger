/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package treehugger

trait Scopes extends api.Scopes { self: Forest =>
  type Scope = List[Tree]

  /** The empty scope (immutable).
    */
  val EmptyScope = Nil

  /** Create a new scope */
  def newScope = Nil
}
