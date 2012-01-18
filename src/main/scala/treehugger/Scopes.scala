/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */
 
package treehugger

trait Scopes extends api.Scopes { self: Forest =>
  class Scope {
  }
  
  /** The empty scope (immutable).
   */
  object EmptyScope extends Scope {
    // override def enter(e: ScopeEntry) {
    //   abort("EmptyScope.enter")
    // }
  }
  
  /** Create a new scope */
  def newScope: Scope = new Scope
}
