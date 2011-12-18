package treehugger
package api

trait Symbols { self: Universe =>

  type Symbol >: Null <: AbsSymbol

  abstract class AbsSymbol { this: Symbol =>
    /** The modifiers of this symbol
     */
    def allModifiers: Set[Modifier.Value]

    /** Does this symbol have given modifier?
     */
    def hasModifier(mod: Modifier.Value): Boolean
    
    /** The owner of this symbol.
     */
    def owner: Symbol

    /** The name of the symbol as a member of the `Name` type.
     */
    def name: Name
    
    /** If symbol is an object definition, its implied associated class,
     *  otherwise NoSymbol
     */
    def moduleClass: Symbol // needed for LiftCode    
    
    /** An id number which is unique for all symbols in this universe */
    def id: Int
  }

  val NoSymbol: Symbol
}
