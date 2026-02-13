package treehugger
package api

abstract class Universe
    extends Symbols
    with Types
    with Constants
    with Scopes
    with Names
    with Trees
    with AnnotationInfos
    with StandardDefinitions
    with TreePrinters {
  type Position
  val NoPosition: Position
}
