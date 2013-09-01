import org.specs2._

class DSL_6TopLevelDefSpec extends DSLSpec { def is =                         s2"""
  This is a specification to check Treehugger DSL

  Compilation units are written as
    `PACKAGEHEADER(sym|"p") := BLOCK(stat, ...)`,                             $unit1
    `BLOCK(stat, ...) inPackage(sym|"p")`, or                                 $unit2
    `BLOCK(stat, ...) withoutPackage`.                                        $unit3

  Packaging are written as
    `PACKAGE(sym|"p") := BLOCK(stat, ...)`.                                   $package1

  Packaging are written as
    `PACKAGEOBJECTDEF(sym|"p") := BLOCK(stat, ...)`.                          $packageobj1
                                                                              """
  
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._
                                                                             
  def unit1 =
    (PACKAGEHEADER("p") := BLOCK(
      OBJECTDEF("M")
    )) must print_as(
      "package p",
      "",
      "object M")

  def unit2 =
    (BLOCK(
      OBJECTDEF("M")
    ) inPackage("p")) must print_as(
      "package p",
      "",
      "object M")

  def unit3 =
    (BLOCK(
      OBJECTDEF("M1"),
      OBJECTDEF("M2")
    ) withoutPackage) must print_as(
      "object M1",
      "",
      "object M2")

  def package1 =
    (PACKAGE("p") := BLOCK(
      OBJECTDEF("M")
    )) must print_as(
      "package p {",
      "  object M",
      "}")    

  def packageobj1 =
    (PACKAGEOBJECTDEF("p") := BLOCK(
      OBJECTDEF("M")
    )) must print_as(
      "package object p {",
      "  object M",
      "}")   
}
