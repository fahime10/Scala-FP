package topics

import lib.picture.Picture
import lib.picture.Picture.*

object Unit06 {
  /*
   * Exercise 61
   */
  @main def exercise61(): Unit =
    val matrix = Picture("01234\n56789")
    val answer = Picture("30  80\n80 255")
    val times = Picture(" \n*").borderLR()
    val equals = Picture(" \n=").borderLR()
    println(matrix)
    println(matrix.frameBNC)
    println(matrix.reflectH.frameBNC)
    println(matrix.reflectV.frameBNC)
    println(matrix.transpose.frameBNC)
    println(matrix.frameBNC + times + matrix.transpose.frameBNC + equals + answer.frameBNC)

  /*
   * Exercise 62
   */
  @main def exercise62(): Unit =
    val triangles: Seq[Picture] = (1 to 8).map(box(_, 1, '*')).scanLeft(empty)(_ + (_, BOT))
    val ascending = triangles.spread(BOT)
    val descending = triangles.spread(TOP, '|')
    //println(triangles)
    //println()
    println(ascending)
    println()
    println(descending)
    println()
    println(ascending + ascending.reflectV)
    println()
    println(descending ^ descending.reflectH)
    println()
    println((descending + descending.reflectV) ^ (descending.reflectH + descending.reflectH.reflectV))
    println()
    println((ascending + ascending.reflectV) ^ (ascending.reflectH + ascending.reflectH.reflectV))

  /*
   * Exercise 63
   */

  @main def exercise63(): Unit =
    val words =
      """The flow method
        |creates a picture by flowing text from a string into a rectangle of
        |given width. The words are identified by removing any whitespace
        |characters from around them. No word is split unless it is longer
        |than the specified width. The depth of the picture is no more than
        |necessary to flow all of the words into the area.
        |""".stripMargin
    val para = flow(width = 40, words)
    println(para.frameBNC)
    println((para + para.map(_.toUpper)).frameBNC)
    println()
    println((para + para.map(_.toUpper).frameL).frameBNC)
    println((para.borderR(' ') + para.map(_.toUpper).borderL(' ').borderL('|')).frameBNC)

  /*
   * Exercise 64
   */
  case class InfRule(premises: Seq[String], conclusion: String, name: String) {
    override def toString: String = this.toPicture.toString

    //def toPicture: Picture = (Picture(premises.mkString(", ")).frameB ^ (Picture(conclusion), CTR)).borderR() +
      //(Picture(s"[$name]"), CTR)

      def toPicture: Picture =
        val comma = Picture(", ")
        val ps = premises map (Picture(_)) intersperse comma
        (ps.spread().frameB ^ (Picture(conclusion), CTR)) +
          (if name.nonEmpty then Picture(s"[$name]") else empty, CTR)
  }

  @main def exercise64(): Unit =
    val modusPonens = InfRule(Seq("A", "A -> B"), "B", "Modus ponens")
    val orElim = InfRule(Seq("A -> C", "B -> C", "A \\/ B"), "C", "Or elimination")
    val reductio = InfRule(Seq("A |- B", "A |- ~B"), "~A", "Reductio ad absurdum")
    val deduction = InfRule(Seq("A |- B"), "A -> B", "Deduction")
    //println(modusPonens)
    //println()
    //println(orElim)
    //println()
    //println(reductio)
    //println()
    //println(deduction)

    val rules: List[InfRule] = List(modusPonens, orElim, reductio, deduction)
    val display =
      for
        (r, i) <- rules.zipWithIndex
      yield
        (Picture((i + 1).toString) + Picture("    ") + r.toPicture).border()

    println(display.toVector.stack())

  /*
   * Exercise 65
   */
  val text: String =
    """
      |Functional programming (FP) has been practised since the 1950s. However, in recent
      |years the paradigm has gained increasing prominence in industry, driven mainly by
      |changes in computer architecture. The FP style moves away from more traditional
      |approaches to algorithm design which includes mutable state and sequential updates
      |towards a style that encourages immutability and higher-order functions.
      |
      |FP can be quite challenging initially to programmers with a traditional background
      |because its underlying principles are very different from those they have practised
      |for many years. However, once the benefits have been realised and understood there
      |are significant advantages to be gained from using FP when appropriate.
      |
      |Although there are the “purer” FP languages such as Haskell, the majority of FP in
      |industry is embedded within mainstream OO languages such as C++, Python, PHP, C#
      |and Java. In this module we will look at programming in a functional style using
      |Scala.
      |
      |Scala is a large language and we cannot cover all of its features in this module.
      |You are encouraged to read around if you are interested to discover many more
      |techniques and ideas.
      |
      |We will be focusing on curried functions, higher order
      |functions, function composition, partial application, immutable data structures
      |(especially lists), lazy lists, domain specific languages, and some applications.
      |
      |Each week we cover a unit which will focus on a particular topic. The lectures
      |for the unit are delivered in week n, and the exercises for that unit are
      |to be undertaken by the end of week (n+1). Solutions to the exercises will be
      |published at 12:00 on the Friday of week (n+2).
      |""".stripMargin

  def toParagraphs(xs: String): Vector[String] =
    val blocks: Vector[String] = xs.split(System.lineSeparator() * 2).toVector
    for
      block <- blocks
      cleanBlock = block.dropWhile(System.lineSeparator() contains _)
        .map(ch => if System.lineSeparator() contains ch then ' ' else ch)
      if cleanBlock.nonEmpty
    yield
      cleanBlock

  @main def exercise65(): Unit =
    val space = Picture(getSpace)
    val paras: Vector[Picture] = toParagraphs(text) map (flow(40, _).borderLR())
    println(paras.spread())
    println()

    // Construct a new picture and print it out. This time, the information should be
    // displayed as a two-column article with the paragraphs numbered, and with a header.

  // this is my attempt, however, it does not produce a two column output
//    val header: Picture = Picture("Introduction to Functional Programming").frameBNC
//    val indexedParas = paras.zipWithIndex
//    val indexes = indexedParas.map(_._2).mkString("").toList
//    val newParas = indexedParas.map(_._1).toList
//    val split = List(newParas.splitAt(newParas.length / 2))
//    val one = split.flatMap(_._1)
//    val two = split.flatMap(_._2)
//
//    println(header)
//    indexes.foreach(x => {
//      //println(Picture("(" + (x.asDigit + 1) + ")") + space + Picture(newParas.slice(x.asDigit, x.asDigit + 1).mkString("")))
//      if (x.asDigit < 3) {
//        println(Picture("(" + (x.asDigit + 1) + ")") +
//          Picture(one.slice(x.asDigit, x.asDigit + 1).mkString("")).borderL(' '))
//        println()
//      } else {
//        println(Picture("(" + (x.asDigit + 1) + ")") +
//          Picture(two.slice(x.asDigit - (newParas.length / 2), x.asDigit + 1 - (newParas.length / 2)).mkString("")).borderL(' '))
//        println()
//      }
//    })

    // this is the answer
    val bulletPoints: Vector[Picture] =
      paras.zipWithIndex map ((p, i) => (Picture(s"(${i + 1}) ") +
        p).borderT())

    val twoColumn: Picture =
      bulletPoints.grouped(3).toVector.map(_.stack()).spread()

    val header = Picture("Introduction to Functional Programming").frameBNC

    println(header ^ (twoColumn, CTR))


  /*
   * Exercise 66
   */
  @main def exercise66(): Unit =
    val numkeys: Vector[Picture] =
      Vector(7, 8, 9, 4, 5, 6, 1, 2, 3) map (k => Picture(k.toString).borderLR().frameNC)
    val keypad1 = numkeys.grouped(3).toVector.map(_.spread()).stack()
    println(keypad1)
    println()

    val row1 = Vector("N", "/", "*", "-").map(Picture(_).borderLR().frameNC).spread()
    val row2 = Vector("7", "8", "9").map(Picture(_).borderLR().frameNC).spread()
    val row3 = Vector("4", "5", "6").map(Picture(_).borderLR().frameNC).spread()
    val row2_3 = (row2 ^ row3) + Picture("+").fixDepth(4, CTR).borderLR().frameNC
    val row4 = Vector("1", "2", "3").map(Picture(_).borderLR().frameNC).spread()
    val row5 = Picture("0").fixWidth(6, CTR).borderLR().frameNC + Picture(".").borderLR().frameNC
    val row4_5 = (row4 ^ row5) + Picture("E").fixDepth(4, CTR).borderLR().frameNC
    lazy val keypad2 = row1 ^ row2_3 ^ row4_5
    println(keypad2)
}