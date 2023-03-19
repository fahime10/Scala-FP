package topics
import lib.logic.L3.*

object Unit07 {

  /*
   * Exercise 71
  */

  @main def exercise71(): Unit =
    val f: Formula = Cond(A, B)
    val env: Map[TruthVar, TruthVal] = Map(A -> U, B -> F)
    println(f)
    println(f.showTruthTable)
    println(f.evaluate(env))
    println()

    val f1: Formula = Eqv(And(A, B), And(B, A))
    val env1: Map[TruthVar, TruthVal] = Map(A -> F, B -> T)
    println(f1)
    println(f1.showTruthTable)
    println(f1.evaluate(env1))
    println(s"Is f1 a tautology: ${f1.isTautology}")
    println()

    val f2: Formula = Eqv(And(A, Or(B, C)), Or(And(A, B), And(A, C)))
    val env2: Map[TruthVar, TruthVal] = Map(A -> U, B -> T, C -> F)
    println(f2)
    println(f2.showTruthTable)
    println(f2.evaluate(env2))
    println(s"Is f2 a tautology: ${f2.isTautology}")
    println()

    val f3: Formula = Cond(A, Cond(Not(A), A))
    val env3: Map[TruthVar, TruthVal] = Map()
    println(f3)
    println(f3.showTruthTable)
    println(s"${f3.evaluate(env3)} = ${f3.evaluate(env3)}")
    println()

    val f4: Formula = Or(A, Not(A))
    val  env4: Map[TruthVar, TruthVal] = Map(A -> U)
    println(f4)
    println(f4.showTruthTable)
    println(f4.evaluate(env4))
    println()

    val f5: Formula = Cond(Not(L (Not(A))), U)
    val env5: Map[TruthVar, TruthVal] = Map(A -> U)
    println(f5)
    println(f5.showTruthTable)
    println(f5.evaluate(env5))
    println(s"Is f5 a tautology: ${f5.isTautology}")
    println()

    val f6: Formula = Eqv(Cond(A, B), Cond(Not(B), Not(A)))
    val env6: Map[TruthVar, TruthVal] = Map(A -> F)
    println(f6)
    println(f6.showTruthTable)
    println(f6.evaluate(env6))
    println(s"Is f6 a tautology: ${f6.isTautology}")
    println()


  /*
   * Exercise 72
  */
  @main def exercise72(): Unit =
    val f: Formula = Cond(A, Eqv(B, And(C, Or(D, Not(Cond(E, F))))))
    println(f)

    val g: Formula = f map {
      case t: TruthVal => t;
      case v: TruthVar => if !(v == D || v == E) then T else v
    }
    println(g)

  /*
   * Exercise 73
  */
  def equiv(f1: Formula, f2: Formula): Boolean = Eqv(f1, f2).isTautology

  @main def exercise73(): Unit =
    println(equiv(A, Not(Not(A))))
    println(equiv(And(A, B), Or(A, B)))
    println()

    val f: Formula = Eqv(Or(And(A, B), C), Cond(A, Cond(B, C)))
    val (names, combinations, results) = f.generateTruthTable
    val values = for (c, r) <- combinations zip results if r == T yield c
    println(s"Values that satisfy $f are")
    println(names.map(_.toString).mkString)
    println("-" * names.length)
    values foreach { (r: Seq[TruthVal]) =>
      println(r.map(_.toString).mkString)
    }
}
