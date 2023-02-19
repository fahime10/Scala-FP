package topics

import scala.annotation.tailrec

object Unit03 {

  val wordList: List[String] = List("The", "quick", "brown", "fox", "jumped", "over", "the", "lazy", "dog")

  /*
   * Exercise 31
   * Using only head and tail extract the following words from wordList:
   * (a) The
   * (b) quick
   * (c) dog
   * Then extract the following words from wordList using only init and last:
   * (d) The
   * (e) lazy
   * (f) dog
   */

  @main
  def Exercise31(): Unit =
    lazy val a = wordList.head
    lazy val b = wordList.tail.head
    lazy val c = wordList.tail.tail.tail.tail.tail.tail.tail.tail.head
    lazy val d = wordList.init.init.init.init.init.init.init.init.head
    lazy val e = wordList.init.last
    lazy val f = wordList.last

    println(a)
    println(b)
    println(c)
    println(d)
    println(e)
    println(f)

  /*
   * Exercise 32
   * Using charList, intList, and wordList, respectively, and at most the operators
   * take, drop, and ::: build the following lists:
   * (a) List(x, y, z, a, b, c, d)
   * (b) List(9, 0)
   * (c) List(over, the, lazy, dog, jumped, The, quick, brown, fox)
   * Use the method splitAt to divide the intList in each of the following ways:
   * (d) (List(0, 1, 2, 3), List(4, 5, 6, 7, 8, 9))
   * (e) (List(0, 1, 2, 3, 4, 5, 6, 7, 8), List(9))
   * (f) (List(), List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
   */

  val charList: List[Char] = ('a' to 'z').toList
  val intList: List[Int] = (0 to 9).toList

  @main
  def Exercise32(): Unit =
    lazy val a = (charList drop 23) ::: (charList take 3)
    lazy val b = (intList drop 9) ::: (intList take 1)
    lazy val c = (wordList drop 5) ::: (wordList take 4)
    lazy val d = (intList take 4) :: List(intList drop 4)
    lazy val e = (intList take 9) :: List(intList drop 9)
    lazy val f = (intList drop 10) :: List(intList take 10)

    println(a)
    println(b)
    println(c)
    println(d)
    println(e)
    println(f)

  /*
   * Exercise 33
   * Construct appropriate (function) arguments ... for map such that (e.g.) intList.map(...) generates
   * each of the following lists:
   * (a) List(0, 2, 4, 6, 8, 10, 12, 14, 16, 18)
   * (b) List(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
   * (c) List(0, 1, 2, 3, 4, 5, 5, 5, 5, 5)
   * (d) List(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
   * (e) List(a, b, c, d, e, f, g, h, i, j)
   * (f) List(The, quick, brown, fox, jumped, over, the, lazy, dog, The)
   * (g) List(false, false, true, true, false, true, false, false, true)
   * (h) List(THE, QUICK, BROWN, FOX, JUMPED, OVER, THE, LAZY, DOG)
   * (i) List(List(T, h, e), List(q, u, i, c, k), List(b, r, o, w, n), List(f, o, x),
   *          List(j, u, m, p, e, d), List(o, v, e, r), List(t, h, e), List(l, a, z, y),
   *          List(d, o, g))
   */

  @main
  def Exercise33(): Unit =
    lazy val a = intList.map(_ * 2)
    lazy val b = intList.map(_ * 2 + 1)
    lazy val c = intList.map(x => x < 5)
    lazy val d = intList.map(intList.reverse)
    lazy val e = charList.map(_ < 'k')
    lazy val f = wordList ::: wordList.take(1)
    lazy val g = wordList.map(_.contains('o'))
    lazy val h = wordList.map(_.toUpperCase())
    lazy val i = wordList.map(_.toList)

    println(a)
    println(b)
    println(c)
    println(d)
    println(e)
    println(f)
    println(g)
    println(h)
    println(i)

  /*
   * Exercise 34
   * Construct appropriate (function) arguments ... and use with filter, takeWhile, or
   * dropWhile, and the relevant list such that (e.g.) intList.filter(...), wordList.takeWhile(...),
   * etc., generates each of the following lists:
   * (a) List(a, a)
   * (b) List(r, d, v, a, r, k)
   * (c) List(a, a, a)
   * (d) List(0, 3, 6, 9)
   * (e) List(a, e, i, o, u)
   * (f) List(The, fox, the, dog)
   * (g) List(The, quick, brown, fox)
   * (h) List(the, lazy, dog)
   * (i) List(x, y, z)
   * (j) List(0, 2, 3, 4, 6, 8, 9)
   */

  //List('a', 'a', 'r', 'd', 'v', 'a', 'r', 'k')
  val aardvark: List[Char] = "aardvark".toList

  @main
  def Exercise34(): Unit =
    //lazy val a = charList.takeWhile(_ < 'b') ::: charList.takeWhile(_ < 'b')
    lazy val a = aardvark.takeWhile(_ == 'a')
    lazy val b = aardvark.dropWhile(_ == 'a')
    lazy val c = charList.filter(_ == 'a')
    lazy val d = intList.filter(_ % 3 == 0)
    //lazy val e = charList.filter(_ == 'a') ::: charList.filter(_ == 'e') ::: charList.filter(_ == 'i') ::: charList.filter(_ == 'o') ::: charList.filter(_ == 'u')
    lazy val e = charList.filter("aeiou".contains(_))
    //lazy val f = wordList.filter(_ == "The") ::: wordList.filter(_ == "fox") ::: wordList.filter(_ == "the") ::: wordList.filter(_ == "dog")
    lazy val f = wordList.filter(_.length == 3)
    //lazy val g = wordList.filter(_ == "The") ::: wordList.filter(_ == "quick") ::: wordList.filter(_ == "brown") ::: wordList.filter(_ == "fox")
    lazy val g = wordList.takeWhile(!_.contains('j'))
    //lazy val h = wordList.filter(_ == "the") ::: wordList.filter(_ == "lazy") ::: wordList.filter(_ == "dog")
    lazy val h = wordList.dropWhile(_ != "the")
    lazy val i = charList.dropWhile(_ < 'x')
    lazy val j = intList.filter(k => k % 2 == 0 || k % 3 == 0)

    println(a)
    println(b)
    println(c)
    println(d)
    println(e)
    println(f)
    println(g)
    println(h)
    println(i)
    println(j)

  /*
   * Exercise 35
   * Write a method, swapAround, that takes a list and returns it with the first half
   * swapped with the second half.  Thus, e.g.,
   *
   * swapAround(List("The", "horse", "the", "cart")) = List(the, cart, the, horse)
   *
   * Try your method out with a few lists of different element types to show it
   * in action.
   */

  def swapAround[A](xs: List[A]): List[A] =
//    val length = xs.length
//    val firstHalf = xs.take(length / 2)
//    val secondHalf = xs.drop(length / 2)
//    secondHalf ::: firstHalf
      val (ys, zs) = xs.splitAt(xs.length / 2)
      zs ::: ys


  @main
  def Exercise35(): Unit =
    println(swapAround(List("The", "horse", "the", "cart")))
    println(swapAround(List(1, 2, 3, 4, 5)))
    println(swapAround(List.empty))
    println(swapAround(List('a', 'b', 'c')))
    println(swapAround(List(true, true, false, true)))


  /*
   * Exercise 36
   * Write a method, groupInto, that takes a list and splits it into packets of a given
   * size. Here are some examples:
   *
   * groupInto(4, "abcdefghijkl".toList) =
   *     List(List(a, b, c, d), List(e, f, g, h), List(i, j, k, l))
   *
   * groupInto(3, List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) =
   *    List(List(0, 1, 2), List(3, 4, 5), List(6, 7, 8), List(9))
   *
   * groupInto(1, wordList) =
   *    List(List(The), List(quick), List(brown), List(fox), List(jumped), List(over),
   *         List(the), List(lazy), List(dog))
   *
   * groupInto(8, aardvark)) =
   *     List(List(a, a, r, d, v, a, r, k))
   *
   * The final list in the result may have fewer elements if there are insufficient
   * elements to make up the packet size.
   */

  def groupInto[A](size: Int, xs: List[A]): List[List[A]] =
    //xs match
    //case List() => Nil
    //case x :: _ => List(xs.take(size)) ::: groupInto(size, xs.drop(size))
      val blocks = (xs.indices by size).toList
      blocks.map(k => xs.drop(k)).map(_.take(size))
      //if size = 4
      //blocks.map(0 => xs.drop(0)).map(_.take(4)) is taking the first 4
      //blocks.map(4 => xs.drop(4)).map(_.take(4)) is dropping the first 4, but taking the next 4 after it
      //blocks.map(8 => xs.drop(8)).map(_.take(4)) is dropping the first 8, but taking the next 4 and so on
      //
      //So, for each number in List(0, 4, 8), which is produced by using (xs.indices by size).toList, it
      // creates a list, making a list of lists


  @main
  def Exercise36(): Unit =
    println(groupInto(4, "abcdefghijkl".toList))
    println(groupInto(3, List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)))
    println(groupInto(3, List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).reverse))
    println(groupInto(1, wordList))
    println(groupInto(8, aardvark))



  /*
   * Exercise 37
   * This exercise builds upon your previous answer and assumes that you have developed
   * a working version of the groupInto method.
   *
   * (a) Write a method, packetReverse, that splits a list into packets, reverses each
   *     packet, and flattens the result. For example:
   *
   *     packetReverse(3, intList) = List( 2, 1, 0, 5, 4, 3, 8, 7, 6, 9)
   *     packetReverse(2, wordList) = List(quick, The, fox, brown, over, jumped, `lazy`, the, dog)
   *     packetReverse(8, aardvark) = List(k, r, a, v, d, r, a, a)
   *
   *     You should avoid inserting a reverse method into a copy of your groupInto method.
   *     Rather, you should combine your groupInto method, with reverse and flatMap to achieve
   *     the result.
   *
   * (b) Write a method, tabulate, that takes a list of numbers and formats it as an HTML
   *     table with the given width. Thus, e.g.,
   *
   *     tabulate(2, List(1, 2, 3, 4) =
   *         <table>
    <tr>
   * <td>1</td>
   * <td>2</td>
   * </tr>
   * <tr>
   <td>3</td>
   * <td>4</td>
   * </tr>
   *         </table>
   *
   *     tabulate(List(1, List(1, 2)) =
   *         <table><tr><td>1</td></tr><tr><td>2</td></tr></table>
   *
   *     This can be solved using groupInto and map methods (although we do not want to restrict
   *     you to these if you wish to explore alternatives.) However, you will find the following
   *     method useful, too:  mkString.  The standard Scala method mkString compresses a list of
   *     strings into a single string.  E.g. List("abc", "def", "gh").mkString = "abcdefgh". (In
   *     fact, mkString is much more general than this - but we leave you to look it up if you
   *     want to learn more about it.)
   */

  def packetReverse[A](size: Int, xs: List[A]): List[A] =
    groupInto(size, xs).flatMap(_.reverse)
    //xs match {
    //case List() => Nil
    //case x :: _ => groupInto(size, xs.take(size).reverse).flatten
    //}

  def tabulate(width: Int, xs: List[Int]): String =
    val elements = xs.map(k => s"<td>$k</td>") // number is wrapped
    val rows = groupInto(width, elements)
      .map(_.mkString)
      .map(r => "<tr>" + r + "</tr>") //list of rows, maps each td inside tr
    "<table>" + rows.mkString + "</table>" //compress rows into a string

  @main
  def Exercise37(): Unit =
    println(packetReverse(4, "abcdefghijkl".toList))
    println(packetReverse(3, intList))
    println(packetReverse(2, wordList))
    println(packetReverse(8, aardvark))
    println(tabulate(2, List(1, 2, 3, 4)))
    println(tabulate(1, List(1, 2)))


  /*
  * Exercise 38
  * Tony Hoare's famous quicksort algorithm can be written recursively. See below. It uses
  * the head of the list as the pivot element which does not work well for nearly or fully
  * ordered data (O(n^2)). But for reasonably random data this version will have the n(log n)
  * performance normally associated with a good implementation of quicksort. The version
  * below is notable for the simplicity of its expression in a functional style.
  *
  * The function random(k) generates a list of k random characters drawn from the digits
  * 0-9 and the upper and lower case letters.
  *
  * Your task is to write the merge sort function that works by splitting the list into
  * two halves, recursively sorting each half, and then merging the resulting ordered
  * lists back together. Demonstrate your function in action.
  *
  */

  def qsort(xs: List[Char]): List[Char] = xs match
    case List() => List()
    case y :: ys =>
      val (less, more) = ys partition (_ <= y)
      qsort(less) ::: y :: qsort(more)
      //To obtain less and more, the tail of the list is partitioned in 2, on the left we have values smaller
      //than the head, which is the "less", and then the "more" is part of the list after the head
      //(on the right).
      //
      //qsort function is then applied with the same principle on the left hand side and it is dealt with first.
      //After all the necessary recursions, it is then applied on the right hand side.
      //We end up with a sorted list on qsort(less), then concatenate with the head, then a sorted list on
      //qsort(more)


  /**
   * Generates a list of k random alphanumeric characters
   */
  def random(k: Int): List[Char] = scala.util.Random.alphanumeric.take(k).toList

  /*
   * merge assumes that each of its arguments are in ascending order. The method
   * merges the two lists into one which is also in ascending order.
   */
  def merge(as: List[Char], bs: List[Char]): List[Char] = (as, bs) match
    case (List(), ys) => ys
    case (xs, List()) => xs
    case (x :: xs, y :: ys) =>
      if (x < y)
        x :: merge(xs, y :: ys)
      else
        y :: merge(x :: xs, ys)


  def msort(xs: List[Char]): List[Char] =
    if xs.length <= 1 then xs
    else
      val (ys, zs) = xs.splitAt(xs.length / 2)
      merge(msort(ys), msort(zs))
      //if xs = 83aQ
      //ys = 83 and zs = aQ
      //merge(msort(83), msort(aQ))
      //
      //left merge first, xs = 83
      //ys = 8 and zs = 3
      //merge(msort(8), msort(3))
      //merge(8, 3)
      //38
      //
      //right merge, xs = aQ
      //ys = a and zs = Q
      //merge(msort(a), msort(Q))
      //merge(a, Q)
      //Qa
      //
      //merge the two
      //38Qa

  @main
  def Exercise38(): Unit =
    val rs = random(4)
    println(rs.mkString)
    println(qsort(rs).mkString)
    println(msort(rs).mkString)


  /*
  * Exercise 39
  */

  extension (xs: List[Int])
    def *(n: Int): List[Int] = xs map (_ * n)
    def +(n: Int): List[Int] = xs map(_ + n)
    def allEven: Boolean = xs forall(_ % 2 == 0)
    def group(n :Int): List[List[Int]] = groupInto(n, xs)

  @main
  def Exercise39(): Unit =
    println(intList * 2)
    println(intList + 200)
    println(List(1, 2, 3, 6, 8, 9).allEven)
    println((List(1, 2, 3, 5, 7, 9, 11) * 2).allEven)
    println(intList group 3)
}
