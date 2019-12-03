package richcollection

import java.util.NoSuchElementException

import scala.annotation.tailrec

class RichList {

  //  implicit def toRichList[A](list: List[A]): RichList = RichList.apply
  //  implicit val list =
  /**
    * P01 (*) Find the last element of a list.
    * Example:
    * scala> last(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 8
    */
  def last(list: List[Int]): Int = {
    list match {
      case Nil => 0
      case x :: Nil => x
      case x :: xs => last(xs)
    }
  }

  /**
    * P02 (*) Find the last but one element of a list.
    * Example:
    * scala> penultimate(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 5
    */
  def lastButOne(list: List[Int]): Int = {
    list match {
      case Nil => 0
      case x :: _ :: Nil => x
      case x :: _ :: xs => lastButOne(xs)
    }
  }

  /**
    * P03 (*) Find the Kth element of a list.
    * By convention, the first element in the list is element 0.
    * Example:
    *
    * scala> nth(2, List(1, 1, 2, 3, 5, 8))
    * res0: Int = 2
    */
  def kthElement(list: List[Int], k: Int): Int = {
    def kthElem(list: List[Int], counter: Int): Int = {
      list match {
        case Nil => 0
        case x :: xs if counter == k => x
        case x :: xs => kthElem(xs, counter + 1)
      }
    }

    if (k <= list.size)
      kthElem(list, 0)
    else 0
  }

  def kthElementBuiltIn[T](list: List[T], k: Int): T = {
    if (list.nonEmpty) list(k) else throw new NoSuchElementException
  }


  /**
    * P04 (*) Find the number of elements of a list.
    * Example:
    * scala> length(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 6
    */
  def noOfElementsBuiltIn[T](list: List[T]): Int = list.size

  def noOfElements[T](list: List[T]): Int = {

    @tailrec
    def elements(ls: List[T], count: Int): Int = {
      ls match {
        case h :: Nil => count + 1
        case h :: li => elements(li, count + 1)
      }
    }

    elements(list, 0)
  }

  /**
    * P05 (*) Reverse a list.
    * Example:
    * scala> reverse(List(1, 1, 2, 3, 5, 8))
    * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
    */
  def reverse(list: List[Int]): List[Int] = {
    list match {
      case Nil => Nil
      case x :: xs => reverse(xs) :+ x
    }
  }


  /**
    * P06 (*) Find out whether a list is a palindrome.
    * Example:
    * scala> isPalindrome(List(1, 2, 3, 2, 1))
    * res0: Boolean = true
    */
  def isPalindrome[T](list: List[T]): Boolean = {
    if (list.length <= 1) true
    else {
      @tailrec
      def isPalRec(left: Int, right: Int): Boolean = {
        if (left > right) true
        else {
          if (list(left) == list(right)) isPalRec(left + 1, right - 1)
          else false
        }
      }

      isPalRec(0, list.length - 1)
    }
  }

  /**
    * P07 (**) Flatten a nested list structure.
    * Example:
    * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
    *
    * @param list
    * @return
    */
  def flattenBuiltIn(list: List[List[Int]]): List[Int] = {
    list.flatten
  }

  def flatten[T](list: List[List[T]]): List[T] = {

    def recList(inputList: List[List[T]], resultList: List[T]): List[T] = {
      inputList match {
        case Nil => resultList
        case head :: ls => recList(ls, resultList ++ head)
      }
    }

    recList(list, List.empty[T])
  }

  /**
    * P08 (**) Eliminate consecutive duplicates of list elements.
    * If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
    * Example:
    *
    * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
    *
    * @param list
    * @return
    */
  def eliminateConsecutiveDuplicate[T](list: List[T]): List[T] = {

    def removeConsecutiveDuplicate(input: List[T], output: List[T], prev: T): List[T] = {
      input match {
        case Nil => output
        case h :: tail if prev == h => removeConsecutiveDuplicate(tail, output, prev)
        case h :: tail => removeConsecutiveDuplicate(tail, output :+ h, h)
      }
    }

    if (list.nonEmpty) removeConsecutiveDuplicate(list.tail, List(list.head), list.head)
    else list
  }

  /**
    * P09 (**) Pack consecutive duplicates of list elements into sublists.
    * If a list contains repeated elements they should be placed in separate sublists.
    * Example:
    *
    * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    */
  def packConsecutiveDuplicateElementsToList[T](list: List[T]): List[List[T]] = {

    @tailrec
    def recPackDuplicates(input: List[T], prev: T, output: List[List[T]], elem: List[T]): List[List[T]] = {
      input match {
        case Nil => output:+elem
        case head :: tail if prev == head => recPackDuplicates(tail, prev, output, elem :+ head)
        case head :: tail => recPackDuplicates(tail, head, output :+ elem, List(head))
      }
    }
    if (list.isEmpty) List.empty[List[T]]
    else recPackDuplicates(list.tail, list.head, List.empty[List[T]], List(list.head))
  }

  /**
    * P10 (*) Run-length encoding of a list.
    * Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
    * Example:
    *
    * scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    */
  def runLengthEncoding[T](list: List[T]): List[(Int, T)] = {
    val packedList = packConsecutiveDuplicateElementsToList(list)

    @tailrec
    def rec(input: List[List[T]], output: List[(Int, T)]): List[(Int, T)] = {
      if (input.nonEmpty) {
        rec(input.tail, output :+ (input.length, input.head.head))
      } else output
    }
    rec(packedList, List.empty[(Int, T)])
  }

}

object RichList {
  def apply[A](): RichList = new RichList

  def apply[A](a: A*): List[A] = List(a: _*)

  implicit def toRichList[A](list: List[A]): RichList = RichList()
}
