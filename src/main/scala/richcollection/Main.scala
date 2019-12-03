package richcollection

object Main {

  def main(args: Array[String]): Unit = {
    val richList= new RichList()
    println("size:(5) "+richList.noOfElements(List(1,2,5,6,6)))
    println(s"Is Palindrome(positive) even number of elem: ${richList.isPalindrome(List(1,2,3,3,2,1))}")
    println(s"Is Palindrome(negative): ${richList.isPalindrome(List(1,2,3,4,2,1))}")
    println(s"Is Palindrome(positive) odd number of elem: ${richList.isPalindrome(List(1,2,3,4,3,2,1))}")
    println(s"Flattening list:  ${richList.flatten(List( List(1,2,3), List(3,4)))}")
    println(s"Eliminate consecutive duplicates: ${richList.eliminateConsecutiveDuplicate(List(1,1,3,3,4,4,4,4))}")
    println(s"Recursive pack consecutive duplicates: ${richList.packConsecutiveDuplicateElementsToList(List(1,1,3,3,4,4,4,4))}")
    println(s"runLengthEncoding: ${richList.runLengthEncoding(List('a','a','x','x','x','p','p','s'))}")

  }
}