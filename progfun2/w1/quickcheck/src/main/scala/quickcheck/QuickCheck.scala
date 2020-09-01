package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /**
    * If you insert any two elements into an empty heap, finding the minimum of the resulting heap
    * should get the smallest of the two elements back.
   */
  property("findMin should return the min of two elements") = forAll { (a: Int, b: Int) =>
    val m = if (a < b) a else b
    val h = insert(b, insert(a, empty))
    findMin(h) == m
  }

  /**
    * You should be able to create a heap from a reverse sorted list
    */
  property("create reversed heap") = forAll { (l: List[Int]) =>
    val sorted = l.sorted
    val h = sorted.foldLeft(empty)((h, x) => insert(x, h))
    heapsort(h) == sorted
  }

  /**
    * If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
    */
  property("deleting the min from a heap of size 1 should return an empty heap") = forAll { (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  def heapsort(h: H): List[A] = if (isEmpty(h)) List() else findMin(h) :: heapsort(deleteMin(h))

  /**
    * Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */
  property("heaps are sorted") = forAll { (h: H) => {
    heapsort(h) == heapsort(h).sorted
  }}
}
