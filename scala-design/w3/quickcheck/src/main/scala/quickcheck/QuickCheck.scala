package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(k, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("fuck") = forAll { (a: Int, b: Int) =>
    val min = a.min(b)
    findMin(insert(b, insert(a, empty))) == min
  }

  property("minOf5") = forAll { a: List[Int] =>
    var h = empty
    var result = true
    val sorted = a.sorted
    a.foreach(item => h = insert(item, h))
    for(i <- sorted.indices) {
      if(findMin(h) != sorted(i)) result = false
      h = deleteMin(h)
    }
    result
  }

}
