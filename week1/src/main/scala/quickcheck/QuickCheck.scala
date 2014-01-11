package quickcheck

import common._

import scala.math._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  
  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == min(a,b)
  }
  
  property("empty1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }
  
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  property("meld1") = forAll { (h1: H, h2: H) =>
    val m = if (isEmpty(h1)) 0 else findMin(h1)
    val n = if (isEmpty(h2)) m else findMin(h2)
    
    val h = meld(h1,h2)
    val nm = if (isEmpty(h)) 0 else findMin(h)
    
    nm == min(m,n)
  }
  
  property("delete1") = forAll { (h: H) =>
    
      def helper(a: Int, ts: H, acc: Boolean): Boolean = {
          if(isEmpty(ts) || !acc) acc
          else {
            val m = findMin(ts)
            helper(m, deleteMin(ts), a <= m && acc)
          }
      }
      
      if(isEmpty(h)) true
      else helper(findMin(h), deleteMin(h), true)
  }
  
  property("delete&meld1") = forAll { (h1: H, h2: H) =>
    
      def helper(xs: H, ys: H, acc: Boolean): Boolean = {
          if(isEmpty(xs) || !acc) acc
          else {
              val mx = findMin(xs)
              val my = findMin(ys)
              helper(deleteMin(xs), deleteMin(ys), (mx == my) && acc)
          }
      }
      
      if(isEmpty(h1) || isEmpty(h2)) true
      else {
          val m = findMin(h1)
          helper(meld(h1,h2), meld(deleteMin(h1),insert(m,h2)), true)
      }
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    m <- oneOf(value(empty), genHeap)
  } yield insert(v, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
