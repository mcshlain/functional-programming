package functional.part4

import functional.part1.maybePart4._
import functional.part1.pList._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object listFold {

  // right assosiative - with linked list this version can not be tail recursive (can optimize with local mutation)
  def foldListRight[A, B](lst: PList[A], zero: B)(f: (A, B) => B): B = lst match {
    case PNil => zero
    case PCons(head, tail) => f(head, foldListRight(tail, zero)(f))
  }
  
  // left assisiative, can have an efficient implementation using tailrec
  def foldListLeft[A, B](lst: PList[A], zero: B)(f: (B, A) => B): B = { 
   
    @tailrec 
    def build(l: PList[A], acc: B): B = l match {
      case PNil => acc
      case PCons(head, tail) => {
        val updatedAcc = f(acc, head)
        build(tail, updatedAcc)
      }
    }
    
    build(lst, zero)
  }


  // scala 3 syntax to add methods to existing classes (in scala 2 this can be done with implicit classes conversions)
  extension [A, B](lst: PList[A]) {
    
    def foldRight(zero: B)(f: (A, B) => B): B = foldListRight(lst, zero)(f)
    
    def foldLeft(zero: B)(f: (B, A) => B): B = foldListLeft(lst, zero)(f)
  }

}
 