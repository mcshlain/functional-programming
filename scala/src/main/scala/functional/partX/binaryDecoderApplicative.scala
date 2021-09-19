package functional.part4

import functional.part3.applicative.Applicative

object binaryDecoderApplicative {

  // TODO: missing a CoApplicative for the matching encoder implementation?
  
  trait BinaryDecoder[A] {
    def decodeStep(bytes: Array[Byte], startIndex: Int): Option[(Int, A)]

    def decode(bytes: Array[Byte]): Option[A] = {
      // start decoding from index 0
      decodeStep(bytes, 0) match {
        case Some((finalIndex, result)) =>
          if (finalIndex != bytes.length) {
            None // we did not cosume the entire array so we haven't succeeded in decoding 
          } else {
            Some(result)
          }
        case None => None
      }
    }
  }

  // we'll use empty list to represent everything is OK
  // but it would be better represented as Either[NonEmptyList[Error], Unit]
  // but since we did not talk about Either yet, we'll use this list representation
  object BinaryDecoder {
    def apply[A](f: (Array[Byte], Int) => Option[(Int, A)]): BinaryDecoder[A] = new BinaryDecoder[A] {
      override def decodeStep(bytes: Array[Byte], startIndex: Int): Option[(Int, A)] = f(bytes, startIndex)
    }
  }

  given Applicative[BinaryDecoder] with {

    // a pure value that has nothing to do with the encoding so we don't decode anything from the array
    def pure[A](a: A): BinaryDecoder[A] = BinaryDecoder(
      (arr, index) => Some((index, a)),
    )

    override def ap[A, B](fa: BinaryDecoder[A], ff: BinaryDecoder[A => B]): BinaryDecoder[B] = BinaryDecoder(
      (arr, index0) => {
        fa.decodeStep(arr, index0) match {
          case Some((index1, a)) =>
            ff.decodeStep(arr, index1) match {
              case Some((index2, f)) => Some((index2, f(a)))
              case None => None
            }
          case None => None
        }
      }
    )
  }

  //
  //  given BinaryDecoder[Int] {
  //    override def decodeStep(bytes: Array[Byte], startIndex: Int): Option[(Int, Int)] = {
  //      if (bytes.length - startIndex < 4) {
  //        None
  //      } else {
  //        val result = Array.ofDim[Int](4)
  //        Array.copy(bytes, startIndex, result, 0, 4)
  //        result.to
  //      }
  //    }
  //  } 


}
