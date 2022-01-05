package functional.part8

object ad_naturalTransformation {

  // In cats this is called FunctionK (K stands of Kind, which is a level above type)
  trait NaturalTransformation[F[_], G[_]] { self =>  // this just gives us an alias for this

    // This is the only method that is not implemented (rembemer this is the __call__ equivelent in python)
    def apply[A](fa: F[A]): G[A]

    // Composition of two natural transformaitons (it's just like function composition)
    def compose[E[_]](f: NaturalTransformation[E, F]): NaturalTransformation[E, G] =
      new NaturalTransformation[E, G] {
        def apply[A](fa: E[A]): G[A] = self(f(fa))
      }

    // composition with a more natural order of arguments (just like we have with functions)
    def andThen[H[_]](f: NaturalTransformation[G, H]): NaturalTransformation[F, H] =
      f.compose(self)
  }


  // The identity natural transformation
  def identityNT[F[_]]: NaturalTransformation[F, F] = new NaturalTransformation {
    def apply[A](fa: F[A]): F[A] = fa
  }

  // A type alias for some syntactic Sugar (tilda arrow)
  // this way "NaturialTransformation[List, Option]" can also be written as "List ~> Option"
  type ~>[F[_], G[_]] = NaturalTransformation[F, G]


  def main(args: Array[String]): Unit = {

    val optToList: Option ~> List = new (Option ~> List) {
      override def apply[A](fa: Option[A]): List[A] = fa match {
        case Some(value) => List(value)
        case None => List.empty
      }
    }

    val listToOpt: List ~> Option = new (List ~> Option) {
      override def apply[A](fa: List[A]): Option[A] = fa match {
        case h:: t => Some(h) // we just ignore all other values in the list
        case Nil => None
      }
    }


    val value = List(1,2,3,4,5)

    println(listToOpt(value))

    val composedNT = listToOpt.andThen(optToList)

    println(composedNT(value))
  }

}
