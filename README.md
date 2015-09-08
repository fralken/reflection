Experiments with Scala reflection
---------------------------------

Instance a `case class` using reflection, retrieving default parameters either at compile time using macros or at runtime.
`case class`s default parameters are methods of the companion object, named `apply$default$N` where `N` is the parameter position in the signature.