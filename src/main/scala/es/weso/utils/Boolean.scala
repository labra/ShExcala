package es.weso.utils

object Boolean {
  
def all(vs: Traversable[Boolean]): Boolean = {
    vs.fold(true)(_ && _)
}
  
def some(vs: Traversable[Boolean]): Boolean = {
    vs.fold(false)(_ || _)
}

}
