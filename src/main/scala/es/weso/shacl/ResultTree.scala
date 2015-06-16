package es.weso.shacl
import es.weso.utils.ProofTree

package object ResultTree {

  type ErrorCondition

  type RTree = ProofTree[ValidationState,ErrorCondition]
  
  
}
