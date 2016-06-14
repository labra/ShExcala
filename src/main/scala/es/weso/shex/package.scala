package es.weso

import es.weso.rdf.nodes.RDFNode
import es.weso.validating.{Checked, ConstraintError, ConstraintReason, MsgError}
import ConstraintReason._

/**
  * shex language definitions
  *
  *
  */
package object shex {


  /**
    * Defines a node that has been checked
    * It can be valid with a ConstraintReason or invalid with one or more ConstraintError's
    *
    */
  type CheckedRDFNode = Checked[RDFNode, ConstraintReason, ConstraintError[RDFNode]]


}
