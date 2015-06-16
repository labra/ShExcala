package es.weso.utils

/**
 * @author Labra
 */
sealed trait ProofTree[+A,+Error]

case class And[A,Error](branches: List[ProofTree[A,Error]]) extends ProofTree[A,Error]
case class Or[A,Error](branches: List[ProofTree[A,Error]]) extends ProofTree[A,Error]
case class Fail[Error](error: Error) extends ProofTree[Nothing,Error]
case class Success[A,Error](info: A) extends ProofTree[A,Nothing]


