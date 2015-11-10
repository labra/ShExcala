package es.weso.rdf.validator

case object ValidatorVersions {
  lazy val validatorVersions = List("SHEX3")

  def available(x: String): Boolean = {
    validatorVersions.contains(x)
  }

  def default = validatorVersions(0)

}
