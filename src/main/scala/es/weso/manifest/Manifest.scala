package es.weso.manifest
import es.weso.rdf.nodes._

case class Manifest(
    label: Option[String], 
    comment: Option[String],
    entries: List[Entry],
    includes: List[(IRI, Option[Manifest])]
    )
    
case class Entry(
    entryType: EntryType,
    name: String, 
    action: ManifestAction,
    result: Result,
    status: Status,
    specRef: Option[IRI]
    ) 
    
sealed trait EntryType
final case object Validate extends EntryType
final case object MatchNodeShape extends EntryType
final case object ValidationTest extends EntryType
final case object ValidationFailure extends EntryType
final case object WellFormedSchema extends EntryType
final case object NonWellFormedSchema extends EntryType
final case object ConvertSchemaSyntax extends EntryType

    
case class ManifestAction(
    schema: Option[IRI],
    schemaFormat: Option[String],
    data: Option[IRI],
    dataFormat: Option[String],
    schemaOutputFormat: Option[IRI],
    node: Option[IRI],
    shape: Option[IRI]
    ) {
  def setSchema(iri: IRI): ManifestAction = {
    this.copy(schema = Some(iri))
  }
  
  def setData(iri: IRI): ManifestAction = {
    this.copy(data = Some(iri))
  }
  
}
    
object ManifestAction {

  def apply(): ManifestAction = {
    ManifestAction(schema = None,
        schemaFormat = None,
        data = None,
        dataFormat = None,
        schemaOutputFormat = None,
        node = None,
        shape = None)
  }
}
    
sealed trait Result {

  def asBoolean: Option[Boolean] = {
    this match {
      case BooleanResult(b) => Some(b)
      case _ => None
    }
  }
} 

final case object ValidResult 
    extends Result
final case class NotValidResult(
    errors: Set[ErrorCondition]
    ) extends Result
final case class BooleanResult(
    value: Boolean
    ) extends Result
final case class IRIResult(
    value: IRI
    ) extends Result
final case object EmptyResult extends Result
    

final case class ErrorCondition(
    root: Option[IRI], 
    predicate: Option[IRI],
    subject: Option[IRI],
    obj: Option[IRI]
    )

case class Status(value: IRI)