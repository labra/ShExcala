package es.weso.shacl

case class SchemaFormat(name: String) 

object SchemaFormat {
  
  lazy val SHEXC_NAME = "SHEXC"
  lazy val JSONAST_NAME = "JSONAST"
  
  lazy val SHEXC   = SchemaFormat(SHEXC_NAME.toUpperCase)
  lazy val JSONAST = SchemaFormat(JSONAST_NAME.toUpperCase)
  lazy val TURTLE = SchemaFormat(DataFormat.TURTLE.name)
  lazy val shexFormats : Seq[SchemaFormat] = 
    Seq(SHEXC,JSONAST)
    
  lazy val rdfFormats : Seq[SchemaFormat] = 
    DataFormat.toList.map(s => SchemaFormat(s.toUpperCase))

  /**
   * Available list of formats
   */
  lazy val formats : Seq[SchemaFormat] = shexFormats ++ rdfFormats
  
  def lookup(str: String): Option[SchemaFormat] = {
    lookupIn(str,formats)
  }
  
  def lookupIn(str:String, ls: Seq[SchemaFormat]): Option[SchemaFormat] = {
    ls.find(_.name == str.toUpperCase)
  }

  /**
   * Check if a format is available
   */
  def available(format: String): Boolean = {
    formats.find(_.name == format.toUpperCase()).nonEmpty 
  }

  lazy val toList: List[String] = 
    formats.map(_.toString).toList

  override def toString(): String = {
    toList.mkString(",")
  }
  
  /**
   * Default schema format
   */
  def default : SchemaFormat = 
    formats(0)
  
}