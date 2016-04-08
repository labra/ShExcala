package es.weso.shex

import org.scalatest._

import es.weso.shex.DataFormat;
import es.weso.shex.SchemaFormat;
import es.weso.shex.SchemaLanguage;
import es.weso.shex.SchemaVocabulary;

class SchemaLanguageTest extends FunSpec with Matchers with TryValues {
  
  describe("SchemaLanguage") {

    it("should lookup schema language") {
      SchemaLanguage.lookup("TURTLE","SHEX").success.value should be(SchemaLanguage(SchemaFormat.TURTLE, SchemaVocabulary.SHEX))
      SchemaLanguage.lookup("turtle","shex").success.value should be(SchemaLanguage(SchemaFormat.TURTLE, SchemaVocabulary.SHEX))
      SchemaLanguage.lookup("TURTLE","SHACL").success.value should be(SchemaLanguage(SchemaFormat.TURTLE, SchemaVocabulary.SHACL))
      SchemaLanguage.lookup("SHEXC","SHEX").success.value should be(SchemaLanguage(SchemaFormat.SHEXC, SchemaVocabulary.SHEX))
      SchemaLanguage.lookup("SHEXC","SHACL").isFailure should be(true)
      SchemaLanguage.lookup("foo","bar").isFailure should be(true)
    }
    
    it("should lookup schema language with only format") {
      SchemaLanguage.lookupOnlyFormat("TURTLE").success.value should be(SchemaLanguage(SchemaFormat.TURTLE, SchemaVocabulary.SHEX))
    }
    
    it ("should lookup only format N-triples"){
      SchemaLanguage.lookupOnlyFormat("N-TRIPLES").success.value should be(SchemaLanguage(SchemaFormat(DataFormat.NTRIPLES.name), SchemaVocabulary.SHEX))
    }

    it ("should lookup only format n-triples lowercase and find it"){
      SchemaLanguage.lookupOnlyFormat("n-triples").success.value should be(SchemaLanguage(SchemaFormat(DataFormat.NTRIPLES.name), SchemaVocabulary.SHEX))
    }
    
    it ("should lookup only format foo and fail"){
      SchemaLanguage.lookupOnlyFormat("foo").failure 
    }
  }

}