package es.weso.shex.parser

import org.scalatest._
import org.scalatest.prop._

import es.weso.rdf.nodes._
import es.weso.rdf._
import es.weso.shex.parser._
import es.weso.shex.parser.ShExParser._
import es.weso.shex._
import es.weso.shex.Cardinality._
import scala.Either._
import es.weso.shex.PREFIXES._
import es.weso.shex.Cardinality._

import es.weso.rdf.PrefixMap


class ShExParserSuite extends ShExParser
    with FunSpecLike
    with Matchers
    with Checkers {

  describe("Shacl Parser") {

    describe("valueClass") {

      it("Should parse a value Class made by IRI") {
        val str = "IRI"

        val state: ShapeParserState = ShapeParserState.initial
        val result = ShExParser.parse(ShExParser.valueClass(state), str)
        result.get._1 should be(IRIKind(None,List()))
        result.get._2 should be(state)
      }

      it("Should parse a value Class made by an iri") {
        val str = "<http://example.org/string>"

        val state: ShapeParserState = ShapeParserState.initial
        val result = ShExParser.parse(ShExParser.valueClass(state), str)
        result.get._1 should be(Datatype(IRI("http://example.org/string"), List()))
        result.get._2 should be(state)
      }

      it("Should parse valueClass with qualified value type") {
        val prefix = "http://example.org/"
        val localName = "a"
        val alias = "ex"
        val str = alias + ":" + localName
        val state = ShapeParserState.initial.addPrefix(alias, IRI(prefix))
        val result = ShExParser.parse(ShExParser.valueClass(state), str)
        result.get._1 should be(Datatype(IRI(prefix + localName), List()))
      }

     it("Should parse a value Class made by reference") {
        val str = "@<a>"

        val state: ShapeParserState = ShapeParserState.initial
        val result = ShExParser.parse(ShExParser.valueClass(state), str)
        val (r,s) = result.get
        
        r should be(SingleShape(IRILabel(IRI("a"))))
      }

     it("Should parse a value Class made by ORs") {
        val str = "@<a> OR @<b>"

        val state: ShapeParserState = ShapeParserState.initial
        val result = ShExParser.parse(ShExParser.valueClassExpr(state), str)
        val (r,s) = result.get
        val a = SingleShape(IRILabel(IRI("a")))
        val b = SingleShape(IRILabel(IRI("b")))
        r should be(OrValueClass(Seq(a,b)))
      }
      
    }

   it("Should parse a value Object made by an IRI") {
        val iri = "http://www.example.org/"
        val str = "<" + iri + ">"

        val state: ShapeParserState = ShapeParserState.initial
        val result = parse(value(state), str)
        result.get._1 should be(ValueIRI(IRI(iri)))
        result.get._2 should be(state)
      }

    }

/*    describe("Prefix directives") {

      it("should parse prefixes") {
        val state = ShapeParserState.initial
        val str = "prefix a: <http://example.org/a/> "
        val result = ShapeParser.parse(ShapeParser.directive(state), str)
        val expected = state.addPrefix("a", IRI("http://example.org/a/"))
        result.get should be(expected)
      }

      it("should parse statement with prefixes") {
        val state = ShapeParserState.initial
        val str = "prefix a: <http://example.org/a/> \n" +
          "prefix b: <http://example.org/b/> \n"

        val expected = state.
          addPrefix("a", IRI("http://example.org/a/")).
          addPrefix("b", IRI("http://example.org/b/"))
        val noShapes: List[List[Shape]] = List()
        val shex: ShEx = ShEx(rules = List(), start = None)
        shouldParseState(shExParser, state, str, (shex, expected))
      }

      it("Should be able to parse default prefix") {
        val ex = "http://example.org/"
        val str = "PREFIX : <" + ex + ">\n" +
          "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" +
          ":s [ :a xsd:string ]"

        val state = ShapeParserState.initial
        val shape: ArcRule = ArcRule(
          id = None,
          n = NameTerm(IRI(ex + "a")),
          v = ValueType(xsd_string))

        val expected: ShEx = ShEx(
          rules = Seq(Shape(label = IRILabel(IRI("http://example.org/s")),
            rule = shape
          )),
          start = None)

        shouldParseIgnoreState(shExParser, state, str, expected)
      }

    } */

  describe("Shapes") {

    it("Should parse labels") {
      val state = ShapeParserState.initial
      val iri = "Label"
      val str = "<" + iri + ">"
      val result = ShExParser.parse(ShExParser.shapeLabel(state), str)
      result.get._1 should be(IRILabel(IRI(iri)))
    }

    it("Should parse qualified labels ") {
      val prefix = "http://example.org/"
      val localName = "a"
      val alias = "ex"
      val str = alias + ":" + localName
      val state = ShapeParserState.initial.addPrefix(alias, IRI(prefix))
      val result = ShExParser.parse(ShExParser.shapeLabel(state), str)
      result.get._1 should be(IRILabel(IRI(prefix + localName)))
    }

    /*     it("Should parse fixedValues with a set of one qualified value") {
       val prefix = "http://example.org/"
       val a = "a"
       val alias = "ex"
       val str = "( " + alias + ":" + a + " )"  
       val state = ShapeParserState.initial.addPrefix(alias,IRI(prefix))
       val result = parse(fixedValues(state),str)
       result.get._1 should be ((Seq(IRI(prefix + a))))
     } 

     it("Should parse fixedValues with a set of two qualified values ") {
       val prefix = "http://example.org/"
       val a = "a"
       val b = "b"
       val alias = "ex"
       val str = "( " + alias + ":" + a + " " + alias + ":" + b + " )"  
       val state = ShapeParserState.initial.addPrefix(alias,IRI(prefix))
       val result = ShapeParser.parse(ShapeParser.fixedValues(state),str)
       result.get._1 should be (ValueSet(Seq(IRI(prefix + a), IRI(prefix + b))))
     } */

    /*      it("Should parse fixedValues with a reference ") {
        val label = "http://example.org/a"
        val str = "@<" + label + ">"
        val state = ShapeParserState.initial
        val result = ShapeParser.parse(ShapeParser.fixedValues(state), str)
        result.get._1 should be(ValueReference(IRILabel(IRI(label))))
      } */

    /*      it("Should parse nameClassAndValue - single iri ") {
        val prefix = "http://example.org/"
        val a = "a"
        val b = "b"
        val alias = "ex"
        val str = alias + ":" + a + " " + alias + ":" + b
        val state = ShapeParserState.initial.addPrefix(alias, IRI(prefix))
        val result = ShapeParser.parse(ShapeParser.nameClassAndValue(state), str)
        result.get._1._1 should be(NameTerm(IRI(prefix + a)))
        result.get._1._2 should be(ValueType(IRI(prefix + b)))
      }

      it("Should parse arc - single iris ") {
        val prefix = "http://example.org/"
        val a = "a"
        val b = "b"
        val alias = "ex"
        val str = alias + ":" + a + " " + alias + ":" + b
        val state = ShapeParserState.initial.addPrefix(alias, IRI(prefix))
        val result = ShapeParser.parse(ShapeParser.arc(state), str)
        val expected: ArcRule = ArcRule(id = None,
          n = NameTerm(IRI(prefix + a)),
          v = ValueType(IRI(prefix + b)))
        result.get._1 should be(expected)
      }

      it("Should parse ruleSpec single") {
        val prefix = "http://example.org/"
        val a = "a"
        val b = "b"
        val alias = "ex"
        val str = "[ " + alias + ":" + a + " " + alias + ":" + b + " ]"
        val state = ShapeParserState.initial.addPrefix(alias, IRI(prefix))
        val result = ShapeParser.parse(ShapeParser.shapeSpec(state), str)
        val expected: ArcRule = ArcRule(id = None,
          n = NameTerm(IRI(prefix + a)),
          v = ValueType(IRI(prefix + b)))
        result.get._1 should be(expected)
      } */

/*    it("Should parse a single shape with rule") {
      val prefix = "http://example.org/"
      val a = "a"
      val b = "b"
      val c = "c"
      val alias = "ex"
      val str = """ex:a {ex:b ex:c }"""
      val state = ShapeParserState.initial.addPrefix(alias, IRI(prefix))
      val expected: ShapeRule =
        ShapeRule(IRILabel(IRI(prefix + a)),
          BasicShape.empty.copy(
              shapeExpr = TripleConstraint.empty.copy(
                id = None,
                iri = IRI(prefix + b),
                value = Datatype(IRI(prefix + c), List()),
                card = defaultCardinality
              ))
        )
      shouldParseIgnoreState(shapeRule, state, str, expected)
    } */

    it("Should parse a single shape (shexSchemaParser)") {
      val prefix = "http://example.org/"
      val a = "a"
      val b = "b"
      val c = "c"
      val alias = "ex"
      val str = "<http://example.org/a> { <http://example.org/b> <http://example.org/c> }"
      val state = ShapeParserState.initial
      val labelA = IRILabel(IRI(prefix + a)) 
      val shapeBC: Shape =
        Shape.fromShapeExpr(
              TripleConstraint.empty.copy(
                id = None,
                iri = IRI(prefix + b),
                value = Datatype(IRI(prefix + c), List()),
                card = defaultCardinality
              ))
      val expected = 
        Schema.empty.copy(
            shexSchema = ShExSchema.empty.copy(shapes = Map(labelA -> shapeBC)))
      shouldParseIgnoreState(shexDoc, state, str, expected)
    }

    it("Should parse a single shape with a prefix") {
      val prefix = "http://example.org/"
      val iriPrefix = IRI(prefix)
      val a = "a"
      val b = "b"
      val c = "c"
      val alias = "ex"
      val str = """|prefix : <http://example.org/>
                   |:a {:b :c }""".stripMargin
      val state = ShapeParserState.initial
      val labelA = IRILabel(IRI(prefix + a))
      val shapeBC: Shape =
        Shape.fromShapeExpr(
              TripleConstraint.empty.copy(
                id = None,
                iri = IRI(prefix + b),
                value = Datatype(IRI(prefix + c), List()),
                card = defaultCardinality
              ))
      val expected = 
        Schema.empty.copy(
            pm = PrefixMap(pm = Map("" -> iriPrefix)),
            shexSchema = ShExSchema.empty.copy(shapes = Map(labelA -> shapeBC)))
      shouldParseIgnoreState(shexDoc, state, str, expected)
    }

    it("Should parse a single shape with a prefix and a blank line (shaclSchemaParser)") {
      val prefix = "http://example.org/"
      val iriPrefix = IRI(prefix)
      val a = "a"
      val b = "b"
      val c = "c"
      val alias = "ex"
      val str = """|prefix : <http://example.org/>
                   |:a {:b :c }
                   |""".stripMargin
      val state = ShapeParserState.initial
      val labelA = IRILabel(IRI(prefix + a))
      val shapeBC: Shape =
        Shape.fromShapeExpr(
              TripleConstraint.empty.copy(
                id = None,
                iri = IRI(prefix + b),
                value = Datatype(IRI(prefix + c), List()),
                card = defaultCardinality
              ))
      val expected = 
        Schema.empty.copy(
            pm = PrefixMap(pm = Map("" -> iriPrefix)),
            shexSchema = ShExSchema.empty.copy(shapes = Map(labelA -> shapeBC)))
      shouldParseIgnoreState(shexDoc, state, str, expected)
    }

    it("Should parse several shapes") {
      val prefix = "http://example.org/"
      val iriPrefix = IRI(prefix)
      val a = "a"
      val b = "b"
      val c = "c"
      val alias = "ex"
      val str = """|prefix : <http://example.org/> 
                   |:a { :b :c }
                   |:b { :b :c } 
                   |""".stripMargin

      val state = ShapeParserState.initial
      val labelA = IRILabel(IRI(prefix + a))
      val labelB = IRILabel(IRI(prefix + b))
      val shapeBC = Shape.fromShapeExpr(
        TripleConstraint.empty.copy(
          id = None,
          iri = IRI(prefix + b),
          value = Datatype(IRI(prefix + c), List()),
          card = defaultCardinality
        ))
      val expected: Schema = 
        Schema.empty.copy(
           pm = PrefixMap(pm = Map("" -> iriPrefix)),
           shexSchema = ShExSchema.empty.copy(shapes = Map(labelA -> shapeBC, labelB -> shapeBC))
        )
      shouldParseIgnoreState(shexDoc, state, str, expected)
    }

/*    it("Should parse with begin") {
      val prefix = "http://example.org/"
      val a = "a"
      val b = "b"
      val c = "c"
      val alias = "ex"
      val str = """|begin = {
                   | <b> <c>
                   |} """.stripMargin

      val state = ShapeParserState.initial
      val begin = IRILabel(IRI("begin"))
      val labelB = IRILabel(IRI("b"))
      val ruleBC = OpenShape(
        shape = TripleConstraintCard(
          id = None,
          iri = IRI("b"),
          value = LiteralDatatype(IRI("c"), List()),
          card = defaultCardinality
        ),
        inclPropSet = Set())
      val shape1: Rule = Rule(label = begin, shapeDefinition = ruleBC, extensionCondition = List())
      val expected: SHACLSchema = SHACLSchema(id = None, rules = List(shape1), start = None)
      shouldParseIgnoreState(shaclSchemaParser, state, str, expected)
    } 
    
    it("Should parse statement with begin") {
      val prefix = "http://example.org/"
      val a = "a"
      val b = "b"
      val c = "c"
      val alias = "ex"
      val str = """|begin = { 
                   | <http://example.org/b> <http://example.org/c> 
                   |}""".stripMargin
      val state = ShapeParserState.initial
      val labelA = IRILabel(IRI(prefix + a))
      val labelB = IRILabel(IRI(prefix + b))
      val ruleBC = OpenShape(
        shape = TripleConstraintCard(
          id = None,
          iri = IRI(prefix + b),
          value = LiteralDatatype(IRI(prefix + c), List()),
          card = defaultCardinality
        ),
        inclPropSet = Set())
      val shape1: Rule = 
        Rule(label = IRILabel(IRI("begin")), 
                              shapeDefinition = ruleBC, 
                              extensionCondition = List())
      val expected = Some(shape1)
      shouldParseIgnoreState(statement, state, str, expected)
    }
    
    it("Should parse single begin") {
      val prefix = "http://example.org/"
      val a = "a"
      val b = "b"
      val c = "c"
      val alias = "ex"
      val str = """|begin = { 
                   | <http://example.org/b> <http://example.org/c> 
                   |}""".stripMargin

      val state = ShapeParserState.initial
      val labelA = IRILabel(IRI(prefix + a))
      val labelB = IRILabel(IRI(prefix + b))
      val ruleBC = OpenShape(
        shape = TripleConstraintCard(
          id = None,
          iri = IRI(prefix + b),
          value = LiteralDatatype(IRI(prefix + c), List()),
          card = defaultCardinality
        ),
        inclPropSet = Set())
      val shape1: Rule = 
        Rule(label = IRILabel(IRI("begin")), 
                              shapeDefinition = ruleBC, 
                              extensionCondition = List())
      val expected = shape1
      shouldParseIgnoreState(begin, state, str, expected)
    } */
    /*  it("Should parse directive with shape") {
        val prefix = "http://example.org/"
        val xsd = "http://www.w3.org/2001/XMLSchema#"
        val str = "PREFIX : <" + prefix + ">\n" +
          ":a [ :b :c ]"
        val state = ShapeParserState.initial
        val result = parse(shExParser(state), str)
        val ruleBC = ArcRule(id = None,
          n = NameTerm(IRI(prefix + "b")),
          v = ValueType(IRI(prefix + "c")))
        val labelA = IRILabel(IRI(prefix + "a"))
        val shape1: Shape = Shape(label = labelA, rule = ruleBC)
        val shex: ShEx = ShEx(rules = List(shape1), start = None)
        result.get._1 should be(shex)
      }

      it("Should parse empty rule") {
        val prefix = "http://example.org/"
        val str = "PREFIX : <" + prefix + ">\n" +
          ":a [ ]"
        val state = ShapeParserState.initial
        val result = ShapeParser.parse(ShapeParser.shExParser(state), str)
        val rule = EmptyRule
        val labelA = IRILabel(IRI(prefix + "a"))
        val shape1: Shape = Shape(label = labelA, rule = rule)
        val shex: ShEx = ShEx(rules = List(shape1), start = None)
        result.get._1 should be(shex)
      }

      it("Should parse or simple") {
        val prefix = "http://example.org/"
        val str = "PREFIX : <" + prefix + ">\n" +
          ":a [ :b :c | :b :d ]"
        val state = ShapeParserState.initial
        val ruleBC = ArcRule(id = None,
          n = NameTerm(IRI(prefix + "b")),
          v = ValueType(IRI(prefix + "c")))
        val ruleBD = ArcRule(id = None,
          n = NameTerm(IRI(prefix + "b")),
          v = ValueType(IRI(prefix + "d")))

        val rule = OrRule(ruleBC, ruleBD)
        val labelA = IRILabel(IRI(prefix + "a"))
        val shape1: Shape = Shape(label = labelA, rule = rule)
        val shex: ShEx = ShEx(rules = List(shape1), start = None)
        shouldParseIgnoreState(shExParser, state, str, shex)
      }

      it("Should parse and simple") {
        val prefix = "http://example.org/"
        val str = "PREFIX : <" + prefix + ">\n" +
          ":a [ :b :c , :b :d ]"
        val state = ShapeParserState.initial
        val ruleBC = ArcRule(id = None,
          n = NameTerm(IRI(prefix + "b")),
          v = ValueType(IRI(prefix + "c")))
        val ruleBD = ArcRule(id = None,
          n = NameTerm(IRI(prefix + "b")),
          v = ValueType(IRI(prefix + "d")))

        val rule = AndRule(ruleBC, ruleBD)
        val labelA = IRILabel(IRI(prefix + "a"))
        val shape1: Shape = Shape(label = labelA, rule = rule)
        val shex: ShEx = ShEx(rules = List(shape1), start = None)
        shouldParseIgnoreState(shExParser, state, str, shex)
      }
    }

    it("Should parse and/or simple") {
      val prefix = "http://example.org/"
      val str = "PREFIX : <" + prefix + ">\n" +
        ":a [ :b :c , (:b :d | :b :e) ]"
      val state = ShapeParserState.initial
      val ruleBC = ArcRule(id = None,
        n = NameTerm(IRI(prefix + "b")),
        v = ValueType(IRI(prefix + "c")))
      val ruleBD = ArcRule(id = None,
        n = NameTerm(IRI(prefix + "b")),
        v = ValueType(IRI(prefix + "d")))
      val ruleBE = ArcRule(id = None,
        n = NameTerm(IRI(prefix + "b")),
        v = ValueType(IRI(prefix + "e")))

      val rule = AndRule(ruleBC, OrRule(ruleBD, ruleBE))
      val labelA = IRILabel(IRI(prefix + "a"))
      val shape: Shape = Shape(label = labelA, rule = rule)
      val shex: ShEx = ShEx(rules = List(shape), start = None)
      shouldParseIgnoreState(shExParser, state, str, shex)
    }

    it("Should parse and/or simple withour paren") {
      val prefix = "http://example.org/"
      val str = "PREFIX : <" + prefix + ">\n" +
        ":a [ :b :c , :b :d | :b :e ]"
      val state = ShapeParserState.initial
      val ruleBC = ArcRule(id = None,
        n = NameTerm(IRI(prefix + "b")),
        v = ValueType(IRI(prefix + "c")))
      val ruleBD = ArcRule(id = None,
        n = NameTerm(IRI(prefix + "b")),
        v = ValueType(IRI(prefix + "d")))
      val ruleBE = ArcRule(id = None,
        n = NameTerm(IRI(prefix + "b")),
        v = ValueType(IRI(prefix + "e")))

      val rule = OrRule(AndRule(ruleBC, ruleBD), ruleBE)
      val labelA = IRILabel(IRI(prefix + "a"))
      val shape: Shape = Shape(label = labelA, rule = rule)
      val shex: ShEx = ShEx(rules = List(shape), start = None)
      shouldParseIgnoreState(shExParser, state, str, shex)
    }

    it("Should parse arc with star") {
      val prefix = "http://example.org/"
      val str = "PREFIX : <" + prefix + ">\n" +
        ":a [ :b :c* ]"
      val state = ShapeParserState.initial
      val ruleBC = star(ArcRule(id = None,
        n = NameTerm(IRI(prefix + "b")),
        v = ValueType(IRI(prefix + "c"))))
      val rule = ruleBC
      val labelA = IRILabel(IRI(prefix + "a"))
      val shape: Shape = Shape(label = labelA, rule = rule)
      val shex: ShEx = ShEx(rules = List(shape), start = None)
      shouldParseIgnoreState(shExParser, state, str, shex)
    }

    it("Should parse arc with stem") {
      val prefix = "http://example.org/"
      val str = "PREFIX : <" + prefix + ">\n" +
        ":a [ - <http://e.org/> ~ :c ]"
      val state = ShapeParserState.initial
      val ruleBC = ArcRule(id = None,
        n = NameAny(Set(IRIStem(IRI("http://e.org/"), true))),
        v = ValueType(IRI(prefix + "c")))
      val rule = ruleBC
      val labelA = IRILabel(IRI(prefix + "a"))
      val shape: Shape = Shape(label = labelA, rule = rule)
      val shex: ShEx = ShEx(rules = List(shape), start = None)
      shouldParseIgnoreState(shExParser, state, str, shex)
    }

    it("Should parse a|(b)") {
      val prefix = "http://example.org/"
      val xsd = "http://www.w3.org/2001/XMLSchema#"
      val str = "PREFIX : <" + prefix + ">\n" +
        "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" +
        ":a [ :a xsd:integer | ( :b xsd:integer ) ]"
      val state = ShapeParserState.initial
      val ruleA = ArcRule(id = None,
        n = NameTerm(IRI(prefix + "a")),
        v = ValueType(IRI(xsd + "integer")))
      val ruleB = ArcRule(id = None,
        n = NameTerm(IRI(prefix + "b")),
        v = ValueType(IRI(xsd + "integer")))
      val labelA = IRILabel(IRI(prefix + "a"))
      val shape: Shape = Shape(label = labelA, rule = OrRule(ruleA, ruleB))
      val shex: ShEx = ShEx(rules = List(shape), start = None)
      shouldParseIgnoreState(shExParser, state, str, shex)
    }

    it("Should parse (b)") {
      val prefix = "http://example.org/"
      val xsd = "http://www.w3.org/2001/XMLSchema#"
      val str = "PREFIX : <" + prefix + ">\n" +
        "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" +
        ":a [ ( :b xsd:integer ) ]"
      val state = ShapeParserState.initial
      val ruleB = ArcRule(id = None,
        n = NameTerm(IRI(prefix + "b")),
        v = ValueType(IRI(xsd + "integer")))
      val labelA = IRILabel(IRI(prefix + "a"))
      val shape: Shape = Shape(label = labelA, rule = ruleB)
      val shex: ShEx = ShEx(rules = List(shape), start = None)
      shouldParseIgnoreState(shExParser, state, str, shex)
    }

    it("Should parse (. . *)") {
      val prefix = "http://example.org/"
      val xsd = "http://www.w3.org/2001/XMLSchema#"
      val str = "PREFIX : <" + prefix + ">\n" +
        "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" +
        ":a [ . . * ]"
      val state = ShapeParserState.initial
      val ruleAny = StarRule(AnyRule)
      val labelA = IRILabel(IRI(prefix + "a"))
      val shape: Shape = Shape(label = labelA, rule = ruleAny)
      val shex: ShEx = ShEx(rules = List(shape), start = None)
      shouldParseIgnoreState(shExParser, state, str, shex)
    }

    it("Should parse (b .)") {
      val prefix = "http://example.org/"
      val xsd = "http://www.w3.org/2001/XMLSchema#"
      val str = "PREFIX : <" + prefix + ">\n" +
        "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" +
        ":a [ ( :b . ) ]"
      val state = ShapeParserState.initial
      val ruleB = ArcRule(id = None,
        n = NameTerm(IRI(prefix + "b")),
        v = ValueAny(Set()))
      val labelA = IRILabel(IRI(prefix + "a"))
      val shape: Shape = Shape(label = labelA, rule = ruleB)
      val shex: ShEx = ShEx(rules = List(shape), start = None)
      shouldParseIgnoreState(shExParser, state, str, shex)
    }

    it("Should parse b xsd:integer") {
      val prefix = "http://example.org/"
      val xsd = "http://www.w3.org/2001/XMLSchema#"
      val str = "PREFIX : <" + prefix + ">\n" +
        "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" +
        ":a [  :b xsd:integer  ]"
      val state = ShapeParserState.initial
      val ruleB = ArcRule(id = None,
        n = NameTerm(IRI(prefix + "b")),
        v = ValueType(IRI(xsd + "integer")))
      val labelA = IRILabel(IRI(prefix + "a"))
      val shape: Shape = Shape(label = labelA, rule = ruleB)
      val shex: ShEx = ShEx(rules = List(shape), start = None)
      shouldParseIgnoreState(shExParser, state, str, shex)
    }

    it("Should parse value exclusions") {
      val prefix = "http://example.org/"
      val void = "http://www.w3.org/2001/XMLSchema#"
      val str = "PREFIX : <" + prefix + ">\n" +
        "PREFIX void: <" + void + ">\n" +
        ":a [  :b ( - void:Dataset ) ]"
      val state = ShapeParserState.initial
      val ruleB = ArcRule(id = None,
        n = NameTerm(IRI(prefix + "b")),
        v = ValueSet(Seq(NoObject(RDFNodeObject(IRI(void + "Dataset"))))))
      val labelA = IRILabel(IRI(prefix + "a"))
      val shape: Shape = Shape(label = labelA, rule = ruleB)
      val shex: ShEx = ShEx(rules = List(shape), start = None)
      shouldParseIgnoreState(shExParser, state, str, shex)
    }

  }
*/
  }

  def shouldParse[A](p: Parser[A], s: String, a: A) {
    val result = parseAll(p, s) match {
      case Success(x, _) => x
      case NoSuccess(msg, _) => fail(msg)
    }
    result should be(a)
  }

  def shouldParseState[A, State](p: State => Parser[(A, State)],
    initial: State,
    s: String,
    expected: (A, State)) {
    val result = parseAll(p(initial), s) match {
      case Success(x, _) => x
      case NoSuccess(msg, _) => fail(msg)
    }
    result should be(expected)
  }

  def shouldParseIgnoreState[A, State](p: State => Parser[(A, State)],
    initial: State,
    s: String,
    expected: A) {
    val result = parseAll(p(initial), s) match {
      case Success(x, _) => x._1
      case NoSuccess(msg, _) => fail(msg)
    }
    if (result != expected) {
      println("\n" + result)
      println("\n" + expected)
    }
    result should be(expected)
  }

}