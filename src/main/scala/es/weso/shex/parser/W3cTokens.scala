package es.weso.shex.parser

import util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.{ Parsers, RegexParsers }
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input._
import scala.util.parsing.input.Positional
import util.parsing.input.CharSequenceReader.EofCh
import es.weso.rdf.nodes._
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.io.Codec
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import scala.collection.immutable.LinearSeq
import es.weso.rdf.PrefixMap

class ParserException(msg: String) extends RuntimeException(msg)

trait W3cTokens
    extends Positional
    with RegexParsers {

  override val skipWhitespace = false
  override val whiteSpace = WS_STR.r

  lazy val IRIREF: Parser[IRI] =
    acceptRegex("IRIREF", IRIREF_STR.r) ^?
      ({
        case x => {
          val rex = "<(.*)>".r
          val rex(cleanIRI) = x // removes < and >
          val scaped = unscapeUchars(cleanIRI)
          scaped match {
            case IRI(iri) => iri
          }
        }
      },
        {
          case x => "Cannot convert " + x + " to IRI"
        })

  lazy val IRIREF_STR = "<(" + IRI_INITIAL_STR + "|" + UCHAR_STR + ")*>"

  lazy val IRI_INITIAL_STR = """[^\u0000-\u0020<>\"{}\|\^`\\]"""

  //  def PNAME_NS_STR	= "(" + PN_PREFIX + ")?:"

  def acceptRegex(name: String, r: Regex): Parser[String] =
    (r | failure(name + " expected with regular expression " + r))

  def PNAME_NS_Parser: Parser[String] =
    //    acceptRegex("PNAME_NS",PNAME_NS_STR.r) 
    (opt(PN_PREFIX) <~ ":" ^^ {
      case None => ""
      case Some(str) => str
    }
      | failure("Expected prefix name"))

  def PNAME_NS(prefixMap: PrefixMap): Parser[IRI] = {
    PNAME_NS_Parser ^?
      ({
        case prefix if (prefixMap.contains(prefix)) => {
          prefixMap.getIRI(prefix).get
        }
      },
        {
          case prefix =>
            "Prefix " + prefix +
              " not found in Namespce map: " +
              prefixMap
        })
  }

  // lazy val PNAME_LN_STR = PNAME_NS_STR + PN_LOCAL

  def PNAME_LN(prefixMap: PrefixMap): Parser[IRI] = {
    PNAME_NS(prefixMap) ~ PN_LOCAL.r ^^
      {
        case prefix ~ local =>
          RDFNode.qNameIRI(prefix, unscapeReservedChars(local))
      }
  }

  lazy val BLANK_NODE_LABEL_STR = "_:(" + PN_CHARS_U + "|[0-9])((" + PN_CHARS_STR + "|\\.)*(" + PN_CHARS_STR + "))?"

  def BLANK_NODE_LABEL(bNodeTable: BNodeTable): Parser[(BNodeId, BNodeTable)] =
    acceptRegex("BLANK_NODE_LABEL", BLANK_NODE_LABEL_STR.r) ^^ {
      s => bNodeTable.getOrAddBNode(removeBNodePrefix(s))
    }

  lazy val LANGTAG = "@" ~> "[a-zA-Z]+(-[a-zA-Z0-9]+)*".r ^^ Lang

  lazy val INTEGER: Parser[Literal] = "[+-]?[0-9]+".r ^^
    { x => IntegerLiteral(str2Int(x)) }

  lazy val DECIMAL: Parser[Literal] = "[+-]?[0-9]*\\.[0-9]+".r ^^
    { x => DecimalLiteral(str2Decimal(x)) }

  lazy val DOUBLE: Parser[Literal] =
    ("[+-]?([0-9]+\\.[0-9]*" + EXPONENT + "|\\.[0-9]+" + EXPONENT + "|[0-9]+" + EXPONENT + ")").r ^^
      { x => DoubleLiteral(str2Double(x)) }

  lazy val EXPONENT = "[eE][+-]?[0-9]+"

  lazy val STRING_LITERAL_QUOTE_STR: String = "\"([^\\u0022\\u005C\\u000A\\u000D]|" + ECHAR_STR + "|" + UCHAR_STR + ")*\""
  lazy val STRING_LITERAL_SINGLE_QUOTE_STR = "'([^\\u0027\\u005C\\u000A\\u000D]|" + ECHAR_STR + "|" + UCHAR_STR + ")*'"
  lazy val STRING_LITERAL_LONG_SINGLE_QUOTE_STR = "'''(('|'')?[^']|" + ECHAR_STR + "|" + UCHAR_STR + ")*'''"
  lazy val STRING_LITERAL_LONG_QUOTE_STR = "\"\"\"(" + NO_TRIPLE_QUOTE + "|" + ECHAR_STR + "|" + UCHAR_STR + ")*\"\"\""

  lazy val NO_TRIPLE_QUOTE = """((\"|\"\")?[^\"\\])"""

  lazy val STRING_LITERAL_QUOTE: Parser[String] = STRING_LITERAL_QUOTE_STR.r ^^ {
    x => removeQuotes(unscape(x), "\"", 1)
  }

  lazy val STRING_LITERAL_SINGLE_QUOTE = STRING_LITERAL_SINGLE_QUOTE_STR.r ^^ {
    x => removeQuotes(unscape(x), "\'", 1)
  }
  lazy val STRING_LITERAL_LONG_SINGLE_QUOTE = STRING_LITERAL_LONG_SINGLE_QUOTE_STR.r ^^
    { x => removeQuotes(unscape(x), "\'", 3) }
  lazy val STRING_LITERAL_LONG_QUOTE = STRING_LITERAL_LONG_QUOTE_STR.r ^^
    { x => removeQuotes(unscape(x), "\"", 3) }

  lazy val UCHAR_STR = "\\\\u" + HEX + "{4}" + "|" + "\\\\U" + HEX + "{8}"
  lazy val UCHAR_Parser: Parser[Char] =
    acceptRegex("UCHAR", UCHAR_STR.r) ^^ { x => UCHAR2char(x) }

  lazy val ECHAR_STR = """\\[tbnrf\\\"\']"""
  lazy val ECHAR_Parser: Parser[Char] =
    acceptRegex("ECHAR", ECHAR_STR.r) ^^ { x => ECHAR2char(x) }

  lazy val WS_STR = """\u0020|\u0009|\u000D|\u000A"""
  lazy val WS = rep(WS_STR.r
    | "#" ~ rep(chrExcept(EofCh, '\n')))

  def chrExcept(cs: Char*) = elem("", ch => (cs forall (ch != _)))

  lazy val ANON_STR = "\\[(" + WS_STR + ")*\\]"

  def ANON(bNodeTable: BNodeTable): Parser[(BNodeId, BNodeTable)] =
    ANON_STR.r ^^ { _ => bNodeTable.newBNode
    }

  lazy val PN_CHARS_BASE_Parser: Parser[Char] = PN_CHARS_BASE.r ^^ { x => str2Char(x) }

  lazy val PN_CHARS_BASE =
    """[a-zA-Z\u00C0-\u00D6\u00D8-\u00F6""" +
      """\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF""" +
      """\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF""" +
      """\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD""" +
      """\x{10000}-\x{EFFFF}]"""

  lazy val PN_CHARS_U = PN_CHARS_BASE + "|_"

  lazy val PN_CHARS_STR = PN_CHARS_U + """|\-|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040]"""

  lazy val PN_CHARS = acceptRegex("PN_CHARS", PN_CHARS_STR.r)

  lazy val PLX = PERCENT + "|" + PN_LOCAL_ESC

  def startMiddleAltRep_STR(start: String, middleAlt: String, repEnd: String): String = {
    "(" + start + "(((" + repEnd + "|" + middleAlt + ")*(" + repEnd + "))?)" + ")"
  }

  def startMiddleAltRep(start: String, middleAlt: String, repEnd: String): Parser[String] =
    startMiddleAltRep_STR(start, middleAlt, repEnd).r

  def PN_PREFIX_STR = startMiddleAltRep_STR(PN_CHARS_BASE, "\\.", PN_CHARS_STR)

  def PN_PREFIX = acceptRegex("PN_PREFIX", PN_PREFIX_STR.r)

  //  lazy val PN_PREFIX_STR = PN_CHARS_BASE + "((" + PN_CHARS + """|\.)*""" + PN_CHARS + ")?" 
  /* lazy val PN_PREFIX = 
   PN_CHARS_BASE.r ~ opt(rep(PN_CHARS_DOT) ~ PN_CHARS) ^^ {
    case c ~ None => c
    case c ~ Some(cs ~ d) => c + cs.flatten + d
  }
 lazy val PN_CHARS_DOT = "." | PN_CHARS */

  lazy val PN_LOCAL = "(" + PN_CHARS_U + "|:|[0-9]|" + PLX + ")((" + PN_CHARS_STR + "|\\.|:|" + PLX + ")*(" + PN_CHARS_STR + "|:|" + PLX + "))?"

  lazy val PERCENT = "%" + HEX + HEX

  lazy val HEX = """[0-9A-Fa-f]"""

  lazy val PN_LOCAL_ESC = """[\\][_~\.\-!$&'\(\)*+,;=/?#@%]"""

  def UCHAR2char(uchar: String): Char = {
    val rex = """\\[uU](.*)""".r
    uchar match {
      case rex(x) => Integer.parseInt(x, 16).toChar
      case _ => throw new ParserException("Internal Error: cannot convert uchar " + uchar + " to " + rex.pattern)
    }
  }

  def ECHAR2char(echar: String): Char = {
    echar match {
      case "\\t" => '\t'
      case "\\b" => '\b'
      case "\\n" => '\n'
      case "\\r" => '\r'
      case "\\f" => '\f'
      case "\\\"" => '\"'
      case _ => throw new ParserException("Internal Error: cannot convert ECHAR " + echar + " to character")
    }
  }

  /**
   * remove quotes from a quoted string
   *
   * @param s input string
   * @param quote Type of quotes, may be simple or double
   * @param Number of quotes, normally 1 or 3
   * @return the new string
   *
   */
  def removeQuotes(s: String, quote: String, number: Int): String = {
    // Note: (?s) enables multiline matching
    val rex = ("(?s)" + quote + "{" + number.toString + "}(.*)" + quote + "{" + number.toString + "}").r
    val rex(newS) = s
    newS
  }

  // from _:xxx to xxx
  def removeBNodePrefix(s: String): String = {
    s.drop(2)
  }

  def str2Double(s: String): Double = s.toDouble
  def str2Decimal(s: String): BigDecimal = BigDecimal(s)
  def str2Int(s: String): Integer = Integer.parseInt(s)

  // The following code does the unscape traversing the list recursively
  def str2Char(str: String): Char = {
    str.charAt(0)
  }

  def hex2String(s: List[Char]): String = {
    try {
      Character.toChars(Integer.parseInt(s.mkString, 16)).mkString
    } catch {
      case e: Exception => throw new Exception("Internal Error 'hex2Char': cannot convert from unicode chars. Value: " +
        s.mkString + "\n " + "Exception raised: " + e.toString)
    }
  }

  def unscape(s: String): String = {

    @tailrec
    def unscapeHelper(s: List[Char], tmp: StringBuilder): String = {
      s match {
        case '\\' :: 'u' :: a :: b :: c :: d :: rs =>
          unscapeHelper(rs, tmp ++= hex2String(a :: b :: c :: d :: Nil))
        case '\\' :: 'U' :: a :: b :: c :: d :: e :: f :: g :: h :: rs =>
          unscapeHelper(rs, tmp ++= hex2String(a :: b :: c :: d :: e :: f :: g :: h :: Nil))
        case '\\' :: 't' :: rs => unscapeHelper(rs, tmp += '\t')
        case '\\' :: 'b' :: rs => unscapeHelper(rs, tmp += '\b')
        case '\\' :: 'n' :: rs => unscapeHelper(rs, tmp += '\n')
        case '\\' :: 'r' :: rs => unscapeHelper(rs, tmp += '\r')
        case '\\' :: 'f' :: rs => unscapeHelper(rs, tmp += '\f')
        case '\\' :: '\"' :: rs => unscapeHelper(rs, tmp += '\"')
        case '\\' :: '\'' :: rs => unscapeHelper(rs, tmp += '\'')
        case '\\' :: '\\' :: rs => unscapeHelper(rs, tmp += '\\')
        case c :: rs => unscapeHelper(rs, tmp += c)
        case Nil => tmp.mkString
      }
    }
    unscapeHelper(s.toList, new StringBuilder)
  }

  def unscapeUchars(s: String): String = {

    @tailrec
    def unscapeUcharsHelper(s: LinearSeq[Char], tmp: StringBuilder): String = {
      s match {
        case '\\' :: 'u' :: a :: b :: c :: d :: rs =>
          unscapeUcharsHelper(rs, tmp ++= hex2String(a :: b :: c :: d :: Nil))
        case '\\' :: 'U' :: a :: b :: c :: d :: e :: f :: g :: h :: rs =>
          unscapeUcharsHelper(rs, tmp ++= hex2String(a :: b :: c :: d :: e :: f :: g :: h :: Nil))
        case c :: rs => unscapeUcharsHelper(rs, tmp += c)
        case Nil => tmp.mkString
      }
    }

    unscapeUcharsHelper(s.toList, new StringBuilder)
  }

  def unscapeReservedChars(s: String): String = {

    @tailrec
    def unscapeReservedCharsHelper(s: List[Char], tmp: StringBuilder): String = {
      s match {
        case '\\' :: c :: rs if "~.-!$&'()*+,;=/?#@%_".contains(c) =>
          unscapeReservedCharsHelper(rs, tmp += c)
        case c :: rs =>
          unscapeReservedCharsHelper(rs, tmp += c)
        case Nil => tmp.mkString
      }
    }

    unscapeReservedCharsHelper(s.toList, new StringBuilder)
  }

  // Alternative way to unscape using regular expressions....
  def hex2str(s: String): String = {
    Integer.parseInt(s.mkString, 16).toChar.toString
  }

  def unscapeUnicode4(s: String): String = {
    val rex = """\\u(\p{XDigit}{4})""".r
    rex.replaceAllIn(s, m => Regex quoteReplacement hex2str(m group 1))
  }

  def unscapeUnicode8(s: String): String = {
    val rex = """\\U(\p{XDigit}{8})""".r
    rex.replaceAllIn(s, m => Regex quoteReplacement hex2str(m group 1))
  }

  def unscapeCtrl(s: String): String = {
    val ctrl = """\\[bnrtf\\"]""".r
    ctrl.replaceAllIn(s, m => Regex quoteReplacement ctrl2str(m group 1))
  }

  def ctrl2str(s: String): String = {
    s match {
      case "\\b" => "\b"
      case "\\t" => "\t"
      case "\\n" => "\n"
      case "\\r" => "\r"
      case "\\f" => "\f"
      case "\\\"" => "\""
      case "\\\'" => "\'"
      case "\\\\" => "\\"
      case s => s
    }
  }

  def unscape2(x: String): String =
    (unscapeUnicode4 _ andThen unscapeUnicode8 andThen unscapeCtrl)(x)
  //------------------------------------

}

