package es.weso.rbe

import org.scalacheck._
import Prop._
import Gen._
import es.weso.collection._

object IntervalSpec extends Properties("Intervals") {
  
  val letter = Gen.oneOf('a','b','c','d','e')
  
  val bag : Gen[Bag[Char]] = Gen.containerOf[List,Char](letter).map(x => Bag.toBag(x))
  
  // Generates a controlled set of cardinalities
  val genCard: Gen[(Int,Int)] = Gen.oneOf(
      (0,0),(0,1),(1,1),(0,2),(1,2),(2,2),(3,2),(20,20),(1,20),(2,20),(3,20)
  )
  
  def genEmpty : Gen[Sorbe[Char]] = 
    const(Empty)
    
  def genSymbol : Gen[Sorbe[Char]] = for {
    (m,n) <- genCard
    a <- letter
  } yield Symbol(a,m,n)
  
  def genAnd(level: Int) : Gen[Sorbe[Char]]= for {
    v1 <- genSorbe(level)
    v2 <- genSorbe(level)
  } yield And(v1,v2)
  
  def genOr(level: Int) : Gen[Sorbe[Char]]= for {
    v1 <- genSorbe(level)
    v2 <- genSorbe(level)
  } yield Or(v1,v2)
  
  def genPlus(level: Int) : Gen[Sorbe[Char]]= for {
    v <- genSorbe(level)
  } yield Plus(v)
  
  def genStar(level: Int) : Gen[Sorbe[Char]]= for {
    v <- genSorbe(level)
  } yield Star(v)
  
  def genSorbe(level:Int) = 
    if (level >= 5) oneOf(genEmpty, genSymbol)
    else {
      val newLevel = level + 1
      oneOf(genEmpty, 
          genSymbol, 
          genAnd(newLevel), 
          genOr(newLevel), 
          genPlus(newLevel), 
          genStar(newLevel))
    }
  
  def sorbe : Gen[Sorbe[Char]] = genSorbe(0)
    

  println("One bag..." + bag.sample)
  println("Printing a sorbe...")
  println("One sorbe..." + sorbe.sample)
  
  def condition(c:Char) = c >= 'a' 

  val smallInteger = Gen.choose(0,100)

  val propSmallInteger = Prop.forAll(smallInteger)(n => n >= 0 && n <= 100)
    
  property("letter in range") = forAll(letter)(l => condition(l))
  property("intervals I(E)") = 
    forAll(sorbe,bag)((e,bag) => {
        e.interval(bag).m >= 0 
    })
  
  property("intervals I(E+) == I(E) + I(E*) ") = 
    forAll(sorbe,bag)((e,bag) => {
      println("bag =" + bag)
      println("e =" + e)
      println("I(e+)=" + Plus(e).interval(bag))
      println("I(e)=" + e.interval(bag))
      println("I(e*)=" + Star(e).interval(bag))
      Plus(e).interval(bag) == e.interval(bag) + Star(e).interval(bag)
    })

}