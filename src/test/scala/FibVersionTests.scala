package amadido.spawn

import org.scalatest.FunSuite

class FibVersionTests extends FunSuite {

  val spawnCode : SpawnCode = SpawnCodeImp

  def testFib(name:String,fib:Int => Prog, showStats:Boolean = false) {
    test(name) {
      val (stats10,res10) = fib(10).exec()
      val (stats11,res11) = fib(11).exec()
      val (stats12,res12) = fib(12).exec()
      assert(res10 == 55)
      assert(res11 == 89)
      assert(res12 == 144)
      if (showStats) {
        println(name+":")
        println("10: "+stats10)
        println("11: "+stats11)
        println("12: "+stats12)
      }
    }
  }

  val progs = new FibVersions(spawnCode)
  import progs._

  testFib("fibSequential",fibSequential)
  testFib("fibParBadLinearLeafSequence",fibParBadLinearLeafSequence)
  testFib("fibParChildMasters",fibParChildMasters)
  testFib("fibParChildSlaves",fibParChildSlaves)
  testFib("fibAsServiceBad",fibAsServiceBad)

}
