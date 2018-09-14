package amadido.spawn

import org.scalatest.FunSuite

class SpawnProgTests extends FunSuite {

  val spawnCode : SpawnCode = SpawnCodeImp
  import spawnCode._

  val a = register("a")
  val b = register("b")
  val x = register("x")
  val w = register("w")
  val k = register("k")
  val c = register("c")
  val h = register("h")

  test("subtract") {
    val prog = seq(List(
      copy(num(42),a),
      sub(reg(a),num(3),a),
      emit(reg(a))
    ))
    val (stats,res) = prog.exec()
    assert(res == 39)
    assert(stats.cycles == 3)
    assert(stats.maxPar == 1)
  }

  def absolute(n:Int) : Prog =  {
    val L1 = label()
    seq(List(
      copy(num(n),a),
      jlz(a,lab(L1)),
      emit(reg(a)),
      die,
      mark(L1),
      sub(num(0),reg(a),a),
      emit(reg(a))
    ))
  }
  
  test("absolutePos") {
    val prog = absolute(42)
    val (stats,res) = prog.exec()
    assert(res == 42)
    assert(stats.cycles == 3)
  }

  test("absoluteNeg") {
    val prog = absolute(-13)
    val (stats,res) = prog.exec()
    assert(res == 13)
    assert(stats.cycles == 4)
  }

  def triangle(n:Int) : Prog =  {
    val L1 = label()
    val L2 = label()
    seq(List(
      copy(num(0),a),
      copy(num(n),x),
      mark(L1),
      jlz(x,lab(L2)),
      add(reg(x),reg(a),a),
      sub(reg(x),num(1),x),
      jump(lab(L1)),
      mark(L2),
      emit(reg(a))
    ))
  }

  test("triangle") {
    val (stats7,res7) = triangle(7).exec()
    val (stats8,res8) = triangle(8).exec()
    val (stats9,res9) = triangle(9).exec()
    assert(res7 == 28)
    assert(res8 == 36)
    assert(res9 == 45)
    assert(stats8.cycles - stats7.cycles == 4)
    assert(stats9.cycles - stats8.cycles == 4)
  }

  test("load/store") {
    val Dub = label()
    val Main = label()
    val Main1 = label()
    val Main2 = label()
    def prog : Prog = seq(List(
      copy(memBase,h),
      jump(lab(Main)),
      mark(Dub),
      add(reg(a),reg(a),a),
      load(k,c),
      jump(reg(c)),
      mark(Main),
      copy(reg(h),k),
      store(lab(Main1),k),
      copy(num(10),a),
      jump(lab(Dub)),
      mark(Main1),
      store(lab(Main2),k),
      jump(lab(Dub)),
      mark(Main2),
      sub(reg(a),num(1),a),
      emit(reg(a))
    ))
    assert(prog.run() == 39)
  }

  test("diamondSpawn") {
    val L1 = label()
    val L2 = label()
    val L3 = label()
    val prog = seq(List(
      copy(num(5),a),
      spawn(L3,w),
      add(reg(a),num(1),a),
      spawn(L1,x),
      add(reg(a),num(1),a),
      spawn(L2,x),
      die,
      mark(L1),
      mul(reg(a),num(10),a),
      send(reg(a),w),
      die,
      mark(L2),
      mul(reg(a),num(100),a),
      send(reg(a),w),
      die,
      mark(L3),
      awaitMaster(x,b),
      add(reg(a),reg(b),a),
      awaitMaster(x,b),
      add(reg(a),reg(b),b),
      emit(reg(b)),
      die
    ))
    assert(prog.run(debug=false) == 765)
  }

  test("replyExample") {
    val L1 = label()
    val prog = seq(List(
      spawn(L1,w),
      send(num(5),w),
      awaitReply(a),
      add(reg(a),num(1),a),
      emit(reg(a)),
      die,
      mark(L1),
      awaitMaster(w,a),
      add(reg(a),reg(a),a),
      send(reg(a),w),
      die
    ))
    assert(prog.run(debug=false) == 11)
  }

  test("sendMustWaitExample") {
    //TODO: make test which doesn't require nop!
    import spawnCode._
    def nop = mark(label()) //TODO: make a real instruction
    val Child = label()
    def prog = seq(List(
      spawn(Child,w),
      send(num(10),w),
      send(num(20),w),
      die,
      mark(Child),
      nop,nop,
      awaitMaster(x,a),
      awaitMaster(x,b),
      add(reg(a),reg(b),a),
      emit(reg(a)),
      die
    ))
    assert(prog.run(debug=false) == 30)
  }

  //TODO: improve semantics of ether! -send#1 wont block, fairness

}
