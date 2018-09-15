package amadido.spawn

class FibVersions(val spawnCode : SpawnCode) {

  import spawnCode._

  // general use registers
  val a = register("a") //numeric accumulator
  val b = register("b") //second numeric
  val t = register("t") //temp for conditional jumps

  def fibSequential(n:Int) : Prog =  {

    //Sequential version of Fib which allocates continuations
    //on a heap (but never GCs!)

    val Main = label()
    val Main1 = label()
    val Fib = label()
    val Fib0 = label()
    val Fib1 = label()
    val Fib2 = label()

    val k = register("k") //continuation, saved in memory
    val c = register("c") //label extracted from k(0)
    val h = register("h") //heap pointer, for next allocation
    val m = register("m") //just allocated memory

    val x = register("x")
    val y = register("y")

    seq(List(

      copy(memBase,h),
      jump(lab(Main)),

      mark(Main),
      copy(num(n),a),
      copy(reg(h),m), add(reg(h),num(1),h), //alloc(1)
      store(lab(Main1),m),
      copy(reg(m),k),
      jump(lab(Fib)),
      mark(Main1),
      emit(reg(a)),  //a holds final result
      die,

      mark(Fib),
      sub(reg(a),num(1),t),
      jgz(t,lab(Fib0)),
      load(k,c), jump(reg(c)), //base-case return(a), fib(0)=0, fib(1)=1

      mark(Fib0), //recursive case
      copy(reg(h),m), add(reg(h),num(3),h), //alloc(3)
      store(lab(Fib1),m),
      add(reg(m),num(1),t), store(reg(k),t),
      add(reg(m),num(2),t), store(reg(a),t),
      sub(reg(a),num(1),a),
      copy(reg(m),k),
      jump(lab(Fib)), // call fib(n-1)

      mark(Fib1), //recursive case, 1st continuation
      copy(reg(a),x),
      copy(reg(k),t),
      add(reg(t),num(1),t), load(t,k),
      add(reg(t),num(1),t), load(t,a),
      copy(reg(h),m), add(reg(h),num(3),h), //alloc(3)
      store(lab(Fib2),m),
      add(reg(m),num(1),t), store(reg(k),t),
      add(reg(m),num(2),t), store(reg(x),t),
      sub(reg(a),num(2),a),
      copy(reg(m),k),
      jump(lab(Fib)), // call fib(n-2)

      mark(Fib2), //recursive case, 2nd continuation
      copy(reg(a),y),
      copy(reg(k),t),
      add(reg(t),num(1),t), load(t,k),
      add(reg(t),num(1),t), load(t,x),
      add(reg(x),reg(y),a),
      load(k,c), jump(reg(c)) //return
    ))
  }

  
  def fibParBadLinearLeafSequence(n:Int) : Prog =  {
    // First attempt to parallize fib: Turned out to have a terrible
    // schoolboy error: All the additions were linearly sequenced because
    // of a message chain sequening through all the leaves.

    val Collect = label()
    val Fib = label()
    val FibRec = label()

    val w = register("w") //destination "wire" to send messages to spawned core
    val x = register("x") //dont care, read as underscore
    
    seq(List(

      spawn(Collect,w),
      copy(num(n),a),
      spawn(Fib,w),
      send(num(0),w),
      die,

      mark(Fib), //arg in a; add incoming from master; send result to w
      sub(reg(a),num(1),t),
      jgz(t,lab(FibRec)),
      awaitMaster(x,b),
      add(reg(b),reg(a),b),
      send(reg(b),w),
      die,

      mark(FibRec),
      sub(reg(a),num(1),a),
      spawn(Fib,w), //left
      sub(reg(a),num(1),a),
      jump(lab(Fib)), //right

      mark(Collect),
      awaitMaster(x,a),
      emit(reg(a)),
      die
    ))
  }


  def fibParChildMasters(n:Int) : Prog =  {
    // Version which fixes the mistake in fibParBadLinearLeafSequence
    // Each recursive call spawns two nodes:
    // - A parent(slave) to do the addition
    // - A sibling(master) to compute the LHS
    // We continue to compute the RHS, and are also master of the parent

    val Collect = label()
    val Fib = label()
    val FibRec = label()
    val Sum = label()

    val w = register("w") //destination "wire" to send messages to spawned core
    val x = register("x") //dont care, read as underscore

    seq(List(

      spawn(Collect,w),
      copy(num(n),a),
      //fallthough

      mark(Fib), //arg in a; send result to w
      sub(reg(a),num(1),t),
      jgz(t,lab(FibRec)),
      send(reg(a),w), //base cases: fib(0)=0, fib(1)=1
      die,

      mark(FibRec),
      spawn(Sum,w),
      sub(reg(a),num(1),a),
      spawn(Fib,x), //left
      sub(reg(a),num(1),a),
      jump(lab(Fib)), //right

      mark(Sum),
      awaitMaster(x,a),
      awaitMaster(x,b),
      add(reg(a),reg(b),a),
      send(reg(a),w),
      die,
      
      mark(Collect),
      awaitMaster(x,a),
      emit(reg(a)), //final result
      die
    ))
  }


  def fibParChildSlaves(n:Int) : Prog =  {
    // More `obvious' version which has two child-slaves at each node

    val Fib = label()
    val FibRec = label()

    val w = register("w") //destination "wire" to send messages to spawned core
    val r = register("r") //destination to "reply" to message from master

    seq(List(

      spawn(Fib,w),
      copy(num(n),a),
      send(reg(a),w),
      awaitReply(a),
      emit(reg(a)), //final result
      die,

      mark(Fib),
      awaitMaster(r,a),
      sub(reg(a),num(1),t),
      jgz(t,lab(FibRec)),
      send(reg(a),r), //base cases: fib(0)=0, fib(1)=1
      die,

      mark(FibRec),
      spawn(Fib,w), //left
      sub(reg(a),num(1),a),
      send(reg(a),w),
      spawn(Fib,w), //right
      sub(reg(a),num(1),a),
      send(reg(a),w),
      awaitReply(a),
      awaitReply(b),
      add(reg(a),reg(b),a),
      send(reg(a),r),
      die
    ))
  }



  def fibAsServiceBad(n:Int) : Prog =  {
    // Modification of: fibParChildSlaves to work as a service
    // Not good when used recursively, as all calls effectively get sequenced

    val FibServiceInit = label()
    val FibService = label()
    val Fib = label()
    val FibRec = label()
    val Die = label()

    val f = register("f") //destination of fib-service
    val r = register("r") //destination to "reply" to message from master
    val x = register("x") //dont care, read as underscore

    seq(List(

      spawn(FibServiceInit,f),
      send(reg(f),f), //so it knows itself
      copy(num(n),a),
      send(reg(a),f),
      awaitReply(a),
      emit(reg(a)), //final result
      send(num(-1),f), //shutdown the service
      die,

      mark(FibServiceInit),
      awaitMaster(x,f), //know myself as f
      mark(FibService),
      awaitMaster(r,a),
      jlz(a,lab(Die)),
      spawn(Fib,x),
      jump(lab(FibService)),

      mark(Fib),
      sub(reg(a),num(1),t),
      jgz(t,lab(FibRec)),
      send(reg(a),r), //base cases: fib(0)=0, fib(1)=1
      die,

      mark(FibRec),
      sub(reg(a),num(1),a),
      send(reg(a),f), //left
      sub(reg(a),num(1),a),
      send(reg(a),f), //right
      awaitReply(a),
      awaitReply(b),
      add(reg(a),reg(b),a),
      send(reg(a),r),

      mark(Die)
    ))
  }

}
