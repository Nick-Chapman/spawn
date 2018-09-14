package amadido.spawn

object SpawnCodeImp extends SpawnCode {

  type Reg = String

  def register : String => Reg = s => s

  trait Lab
  object Lab {
    private case class Rep(lab:Int) extends Lab {
      override def toString = "$" + s"$lab"
    }
    private var next : Int = 0
    def gen() : Lab = {
      val lab = Rep(next)
      next = next + 1
      lab
    }
  }

  def label() : Lab = Lab.gen()

  trait Addr {
    def offset : Int => Addr
  }
  object Addr {
    def base : Addr = Rep(1000)
    private case class Rep(x:Int) extends Addr {
      override def toString = s"#$x"
      def offset = n => Rep(x+n)
    }
  }

  case object WeBeHung extends Exception()
  case object WeBeHung2 extends Exception()
  case class UnknownReg(s:String) extends Exception(s)
  case class CantFindLabel(lab:Lab) extends Exception(lab.toString)
  case class RuntimeTypeError(s:String) extends Exception(s)

  trait Box
  object Box {
    private case class Rep(lab:Int) extends Box {
      override def toString = "$" + s"$lab"
    }
    private var next : Int = 0
    def gen() : Box = {
      val lab = Rep(next)
      next = next + 1
      lab
    }
  }

  sealed trait Value {

    def getNum : Int = this match {
      case Value.Num(x) => x
      case x => throw RuntimeTypeError(s"getNum: $x")
    }

    def getLabel : Lab = this match {
      case Value.Label(x) => x
      case x => throw RuntimeTypeError(s"getLabel: $x")
    }

    def getAddress : Addr = this match {
      case Value.Address(x) => x
      case x => throw RuntimeTypeError(s"getAddress: $x")
    }

    def getBox : Box = this match {
      case Value.VBox(x) => x
      case x => throw RuntimeTypeError(s"getBox: $x")
    }

  }

  object Value {

    case class Num(x:Int) extends Value
    case class Label(x:Lab) extends Value
    case class Address(x:Addr) extends Value
    case class VBox(x:Box) extends Value
    
    def add(v1:Value,v2:Value) = (v1,v2) match {
      case (Num(n1),Num(n2)) => Num(n1 + n2)
      case (Address(a1),Num(n2)) => Address(a1.offset(n2))
      case _ => throw RuntimeTypeError(s"add($v1,$v2)")
    }

    def sub(v1:Value,v2:Value) = (v1,v2) match {
      case (Num(n1),Num(n2)) => Num(n1 - n2)
      case _ => throw RuntimeTypeError(s"sub($v1,$v2)")
    }

    def mul(v1:Value,v2:Value) = (v1,v2) match {
      case (Num(n1),Num(n2)) => Num(n1 * n2)
      case _ => throw RuntimeTypeError(s"mul($v1,$v2)")
    }

    def ltz : Value => Boolean = {
      case Num(x) => (x < 0)
      case v => throw RuntimeTypeError(s"ltz($v)")
    }

    def gtz : Value => Boolean = {
      case Num(x) => (x > 0)
      case v => throw RuntimeTypeError("gtz($v)")
    }

  }


  type Mem = Map[Addr,Value]
  object Mem {
    def empty : Mem = Map()
  }

  type RegState = Map[Reg,Value]
  object RegState {
    def empty : RegState = Map()
  }

  
  sealed trait Atom
  object Atom {
    case class Lit(x:Value) extends Atom
    case class InReg(x:Reg) extends Atom

    def eval(x:Atom, rs:RegState) : Value = x match {
      case Lit(v) => v
      case InReg(r) => rs.get(r) match {
        case Some(v) => v
        case None => throw UnknownReg(r)
      }
    }

  }

  def num : Int => Atom = x => Atom.Lit(Value.Num(x))
  def reg : Reg => Atom = x => Atom.InReg(x)
  def lab : Lab => Atom = x => Atom.Lit(Value.Label(x))
  def memBase : Atom = Atom.Lit(Value.Address(Addr.base))

  object Cond {
    case object Ltz extends Cond
    case object Gtz extends Cond
  }
  sealed trait Cond {
    def eval(v:Value) : Boolean = this match {
      case Cond.Ltz => Value.ltz(v)
      case Cond.Gtz => Value.gtz(v)
    }
  }

  sealed trait Instr
  object I {
    case class Mark(x:Lab) extends Instr
    case class Copy(x:Atom, t:Reg) extends Instr
    case class Add(x:Atom, y:Atom, t:Reg) extends Instr
    case class Sub(x:Atom, y:Atom, t:Reg) extends Instr
    case class Mul(x:Atom, y:Atom, t:Reg) extends Instr
    case class Jump(dest:Atom) extends Instr
    case class CondJump(scrutinee:Atom, c:Cond, dest:Atom) extends Instr
    case class Load(mem:Atom, t:Reg) extends Instr
    case class Store(value:Atom, mem:Atom) extends Instr
    case class Spawn(child:Atom, w:Reg) extends Instr
    case class Die() extends Instr
    case class Send(value:Atom, dest:Atom) extends Instr
    case class AwaitMaster(who:Reg, what:Reg) extends Instr
    case class AwaitReply(what:Reg) extends Instr
    case class Emit(result:Atom) extends Instr
  }


  trait Pid
  object Pid {
    private case class Rep(pid:Int) extends Pid {
      override def toString = s"[$pid]"
    }
    private var next : Int = 0
    def gen() : Pid = {
      val pid = Rep(next)
      next = next + 1
      pid
    }
  }

  case class Message(contents:Value,reply:Box)

  sealed trait Eth {
    def send(from:Pid,to:Box,m:Message) : Option[Eth] //None, caller must stall
    def receive(inbox:Box) : Option[(Message,Eth)] //None, caller must stall
  }
  object Eth {
    private case class Rep(m:Map[Box,Message]) extends Eth {

      //TODO: allow continue after send, even when dest is full
      //except: disallow multiple sends in flight from same pid
      def send(from:Pid,dest:Box,mes:Message) : Option[Eth] = {
        m.get(dest) match {
          case Some(_) => None //full, stall
          case None => Some(Rep(m + (dest -> mes)))
        }
      }
      def receive(key:Box) : Option[(Message,Eth)] = {
        m.get(key) match {
          case None => None //empty, stall
          case Some(mes) => Some((mes, Rep(m - key)))
        }
      }
    }
    def empty : Eth = Rep(Map())
  }

  sealed trait Core {
    def pid : Pid
    def primaryBox : Box
    def secondaryBox : Box
    def setProgramCounter : Prog => Core
    def spawnProgramCounter : Prog => (Core,Value)
    def fetch : Option[(Instr,Core)]
    def eval : Atom => Value
    def setReg(debug:Boolean) : (Reg,Value) => Core
  }

  object Core {

    def initializeWithProgram(p:Prog) : Core = {
      Rep(
        Pid.gen(), //TODO: have caller allocate pid, starting from 1 each run
        Prog.instructions(p),
        RegState.empty,
        Box.gen(),
        Box.gen()
      )
    }

    private case class Rep(
      pid : Pid,
      pc : List[Instr],
      rs : RegState,
      pbox : Box,
      sbox : Box
    ) extends Core {

      def primaryBox = pbox
      def secondaryBox = sbox

      def setProgramCounter = p => {
        Rep(pid, Prog.instructions(p),rs,pbox,sbox)
      }

      def spawnProgramCounter = p => {
        val pid = Pid.gen()
        val pbox = Box.gen()
        val sbox = Box.gen()
        (Rep(pid, Prog.instructions(p),rs,pbox,sbox), Value.VBox(pbox))
      }

      def fetch =
        pc match {
          case Nil => None
          case instruction::xs => Some((instruction,Rep(pid,xs,rs,pbox,sbox)))
        }

      def eval =
        atom => Atom.eval(atom,rs)

      def setReg(debug:Boolean) =
        (dest,value) => {
          if (debug) println(s"$pid $dest = $value")
          Rep(pid, pc, rs + (dest->value), pbox, sbox)
        }

    }

  }


  def simulate(theProg:Prog,debug:Boolean) : (Stats,Option[Value]) = {

    def locateLabel(lab:Lab) : Prog = {
      def loop : List[Instr] => Prog = {
        case Nil => throw CantFindLabel(lab)
        case I.Mark(m)::xs => if (m==lab) Prog(xs) else loop(xs)
        case _::xs => loop(xs)
      }
      loop(Prog.instructions(theProg))
    }

    val emitBox = Box.gen()

    //TODO: move execute to top level
    def execute(eth:Eth, mem:Mem, core:Core,i:Instr) : Option[(Eth,Mem,List[Core])] = {

      val pid = core.pid
      if (debug) println(s"$pid execute: $i")

      def straightLine(core:Core) : List[Core] = List(core)

      i match {

        case I.Mark(_) =>
          Some(eth,mem, straightLine(core)) //better to not take a sim cycle here

        case I.Copy(atom,dest) =>
          val v = core.eval(atom)
          Some(eth,mem, straightLine(core.setReg(debug)(dest,v)))

        case I.Add(atom1,atom2,dest) =>
          val v1 = core.eval(atom1)
          val v2 = core.eval(atom2)
          val v = Value.add(v1,v2)
          Some(eth,mem, straightLine(core.setReg(debug)(dest,v)))

        case I.Sub(atom1,atom2,dest) =>
          val v1 = core.eval(atom1)
          val v2 = core.eval(atom2)
          val v = Value.sub(v1,v2)
          Some(eth,mem, straightLine(core.setReg(debug)(dest,v)))

        case I.Mul(atom1,atom2,dest) =>
          val v1 = core.eval(atom1)
          val v2 = core.eval(atom2)
          val v = Value.mul(v1,v2)
          Some(eth,mem, straightLine(core.setReg(debug)(dest,v)))

        case I.Jump(dest) =>
          val lab = core.eval(dest).getLabel
          Some(eth,mem, straightLine(core.setProgramCounter(locateLabel(lab))))
          
        case I.CondJump(scrutinee,cond,dest) =>
          val v = core.eval(scrutinee)
          val lab = core.eval(dest).getLabel
          if (cond.eval(v)) {
            Some(eth,mem, straightLine(core.setProgramCounter(locateLabel(lab))))
          } else {
            Some(eth,mem, straightLine(core))
          }

        case I.Load(addr,reg) => 
          val k = core.eval(addr).getAddress
          val v = mem(k)
          Some(eth,mem, straightLine(core.setReg(debug)(reg,v)))

        case I.Store(value,addr) =>
          val v = core.eval(value)
          val k = core.eval(addr).getAddress
          val mem1 : Mem = mem + (k -> v)
          if (debug) println(s"$k = $v")
          Some(eth,mem1, straightLine(core))

        case I.Spawn(child,w) =>
          //TODO: dont spawn if resources unavailable. NoBox for parent
          val childLabel = core.eval(child).getLabel
          val (core1,box) = core.spawnProgramCounter(locateLabel(childLabel))
          val core2 = core.setReg(debug)(w,box)
          Some(eth,mem,List(core1,core2))

        case I.Die() =>
          //TODO: should stall if have undelivered post
          // only becomes possible if allow send to continue before delivery
          Some(eth,mem,List())

        case I.Send(aValue,aDest) =>
          val pid : Pid = core.pid
          val dest = core.eval(aDest).getBox
          val value = core.eval(aValue)
          val replyBox = core.secondaryBox
          val message = Message(value,replyBox)
          eth.send(pid,dest,message) match {
            case None =>
              if (debug) println(s"$pid - STALL")
              None
            case Some(eth1) =>
              Some(eth1,mem,straightLine(core))
          }

        case I.AwaitMaster(who,what) =>
          val myPrimBox : Box = core.primaryBox
          eth.receive(myPrimBox) match {
            case None =>
              if (debug) println(s"$pid - STALL")
              None
            case Some((mes,eth1)) =>
              val core1 = core.setReg(debug)(what,mes.contents)
                .setReg(debug)(who,Value.VBox(mes.reply))
              Some(eth1,mem,straightLine(core1))
          }

        case I.AwaitReply(what) =>
          val myBox : Box = core.secondaryBox
          eth.receive(myBox) match {
            case None =>
              if (debug) println(s"$pid - STALL")
              None
            case Some((mes,eth1)) =>
              val core1 = core.setReg(debug)(what,mes.contents)
              Some(eth1,mem,straightLine(core1))
          }

        case I.Emit(aResult) =>
          val pid : Pid = core.pid
          val dest = emitBox
          val value = core.eval(aResult)
          val replyBox = core.secondaryBox
          val message = Message(value,replyBox)
          eth.send(pid,dest,message) match {
            case None => 
              println(s"$pid - STALL")
              None
            case Some(eth1) =>
              Some(eth1,mem,straightLine(core))
          }

      }
    }

    def stepOneCore(eth:Eth,mem:Mem,core:Core) : (Int,Eth,Mem,List[Core]) = {
      core.fetch match {
        case None => (0,eth,mem,Nil)
        case Some((instruction,core1)) =>
          execute(eth,mem,core1,instruction) match {
            case None => (1,eth,mem,List(core)) //STALL
            case Some((eth,mem,cores)) => (0,eth,mem,cores)
          }
      }
    }

    def stepAllCores(eth:Eth, mem:Mem, cores:List[Core])
        : (Int,Eth,Mem,List[Core]) =
      cores match {
        case Nil => (0,eth,mem,Nil)
        case head::tail =>
          val (xstall1,eth1,mem1,res1) = stepOneCore(eth,mem,head)
          val (xstall2,eth2,mem2,res2) = stepAllCores(eth1,mem1,tail)
          val xstall = xstall1 + xstall2
          (xstall, eth2, mem2, res1 ++ res2)
      }

    def loop(cycle:Int, maxPar:Int, stalls:Int,
      eth:Eth, mem:Mem, cores:List[Core]
    ) : (Stats,Eth) = {
      if(debug) println(s"-----")
      val (xstall,eth1,mem1,cores1) = stepAllCores(eth, mem, cores)
      if (xstall == cores.size) {
        throw WeBeHung
      }
      if (cores == cores1) {
        throw WeBeHung2 //not needed now we look at stalls?
      }
      val stalls1 = stalls + xstall
      cores1 match {
        case Nil =>
          val stats = Stats(cycles=cycle, maxPar=maxPar,stalls=stalls)
          (stats,eth1)
        case _ =>
          val n = cores.size
          val maxPar1 = if (n > maxPar) n else maxPar //TODO: use standard max
          loop(cycle+1,maxPar1,stalls1,eth1,mem1,cores1)
      }
    }

    val initEth = Eth.empty
    val initMem = Mem.empty
    val initCore = Core.initializeWithProgram(theProg)
    val (stats,finalEth) = loop(cycle=0, maxPar=1, stalls=0,
      initEth,initMem,List(initCore))

    val vOpt = finalEth.receive(emitBox) match {
      case Some((mes,_)) => Some(mes.contents)
      case None => None
    }

    (stats,vOpt)
  }


  object Prog {

    private case class Rep(xs:List[Instr]) extends Prog {
      def exec(debug:Boolean) : (Stats,Int) = {
        val (stats,v) = simulate(this,debug)
        (stats,v.get.getNum)
      }
    }

    def empty : Prog = Rep(Nil)
    def apply(xs:List[Instr]) : Prog = Rep(xs)
    def apply(x:Instr) : Prog = Rep(List(x))
    def seq(progs:List[Prog]) : Prog = Rep (progs.flatMap { case Rep(xs) => xs })
    def instructions : Prog => List[Instr] = { case Rep(xs) => xs }
  }


  def empty = Prog.empty
  def seq = Prog.seq
  def mark = x => Prog(I.Mark(x))

  def copy = (x,t) => Prog(I.Copy(x,t))
  def add = (x,y,t) => Prog(I.Add(x,y,t))
  def sub = (x,y,t) => Prog(I.Sub(x,y,t))
  def mul = (x,y,t) => Prog(I.Mul(x,y,t))

  def jump : Atom => Prog = x => Prog(I.Jump(x))
  def jlz : (Reg,Atom) => Prog = (x,t) => Prog(I.CondJump(reg(x),Cond.Ltz,t))
  def jgz : (Reg,Atom) => Prog = (x,t) => Prog(I.CondJump(reg(x),Cond.Gtz,t))

  def load(mem:Reg, t:Reg) = Prog(I.Load(reg(mem),t))
  def store(value:Atom, mem:Reg) = Prog(I.Store(value,reg(mem)))

  def spawn(child:Lab, r:Reg) = Prog(I.Spawn(lab(child),r))
  def die = Prog(I.Die())

  def send(value:Atom, dest:Reg) = Prog(I.Send(value,reg(dest)))
  def awaitMaster(who:Reg, what:Reg) = Prog(I.AwaitMaster(who,what))
  def awaitReply(what:Reg) = Prog(I.AwaitReply(what))
  
  def emit(result:Atom) = Prog(I.Emit(result))

}
