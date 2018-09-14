package amadido.spawn

trait SpawnCode {

  type Reg
  def register : String => Reg

  type Lab
  def label() : Lab

  type Atom
  def num : Int => Atom
  def reg : Reg => Atom
  def lab : Lab => Atom
  def memBase : Atom

  case class Stats(cycles:Int,maxPar:Int,stalls:Int) {
    override def toString = s"(cycles=$cycles,maxPar=$maxPar,stalls=$stalls)"
  }

  trait Prog {
    def exec(debug:Boolean = false) : (Stats,Int)
    def run(debug:Boolean = false) : Int = exec(debug)._2
  }

  def empty : Prog
  def seq : List[Prog] => Prog
  def mark : Lab => Prog

  def copy : (Atom,Reg) => Prog
  def add : (Atom,Atom,Reg) => Prog
  def sub : (Atom,Atom,Reg) => Prog
  def mul : (Atom,Atom,Reg) => Prog

  def jump : Atom => Prog
  def jlz : (Reg,Atom) => Prog
  def jgz : (Reg,Atom) => Prog

  def load(mem:Reg, into:Reg) : Prog
  def store(value:Atom, mem:Reg) : Prog

  def spawn(child:Lab, box:Reg) : Prog
  def die : Prog

  def send(value:Atom, dest:Reg) : Prog
  def awaitMaster(who:Reg, what:Reg) : Prog
  def awaitReply(what:Reg) : Prog

  def emit(result:Atom) : Prog
  
}
