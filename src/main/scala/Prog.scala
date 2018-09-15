package amadido.spawn

trait Prog {
  def exec(debug:Boolean = false) : (Stats,Int)
  def run(debug:Boolean = false) : Int = exec(debug)._2 //ignore Stats
}
