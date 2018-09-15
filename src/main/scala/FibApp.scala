package amadido.spawn

object FibApp extends App {

  val n = 1 //args(1).toInt
  
  val spawnCode : SpawnCode = SpawnCodeImp
  val examples = new FibVersions(spawnCode)

  val fib = examples.fibParChildSlaves _
  val (stats,res) = fib(n).exec(debug=true)

  println(s"res: $res")
  println(s"stats: $stats")

}
