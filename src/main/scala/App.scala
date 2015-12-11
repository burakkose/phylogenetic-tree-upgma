import alignment.NeedlemanWunsch

object App extends App {


  val nw = new NeedlemanWunsch("AAA", "A", (1, -1, -1))
  print(nw.nw(false))
}
