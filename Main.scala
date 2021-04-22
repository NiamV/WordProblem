// fsc State.scala ToddCoxeter.scala Vector.scala Vertex.scala Group.scala Graph.scala Main.scala

object Main{
    def main(args: Array[String]): Unit = {
        val tc = new TC()
        val states = tc.run()

        // for(state <- states){
        //     println(state.idents.mkString(","))
        // }

        var tcgraph = new Graph(states(0).makeGroup, states, states(0).active)

        tcgraph.top.visible = true

        val runTC = new Thread{
            override def run{
                var i = 1
                for(state <- states){
                    tcgraph.top.close()
                    tcgraph = new Graph(state.makeGroup, states, state.active)
                    tcgraph.top.visible = true

                    Thread.sleep((2000.0/((i))).toInt)
                    i += 1
                }
            }
        }

        runTC.start()

        println(states.last.idents.mkString(","))
    }
}