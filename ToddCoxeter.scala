class TC{
    var idents: Array[Int] = Array()
    var neighbours = Array[Array[Int]]()
    var to_visit = 0

    val ngens = 2

    val rels = Array(
        Array(1, 0),
        Array(3, 2),
        Array(0, 0, 0),
        Array(2, 2),
        Array(0, 2, 0, 2)
    )

    def find(c: Int): Int = {
        // println("Finding", c, idents.mkString(","))
        var c2 = idents(c)
        if(c == c2){
            return c
        }
        else{
            c2 = find(c2)
            idents(c) = c2
            return c2
        }
    }

    def newNode(): Int = {
        var c = idents.length
        idents = idents :+ c
        neighbours = neighbours :+ Array.fill(2*ngens)(-1)
        return c
    }

    def unify(c1: Int, c2: Int): Unit = {
        // println(idents.mkString(","))
        // println(neighbours.map(_.mkString(",")).mkString(", \n"))
        // println(" ")
        var d1 = find(c1)
        var d2 = find(c2)

        if(d1 == d2){
            return
        }

        var e1 = Array(d1, d2).min
        var e2 = Array(d1, d2).max
        idents(e2) = e1

        for(d <- 0 until 2*ngens){
            var n1 = neighbours(e1)(d)
            var n2 = neighbours(e2)(d)

            if(n1 == -1){
                neighbours(e1)(d) = n2
            }
            else if(n2 != -1){
                unify(n1, n2)
            }
        }
    }

    def follow(c: Int, d: Int): Int = {
        var c1 = find(c)
        var ns = neighbours(c1)

        if(ns(d) == -1){
            neighbours(c1)(d) = newNode()
        }

        return find(ns(d))
    }

    def followp(c: Int, ds: Array[Int]): Int = {
        var c1 = find(c)
        for(d <- ds.reverse){
            c1 = follow(c1, d)
        }
        return c1
    } 

    def copyTable(n: Array[Array[Int]]): Array[Array[Int]] = {
        val r = n.length
        val c = n(0).length

        val newArray = Array.ofDim[Int](r, c)

        for(i <- 0 until r){
            for(j <- 0 until c){
                newArray(i)(j) = n(i)(j)
            }
        }

        return newArray
    }  
        
    // -----------------------------------------


    def run(): Array[State] = {
        var states = Array[State]()

        var start = newNode()

        while(to_visit < idents.length){
            var c = find(to_visit)

            if(c == to_visit){
                for(rel <- rels){
                    // println(c, rel.mkString(","))
                    unify(followp(c, rel), c)
                    //print(to_visit)
                    states = states :+ new State(idents, copyTable(neighbours), to_visit)
                }
            }

            states = states :+ new State(idents, copyTable(neighbours), to_visit)
            //println(to_visit)
            to_visit += 1

			// println(idents.mkString(", "))
        	// println(neighbours.map(_.mkString(",")).mkString(", \n"))
			// println()
        }

        // println(idents.mkString(", "))
        // println(neighbours.map(_.mkString(",")).mkString(", \n"))
		// println()

        return states
    }

    // def main(args: Array[String]): Unit = {
    //     print("Todd Coxeter Algorithm")

	// 	run()
    // }
}