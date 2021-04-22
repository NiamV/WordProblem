class State(identsInput: Array[Int], neighboursInput: Array[Array[Int]], activeInput: Int){
    val idents = identsInput
    val neighbours = neighboursInput

    val elems = idents.toSet.toArray.sorted
    val active = elems.indexOf(idents(activeInput))

    def makeGroup(): Group = {
        val elems = idents.toSet.toArray

        val cayleyTable = Array.ofDim[Int](elems.length, neighbours.length/2)

        for(i <- 0 until elems.length){
            for(j <- 0 until neighbours.length/2){
                cayleyTable(i)(j) = -1
            }
        }

        var i = 0
        var j = 0


        for(n <- neighbours){
            for(d <- n){
                if(d != -1 && n.indexOf(d) % 2 == 0){
                    // println(j)
                    // println(elems.indexOf(idents(j)))
                    // println(n.indexOf(d)/2)

                    // println(n.mkString(","), d)
                    // println(idents.mkString(","))
                    // println(elems.indexOf(idents(d)))
                    // println(" ")
                    cayleyTable(elems.indexOf(idents(j)))(n.indexOf(d)/2) = elems.indexOf(idents(d))
                }
            }

            j += 1
        }

        val generators = neighbours(0).filter(neighbours(0).indexOf(_) % 2 == 0)

        return new Group((0 until elems.length).toArray, cayleyTable, generators)
    }
}