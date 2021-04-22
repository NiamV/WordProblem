/** Each object represents a Cayley Table for a group, checking the group axioms.
  * @param inputTable The Cayley Table for the group
*/

class CayleyTable(inputTable: Array[Array[Int]]){
    /** Accessor for the table */
    def getTable(): Array[Array[Int]] = inputTable

    private val elements = inputTable(0)
    
    private def isLatin(): Boolean = {
        def containsAllElements(set: Array[Int]): Boolean = {
            var hasAll = true
            for(elem <- elements){
                hasAll = hasAll && set.contains(elem)
            }

            return hasAll
        }


        var rowsContains = true
        for(row <- inputTable){
            rowsContains = rowsContains && containsAllElements(row)
        }

        val transposeTable = inputTable.transpose
        
        var columnsContains = true
        for(column <- transposeTable){
            columnsContains = columnsContains && containsAllElements(column)
        }

        return columnsContains && rowsContains
    }

    private def isAssociative(): Boolean = {
        var associative = true
        
        for(g1 <- elements){
            for(g2 <- elements){
                for(g3 <- elements){
                    val g1g2 = getTable()(elements.indexOf(g1))(elements.indexOf(g2))
                    val g2g3 = getTable()(elements.indexOf(g2))(elements.indexOf(g3))

                    val left = getTable()(elements.indexOf(g1g2))(elements.indexOf(g3))
                    val right = getTable()(elements.indexOf(g1))(elements.indexOf(g2g3))

                    if(left != right){
                        associative = false
                    }
                }
            }
        }

        return associative
    }

    assert(isLatin)
    assert(isAssociative)
    
    override def toString: String = {
        val size = elements.size
        val maxStrLength = elements.map(_.toString.length).max

        val formattedTable = Array.ofDim[String](size+2, size+2)
        formattedTable(0)(0) = " "*maxStrLength
        for(i <- 0 until size){
            val elemName = elements(i).toString
            val buffer = " "*(maxStrLength - elemName.length)
            val bufferedElem = elemName + buffer
            
            formattedTable(0)(i+2) = bufferedElem
            formattedTable(1)(i+2) = "---"

            formattedTable(i+2)(0) = bufferedElem
            formattedTable(i+2)(1) = " | "
        }

        formattedTable(0)(1) = " | "
        formattedTable(1)(1) = "-+-"
        formattedTable(1)(0) = "---"

        for(i <- 0 until size){
            for(j <- 0 until size){
                val elemName = inputTable(i)(j).toString
                val buffer = " "*(maxStrLength - elemName.length)
                formattedTable(i+2)(j+2) = elemName + buffer
            }
        }

        return formattedTable.map(_.mkString(" ")).mkString("\n")
    }
}


/** Object represents a group in terms of its elements and Cayley Table
  * @param elements The set of elements in the group
  * @param cayleyTable The Cayley Table for the group
  * @param generators The subset of elements that generate the group
*/
class Group(elements: Array[Int], cayleyTable: Array[Array[Int]], generators: Array[Int]){
    /** Accessor for the Cayley Table */
    def getTable(): Array[Array[Int]] = cayleyTable

    /** Accessor for the list of elements */
    def getElements(): Array[Int] = elements

    /** Accessor for the list of generators */
    def getGenerators(): Array[Int] = generators

    /** @return the element g*h */
    def multiply(g: Int, h: Int): Int = {
        val gIndex = generators.indexOf(g)
        val hIndex = elements.indexOf(h)

        return cayleyTable(hIndex)(gIndex)
    }
}