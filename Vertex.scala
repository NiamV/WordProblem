class Vertex(e: Int, xCoord: Int, yCoord: Int, selectedInput: Boolean, active: Boolean){
    def element: Int = e
    def name: String = e.toString

    private var currentx = xCoord
    def x: Int = currentx
    def updatex(newx: Int): Unit = {
        currentx = newx
    }
    
    private var currenty = yCoord
    def y: Int = currenty
    def updatey(newy: Int): Unit = {
        currenty = newy
    }

    private var isSelected = selectedInput
    def selected(): Boolean = isSelected
    def updateSelected(): Unit = {
        isSelected = !isSelected
    }

    private var currentActive = active
    def active(): Boolean = currentActive
    def updateActive(newActive: Boolean) = {
        currentActive = newActive
    }
}