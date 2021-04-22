import scala.swing._
import scala.swing.event._
import java.awt.Color
import java.awt.event._

class Graph(group: Group, states: Array[State], activeElem: Int) extends SimpleSwingApplication{
    def top = new MainFrame{
        var elements = group.getElements()
        var order = elements.size

        var generators = group.getGenerators()
        val colours = Array(
            new Color(252, 53, 3),
            new Color(3, 69, 252),
            new Color(21, 209, 102),
            new Color(156, 21, 209)
        )
        
        private def getCoords(total: Int, current: Int): (Int, Int) = {
            val angle = 2 * math.Pi * current / total

            val x = (300 * math.sin(angle)).toInt
            val y = (300 * math.cos(angle)).toInt

            return (450 + x, 450 - y)
        }

        var verticies = new Array[Vertex](order)
        for(i <- 0 until order){
            val coords = getCoords(order, i)
            verticies(i) = new Vertex(elements(i), coords._1, coords._2, false, false)
        }

        private def findVertex(e: Int): Int = {
            var index = 0
            while(verticies(index).element != e){
                index += 1
            }

            return index
        }

        private def overVertex(x: Int, y: Int): Int = {
            var result = -1

            var i = 0
            for(v <- verticies){
                if(math.abs(v.x - x) < 20 && math.abs(v.y - y) < 20){
                    result = i
                }

                i += 1
            }

            return result
        }

        def updateGroup(g: Group): Unit = {
            var elements = group.getElements()
            var order = elements.size

            var generators = group.getGenerators()

            var verticies = new Array[Vertex](order)
            for(i <- 0 until order){
                val coords = getCoords(order, i)
                verticies(i) = new Vertex(elements(i), coords._1, coords._2, false, false)
            }

            graphGraphic.repaint()
        }

        val graphGraphic = new Component{
            override def paintComponent(g: Graphics2D){
                super.paintComponent(g)

                g.setColor(new Color(255, 255, 255))
                g.fillRect(0, 0, 1000, 900)
                g.setColor(new Color(0, 0, 0))

                def drawArrow(p: Int, q: Int) = {
                    val startVertex = verticies(findVertex(p))
                    val endVertex = verticies(findVertex(q))

                    val sx = startVertex.x
                    val sy = startVertex.y
                    val ex = endVertex.x
                    val ey = endVertex.y

                    val vector = new Vector(sx, sy, ex, ey)

                    val floatingVector = new FloatingVector(ex - sx, ey - sy)
                    val shiftStart = floatingVector.scale(40)
                    val shiftEnd = floatingVector.scale(-40)

                    val arrowVector = vector.moveStart(shiftStart).moveEnd(shiftEnd)

                    g.drawLine(arrowVector.startx, arrowVector.starty, arrowVector.endx, arrowVector.endy)

                    val headVectorLeft = floatingVector.scale(20).rotate(5 * math.Pi / 6)
                    val headVectorRight = floatingVector.scale(20).rotate(-5 * math.Pi / 6)

                    val leftEndPoint = arrowVector.moveEnd(headVectorLeft)
                    val rightEndPoint = arrowVector.moveEnd(headVectorRight)

                    val xPoints = Array(arrowVector.endx, leftEndPoint.endx, rightEndPoint.endx)
                    val yPoints = Array(arrowVector.endy, leftEndPoint.endy, rightEndPoint.endy)

                    g.fillPolygon(xPoints, yPoints, 3)
                }

                for(v <- verticies){
                    if(v.element == activeElem){
                        g.setColor(new Color(0, 255, 0))
                    }

                    val fontMetrics = g.getFontMetrics()
                    val strCoordx = v.x - (fontMetrics.stringWidth(v.name) / 2)
                    val strCoordy = v.y + (fontMetrics.getHeight() / 4)
            
                    g.drawString(v.name, strCoordx, strCoordy)
                    g.drawOval(v.x-25, v.y-25, 50, 50)

                    g.setColor(new Color(0, 0, 0))
                }

                var i = 0
                for(gen <- generators){
                    g.setColor(colours(i))
                    for(h <- elements){
                        if(group.multiply(gen, h) != -1){
                            drawArrow(h, group.multiply(gen, h))
                        }
                    }
                    i += 1
                }
            }

            listenTo(mouse.clicks)
            listenTo(mouse.moves)
            
            reactions += {
                case e: MouseClicked => {
                    val selectedVertex = overVertex(e.point.x, e.point.y)
                    if(selectedVertex != -1){
                        verticies(selectedVertex).updateSelected()
                    }

                    repaint()
                }
                case e: MouseDragged => {
                    val selectedVertex = overVertex(e.point.x, e.point.y)

                    if(selectedVertex != -1){
                        verticies(selectedVertex).updatex(e.point.x)
                        verticies(selectedVertex).updatey(e.point.y)
                    }

                    repaint()
                }
            }

            border = new javax.swing.border.LineBorder(new Color(0,0,0))
            minimumSize = new Dimension(1000, 900)
            preferredSize = minimumSize
            maximumSize = minimumSize
        }
         
        var generatorsLabel= new BoxPanel(Orientation.Vertical){
            contents += new Label("Generators:"){
                font = new Font("Dialog", java.awt.Font.BOLD, 18)
            }
            
            var i = 0
            for(g <- generators){
                contents += new Label("    " + (((g+1)/2)+96).toChar){
                    foreground = colours(i)
                    font = new Font("Dialog", java.awt.Font.BOLD, 18)
                }
                i += 1
            }

            val compsize = new Dimension(200, 900)
            preferredSize = compsize
            minimumSize = compsize
            maximumSize = compsize
        }
        
        contents = new BorderPanel{
            add(graphGraphic, BorderPanel.Position.Center)
            add(generatorsLabel, BorderPanel.Position.East)
        }

        size = new Dimension(1200, 1000)
    }
}

case class UpdateDFSGraph() extends Event