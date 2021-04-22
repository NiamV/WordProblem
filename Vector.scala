class Vector(sx: Int, sy: Int, ex: Int, ey: Int){
    def startx = sx
    def starty = sy
    def endx = ex
    def endy = ey
    
    val dx = ex - sx
    val dy = ey - sy
    
    def magnitude(): Double = {
        math.sqrt(dx*dx + dy*dy)
    }
    
    def angle(): Double = {
        var a = math.atan(dy.toDouble / dx.toDouble)

        if(dx < 0){
            if(dy > 0){
                a += math.Pi
            } else {
                a -= math.Pi
            }
        }

        return a
    }

    def moveStart(v: FloatingVector): Vector = {
        return new Vector(sx + v.x, sy + v.y, ex, ey)
    }

    def moveEnd(v: FloatingVector): Vector = {
        return new Vector(sx, sy, ex + v.x, ey + v.y)
    }
}

class FloatingVector(xInput: Int, yInput: Int){
    def x = xInput
    def y = yInput
    
    def magnitude(): Double = {
        math.sqrt(x*x + y*y)
    }

    def angle(): Double = {
        var a = math.atan(y.toDouble / x.toDouble)

        if(x < 0){
            if(y > 0){
                a += math.Pi
            } else {
                a -= math.Pi
            }
        }

        return a
    } 

    def scale(newMag: Int): FloatingVector = {
        val sf = newMag / magnitude()

        val newx = (x.toDouble * sf).toInt
        val newy = (y.toDouble * sf).toInt
        
        return new FloatingVector(newx, newy)
    }

    def rotate(angle: Double): FloatingVector = {
        val newx = x.toDouble * math.cos(angle) - y.toDouble * math.sin(angle)
        val newy = x.toDouble * math.sin(angle) + y.toDouble * math.cos(angle)

        return new FloatingVector(newx.toInt, newy.toInt)
    }
}