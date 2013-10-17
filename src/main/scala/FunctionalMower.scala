/**
 * @author David Galichet.
 */
object FunctionalMower {

    def run(garden: Garden, instructions: List[Instruction]): State[List[Position], Position] = instructions match {
        case Nil => State { previous => (previous, previous.head) }
        case A::tail => State[List[Position], Position] { previous =>
            val next = previous.head.move(garden)
            (next::previous, next)
        }.flatMap( _ => run(garden, tail) )
        case head::tail => State[List[Position], Position] { previous =>
            val next = previous.head.copy(orientation = previous.head.orientation.turn(head))
            (next::previous, next)
        }.flatMap( _ => run(garden, tail) )
    }
}

case class Point(x: Int, y: Int)

case class Garden(bottomLeft: Point, topRight: Point) {
    def isInGarden(current: Point): Boolean =
        bottomLeft.x <= current.x && current.x <= topRight.x && bottomLeft.y <= current.y && current.y <= topRight.y
}

case class Position(point: Point, orientation: Orientation) {
    def move(garden: Garden): Position = {
        val newPosition = this.orientation match { // Good candidate for Lenses !
            case North => this.copy(point = this.point.copy(y = this.point.y + 1))
            case South => this.copy(point = this.point.copy(y = this.point.y - 1))
            case East => this.copy(point = this.point.copy(x = this.point.x + 1))
            case West => this.copy(point = this.point.copy(x = this.point.x - 1))
        }
        if (garden.isInGarden(newPosition.point)) newPosition else this
    }
}

sealed trait Orientation {
    def turn(instruction: Instruction): Orientation
}
case object North extends Orientation {
    def turn(instruction: Instruction) = instruction match {
        case L => West
        case R => East
        case _ => this
    }
}
case object South extends Orientation {
    def turn(instruction: Instruction) = instruction match {
        case L => East
        case R => West
        case _ => this
    }
}
case object East extends Orientation {
    def turn(instruction: Instruction) = instruction match {
        case L => North
        case R => South
        case _ => this
    }
}
case object West extends Orientation {
    def turn(instruction: Instruction) = instruction match {
        case L => South
        case R => North
        case _ => this
    }
}

sealed trait Instruction
case object L extends Instruction // turn Left
case object R extends Instruction // turn Right
case object A extends Instruction // Go on

