/**
 * @author David Galichet.
 */
object Tondeuse {

    def run(garden: Garden, instructions: List[Instruction]): Position => Position = { initial =>
        instructions match {
            case head::tail if head == A => run(garden, tail)(initial.move(garden))
            case head::tail => run(garden, tail)(initial.copy(orientation = initial.orientation.turn(head)))
            case _ => initial
        }
    }
}

case class Point(x: Int, y: Int)

case class Garden(bottomLeft: Point, topRight: Point)

case class Position(point: Point, orientation: Orientation) {
    def move(implicit garden: Garden): Position = this.orientation match {
        case North => if (isPossiblePosition(this.point.copy(y = this.point.y + 1))) this.copy(point = this.point.copy(y = this.point.y + 1)) else this
        case South => if (isPossiblePosition(this.point.copy(y = this.point.y - 1))) this.copy(point = this.point.copy(y = this.point.y - 1)) else this
        case East => if (isPossiblePosition(this.point.copy(x = this.point.x + 1))) this.copy(point = this.point.copy(x = this.point.x + 1)) else this
        case West => if (isPossiblePosition(this.point.copy(x = this.point.x - 1))) this.copy(point = this.point.copy(x = this.point.x - 1)) else this
    }

    private def isPossiblePosition(current: Point)(implicit garden: Garden) =
        garden.bottomLeft.x <= current.x &&
            current.x <= garden.topRight.x &&
            garden.bottomLeft.y <= current.y &&
            current.y <= garden.topRight.y
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

