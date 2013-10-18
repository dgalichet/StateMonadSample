/**
 * @author David Galichet.
 */
object FunctionalMower {

    def stackInstructions(instructions: List[Instruction]): State[MowerState, Position] = instructions match {
        case Nil => State { previous => (previous, previous.positions.head) }
        case A::tail => State[MowerState, Position] { mowerState =>
            val next = mowerState.currentPosition.move(mowerState.garden)
            (mowerState.nextPosition(next), next)
        }.flatMap( _ => stackInstructions(tail) )
        case head::tail => State[MowerState, Position] { mowerState =>
            val next = mowerState.currentPosition.copy(orientation = mowerState.currentPosition.orientation.turn(head))
            (mowerState.nextPosition(next), next)
        }.flatMap( _ => stackInstructions(tail) )
    }

    def stateProcessingRate(instructions: List[Instruction]): State[MowerState, Double] =
        stackInstructions(instructions).flatMap( _ => State.gets(FunctionalMower.processingRate) )

    def processingRate(s: MowerState): Double = {
        val totalSize = (s.garden.topRight.x - s.garden.bottomLeft.x) * (s.garden.topRight.y - s.garden.bottomLeft.y)
        val processedSize = s.positions.map(_.point).distinct.size
        processedSize.toDouble / totalSize.toDouble
    }
}

case class MowerState(garden: Garden, positions: List[Position]) {
    def currentPosition = positions.head
    def nextPosition(next: Position) = this.copy(positions = next::positions)
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

