package scalaio.robots

import scalaio.State

/**
 * @author David Galichet.
 */
object Robots {

    def compileInstructions(i1: List[Instruction], i2: List[Instruction]): State[Playground, (Score, Score)] = i1 match {
        case Nil if i2 == Nil => State.gets(_.scores)
        case Nil => State[Playground, (Score, Score)] { s => (s.swapRobots(), s.scores) }.flatMap { _ => compileInstructions(i2, i1) }
        case head::tail => State[Playground, (Score, Score)] { s =>
            val s1 = processInstruction(head)(s)
            (s1.swapRobots(), s1.scores)
        }.flatMap { _ => compileInstructions(i2, tail) }
    }

    def declareWinners(scores: (Score, Score)): String = {
        val (winner, looser) = scores match {
            case (s1, s2) if s1.score > s2.score => (s1, s2)
            case (s1, s2) => (s2, s1)
        }
        s"Robot ${winner.player} wins againts ${looser.player} with a score of ${winner.score} over ${looser.score}"
    }

    def processInstruction(i: Instruction)(s: Playground): Playground = {
        val next = i match {
            case A => s.r1.currentPosition.move(s)
            case i => s.r1.currentPosition.turn(i)
        }

        if (s.coins.contains(next.point)) {
            s.copy(coins = s.coins - next.point, r1 = s.r1.addCoin(next.point).addPosition(next))
        } else {
            s.copy(r1 = s.r1.addPosition(next))
        }
    }
}

case class Playground(bottomLeft: Point, topRight: Point, coins: Set[Point], r1: Robot, r2: Robot) {

    assert(bottomLeft.x < topRight.x && bottomLeft.y < topRight.y,
        s"Bad Playground definition. (${bottomLeft.x}, ${bottomLeft.y}) must be < (${topRight.x}, ${topRight.y})")
    assert(coins.foldLeft(true)(_ && isInPlayground(_)), "All coins must be in Playground !")

    def isInPlayground(point: Point): Boolean =
        bottomLeft.x <= point.x && point.x <= topRight.x && bottomLeft.y <= point.y && point.y <= topRight.y

    def isPossiblePosition(pos: Position): Boolean = isInPlayground(pos.point) && r2.currentPosition.point != pos.point

    lazy val scores: (Score, Score) = (r1.score, r2.score)

    def swapRobots(): Playground = this.copy(r1 = r2, r2 = r1)
}


case class Robot(player: Player, positions: List[Position], coins: List[Point] = Nil) {
    lazy val currentPosition = positions.head

    lazy val score: Score = Score(player, coins.size)

    def addPosition(next: Position): Robot = this.copy(positions = next::positions)

    def addCoin(coin: Point): Robot = this.copy(coins = coin::coins)
}

sealed trait Player
case object R1 extends Player
case object R2 extends Player

case class Score(player: Player, score: Int)

case class Point(x: Int, y: Int)

case class Position(point: Point, direction: Direction) {
    def move(s: Playground): Position = {
        val newPosition = this.direction match { // Good candidate for Lenses !
            case North => this.copy(point = this.point.copy(y = this.point.y + 1))
            case South => this.copy(point = this.point.copy(y = this.point.y - 1))
            case East => this.copy(point = this.point.copy(x = this.point.x + 1))
            case West => this.copy(point = this.point.copy(x = this.point.x - 1))
        }
        if (s.isPossiblePosition(newPosition)) newPosition else this
    }

    def turn(instruction: Instruction): Position = this.copy(direction = direction.turn(instruction))
}

object Position {
    def apply(x: Int, y: Int, direction: Direction): Position = Position(Point(x, y), direction)
}

sealed trait Direction {
    def turn(instruction: Instruction): Direction
}
case object North extends Direction {
    def turn(instruction: Instruction) = instruction match {
        case L => West
        case R => East
        case _ => this
    }
}
case object South extends Direction {
    def turn(instruction: Instruction) = instruction match {
        case L => East
        case R => West
        case _ => this
    }
}
case object East extends Direction {
    def turn(instruction: Instruction) = instruction match {
        case L => North
        case R => South
        case _ => this
    }
}
case object West extends Direction {
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

