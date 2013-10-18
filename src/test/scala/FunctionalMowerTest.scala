import org.specs2.mutable.Specification

/**
 * @author David Galichet.
 */
class FunctionalMowerTest extends Specification {

    val playGround = Garden(Point(0, 0), Point(5, 5))

    "A mower" should {
        "be West oriented when turned left and initial orientation is North" in {
            val initialState = MowerState(playGround, List(Position(Point(2, 2), North)))
            FunctionalMower.stackInstructions(List(L)).run(initialState)._2 === Position(Point(2, 2), West)
        }
        "be East oriented when turned right and initial orientation is North" in {
            val initialState = MowerState(playGround, List(Position(Point(2, 2), North)))
            FunctionalMower.stackInstructions(List(R)).run(initialState)._2 === Position(Point(2, 2), East)
        }
        "be East oriented when turned left and initial orientation is South" in {
            val initialState = MowerState(playGround, List(Position(Point(2, 2), South)))
            FunctionalMower.stackInstructions(List(L)).run(initialState)._2 === Position(Point(2, 2), East)
        }
        "be West oriented when turned right and initial orientation is South" in {
            val initialState = MowerState(playGround, List(Position(Point(2, 2), South)))
            FunctionalMower.stackInstructions(List(R)).run(initialState)._2 === Position(Point(2, 2), West)
        }
        "be South oriented when turned left and initial orientation is West" in {
            val initialState = MowerState(playGround, List(Position(Point(2, 2), West)))
            FunctionalMower.stackInstructions(List(L)).run(initialState)._2 === Position(Point(2, 2), South)
        }
        "be North oriented when turned right and initial orientation is West" in {
            val initialState = MowerState(playGround, List(Position(Point(2, 2), West)))
            FunctionalMower.stackInstructions(List(R)).run(initialState)._2 === Position(Point(2, 2), North)
        }
        "be North oriented when turned left and initial orientation is East" in {
            val initialState = MowerState(playGround, List(Position(Point(2, 2), East)))
            FunctionalMower.stackInstructions(List(L)).run(initialState)._2 === Position(Point(2, 2), North)
        }
        "be South oriented when turned right and initial orientation is East" in {
            val initialState = MowerState(playGround, List(Position(Point(2, 2), East)))
            FunctionalMower.stackInstructions(List(R)).run(initialState)._2 === Position(Point(2, 2), South)
        }
        "be able to follow several instructions" in {
            val initialState = MowerState(playGround, List(Position(Point(1, 2), North)))
            FunctionalMower.stackInstructions(List(L, A, L, A, L, A, L, A, A)).run(initialState)._2 === Position(Point(1, 3), North)
        }
        "be able to follow several instructions which can leave playground" in {
            val initialState = MowerState(playGround, List(Position(Point(3, 3), East)))
            FunctionalMower.stackInstructions(List(A, A, R, A, A, R, A, R, R, A)).run(initialState)._2 === Position(Point(5, 1), East)
        }
        "be able to know which percentage of grass has been cut" in {
            def createPositions(pos: List[(Int, Int)]): List[Position] = pos.map{ case (x, y) => Position(Point(x, y), North) }
            val fivePositions = createPositions(List((1, 1), (2, 2), (3, 3), (4, 4), (5, 5)))
            val fivePositionsWithDuplicate = createPositions(List((1, 1), (2, 2), (1, 1), (4, 4), (5, 5)))
            FunctionalMower.processingRate(MowerState(playGround, Nil)) === 0.0 and
                fivePositions.size === 5 and
                FunctionalMower.processingRate(MowerState(playGround, fivePositions)) === 0.2 and
                FunctionalMower.processingRate(MowerState(playGround, fivePositionsWithDuplicate)) === 0.16
        }
        "We can transform the state to have a result as percentage of cutted grass" in {
            val initialState = MowerState(playGround, List(Position(Point(3, 3), East)))
            FunctionalMower.stateProcessingRate(List(A, A, R, A, A)).run(initialState)._2 === 0.2

        }
    }
}
