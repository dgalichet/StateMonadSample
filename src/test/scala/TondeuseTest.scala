import org.specs2.mutable.Specification

/**
 * @author David Galichet.
 */
class TondeuseTest extends Specification {

    val playGround = Garden(Point(0, 0), Point(5, 5))

    "A tondeuse" should {
        "be West oriented when turned left and initial orientation is North" in {
            Tondeuse.run(playGround, List(L)).run(List(Position(Point(2, 2), North)))._2 === Position(Point(2, 2), West)
        }
        "be East oriented when turned right and initial orientation is North" in {
            Tondeuse.run(playGround, List(R)).run(List(Position(Point(2, 2), North)))._2 === Position(Point(2, 2), East)
        }
        "be East oriented when turned left and initial orientation is South" in {
            Tondeuse.run(playGround, List(L)).run(List(Position(Point(2, 2), South)))._2 === Position(Point(2, 2), East)
        }
        "be West oriented when turned right and initial orientation is South" in {
            Tondeuse.run(playGround, List(R)).run(List(Position(Point(2, 2), South)))._2 === Position(Point(2, 2), West)
        }
        "be South oriented when turned left and initial orientation is West" in {
            Tondeuse.run(playGround, List(L)).run(List(Position(Point(2, 2), West)))._2 === Position(Point(2, 2), South)
        }
        "be North oriented when turned right and initial orientation is West" in {
            Tondeuse.run(playGround, List(R)).run(List(Position(Point(2, 2), West)))._2 === Position(Point(2, 2), North)
        }
        "be North oriented when turned left and initial orientation is East" in {
            Tondeuse.run(playGround, List(L)).run(List(Position(Point(2, 2), East)))._2 === Position(Point(2, 2), North)
        }
        "be South oriented when turned right and initial orientation is East" in {
            Tondeuse.run(playGround, List(R)).run(List(Position(Point(2, 2), East)))._2 === Position(Point(2, 2), South)
        }
        "be able to follow several instructions" in {
            Tondeuse.run(playGround, List(L, A, L, A, L, A, L, A, A)).run(List(Position(Point(1, 2), North)))._2 === Position(Point(1, 3), North)
        }
        "be able to follow several instructions which can leave playground" in {
            Tondeuse.run(playGround, List(A, A, R, A, A, R, A, R, R, A)).run(List(Position(Point(3, 3), East)))._2 === Position(Point(5, 1), East)
        }
        //"be able to know which percentage of grass has been cut"

    }
}
