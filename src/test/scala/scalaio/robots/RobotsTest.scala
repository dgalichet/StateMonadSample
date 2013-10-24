package scalaio.robots

import org.specs2.mutable.Specification
import scalaio.State

/**
 * @author David Galichet.
 */
class RobotsTest extends Specification {

    val simplePlaygroundState = Playground(
        Point(0, 0),
        Point(3, 3),
        Set.empty[Point],
        Robot(R1, Position(0, 0, North)::Nil),
        Robot(R2, Position(3, 3, South)::Nil))

    val playGroundWithCoins = Playground(
        Point(0, 0),
        Point(3, 3),
        Set(Point(0, 1), Point(1, 2), Point(3, 1)),
        Robot(R1, Position(0, 0, North)::Nil),
        Robot(R2, Position(3, 3, South)::Nil))

    "A robots simulator" should {
        "return initial state if no instruction is provided" in {
            val (state, (score1, score2)) = Robots.compileInstructions(Nil, Nil).run(simplePlaygroundState)
            simplePlaygroundState === state and score1 === Score(R1, 0) and score2 === Score(R2, 0)
        }
        "return a state convenient regarding to instructions" in {
            val (state, (_, _)) = Robots.compileInstructions(List(A, A, R, A), List(A, A, R, A)).run(simplePlaygroundState)
            state.r1.player === R1 and state.r1.currentPosition === Position(1, 2, East) and
                state.r2.player === R2  and state.r2.currentPosition === Position(2, 1, West)
        }
        "We can use State.gets to get final position" in {
            val (_, (r1, r2)) = Robots.compileInstructions(List(A, A, R, A), List(A, A, R, A))
                .flatMap( _ => State.gets { s => (s.r1, s.r2) } )
                .run(simplePlaygroundState)
            r1.player === R1 and r1.currentPosition === Position(1, 2, East) and
                r2.player === R2 and r2.currentPosition === Position(2, 1, West)
        }
        "We can also use for comprehension to get final position" in {
            val stateP = for {
                state <- Robots.compileInstructions(List(A, A, R, A), List(A, A, R, A))
                stateP <- State.gets { s: Playground => (s.r1, s.r2) }
            } yield stateP
            val (_, (r1, r2)) = stateP.run(simplePlaygroundState)
            r1.player === R1 and r1.currentPosition === Position(1, 2, East) and
                r2.player === R2 and r2.currentPosition === Position(2, 1, West)
        }
        "when instructions make robot go out of playground, nothing to do" in {
            val (state, (_, _)) = Robots.compileInstructions(List(A, A, L, A), Nil).run(simplePlaygroundState)
            state.r2.player === R1 and state.r2.currentPosition === Position(0, 2, West)
        }
        "A robot can't go where another robot is actually" in {
            val (state, (_, _)) = Robots.compileInstructions(List(A, A, R, A, A, A), List(A, R, A, A, L, A)).run(simplePlaygroundState)
            state.r1.player === R1 and state.r1.currentPosition === Position(1, 2, East) and
            state.r2.player === R2 and state.r2.currentPosition === Position(2, 1, South)
        }
        "A robot collect some coins when he pass first on it" in {
            val (_, (score1, score2)) = Robots.compileInstructions(List(A, A, R, A, A), List(A, A, A, R, A)).run(playGroundWithCoins)
            score1.player === R1 and score1.score === 2 and score2.player === R2 and score2.score === 1
        }
        "We can use State.map to declare final result" in {
            val (_, result) = Robots.compileInstructions(List(A, A, R, A, A), List(A, A, A, R, A))
                .map(Robots.declareWinners)
                .run(playGroundWithCoins)
            result === "Robot R1 wins againts R2 with a score of 2 over 1"
        }
        "We simulate competition" in {
            val playground = Playground(
                Point(0, 0), Point(3, 3),
                Set(Point(0, 1), Point(0, 3), Point(1, 2), Point(2, 2), Point(3, 1)),
                Robot(R1, List(Position(0, 0, North))),
                Robot(R2, List(Position(3, 3, South))))
            val (_, result) = Robots.compileInstructions(List(A, A, R, A, A, A), List(A, R, A, L, A, A))
                .map(Robots.declareWinners)
                .run(playground)
            result === "Robot R1 wins againts R2 with a score of 2 over 1"
        }
    }

}
