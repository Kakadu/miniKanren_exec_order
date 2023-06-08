import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Test
import org.klogic.core.Term
import org.klogic.core.run
import org.klogic.utils.terms.LogicList
import org.klogic.utils.terms.Symbol
import org.klogic.utils.terms.Symbol.Companion.toSymbol
import utils.UnificationsController
import utils.reverso

class ReversoTest {
    @AfterEach
    fun clear() {
        UnificationsController.onFinish()
    }

    // May be run manually using './gradlew :test --tests "ReversoTest.testReverso"'
    @Test
    fun testReverso() {
        val a = LogicList.logicListOf("1".toSymbol(), "2".toSymbol())

        val goal = { q: Term<LogicList<Symbol>> -> reverso(a, q) }
        run(1, goal)

        UnificationsController.onFinish()
    }
    
    @Test
    fun testReverso2() {
        val a = LogicList.logicListOf("1".toSymbol()
        , "2".toSymbol()
        )

        val goal = { q: Term<LogicList<Symbol>> -> reverso(q, a) }
        run(1, goal)

        UnificationsController.onFinish()
    }
}