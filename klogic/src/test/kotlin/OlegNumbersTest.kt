import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Test
import org.klogic.core.Term
import org.klogic.core.run
import utils.UnificationsController
import org.klogic.utils.terms.OlegLogicNumber
import org.klogic.utils.terms.OlegLogicNumber.Companion.toOlegLogicNumber
import org.klogic.utils.terms.expᴼ
import org.klogic.utils.terms.logᴼ

class OlegNumbersTest {
    @AfterEach
    fun clear() {
        UnificationsController.onFinish()
    }

    @Test
    fun testExpo1() {
        val base = 3u.toOlegLogicNumber()
        val power = 5u.toOlegLogicNumber()

        println("$base^$power")
        run(1, { r: Term<OlegLogicNumber> -> expᴼ(base, power, r) })
        UnificationsController.onFinish()
    }

    @Test
    fun testLogo1() {
        val n = 14u.toOlegLogicNumber()
        val b = 2u.toOlegLogicNumber()
        val r = 5u.toOlegLogicNumber()

        println("log $n base $b with reminder $r")
        run(1, { q: Term<OlegLogicNumber> -> logᴼ(n, b, q, r) })

        UnificationsController.onFinish()
    }
}
