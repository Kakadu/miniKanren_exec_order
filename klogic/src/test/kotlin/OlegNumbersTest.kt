import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Test
import org.klogic.core.Term
import org.klogic.core.run
import utils.UnificationsController
import utils.OlegLogicNumber
import utils.OlegLogicNumber.Companion.toOlegLogicNumber
import utils.expᴼ
import utils.logᴼ

class OlegNumbersTest {
    @AfterEach
    fun clear() {
        UnificationsController.onFinish()
    }

    @Test
    fun testExpo1() {
        val base = 2u.toOlegLogicNumber()
        val power = 3u.toOlegLogicNumber()

        println("$base^$power")
        run(1, { r: Term<utils.OlegLogicNumber> -> expᴼ(base, power, r) })
        UnificationsController.onFinish()
    }

    @Test
    fun testLogo1() {
        val n = 8u.toOlegLogicNumber()
        val b = 2u.toOlegLogicNumber()
        val r = 0u.toOlegLogicNumber()

        println("log $n base $b with reminder $r")
        run(1, { q: Term<OlegLogicNumber> -> logᴼ(n, b, q, r) })

        UnificationsController.onFinish()
    }
}
