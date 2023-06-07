import kotlin.reflect.KFunction0

private val tasks: Map<String, KFunction0<Unit>> = mapOf(
    "appendo1" to AppendoTest()::testAppendo1,
    "appendo2" to AppendoTest()::testAppendo2,
    "reverso" to ReversoTest()::testReverso,
    "expo1" to OlegNumbersTest()::testExpo1,
    "logo1" to OlegNumbersTest()::testLogo1,
)

fun main(args: Array<String>) {
    args.singleOrNull()?.let {
        tasks[it]?.invoke()
    } ?: System.err.println(
        """
"Unknown command ${args.contentToString()}, available commands
${tasks.keys.joinToString(System.lineSeparator())}
        """.trimIndent()
    )
}
