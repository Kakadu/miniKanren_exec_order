package utils

import org.klogic.core.*
import org.klogic.utils.terms.LogicList
import org.klogic.utils.terms.Nil.nilLogicList
import org.klogic.utils.terms.plus
import org.klogic.utils.terms.toLogicList

object UnificationsController {
    private var unificationsCounter: Int = 0

    fun onUnification() {
        ++unificationsCounter
    }

    fun onFinish() {
        System.out.printf("unifications: %d\n", unificationsCounter)
        clear()
    }

    private fun clear() {
        unificationsCounter = 0
    }
}

infix fun <T : Term<T>> Term<T>.debugUnify(other: Term<T>): Goal = { state: State ->
    System.out.printf("%s %s\n", this, other)
    UnificationsController.onUnification()

    (this unify other)(state)
}

typealias ListTerm<T> = Term<LogicList<T>>

fun <T : Term<T>> appendo(a: ListTerm<T>, b: ListTerm<T>, ab: ListTerm<T>): Goal = { state ->
    System.out.printf("appendo: %s %s %s\n", a, b, ab)

    conde(
        and(
            a debugUnify nilLogicList(),
            b debugUnify ab
        ),
        freshTypedVars<T, LogicList<T>, LogicList<T>> { head, tail, rest ->
            and(
                a debugUnify head + tail,
                ab debugUnify head + rest,
                appendo(tail, b, rest)
            )
        }
    )(state)
}

fun <T : Term<T>> reverso(a: ListTerm<T>, b: ListTerm<T>): Goal = { state ->
    System.out.printf("appendo: %s %s\n", a, b)

    conde(
        and(
            a debugUnify nilLogicList(),
            a debugUnify b
        ),
        freshTypedVars<T, LogicList<T>, LogicList<T>> { h, t, tmp ->
            and(
                a debugUnify h + t,
                reverso(t, tmp),
                appendo(
                    tmp,
                    h.toLogicList(),
                    b
                )
            )
        }
    )(state)
}
