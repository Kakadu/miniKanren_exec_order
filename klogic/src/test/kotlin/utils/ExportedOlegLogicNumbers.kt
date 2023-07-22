@file:Suppress("FunctionName", "NonAsciiCharacters", "TestFunctionName", "RedundantLambdaOrAnonymousFunction")

package utils.generated

//import org.klogic.core.*
import org.klogic.core.*
import org.klogic.core.Term
import org.klogic.core.Var.Companion.createTypedVar
import org.klogic.utils.terms.LogicList
import org.klogic.utils.terms.LogicList.Companion.logicListOf
import org.klogic.utils.terms.Nil.nilLogicList
import org.klogic.utils.terms.plus

import utils.LogicInt
import utils.LogicInt.Companion.toLogic
import utils.debugUnify
import utils.freshTypedVar

val digitZero: Digit = 0.toLogic()
val digitOne: Digit = 1.toLogic()

typealias Digit = LogicInt
typealias OlegLogicNumber = LogicList<Digit>

val zero: OlegLogicNumber = logicListOf()
val one: OlegLogicNumber = logicListOf(1.toLogic())
val three: OlegLogicNumber = logicListOf(1.toLogic(), 1.toLogic())

private infix fun <T: Term<T>> Term<T>.`====`(other: Term<T>): Goal  = this.debugUnify(other)
fun <T : Term<T>> fresh(): Var<T> = utils.freshTypedVar()

fun UInt.toOlegLogicNumber(): OlegLogicNumber = toLogicList()
fun UInt.toLogicList(): LogicList<Digit> =
    when {
        this == 0u -> nilLogicList()
        this % 2u == 0u -> digitZero + (this / 2u).toLogicList()
        else -> digitOne + (this / 2u).toLogicList()
    }

fun <T> pause(f: () -> RecursiveStream<T>): RecursiveStream<T> = ThunkStream(f)


fun poso(n: Term<LogicList<LogicInt>>): Goal =
    { st: State ->
        pause {
            val h: Term<LogicInt> = fresh();
            val t: Term<LogicList<LogicInt>> = fresh();
            (n `====` (h + t))(st)
        }
    }


fun zeroo(n: Term<LogicList<LogicInt>>): Goal =
    (n `====` nilLogicList())

fun appendo(
    l: Term<LogicList<LogicInt>>, s: Term<LogicList<LogicInt>>,
    out: Term<LogicList<LogicInt>>
): Goal =
    { st: State ->
        pause {
            (((((l `====` nilLogicList())(st))
                    bind
                    ((s `====` out))))
                    mplus

                    (pause {
                        { st: State ->
                            pause {
                                val a: Term<LogicInt> = fresh();
                                val d: Term<LogicList<LogicInt>> = fresh();
                                val res: Term<LogicList<LogicInt>> = fresh();
                                ((((((a + d) `====` l)(st))
                                        bind
                                        (((a + res) `====` out))))
                                        bind
                                        (appendo(d, s, res)))
                            }
                        }(st)
                    }))
        }
    }

fun gt1o(n: Term<LogicList<LogicInt>>): Goal =
    { st: State ->
        pause {
            val a: Term<LogicInt> = fresh();
            val ad: Term<LogicInt> = fresh();
            val dd: Term<LogicList<LogicInt>> = fresh();
            (n `====` (a + (ad + dd)))(st)
        }
    }

fun full_addero(
    b: Term<LogicInt>, x: Term<LogicInt>, y: Term<LogicInt>,
    r: Term<LogicInt>, c: Term<LogicInt>
): Goal =
    { st: State ->
        pause {
            (((((((((((0.toLogic() `====` b)(st))
                    bind
                    ((0.toLogic() `====` x))))
                    bind
                    ((0.toLogic() `====` y))))
                    bind
                    ((0.toLogic() `====` r))))
                    bind
                    ((0.toLogic() `====` c))))
                    mplus

                    (pause {
                        (((((((((((1.toLogic() `====` b)(st))
                                bind
                                ((0.toLogic() `====` x))))
                                bind
                                ((0.toLogic() `====` y))))
                                bind
                                ((1.toLogic() `====` r))))
                                bind
                                ((0.toLogic() `====` c))))
                                mplus

                                (pause {
                                    (((((((((((0.toLogic() `====` b)(st))
                                            bind
                                            ((1.toLogic() `====` x))))
                                            bind
                                            ((0.toLogic() `====` y))))
                                            bind
                                            ((1.toLogic() `====` r))))
                                            bind
                                            ((0.toLogic() `====` c))))
                                            mplus

                                            (pause {
                                                (((((((((((1.toLogic() `====` b)(st))
                                                        bind
                                                        ((1.toLogic() `====` x))))
                                                        bind
                                                        ((0.toLogic() `====` y))))
                                                        bind
                                                        ((0.toLogic() `====` r))))
                                                        bind
                                                        ((1.toLogic() `====` c))))
                                                        mplus

                                                        (pause {
                                                            (((((((((((0.toLogic() `====` b)(st))
                                                                    bind
                                                                    ((0.toLogic() `====` x))))
                                                                    bind
                                                                    ((1.toLogic() `====` y))))
                                                                    bind
                                                                    ((1.toLogic() `====` r))))
                                                                    bind
                                                                    ((0.toLogic() `====` c))))
                                                                    mplus

                                                                    (pause {
                                                                        ((((
                                                                                ((
                                                                                        ((
                                                                                                (((1.toLogic() `====` b)(
                                                                                                    st
                                                                                                ))
                                                                                                        bind
                                                                                                        ((0.toLogic() `====` x))))
                                                                                                bind
                                                                                                ((1.toLogic() `====` y))))
                                                                                        bind
                                                                                        ((0.toLogic() `====` r))))
                                                                                bind
                                                                                ((1.toLogic() `====` c))))
                                                                                mplus

                                                                                (pause {
                                                                                    ((
                                                                                            ((
                                                                                                    ((
                                                                                                            ((
                                                                                                                    (((0.toLogic() `====` b)(
                                                                                                                        st
                                                                                                                    ))
                                                                                                                            bind
                                                                                                                            ((1.toLogic() `====` x))))
                                                                                                                    bind
                                                                                                                    ((1.toLogic() `====` y))))
                                                                                                            bind
                                                                                                            ((0.toLogic() `====` r))))
                                                                                                    bind
                                                                                                    ((1.toLogic() `====` c))))
                                                                                            mplus

                                                                                            (
                                                                                                    pause {
                                                                                                        ((
                                                                                                                ((
                                                                                                                        ((
                                                                                                                                (((1.toLogic() `====` b)(
                                                                                                                                    st
                                                                                                                                ))
                                                                                                                                        bind
                                                                                                                                        ((1.toLogic() `====` x))))
                                                                                                                                bind
                                                                                                                                ((1.toLogic() `====` y))))
                                                                                                                        bind
                                                                                                                        ((1.toLogic() `====` r))))
                                                                                                                bind
                                                                                                                ((1.toLogic() `====` c)))
                                                                                                    }))
                                                                                }))
                                                                    }))
                                                        }))
                                            }))
                                }))
                    }))
        }
    }

fun addero(
    d: Term<LogicInt>, n: Term<LogicList<LogicInt>>,
    m: Term<LogicList<LogicInt>>, r: Term<LogicList<LogicInt>>
): Goal =
    { st: State ->
        pause {
            (((((((0.toLogic() `====` d)(st))
                    bind
                    ((m `====` nilLogicList()))))
                    bind
                    ((n `====` r))))
                    mplus

                    (pause {
                        (((((((((0.toLogic() `====` d)(st))
                                bind
                                ((n `====` nilLogicList()))))
                                bind
                                ((m `====` r))))
                                bind
                                (poso(m))))
                                mplus

                                (pause {
                                    (((((((1.toLogic() `====` d)(st))
                                            bind
                                            ((m `====` nilLogicList()))))
                                            bind
                                            (addero(0.toLogic(), n, one, r))))
                                            mplus

                                            (pause {
                                                (((((((((1.toLogic() `====` d)(st))
                                                        bind
                                                        ((n `====` nilLogicList()))))
                                                        bind
                                                        (poso(m))))
                                                        bind
                                                        (addero(
                                                            0.toLogic(), m, one,
                                                            r
                                                        ))))
                                                        mplus

                                                        (pause {
                                                            (((((((n `====` one)(st))
                                                                    bind
                                                                    ((m `====` one))))
                                                                    bind
                                                                    ({ st: State ->
                                                                        pause {
                                                                            val a: Term<LogicInt> = fresh();
                                                                            val c: Term<LogicInt> = fresh();
                                                                            (((
                                                                                    (a +
                                                                                            (c + nilLogicList())) `====` r)(
                                                                                st
                                                                            ))
                                                                                    bind
                                                                                    (
                                                                                            full_addero(
                                                                                                d,
                                                                                                1.toLogic(),
                                                                                                1.toLogic(),
                                                                                                a, c
                                                                                            )))
                                                                        }
                                                                    })))
                                                                    mplus
                                                                    (pause {
                                                                        (((((n `====` one)(st))
                                                                                bind
                                                                                (
                                                                                        gen_addero(
                                                                                            d,
                                                                                            n, m, r
                                                                                        ))))
                                                                                mplus

                                                                                (pause {
                                                                                    ((
                                                                                            ((
                                                                                                    ((
                                                                                                            (((m `====` one)(
                                                                                                                st
                                                                                                            ))
                                                                                                                    bind
                                                                                                                    (gt1o(
                                                                                                                        n
                                                                                                                    ))))
                                                                                                            bind
                                                                                                            (gt1o(r))))
                                                                                                    bind
                                                                                                    (
                                                                                                            addero(
                                                                                                                d,
                                                                                                                one, n,
                                                                                                                r
                                                                                                            ))))
                                                                                            mplus

                                                                                            (
                                                                                                    pause {
                                                                                                        ((
                                                                                                                gt1o(n)(
                                                                                                                    st
                                                                                                                ))
                                                                                                                bind
                                                                                                                (
                                                                                                                        gen_addero(
                                                                                                                            d,
                                                                                                                            n,
                                                                                                                            m,
                                                                                                                            r
                                                                                                                        )))
                                                                                                    }))
                                                                                }))
                                                                    }))
                                                        }))
                                            }))
                                }))
                    }))
        }
    }

fun gen_addero(
    d: Term<LogicInt>, n: Term<LogicList<LogicInt>>,
    m: Term<LogicList<LogicInt>>, r: Term<LogicList<LogicInt>>
): Goal =
    { st: State ->
        pause {
            val a: Term<LogicInt> = fresh();
            val b: Term<LogicInt> = fresh();
            val c: Term<LogicInt> = fresh();
            val e: Term<LogicInt> = fresh();
            val x: Term<LogicList<LogicInt>> = fresh();
            val y: Term<LogicList<LogicInt>> = fresh();
            val z: Term<LogicList<LogicInt>> = fresh();
            ((((((((((((((a + x) `====` n)(st))
                    bind
                    (((b + y) `====` m))))
                    bind
                    (poso(y))))
                    bind
                    (((c + z) `====` r))))
                    bind
                    (poso(z))))
                    bind
                    (full_addero(d, a, b, c, e))))
                    bind
                    (addero(e, x, y, z)))
        }
    }

fun pluso(
    n: Term<LogicList<LogicInt>>, m: Term<LogicList<LogicInt>>,
    k: Term<LogicList<LogicInt>>
): Goal =
    addero(0.toLogic(), n, m, k)

fun minuso(
    n: Term<LogicList<LogicInt>>, m: Term<LogicList<LogicInt>>,
    k: Term<LogicList<LogicInt>>
): Goal =
    pluso(m, k, n)

fun bound_multo(
    q: Term<LogicList<LogicInt>>, p: Term<LogicList<LogicInt>>,
    n: Term<LogicList<LogicInt>>, m: Term<LogicList<LogicInt>>
): Goal =
    { st: State ->
        pause {
            (((((q `====` zero)(st))
                    bind
                    (poso(p))))
                    mplus

                    (pause {
                        { st: State ->
                            pause {
                                val a0: Term<LogicInt> = fresh();
                                val a1: Term<LogicInt> = fresh();
                                val a2: Term<LogicInt> = fresh();
                                val a3: Term<LogicInt> = fresh();
                                val x: Term<LogicList<LogicInt>> = fresh();
                                val y: Term<LogicList<LogicInt>> = fresh();
                                val z: Term<LogicList<LogicInt>> = fresh();
                                (((((q `====` (a0 + x))(st))
                                        bind
                                        ((p `====` (a1 + y)))))
                                        bind
                                        ({ st: State ->
                                            pause {
                                                (((((((n `====` zero)(st))
                                                        bind
                                                        ((m `====` (a2 + z)))))
                                                        bind
                                                        (bound_multo(x, y, z, zero))))
                                                        mplus

                                                        (pause {
                                                            (((n `====` (a3 + z))(st))
                                                                    bind
                                                                    (bound_multo(x, y, z, m)))
                                                        }))
                                            }
                                        }))
                            }
                        }(st)
                    }))
        }
    }

fun multo(
    n: Term<LogicList<LogicInt>>, m: Term<LogicList<LogicInt>>,
    p: Term<LogicList<LogicInt>>
): Goal =
    { st: State ->
        pause {
            (((((n `====` zero)(st))
                    bind
                    ((p `====` zero))))
                    mplus

                    (pause {
                        ((((((poso(n)(st))
                                bind
                                ((m `====` zero))))
                                bind
                                ((p `====` zero))))
                                mplus

                                (pause {
                                    (((((((n `====` one)(st))
                                            bind
                                            (poso(m))))
                                            bind
                                            ((m `====` p))))
                                            mplus

                                            (pause {
                                                ((((((gt1o(n)(st))
                                                        bind
                                                        ((m `====` one))))
                                                        bind
                                                        ((n `====` p))))
                                                        mplus

                                                        (pause {
                                                            (({ st: State ->
                                                                pause {
                                                                    val x: Term<LogicList<LogicInt>> = fresh();
                                                                    val z: Term<LogicList<LogicInt>> = fresh();
                                                                    ((((((
                                                                            ((
                                                                                    (((n `====`
                                                                                            (0.toLogic() + x))(st))
                                                                                            bind
                                                                                            (poso(x))))
                                                                                    bind
                                                                                    ((p `====`
                                                                                            (0.toLogic() + z)))))
                                                                            bind
                                                                            (poso(z))))
                                                                            bind
                                                                            (gt1o(m))))
                                                                            bind
                                                                            (multo(
                                                                                x,
                                                                                m, z
                                                                            )))
                                                                }
                                                            }(st))
                                                                    mplus
                                                                    (pause {
                                                                        (({ st: State ->
                                                                            pause {
                                                                                val x: Term<LogicList<LogicInt>> =
                                                                                    freshTypedVar();
                                                                                val y: Term<LogicList<LogicInt>> =
                                                                                    freshTypedVar();
                                                                                ((((
                                                                                        ((
                                                                                                (((n `====`
                                                                                                        (1.toLogic() + x))(
                                                                                                    st
                                                                                                ))
                                                                                                        bind
                                                                                                        (poso(x))))
                                                                                                bind
                                                                                                ((m `====`
                                                                                                        (0.toLogic() + y)))))
                                                                                        bind
                                                                                        (poso(y))))
                                                                                        bind
                                                                                        (
                                                                                                multo(
                                                                                                    m,
                                                                                                    n, p
                                                                                                )))
                                                                            }
                                                                        }(st))
                                                                                mplus
                                                                                (pause {
                                                                                    { st: State ->
                                                                                        pause {
                                                                                            val x: Term<LogicList<LogicInt>> =
                                                                                                freshTypedVar();
                                                                                            val y: Term<LogicList<LogicInt>> =
                                                                                                freshTypedVar();
                                                                                            ((((
                                                                                                    ((
                                                                                                            (((n `====`
                                                                                                                    (1.toLogic() + x))(
                                                                                                                st
                                                                                                            ))
                                                                                                                    bind
                                                                                                                    (poso(
                                                                                                                        x
                                                                                                                    ))))
                                                                                                            bind
                                                                                                            ((m `====`
                                                                                                                    (1.toLogic() + y)))))
                                                                                                    bind
                                                                                                    (poso(y))))
                                                                                                    bind
                                                                                                    (
                                                                                                            odd_multo(
                                                                                                                x,
                                                                                                                n, m, p
                                                                                                            )))
                                                                                        }
                                                                                    }(st)
                                                                                }))
                                                                    }))
                                                        }))
                                            }))
                                }))
                    }))
        }
    }

fun odd_multo(
    x: Term<LogicList<LogicInt>>, n: Term<LogicList<LogicInt>>,
    m: Term<LogicList<LogicInt>>, p: Term<LogicList<LogicInt>>
): Goal =
    { st: State ->
        pause {
            val q: Term<LogicList<LogicInt>> = fresh();
            ((((bound_multo(q, p, n, m)(st))
                    bind
                    (multo(x, m, q))))
                    bind
                    (pluso((0.toLogic() + q), m, p)))
        }
    }

fun eqlo(n: Term<LogicList<LogicInt>>, m: Term<LogicList<LogicInt>>): Goal =
    { st: State ->
        pause {
            (((((n `====` zero)(st))
                    bind
                    ((m `====` zero))))
                    mplus

                    (pause {
                        (((((n `====` one)(st))
                                bind
                                ((m `====` one))))
                                mplus

                                (pause {
                                    { st: State ->
                                        pause {
                                            val a: Term<LogicInt> = fresh();
                                            val x: Term<LogicList<LogicInt>> = fresh();
                                            val b: Term<LogicInt> = fresh();
                                            val y: Term<LogicList<LogicInt>> = fresh();
                                            ((((((((((a + x) `====` n)(st))
                                                    bind
                                                    (poso(x))))
                                                    bind
                                                    (((b + y) `====` m))))
                                                    bind
                                                    (poso(y))))
                                                    bind
                                                    (eqlo(x, y)))
                                        }
                                    }(st)
                                }))
                    }))
        }
    }

fun ltlo(n: Term<LogicList<LogicInt>>, m: Term<LogicList<LogicInt>>): Goal =
    { st: State ->
        pause {
            (((((n `====` zero)(st))
                    bind
                    (poso(m))))
                    mplus

                    (pause {
                        (((((n `====` one)(st))
                                bind
                                (gt1o(m))))
                                mplus

                                (pause {
                                    { st: State ->
                                        pause {
                                            val a: Term<LogicInt> = fresh();
                                            val x: Term<LogicList<LogicInt>> = fresh();
                                            val b: Term<LogicInt> = fresh();
                                            val y: Term<LogicList<LogicInt>> = fresh();
                                            ((((((((((a + x) `====` n)(st))
                                                    bind
                                                    (poso(x))))
                                                    bind
                                                    (((b + y) `====` m))))
                                                    bind
                                                    (poso(y))))
                                                    bind
                                                    (ltlo(x, y)))
                                        }
                                    }(st)
                                }))
                    }))
        }
    }

fun lelo(n: Term<LogicList<LogicInt>>, m: Term<LogicList<LogicInt>>): Goal =
    { st: State ->
        pause {
            ((eqlo(n, m)(st))
                    mplus

                    (pause { ltlo(n, m)(st) }))
        }
    }

fun lto(n: Term<LogicList<LogicInt>>, m: Term<LogicList<LogicInt>>): Goal =
    { st: State ->
        pause {
            ((ltlo(n, m)(st))
                    mplus

                    (pause {
                        ((eqlo(n, m)(st))
                                bind
                                ({ st: State ->
                                    pause {
                                        val x: Term<LogicList<LogicInt>> = fresh();
                                        ((poso(x)(st))
                                                bind
                                                (pluso(n, x, m)))
                                    }
                                }))
                    }))
        }
    }

fun leo(n: Term<LogicList<LogicInt>>, m: Term<LogicList<LogicInt>>): Goal =
    { st: State ->
        pause {
            (((n `====` m)(st))
                    mplus

                    (pause { lto(n, m)(st) }))
        }
    }

@Suppress("LocalVariableName")
fun splito(
    n: Term<LogicList<LogicInt>>, r: Term<LogicList<LogicInt>>,
    l: Term<LogicList<LogicInt>>, h: Term<LogicList<LogicInt>>
): Goal =
    { st: State ->
        pause {
            (((((((n `====` zero)(st))
                    bind
                    ((h `====` zero))))
                    bind
                    ((l `====` zero))))
                    mplus

                    (pause {
                        (({ st: State ->
                            pause {
                                val b: Term<LogicInt> = fresh();
                                val n_: Term<LogicList<LogicInt>> = fresh();
                                (((((((n `====` (0.toLogic() + (b + n_)))(st))
                                        bind
                                        ((r `====` zero))))
                                        bind
                                        ((h `====` (b + n_)))))
                                        bind
                                        ((l `====` zero)))
                            }
                        }(st))
                                mplus
                                (pause {
                                    (({ st: State ->
                                        pause {
                                            val n_: Term<LogicList<LogicInt>> = fresh();
                                            (((((((n `====` (1.toLogic() + n_))(st))
                                                    bind
                                                    ((r `====` zero))))
                                                    bind
                                                    ((n_ `====` h))))
                                                    bind
                                                    ((l `====` one)))
                                        }
                                    }(st))
                                            mplus
                                            (pause {
                                                (({ st: State ->
                                                    pause {
                                                        val b: Term<LogicInt> = fresh();
                                                        val n_: Term<LogicList<LogicInt>> = fresh();
                                                        val a: Term<LogicInt> = fresh();
                                                        val r_: Term<LogicList<LogicInt>> = fresh();
                                                        (((((((n `====` (0.toLogic() +
                                                                (b + n_)))(st))
                                                                bind
                                                                (((a + r_) `====` r))))
                                                                bind
                                                                ((l `====` zero))))
                                                                bind
                                                                (splito(
                                                                    (b + n_), r_,
                                                                    zero, h
                                                                )))
                                                    }
                                                }(st))
                                                        mplus
                                                        (pause {
                                                            (({ st: State ->
                                                                pause {
                                                                    val n_: Term<LogicList<LogicInt>> = fresh();
                                                                    val a: Term<LogicInt> = fresh();
                                                                    val r_: Term<LogicList<LogicInt>> = fresh();
                                                                    (((((((n `====`
                                                                            (1.toLogic() + n_))(st))
                                                                            bind
                                                                            ((r `====`
                                                                                    (a + r_)))))
                                                                            bind
                                                                            ((l `====` one))))
                                                                            bind
                                                                            (splito(
                                                                                n_,
                                                                                r_, zero, h
                                                                            )))
                                                                }
                                                            }(st))
                                                                    mplus
                                                                    (pause {
                                                                        { st: State ->
                                                                            pause {
                                                                                val b: Term<LogicInt> = fresh();
                                                                                val n_: Term<LogicList<LogicInt>> =
                                                                                    freshTypedVar();
                                                                                val a: Term<LogicInt> = fresh();
                                                                                val r_: Term<LogicList<LogicInt>> =
                                                                                    freshTypedVar();
                                                                                val l_: Term<LogicList<LogicInt>> =
                                                                                    freshTypedVar();
                                                                                (((((((((n `====`
                                                                                        (b + n_))(st))
                                                                                        bind
                                                                                        ((r `====`
                                                                                                (a + r_)))))
                                                                                        bind
                                                                                        ((l `====`
                                                                                                (b + l_)))))
                                                                                        bind
                                                                                        (poso(l_))))
                                                                                        bind
                                                                                        (splito(
                                                                                            n_,
                                                                                            r_, l_, h
                                                                                        )))
                                                                            }
                                                                        }(st)
                                                                    }))
                                                        }))
                                            }))
                                }))
                    }))
        }
    }

fun divo(
    n: Term<LogicList<LogicInt>>, m: Term<LogicList<LogicInt>>,
    q: Term<LogicList<LogicInt>>, r: Term<LogicList<LogicInt>>
): Goal =
    { st: State ->
        pause {
            (((((((r `====` n)(st))
                    bind
                    ((q `====` zero))))
                    bind
                    (lto(n, m))))
                    mplus

                    (pause {
                        (((((((((q `====` one)(st))
                                bind
                                (eqlo(n, m))))
                                bind
                                (pluso(r, m, n))))
                                bind
                                (lto(r, m))))
                                mplus

                                (pause {
                                    ((((((ltlo(m, n)(st))
                                            bind
                                            (lto(r, m))))
                                            bind
                                            (poso(q))))
                                            bind
                                            ({ st: State ->
                                                pause {
                                                    val nh: Term<LogicList<LogicInt>> = fresh();
                                                    val nl: Term<LogicList<LogicInt>> = fresh();
                                                    val qh: Term<LogicList<LogicInt>> = fresh();
                                                    val ql: Term<LogicList<LogicInt>> = fresh();
                                                    val qlm: Term<LogicList<LogicInt>> = fresh();
                                                    val qlmr: Term<LogicList<LogicInt>> = fresh();
                                                    val rr: Term<LogicList<LogicInt>> = fresh();
                                                    val rh: Term<LogicList<LogicInt>> = fresh();
                                                    ((((splito(n, r, nl, nh)(st))
                                                            bind
                                                            (splito(q, r, ql, qh))))
                                                            bind
                                                            ({ st: State ->
                                                                pause {
                                                                    (((((((((nh `====` zero)(st))
                                                                            bind
                                                                            ((qh `====` zero))))
                                                                            bind
                                                                            (minuso(
                                                                                nl, r,
                                                                                qlm
                                                                            ))))
                                                                            bind
                                                                            (multo(ql, m, qlm))))
                                                                            mplus

                                                                            (pause {
                                                                                ((((
                                                                                        ((
                                                                                                ((
                                                                                                        ((
                                                                                                                poso(nh)(
                                                                                                                    st
                                                                                                                ))
                                                                                                                bind
                                                                                                                (
                                                                                                                        multo(
                                                                                                                            ql,
                                                                                                                            m,
                                                                                                                            qlm
                                                                                                                        ))))
                                                                                                        bind
                                                                                                        (
                                                                                                                pluso(
                                                                                                                    qlm,
                                                                                                                    r,
                                                                                                                    qlmr
                                                                                                                ))))
                                                                                                bind
                                                                                                (
                                                                                                        minuso(
                                                                                                            qlmr,
                                                                                                            nl, rr
                                                                                                        ))))
                                                                                        bind
                                                                                        (
                                                                                                splito(
                                                                                                    rr,
                                                                                                    r, zero,
                                                                                                    rh
                                                                                                ))))
                                                                                        bind
                                                                                        (divo(
                                                                                            nh,
                                                                                            m, qh,
                                                                                            rh
                                                                                        )))
                                                                            }))
                                                                }
                                                            }))
                                                }
                                            }))
                                }))
                    }))
        }
    }

fun repeated_mul(
    n: Term<LogicList<LogicInt>>, q: Term<LogicList<LogicInt>>,
    nq: Term<LogicList<LogicInt>>
): Goal =
    { st: State ->
        pause {
            ((((((poso(n)(st))
                    bind
                    ((q `====` zero))))
                    bind
                    ((nq `====` one))))
                    mplus

                    (pause {
                        (((((q `====` one)(st))
                                bind
                                ((n `====` nq))))
                                mplus

                                (pause {
                                    ((gt1o(q)(st))
                                            bind
                                            ({ st: State ->
                                                pause {
                                                    val q1: Term<LogicList<LogicInt>> = fresh();
                                                    val nq1: Term<LogicList<LogicInt>> = fresh();
                                                    ((((pluso(q1, one, q)(st))
                                                            bind
                                                            (repeated_mul(n, q1, nq1))))
                                                            bind
                                                            (multo(nq1, n, nq)))
                                                }
                                            }))
                                }))
                    }))
        }
    }

fun exp2(
    n: Term<LogicList<LogicInt>>, b: Term<LogicList<LogicInt>>,
    q: Term<LogicList<LogicInt>>
): Goal =
    { st: State ->
        pause {
            (((((n `====` one)(st))
                    bind
                    ((q `====` zero))))
                    mplus

                    (pause {
                        ((((((gt1o(n)(st))
                                bind
                                ((q `====` one))))
                                bind
                                ({ st: State ->
                                    pause {
                                        val s: Term<LogicList<LogicInt>> = fresh();
                                        splito(n, b, s, one)(st)
                                    }
                                })))
                                mplus
                                (pause {
                                    (({ st: State ->
                                        pause {
                                            val q1: Term<LogicList<LogicInt>> = fresh();
                                            val b2: Term<LogicList<LogicInt>> = fresh();
                                            (((((((((q `====` (0.toLogic() + q1))(st))
                                                    bind
                                                    (poso(q1))))
                                                    bind
                                                    (ltlo(b, n))))
                                                    bind
                                                    (appendo(
                                                        b, (1.toLogic() + b),
                                                        b2
                                                    ))))
                                                    bind
                                                    (exp2(n, b2, q1)))
                                        }
                                    }(st))
                                            mplus
                                            (pause {
                                                { st: State ->
                                                    pause {
                                                        val q1: Term<LogicList<LogicInt>> = fresh();
                                                        val nh: Term<LogicList<LogicInt>> = fresh();
                                                        val b2: Term<LogicList<LogicInt>> = fresh();
                                                        val s: Term<LogicList<LogicInt>> = fresh();
                                                        (((((((((((q `====` (1.toLogic() + q1))(st))
                                                                bind
                                                                (poso(q1))))
                                                                bind
                                                                (poso(nh))))
                                                                bind
                                                                (splito(n, b, s, nh))))
                                                                bind
                                                                (appendo(
                                                                    b,
                                                                    (1.toLogic() + b), b2
                                                                ))))
                                                                bind
                                                                (exp2(nh, b2, q1)))
                                                    }
                                                }(st)
                                            }))
                                }))
                    }))
        }
    }

fun logo(
    n: Term<LogicList<LogicInt>>, b: Term<LogicList<LogicInt>>,
    q: Term<LogicList<LogicInt>>, r: Term<LogicList<LogicInt>>
): Goal =
    { st: State ->
        pause {
            (((((((((n `====` one)(st))
                    bind
                    (poso(b))))
                    bind
                    ((q `====` zero))))
                    bind
                    ((r `====` zero))))
                    mplus

                    (pause {
                        (((((((q `====` zero)(st))
                                bind
                                (lto(n, b))))
                                bind
                                (pluso(r, one, n))))
                                mplus

                                (pause {
                                    (((((((((q `====` one)(st))
                                            bind
                                            (gt1o(b))))
                                            bind
                                            (eqlo(n, b))))
                                            bind
                                            (pluso(r, b, n))))
                                            mplus

                                            (pause {
                                                (((((((b `====` one)(st))
                                                        bind
                                                        (poso(q))))
                                                        bind
                                                        (pluso(r, one, n))))
                                                        mplus

                                                        (pause {
                                                            (((((((b `====` zero)(st))
                                                                    bind
                                                                    (poso(q))))
                                                                    bind
                                                                    ((r `====` n))))
                                                                    mplus

                                                                    (pause {
                                                                        (((((
                                                                                (0.toLogic() +
                                                                                        (1.toLogic() + nilLogicList())) `====` b)(
                                                                            st
                                                                        ))
                                                                                bind
                                                                                (
                                                                                        { st: State ->
                                                                                            pause {
                                                                                                val a: Term<LogicInt> =
                                                                                                    freshTypedVar();
                                                                                                val ad: Term<LogicInt> =
                                                                                                    freshTypedVar();
                                                                                                val dd: Term<LogicList<LogicInt>> =
                                                                                                    freshTypedVar();
                                                                                                ((
                                                                                                        ((
                                                                                                                ((
                                                                                                                        poso(
                                                                                                                            dd
                                                                                                                        )(st))
                                                                                                                        bind
                                                                                                                        ((n `====`
                                                                                                                                (a +
                                                                                                                                        (ad + dd))))))
                                                                                                                bind
                                                                                                                (
                                                                                                                        exp2(
                                                                                                                            n,
                                                                                                                            nilLogicList(),
                                                                                                                            q
                                                                                                                        ))))
                                                                                                        bind
                                                                                                        (
                                                                                                                { st: State ->
                                                                                                                    pause {
                                                                                                                        val s: Term<LogicList<LogicInt>> =
                                                                                                                            freshTypedVar();
                                                                                                                        splito(
                                                                                                                            n,
                                                                                                                            dd,
                                                                                                                            r,
                                                                                                                            s
                                                                                                                        )(st)
                                                                                                                    }
                                                                                                                }))
                                                                                            }
                                                                                        })))
                                                                                mplus

                                                                                (
                                                                                        pause {
                                                                                            ((
                                                                                                    ((
                                                                                                            { st: State ->
                                                                                                                pause {
                                                                                                                    val a: Term<LogicInt> =
                                                                                                                        freshTypedVar();
                                                                                                                    val ad: Term<LogicInt> =
                                                                                                                        freshTypedVar();
                                                                                                                    val add: Term<LogicInt> =
                                                                                                                        freshTypedVar();
                                                                                                                    val ddd: Term<LogicList<LogicInt>> =
                                                                                                                        freshTypedVar();
                                                                                                                    { st: State ->
                                                                                                                        pause {
                                                                                                                            (((b `====` three)(
                                                                                                                                st
                                                                                                                            ))
                                                                                                                                    mplus

                                                                                                                                    (
                                                                                                                                            pause {
                                                                                                                                                (b `====`
                                                                                                                                                        (a +
                                                                                                                                                                (ad +
                                                                                                                                                                        (add + ddd))))(
                                                                                                                                                    st
                                                                                                                                                )
                                                                                                                                            }))
                                                                                                                        }
                                                                                                                    }(st)
                                                                                                                }
                                                                                                            }(st))
                                                                                                            bind
                                                                                                            (
                                                                                                                    ltlo(
                                                                                                                        b,
                                                                                                                        n
                                                                                                                    ))))
                                                                                                    bind
                                                                                                    (
                                                                                                            { st: State ->
                                                                                                                pause {
                                                                                                                    val bw1: Term<LogicList<LogicInt>> =
                                                                                                                        freshTypedVar();
                                                                                                                    val bw: Term<LogicList<LogicInt>> =
                                                                                                                        freshTypedVar();
                                                                                                                    val nw: Term<LogicList<LogicInt>> =
                                                                                                                        freshTypedVar();
                                                                                                                    val nw1: Term<LogicList<LogicInt>> =
                                                                                                                        freshTypedVar();
                                                                                                                    val ql1: Term<LogicList<LogicInt>> =
                                                                                                                        freshTypedVar();
                                                                                                                    val ql: Term<LogicList<LogicInt>> =
                                                                                                                        freshTypedVar();
                                                                                                                    val s: Term<LogicList<LogicInt>> =
                                                                                                                        freshTypedVar();
                                                                                                                    ((
                                                                                                                            ((
                                                                                                                                    ((
                                                                                                                                            ((
                                                                                                                                                    ((
                                                                                                                                                            ((
                                                                                                                                                                    ((
                                                                                                                                                                            ((
                                                                                                                                                                                    ((
                                                                                                                                                                                            exp2(
                                                                                                                                                                                                b,
                                                                                                                                                                                                zero,
                                                                                                                                                                                                bw1
                                                                                                                                                                                            )(st))
                                                                                                                                                                                            bind
                                                                                                                                                                                            (
                                                                                                                                                                                                    pluso(
                                                                                                                                                                                                        bw1,
                                                                                                                                                                                                        one,
                                                                                                                                                                                                        bw
                                                                                                                                                                                                    ))))
                                                                                                                                                                                    bind
                                                                                                                                                                                    (
                                                                                                                                                                                            ltlo(
                                                                                                                                                                                                q,
                                                                                                                                                                                                n
                                                                                                                                                                                            ))))
                                                                                                                                                                            bind
                                                                                                                                                                            (
                                                                                                                                                                                    { st: State ->
                                                                                                                                                                                        pause {
                                                                                                                                                                                            val q1: Term<LogicList<LogicInt>> =
                                                                                                                                                                                                freshTypedVar();
                                                                                                                                                                                            val bwq1: Term<LogicList<LogicInt>> =
                                                                                                                                                                                                freshTypedVar();
                                                                                                                                                                                            ((
                                                                                                                                                                                                    ((
                                                                                                                                                                                                            pluso(
                                                                                                                                                                                                                q,
                                                                                                                                                                                                                one,
                                                                                                                                                                                                                q1
                                                                                                                                                                                                            )(st))
                                                                                                                                                                                                            bind
                                                                                                                                                                                                            (
                                                                                                                                                                                                                    multo(
                                                                                                                                                                                                                        bw,
                                                                                                                                                                                                                        q1,
                                                                                                                                                                                                                        bwq1
                                                                                                                                                                                                                    ))))
                                                                                                                                                                                                    bind
                                                                                                                                                                                                    (
                                                                                                                                                                                                            lto(
                                                                                                                                                                                                                nw1,
                                                                                                                                                                                                                bwq1
                                                                                                                                                                                                            )))
                                                                                                                                                                                        }
                                                                                                                                                                                    })))
                                                                                                                                                                    bind
                                                                                                                                                                    (
                                                                                                                                                                            exp2(
                                                                                                                                                                                n,
                                                                                                                                                                                zero,
                                                                                                                                                                                nw1
                                                                                                                                                                            ))))
                                                                                                                                                            bind
                                                                                                                                                            (
                                                                                                                                                                    pluso(
                                                                                                                                                                        nw1,
                                                                                                                                                                        one,
                                                                                                                                                                        nw
                                                                                                                                                                    ))))
                                                                                                                                                    bind
                                                                                                                                                    (
                                                                                                                                                            divo(
                                                                                                                                                                nw,
                                                                                                                                                                bw,
                                                                                                                                                                ql1,
                                                                                                                                                                s
                                                                                                                                                            ))))
                                                                                                                                            bind
                                                                                                                                            (
                                                                                                                                                    pluso(
                                                                                                                                                        ql,
                                                                                                                                                        one,
                                                                                                                                                        ql1
                                                                                                                                                    ))))
                                                                                                                                    bind
                                                                                                                                    (
                                                                                                                                            lelo(
                                                                                                                                                ql,
                                                                                                                                                q
                                                                                                                                            ))))
                                                                                                                            bind
                                                                                                                            (
                                                                                                                                    { st: State ->
                                                                                                                                        pause {
                                                                                                                                            val bql: Term<LogicList<LogicInt>> =
                                                                                                                                                freshTypedVar();
                                                                                                                                            val qh: Term<LogicList<LogicInt>> =
                                                                                                                                                freshTypedVar();
                                                                                                                                            val s2: Term<LogicList<LogicInt>> =
                                                                                                                                                freshTypedVar();
                                                                                                                                            val qdh: Term<LogicList<LogicInt>> =
                                                                                                                                                freshTypedVar();
                                                                                                                                            val qd: Term<LogicList<LogicInt>> =
                                                                                                                                                freshTypedVar();
                                                                                                                                            ((
                                                                                                                                                    ((
                                                                                                                                                            ((
                                                                                                                                                                    ((
                                                                                                                                                                            ((
                                                                                                                                                                                    repeated_mul(
                                                                                                                                                                                        b,
                                                                                                                                                                                        ql,
                                                                                                                                                                                        bql
                                                                                                                                                                                    )(st))
                                                                                                                                                                                    bind
                                                                                                                                                                                    (
                                                                                                                                                                                            divo(
                                                                                                                                                                                                nw,
                                                                                                                                                                                                bw1,
                                                                                                                                                                                                qh,
                                                                                                                                                                                                s2
                                                                                                                                                                                            ))))
                                                                                                                                                                            bind
                                                                                                                                                                            (
                                                                                                                                                                                    pluso(
                                                                                                                                                                                        ql,
                                                                                                                                                                                        qdh,
                                                                                                                                                                                        qh
                                                                                                                                                                                    ))))
                                                                                                                                                                    bind
                                                                                                                                                                    (
                                                                                                                                                                            pluso(
                                                                                                                                                                                ql,
                                                                                                                                                                                qd,
                                                                                                                                                                                q
                                                                                                                                                                            ))))
                                                                                                                                                            bind
                                                                                                                                                            (
                                                                                                                                                                    leo(
                                                                                                                                                                        qd,
                                                                                                                                                                        qdh
                                                                                                                                                                    ))))
                                                                                                                                                    bind
                                                                                                                                                    (
                                                                                                                                                            { st: State ->
                                                                                                                                                                pause {
                                                                                                                                                                    val bqd: Term<LogicList<LogicInt>> =
                                                                                                                                                                        freshTypedVar();
                                                                                                                                                                    val bq1: Term<LogicList<LogicInt>> =
                                                                                                                                                                        freshTypedVar();
                                                                                                                                                                    val bq: Term<LogicList<LogicInt>> =
                                                                                                                                                                        freshTypedVar();
                                                                                                                                                                    ((
                                                                                                                                                                            ((
                                                                                                                                                                                    ((
                                                                                                                                                                                            ((
                                                                                                                                                                                                    repeated_mul(
                                                                                                                                                                                                        b,
                                                                                                                                                                                                        qd,
                                                                                                                                                                                                        bqd
                                                                                                                                                                                                    )(st))
                                                                                                                                                                                                    bind
                                                                                                                                                                                                    (
                                                                                                                                                                                                            multo(
                                                                                                                                                                                                                bql,
                                                                                                                                                                                                                bqd,
                                                                                                                                                                                                                bq
                                                                                                                                                                                                            ))))
                                                                                                                                                                                            bind
                                                                                                                                                                                            (
                                                                                                                                                                                                    multo(
                                                                                                                                                                                                        b,
                                                                                                                                                                                                        bq,
                                                                                                                                                                                                        bq1
                                                                                                                                                                                                    ))))
                                                                                                                                                                                    bind
                                                                                                                                                                                    (
                                                                                                                                                                                            pluso(
                                                                                                                                                                                                bq,
                                                                                                                                                                                                r,
                                                                                                                                                                                                n
                                                                                                                                                                                            ))))
                                                                                                                                                                            bind
                                                                                                                                                                            (
                                                                                                                                                                                    lto(
                                                                                                                                                                                        n,
                                                                                                                                                                                        bq1
                                                                                                                                                                                    )))
                                                                                                                                                                }
                                                                                                                                                            }))
                                                                                                                                        }
                                                                                                                                    }))
                                                                                                                }
                                                                                                            }))
                                                                                        }))
                                                                    }))
                                                        }))
                                            }))
                                }))
                    }))
        }
    }

fun expo(
    b: Term<LogicList<LogicInt>>, q: Term<LogicList<LogicInt>>,
    n: Term<LogicList<LogicInt>>
): Goal =
    logo(n, b, q, zero)
