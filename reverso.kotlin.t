$ (cd klogic && ./gradlew :test --tests "ReversoTest.testReverso" | sed '/BUILD SUCCESSFUL in/d')

> Task :test

ReversoTest > testReverso() STANDARD_OUT
    appendo: (1, 2) _.0
    (1, 2) ()
    (1, 2) (_.1, _.2)
    appendo: _.2 _.3
    _.2 ()
    _.2 (_.4, _.5)
    appendo: _.5 _.6
    _.5 ()
    _.5 _.6
    appendo: _.6 (_.4) _.3
    _.6 ()
    (_.4) _.3
    appendo: _.3 (_.1) _.0
    _.5 (_.7, _.8)
    _.3 ()
    _.6 (_.7, _.8)
    _.3 (_.7, _.8)
    _.0 (_.7, _.9)
    appendo: _.8 (_.1) _.9
    _.8 ()
    (_.1) _.9
    unifications: 15
