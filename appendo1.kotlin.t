  $ (cd klogic && ./gradlew :test --tests "AppendoTest.testAppendo1" | sed '/BUILD SUCCESSFUL in/d')
  > Task :compileKotlin NO-SOURCE
  > Task :compileJava NO-SOURCE
  > Task :processResources NO-SOURCE
  > Task :classes UP-TO-DATE
  > Task :processTestResources NO-SOURCE
  > Task :compileTestKotlin
  > Task :compileTestJava NO-SOURCE
  > Task :testClasses UP-TO-DATE
  
  > Task :test
 
 AppendoTest > testAppendo1() STANDARD_OUT
    appendo: (0) (1) _.0
    (0) ()
    (0) (_.1, _.2)
    _.0 (_.1, _.3)
    appendo: _.2 (1) _.3
    _.2 ()
    (1) _.3
    unifications: 5
  
  2 actionable tasks: 2 executed
