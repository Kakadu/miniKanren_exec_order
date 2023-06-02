plugins {
    id("java")
    kotlin("jvm") version "1.8.21"
}

repositories {
    mavenCentral()
    maven {
        url = uri("https://jitpack.io")
    }
}

dependencies {
    testImplementation(platform("org.junit:junit-bom:5.9.1"))
    testImplementation("org.junit.jupiter:junit-jupiter")
    // core
    testImplementation("com.github.UnitTestBot.klogic:klogic-core:0.1.5")
    // util terms
    testImplementation("com.github.UnitTestBot.klogic:klogic-utils:0.1.5")
}

tasks.test {
    useJUnitPlatform()
    testLogging {
        outputs.upToDateWhen {false}
        showStandardStreams = true
    }
}