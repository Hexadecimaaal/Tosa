import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

group = "io.github.hexadecimaaal"
version = "0.0.1"

repositories {
  mavenCentral()
  maven("https://jitpack.io")
}

plugins {
  kotlin("jvm") version "1.3.50"
  id("org.jetbrains.grammarkit") version "2019.2"
  antlr
}

dependencies {
  implementation(kotlin("stdlib-jdk8"))
  implementation("com.github.alexandrepiveteau.parser-combinators-kotlin:parser-combinators:1.1.0")
  implementation("com.github.alexandrepiveteau.parser-combinators-kotlin:parser-combinators-primitives:1.1.0")
  antlr("org.antlr:antlr4:4.5")
}

val compileKotlin : KotlinCompile by tasks
compileKotlin.kotlinOptions {
  jvmTarget = "1.8"
}

val compileTestKotlin : KotlinCompile by tasks
compileTestKotlin.kotlinOptions {
  jvmTarget = "1.8"
}
