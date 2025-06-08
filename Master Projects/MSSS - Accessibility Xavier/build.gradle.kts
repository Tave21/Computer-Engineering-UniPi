// Top-level build file where you can add configuration options common to all sub-projects/modules.
plugins {
    alias(libs.plugins.android.application) apply false
    alias(libs.plugins.kotlin.android) apply false
    id("com.chaquo.python") version "16.1.0" apply false
}

buildscript {
    repositories {
        google()
        mavenCentral()
        maven {
            url = uri("https://seeso.jfrog.io/artifactory/visualcamp-eyedid-sdk-android-release")
        }

    }
    dependencies {
        classpath("com.android.tools.build:gradle:8.1.1") // o la tua versione corrente
    }
}