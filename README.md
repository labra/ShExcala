[![Stories in Ready](https://badge.waffle.io/labra/ShExcala.png?label=ready&title=Ready)](https://waffle.io/labra/ShExcala)
# ShExcala - Shape Expressions in Scala

This repository contains an RDF Shape Expressions validator implemented in Scala. 

## Compiling from sources

### Pre-requisites

ShExcala has been implemented in [Scala](scala-lang.org) and uses [SBT](http://www.scala-sbt.org/) to build. 

The only pre-requisite to build ShExcala is to have SBT installed (version 0.13.5 or bigger)

To compile and run tests

```
$ sbt test
```

To run from sbt

```
$ sbt run <parameters>
```

To create a standalone binary jar

```
$ sbt universal:packageBin
```

It will create a file shexcala-<version-number>.zip file located at: `target/universal/shexcala-<version>.zip` 

That file contains two excutable files for both Linux (`shexcala`) and Windows (`shexcala.bat`) which can be invoked from the command line.

## Binaries 

Compiled binaries are available [here](http://labra.github.io/ShExcala/).

## Usage

ShExcala can be used as a command line tool or as a library. Given that it has been implemented in Scala, 
 it can be used as a Jar library from any JVM compatible language, like Java, Scala, Clojure, etc.
 
* See [this link](https://github.com/labra/ShExcala/wiki) on how to use ShExcala from the command line.
* In this link we explain how to use ShExcala from Java
* In the near future, we are planning to use [ScalaJs](http://www.scala-js.org/) to compile 
  ShExcala to Javascript. 

## Links

* [More info](http://labra.github.io/ShExcala/)
* [ShEx by example](http://www.w3.org/2014/Talks/1209-shex-egp)
* [Binaries](https://bintray.com/weso/weso-releases/shExcala/view)
* [Release Notes](https://github.com/labra/ShExcala/tree/master/notes)
* [![Build Status](https://travis-ci.org/labra/ShExcala.svg?branch=master)](https://travis-ci.org/labra/ShExcala)

## Author

[Jose Emilio Labra Gayo](http://www.di.uniovi.es/~labra), [WESO Research Group](http://www.weso.es)

## Acknowlegments

[Eric Prud'hommeaux](http://www.w3.org/People/Eric/), [W3c](http://www.w3c.org) 
