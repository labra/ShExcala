# ShExcala

RDF Shape Expressions in Scala

This repository contains an RDF Shape Expressions validator implemented in Scala

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






## Usage

## Links

* [More info](http://labra.github.io/ShExcala/)
* [Binaries](https://bintray.com/weso/weso-releases/shExcala/view)
* [![Build Status](https://travis-ci.org/labra/ShExcala.svg?branch=master)](https://travis-ci.org/labra/ShExcala)

## Author

[Jose Emilio Labra Gayo](http://www.di.uniovi.es/~labra), [WESO Research Group](http://www.weso.es)

