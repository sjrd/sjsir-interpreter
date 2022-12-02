# Scala.js IR Interpreter

`sjsir-interpreter` is an interpreter for `.sjsir` files, i.e., the IR of [Scala.js](https://www.scala-js.org/).

Normally, `.sjsir` files are an intermediate product in the Scala.js compilation pipeline.
They are processed by the *linker* to produce one or more `.js` files, which can be run by a JS engine.

In contrast, `sjsir-interpreter` directly reads and executes `.sjsir` files.
This is (much) slower, but it allows to dynamically load more `.sjsir` files.

## Usage

The Scala.js IR Interpreter comes as a Scala.js library exposing a very small surface API.

Depend on it with

```scala
libraryDependencies += "be.doeraene" %%% "sjsir-interpreter" % "0.3.0"
```

This library requires ECMAScript 2016+.
Configure it in your settings with

```scala
import org.scalajs.linker.interface.ESVersion
scalaJSLinkerConfig ~= { _.withESFeatures(_.withESVersion(ESVersion.ES2016)) },
```

Then use an instance of `org.scalajs.sjsirinterpreter.core.Interpreter`.

## Local tests

### Unit tests

Run the core API tests of `Interpreter`:

```
> sjsir-interpreter/test
```

Run the entire Scala.js test suite, interpreted by the `sjsir-interpreter`:

```
> scalajs-test-suite/test
```

### Demos

The `sample` project is configured to be run with the interpreter.
Compile and run it through the interpreter with

```
> sample/run
```

The `sjsir-interpreter-browser` project provides an in-browser demo of the reversi, interpreted.
Build it with

```
> sjsir-interpreter-browser/fastOptJS
```

then start a local HTTP server from within the `stage/` directory, for example with `npx http-server`:

```
$ cd stage/
$ npx http-server
```

and open the displayed local URL in a browser.

## License

This project is distributed under the Apache 2.0 License.

## Credits

This project was prototyped by [@youroff](https://github.com/youroff) in the context of a master project at EPFL, Switzerland.
