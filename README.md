# cozo-hs

### A library with bindings and simple types to the C API of the embeddable Cozo database.

## Installation

This library relies on pkg-config for libcozo_c being available on your system. You will need to have `cozo_c.h` and `libcozo_c.so` installed somewhere your linker can find it and it must be registered with pkg-config. 

To do so, you can download both the header files as well as the library from the [Cozo project repository](https://github.com/cozodb/cozo) or you could clone that repository and build it from source. 

If there is no official way to install this library on your OS/Distro (Debian for example) then you can put the library and headers in common paths (/usr/local/lib, /usr/local/include respectively) then manually create a pkg-config file for them. Just make sure that the version you specify matches what you have installed!

Here is an example libcozo_c.pc:
```
Name: cozo_c
Description: C Interface with the Cozo embeddable database.
Version: 0.7.5
Cflags: -I/usr/local/include
Libs: -L/usr/local/lib -lcozo_c
```

After that, a simple `cabal build` can make sure that the C compiler is linking correctly.

After building, you may want to ensure that the runtime is linked correctly as well. Run `cabal repl` to try out some of the code or `cabal test` to run the test-suite.

Following all of this, you can include this library as a dependency in other Cabal projects the normal way.

## Notes on Usage

This library makes heavy use of aeson given that the C API is defined almost entirely in terms of JSON. Therefore, some primitive data types which match the structure of returned JSON are provided as part of this library.

If you desire to use your own serialization scheme rather than the types provided, the underlying bindings are exposed in the form of functions with a `'` suffix. All of these functions deal with strict `ByteStrings` that are marshalled from the strings produced by C.

All errors are handled in a pure Either.

A very simple program which opens an in-memory connection and runs a query is as follows:
```Haskell
{-#LANGUAGE OverloadedStrings#-}
import Database.Cozo
import Control.Exception (bracket, throwIO)

main :: IO ()
main = 
  bracket 
    (open "mem" "" "{}" >>= either throwIO pure)
    (fmap (const ()) . close)
    $ \c -> do
      er <- runQuery c "?[] <- [[1,2,3]]" KM.empty
      case er of
        Left e -> throwIO e
        Right (CozoResult badOkay) -> 
          case badOkay of
            Left bad -> print . cozoBadMessage $ bad
            Right okay -> print . namedRowsRows . cozoOkayNamedRows $ okay
```

This program will output:

```
[[Number 1.0,Number 2.0,Number 3.0]]
```