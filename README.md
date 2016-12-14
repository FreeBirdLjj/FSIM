# FSIM

A simple websocket-based IM server written in fsharp.

## Design

```
          +---------------+
          | MessageCenter |
          +---------------+
                  ^
                  |
           +------------+           +------------------+
           | UserCenter | <-------> | RegisterSessions |
           +------------+           +------------------+
                  ^
                  |
          +---------------+
          | SessionCenter |
          +---------------+
          +       ^      +
         /        |       \
      |/_         v        _\|
+----------+ +----------+ +----------+
| Session1 | | Session2 | | Session3 | ...
+----------+ +----------+ +----------+
```

For more information, see [Design.md] (Design.md).

## Pre-requirements

-   `mono` installed with `fsharp` support

## How to build

```shell
$ mono .paket/paket.bootstrapper.exe
$ mono .paket/paket.exe install
$ fsharpi build.fsx
```

## How to run

```shell
$ mono build/main.exe
```

