# listr

Tiny tool for listing directory contents.

Usage
-----

Give a list of directories, and it will list all the files in them, non-recursively.

```sh
$ listr src/ app/
/path/here/listr/src/Lib.hs
/path/here/listr/app/Main.hs
/path/here/listr/stack.yaml
/path/here/listr/README.md
/path/here/listr/package.yaml
/path/here/listr/Setup.hs
/path/here/listr/listr.cabal
/path/here/listr/LICENSE
```

I often use `entr` to automaize build and test as I code. With `listr`, it's easy to specify which directories to watch.

```sh
listr src/tools/ src/parser | entr make build
```

Installation
------------

Dependencies:
- stack (comes with Haskell platform and many other distributions)

```sh
git clone https://github.com/hjorthjort/listr.git
cd listr
stack install
```

As usual, make sure that `~/.local/bin`, or wherever 'stack' installs packages for you, is in your `PATH` variable.
