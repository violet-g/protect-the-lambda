# Protect The Lambda

The objective of this assignment is to implement a FP-inspired Tafl game.

Please consult the assignment specification for the game rules and submission's functional requirements.

The project build is managed using Cabal, which was introduced in Week 8 during lectures.

For more information for using Cabal please consult:

https://www.haskell.org/cabal/users-guide/index.html

Here we provide some quick instructions for how to build and test the project.

Please remember to update the Cabal file with your information, including
any extra dependencies your project requires.

## Project Initialisation

To initialise the project:

```sh
$ cabal configure --user
```

It is good practise to develop our projects in a sandbox. Fortunately, Cabal has this functionality built in.

We initialise the sandbox as follows:

```sh
$ cabal sandbox init
```

Then we install the project's specified dependencies into the sandbox:


```sh
$ cabal install --dependencies-only
```

If you add a dependency to your project, then please call this command again.

The sandbox can be destroyed using:

```sh
$ cabal sandbox delete
```


## Build the project

Once the project has been initialised we can use the following commands to interact with our project.

### Building

```sh
$ cabal build
```

This will generate an executable within a folder in `.cabal-sandbox/...`. The precise path is determined by your version of GHC, Computer Architecture, and Cabal used.

### Cleaning

```sh
$ cabal clean
```


### Testing

To run all the black-box tests, this is the sequence I go through ...

```sh
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal install --only-dependences --enable-tests
$ cabal build
$ cabal install
$ cabal test
```

### Install

If you wish to install the game into your system, you can use the command:

```sh
$ cabal install
```

This will install the executable into Cabal's `bin` directory.

On Linux/BSD machines this will be: `$HOME/.caba/bin/`

On Windows machines this will be `%APPDATA%\cabal/bin/`

## Git/Hg Ignore files

For those using Git, GitHub publishes a useful `.gitignore` file suitable for Haskell:

https://github.com/github/gitignore/blob/master/Haskell.gitignore

For those that use Mercurial, the same ignore file can be adapted for your Haskell project.
