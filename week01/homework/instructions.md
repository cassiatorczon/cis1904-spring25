# Homework 0: Getting Started

The purpose of this assignment is to make sure you set up Haskell as soon as
possible. As a result, we may ask you to do things that we won't explain in
detail — just follow the steps and check that your programming environment
responds as expected. Please come to office hours or post on Ed if you run into
issues.

**Due Date**: Wednesday, Jan. 22 at 11:59 p.m.

## Installation

The Git instructions are a strong suggestion. The Haskell instructions are
_requirements_: we will expect you to use Stack and VSCode. If you encounter problems due to using a different setup, we cannot promise help with debugging it.

### Step 0: Git

(This step is closely adapted from CIS 5710's instructions.)

The in-class exercises and homework assignments will be distributed via GitHub.

1.  If this is your first time using git, you should find and follow a tutorial
    online to learn the basic concepts. In the process, you should install
    [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git), create
    a [github](https://github.com/join) account, and (optionally) setup an [SSH
    key](https://docs.github.com/en/authentication/connecting-to-github-with-ssh).

    The main commands you should understand are `clone`, `fetch`, `merge`, `add`, `commit`, `status`, `push`, `pull`.

2.  [Create](https://github.com/new) a new, **private** repo. Do **not** select
    any options under "repository template", "quick setup", or "initialize this repository
    with."

3.  Clone your repo:

    ```
    git clone git@github.com:YOURUSERNAME/YOURREPO.git
    cd YOURREPO
    ```
    You may get a warning about closing an empty repository. This is expected.

4.  Connect to our class repo:

    ```
    git remote add upstream https://github.com/cassiatorczon/cis1904-spring25.git
    git fetch upstream
    ```

5.  Merge the changes and push them back to GitHub:
    ```
    git merge upstream/main
    git push
    ```

6.  Whenever there are changes to the class repo, get those changes locally,
    too:

    ```
    git fetch upstream
    git merge upstream/main
    ```

    You will want to do this before each class and after each homework is
    released.

### Step 1: GHCup and Stack

(The remaining steps are closely adapted from CIS 5520's instructions.)

We will manage Haskell versions and packages using Stack.

1. Install [GHCup](https://www.haskell.org/ghcup/) by following the instructions
   for your machine on the homepage.

   **Make sure to choose "yes" when asked about installing the Haskell Language
   Server and better integration with Stack, and if you want the installation location to PATH.**

3. At this point, you should be able to run Haskell programs! (You may need to
   restart the terminal for the installation to take effect.)

   Within the `week01/homework` directory, run this command to start the
   [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop):

   ```
   stack ghci Exercises.hs
   ```

   Then, input `main` and press enter — this evaluates `main`. It should print
   out "You have installed Haskell!" There should also be an error message
   saying that a test failed; we will fix this in the last part.
   You can use :q to quit the REPL.

### Step 2: VSCode

We will use Visual Studio Code as our editor.

1.  Install [VSCode](https://code.visualstudio.com/) by following the
    instructions on the homepage.

2.  Within VSCode, install the extensions named `Haskell` and `haskell-linter`.

3.  Open the command palette (`Ctrl-Shift-P` or `Cmd-Shift-P`) and go to
    "Preferences: Open User Settings (JSON)." Paste in these settings:

    ```
    "editor.formatOnSave": true,
    "haskell.hlint.logLevel": "warn",
    "haskell.hlint.run": "onSave",
    "haskell.formattingProvider": "ormolu",
    "haskell.toolchain": {
        "hls": "2.9.0.1",
        "ghc": null,
        "cabal": null,
        "stack": null
    },
    "haskell.serverEnvironment": {
        "PATH": "${HOME}/.ghcup/bin:$PATH"
    },
    "haskell.manageHLS": "GHCup"
    ```

    If you have used VSCode before, you may have other settings already in this file. Do not overwrite these unless they are duplicates.
    If you have not used VSCode before, you may need to enclose the above settings in `{ }`.

4.  Open the `homework` folder for `week01` in VSCode.

    You must have this folder (`homework`) open — not a parent directory (such
    as `week01`) or an individual file (such as `Exercises.hs`). The top label
    in the file explorer should be "homework", the folder with a .cabal file in it.

    **Important**: Please closely read the above instruction!

    If you have the
    wrong folder open, you may not immediately notice that something is wrong,
    since some Haskell features will work regardless, but the most important
    ones (such as type checking) often will not. It is critically important that
    when you are doing a homework, you have precisely the `homework` folder
    open, and that when you are doing in-class exercises, you have precisely the
    `class` folder open — no more, no less.

5.  Go to `Exercises.hs` and complete Exercises 0, 1, and 2, which check that
    the compiler, autoformatter, and linter are working as expected,
    respectively.

6.  VSCode support for Haskell is excellent overall, but it can be finicky at
    times.

    If Exercise 0 is not working but you are not getting an error message from
    VSCode, this may mean the language server hasn't "woken up" yet. Click
    around a bit in `Exercises.hs` until `Processing` briefly appears in the
    blue bar at the bottom of the screen.

    Otherwise, problems often go away if you go to the command palette and run
    "Haskell: Restart Haskell LSP Server" or "Developer: Reload Window."

7. If Exercise 1 does not work, make sure that the installation location is
   in PATH. There are various ways to view your PATH variable depending on machine; on Linux or MacOS, you can run `echo $PATH` in your terminal; on Windows, you can run `echo %PATH%`, and some machines also have a GUI for environment variables.

7. If Exercise 2 does not work, try the following. Run

    ```
    stack install hlint
    ```

    and take note of the message at the end saying where `hlint` was installed.

    Then add that path to the user settings above.

    ```
    "haskell.hlint.executablePath": "<path-to-hlint>"
    ```

## Submission

Now that we have our basic setup, we will walk through the workflow of
completing exercises, running tests, and submitting to Gradescope.

1. Start GHCi with

   ```
   stack ghci Exercises.hs
   ```

   Run `main`. It should give the same message as before.

2. Go to `Exercises.hs` and complete Exercise 3.

3. In GHCi, input `:r` to reload the file. Run `main` again. The test should
   pass now.

4. Submit only `Exercises.hs` to Gradescope. It should pass the autograder test
   for Exercise 3.
