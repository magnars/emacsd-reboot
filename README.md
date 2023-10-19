# .emacs.d

An Emacs configuration for Norwegian keyboard on OSX (lol), as seen on emacsrocks.com and parens-of-the-dead.com.

## Installation

Download [Emacs for Mac OSX](http://emacsformacosx.com/).

## Out of band dependencies

- Spell checking:

    ```
    brew install aspell --lang=en
    ```

- Static analysis for Clojure with LSP

    ```
    brew install clojure-lsp/brew/clojure-lsp-native
    ```

- Fast grepping:

    ```
    brew install ripgrep
    ```

- Make dired happy, install `gls` as replacement for `ls`:

    ```
    brew install xz coreutils
    ```

- Stop clojure-lsp from adding duplicate namespace declarations:

    Open `.config/clojure-lsp/config.edn` and add:

    ```
    {:auto-add-ns-to-new-files? false}
    ```

    This is already handled better by clj-refactor (which also inserts test
    declarations in relevant namespaces).
