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

    or get the nightly:

    ```
    bash <(curl https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install) --version nightly --dir ~/bin
    ```

    (place somewhere on your path before homebrew)


- Fast grepping:

    ```
    brew install ripgrep
    ```

- Make dired happy, install `gls` as replacement for `ls`:

    ```
    brew install xz coreutils
    ```

- Stop clojure-lsp from adding duplicate namespace declarations:

    Open `~/.config/clojure-lsp/config.edn` and add:

    ```
    {:auto-add-ns-to-new-files? false}
    ```

    This is already handled better by clj-refactor (which also inserts test
    declarations in relevant namespaces).

- Add dependencies to the project without CIDER running:

    ```
    brew install babashka/brew/neil
    ```

- Static analysis for CSS with LSP

    ```
    npm install -g vscode-langservers-extracted
    ```

- On a Mac you might want to do this, to disable `C-M-d` in your OS, making it
  available for `paredit-forward-down`:

    ```
    defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'
    ```

- If you want square emojis for that monospaced goodness, install the Noto Color
  Squaremoji font from [Sardtok/noto-squaremoji](https://github.com/Sardtok/noto-squaremoji)

## Tips for using these emacs settings

If you want to use my settings straight out of the box, here are some things to note:

 * This is my personal emacs configuration. I am constantly tuning it to my
   preferences. You should consider doing the same. Maybe start with a blank emacs +
   [Technomancy's better-defaults package](https://git.sr.ht/~technomancy/better-defaults),
   and then dig through this repo for useful nuggets, instead of forking it directly.

 * The key bindings are optimized for a norwegian keyboard layout.

 * You quit emacs with `C-x r q`, mnemonic *Really Quit*.

 * Find file in project with `C-x p f`, in dir with `C-x C-f`, recent with `C-x f`

 * Switch to a project with `C-x p p`

 * Add your user- and project-specific stuff in .emacs.d/users/[machine name]/*.el

 * `C-h` is rebound to backspace, like in the shell. Get help on `F1` instead.

 * Autocomplete with `C-,` or `C-.`, try both to get a feel for them. (autocomplete entire lines with `C-:`)

 * expand-region is your friend. Find its bound key by doing `F1 f er/expand-region`

 * Undo with `C-_` and redo with `M-_`. Watch the undo-tree with `C-x u`

 * Indent and clean up white space in the entire buffer with `C-c n`

 * On a mac, the Meta key `M` is bound to Command.

 * I recommend rebinding Caps Lock to Ctrl and use that instead of the often badly placed Ctrl-key.

 * Watch [emacsrocks.com](http://emacsrocks.com)

## Survival guide for the first week of emacs

When you start using emacs for the first time, your habits fight you every inch
of the way. Your fingers long for the good old familiar keybindings. Here's an
overview of the most commonly used shortcuts to get you through this pain:

* `C      ` Shorthand for the ctrl-key
* `M      ` Shorthand for the meta-key (bound to command on my mac settings)
* `S      ` Shorthand for the shift-key
* `s      ` Shorthand for the super-key (bount to option on my mac settings)

### Files

* `C-x C-f` Open a file. Starts in the current directory
* `C-x f  ` Open a recently visited file
* `C-x p f` Open a file in the current project (based on .git ++)
* `C-x C-s` Save this file
* `C-x C-w` Save as ...
* `C-x C-j` Jump to this files' current directory
* `C-x b  ` Switch to another open file (buffer)
* `C-x C-b` List all open files (buffers)

### Cut copy and paste

* `C-space` Start marking stuff. C-g to cancel.
* `C-w    ` Cut (aka kill)
* `C-k    ` Cut till end of line
* `M-w    ` Copy
* `C-y    ` Paste (aka yank)
* `M-y    ` Cycle last paste through previous kills
* `C-x C-y` Choose what to paste from previous kills
* `C-@    ` Mark stuff quickly. Press multiple times

### General

* `C-g    ` Quit out of whatever mess you've gotten yourself into
* `M-x    ` Run a command by name
* `C-.    ` Autocomplete
* `C-_    ` Undo
* `M-_    ` Redo
* `C-x u  ` Show the undo-tree
* `C-x m  ` Open magit. It's a magical git interface for emacs

### Navigation

* `C-arrow` Move past words/paragraphs
* `C-a    ` Go to start of line
* `C-e    ` Go to end of line
* `M-g M-g` Go to line number
* `C-x C-i` Go to symbol
* `C-s    ` Search forward. Press `C-s` again to go further.
* `C-r    ` Search backward. Press `C-r` again to go further.

### Window management

* `C-x 0  ` Close this window
* `C-x 1  ` Close other windows
* `C-x 2  ` Split window horizontally
* `C-x 3  ` Split window vertically
* `S-arrow` Jump to window to the left/right/up/down

### Help

* `F1 t   ` Basic tutorial
* `F1 k   ` Help for a keybinding
* `F1 r   ` Emacs' extensive documentation
