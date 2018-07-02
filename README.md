# .emacs.d

## Emacs
Install [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus):

```
brew tap d12frosted/emacs-plus
brew install emacs-plus --without-spacemacs-icon --with-24bit-color --with-natural-title-bar
```

## Additional Setup
### Code Navigation
1. Install [**GNU GLOBAL**](https://www.gnu.org/software/global/) with [**CTAGS**](https://github.com/universal-ctags/ctags): `brew install global --with-ctags`
2. Install *pygments*: `pip install pygments`



### Time Tracking
1. Install [Wakatime](https://wakatime.com/) `pip3 install wakatime`



### C++ Code-Completion ([Irony](https://github.com/Sarcasm/irony-mode) + RTags)
1. Install **LLVM**: `brew install llvm`
2. Install **Irony** from within Emacs: `M-x irony-install-server`

**You may need to change the command to**:
```
CMAKE
-DLIBCLANG_INCLUDE_DIR\=/usr/local/opt/llvm/include/
-DLIBCLANG_LIBRARY\=/usr/local/opt/llvm/lib/libclang.dylib
-DCMAKE_INSTALL_PREFIX\=/Users/gs/.emacs.d/irony/
/Users/gs/.emacs.d/elpa/irony-20170523.618/server
&&
cmake --build . --use-stderr --config Release --target install
```

1. Install rtags: `M-x rtags-install`
2. `cd ~/.emacs.d/elpa/rtags-20180619.823/rtags-2.18`
3. `make install`

Before using RTags, start the rtags daemon with `rdm &`. You then need to start
the RTags client (`rc`) from within your project root:
`make -nk | rc -c -`

Or provide `compile_commands.json` and start with `rc -J`.
You can use [**xcpretty**](https://github.com/supermarin/xcpretty) on macOS or
[**bear**](https://github.com/rizsotto/Bear) on other plattforms:
`xcodebuild | xcpretty -r json-compilation-database -o compile_commands.json`



### C++ Code-Completion ([YouCompleteMe](https://github.com/Valloric/YouCompleteMe))
1. Clone the YouCompleteMe reop as a submodule: `git submodule update --init --recursive` to clone `ycmd` repo into `/ycmd` dir.
2. Build YouCompleteMe: `cd ~/.emacs.d/ycmd` & `./build.py --clang-completer`
```
brew install --HEAD universal-ctags/universal-ctags/universal-ctags
```



### Python Code-Completion ([Jedi](https://github.com/tkf/emacs-jedi))
- Install virtualenv: `pip3 install virtualenv`
- Install **Jedi** from within Emacs: `M-x jedi:install-server` Install the jedi server



### Javascript Code-Completion ([Tern](https://github.com/ternjs/tern))
1. Configure **Tern** (copy `configs/external/.tern-config` file into your `$HOME` dir):



### Swift Code-Completion ([SourceKittenDaemon](https://github.com/terhechte/SourceKittenDaemon/releases/))
1. Install **SourceKittenDaemon** :cat:



### Haskell Code-Completion ([Intero](https://github.com/commercialhaskell/intero))
1. Install **Docker*: `brew install docker` to use **intero**
2. Install **Stack*: `brew install stack`
3. Setup Stack via `stack setup`

Make sure to use `ghc` version larger than `8.2.1`. Intero seems to fail an `8.0.2`
according to this [github issue](https://github.com/commercialhaskell/intero/issues/428)



