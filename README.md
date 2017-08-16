# .emacs.d

Install [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus): `brew install emacs-plus --with-no-title-bars --without-spacemacs-icon`

---
## Language specific preliminaries

### C++
1. Installing **IRONY SERVER**:
```
CMAKE
-DLIBCLANG_INCLUDE_DIR\=/usr/local/opt/llvm/include/
-DLIBCLANG_LIBRARY\=/usr/local/opt/llvm/lib/libclang.dylib
-DCMAKE_INSTALL_PREFIX\=/Users/gs/.emacs.d/irony/
/Users/gs/.emacs.d/elpa/irony-20170523.618/server
&&
cmake --build . --use-stderr --config Release --target install
```

2. Install **CTAGS**
- Install *pygments*:
  `pip install pygments`
- Install *GNU Global* with *ctags* (Source code tagging system):
  `brew install global --with-ctags`

### Python
- Install virtualenv: `pip3 install virtualenv`
- `M-x jedi:install-server` Install the jedi server

### Javascript
1. Configure **TERN** by creating `.tern-config` file in your `$HOME` dir:

```json
{
  "libs": [
    "browser",
    "jquery"
  ],
  "loadEagerly": [
  ],
  "plugins": {
    "requirejs": {
      "baseURL": "./",
      "paths": {}
    }
  }
}
```

### Swift
1. Install [SourceKittenDaemon](https://github.com/terhechte/SourceKittenDaemon/releases/) first.

### Haskell
1. Install **Docker*: `brew install docker` to use **intero**.
2. Install **Stack*: `brew install stack`
3. Setup Stack via `stack setup`.

---

## Shortcuts
 -  `C-M-i`       -> counsel-irony auto-complete
 -  `C-p-p`       -> projectile-switch-project
 -  `C-q <tab>`   -> insert tab
 -  `C-x C--`     -> decrease font size
 -  `C-x C-+`     -> increase font size
 -  `F3`          -> start keyboard macro
 -  `F4`          -> define keyboard macro/insert macro
 -  `C-c e`       -> expand yasnippet
 -  `C-c <C-tab>` -> auto-complete yasnippet
