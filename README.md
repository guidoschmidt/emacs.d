# .emacs.d

Install [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus): `brew install emacs-plus --with-no-title-bars --without-spacemacs-icon`

## C++ IDE
### irony
Installing irony server:
```
cmake
-DLIBCLANG_INCLUDE_DIR\=/usr/local/opt/llvm/include/
-DLIBCLANG_LIBRARY\=/usr/local/opt/llvm/lib/libclang.dylib
-DCMAKE_INSTALL_PREFIX\=/Users/gs/.emacs.d/irony/
/Users/gs/.emacs.d/elpa/irony-20170523.618/server
&&
cmake --build . --use-stderr --config Release --target install
```

##### ctags
- Install *pygments*:
  `pip install pygments`
- Install *GNU Global* with *ctags* (Source code tagging system):
  `brew install global --with-ctags`

## Python IDE
- `M-x jedi:install-server` Install the jedi server

## Javascript IDE
### tern
Create `.tern-config` file in your `$HOME` dir:

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

## Swift
Install [SourceKittenDaemon](https://github.com/terhechte/SourceKittenDaemon/releases/) first.

## Shortcuts
- `C-M-i`      counsel-irony auto-complete
- `C-p-p`      projectile-switch-project
- `C-q <tab>`  insert tab
- `C-x C--`    decrease font size
- `C-x C-+`    increase font size
- `F3`         start keyboard macro
- `F4`         define keyboard macro/insert macro
