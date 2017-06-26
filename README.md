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

### Common Shortcuts
* `C-M-i` *counsel-irony auto-complete*
* `C-p-p` *projectile-switch-project*

## Python IDE
- `M-x jedi:install-server` Install the jedi server
