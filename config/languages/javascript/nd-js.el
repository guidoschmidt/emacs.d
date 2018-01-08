;;; nd-js.el --- To generate NaturalDocs style docstrings for
;;; javascript
;;
;; Author: Vineet Naik <naikvin@gmail.com>
;; Created: 31st March 2013
;; License: No license. Consider this as public domain.
;;
;; What?
;; =====
;;
;; This mode provides functionality to generate NaturalDocs style
;; doctrings for javascript function and variable definition at point.
;;
;; Installation and Usage
;; ======================
;;
;; Add the following lines to your emacs init file,
;;
;;   (add-to-list 'load-path "/path/to/this/dir")
;;   (load "nd-js")
;;   (add-hook 'js-mode-hook
;;             (lambda () (local-set-key (kbd "C-c d") #'nd-js-doc)))
;;
;; The above lines will bind the function to the key `C-c d`. In your
;; javascript code, move point to definition of a function or variable
;; and use `C-c d`.

;; Code starts here

(defun nd-js-doc ()
  "Interactive function to generate NaturalDocs style
documentation for javascript function or variable definition at
point.

When in js-mode, it is bound to C-c d"
  (interactive)
  (when (or (string= major-mode "js-mode")
            (string= major-mode "js2-mode"))
    (let ((jsdef (nd-js-match-def (current-line-string))))
      (if jsdef
          (progn
            (previous-line)
            (newline-and-indent)
            (let ((type (car jsdef))
                  (name (cadr jsdef))
                  (args (cadr (cdr jsdef))))
              (insert "/**")
              (insert (nd-js-commentize (nd-js-doc-topic type name) t))
              (insert (nd-js-commentize (format "<describe %s>" name) t))
              (when args
                (insert (nd-js-commentize "Parameters:" t))
                (dolist (a args)
                  (insert (nd-js-commentize (format "  (<type>) %s - <describe %s>" a a))))
                (insert (nd-js-commentize "")))
              (when (string= type "function")
                (insert (nd-js-commentize "Returns:" t))
                (insert (nd-js-commentize "  <describe return type>" t)))
              (insert "\n*/")
              (nd-js-indent-comment-region)))
        (message "Not a valid javascript definition")))))


(defun nd-js-commentize (text &optional appendblank)
  "Format a line of string to be added inside js comment (javadoc
style)

The optional appendblank is passed as t, it appends a blank line
below the string"
  (if appendblank
      (format "\n * %s\n *" text)
    (format "\n * %s" text)))


(defun nd-js-indent-comment-region ()
  "Select js comment region and indent it"
  (search-backward "/**")
  (let ((beg (point-at-bol)))
    (search-forward "*/")
    (indent-region beg (point-at-eol))))


(defun nd-js-doc-topic (type name)
  "Return a string formatted as per NaturalDoc topic using the
type and name"
  (format "%s: %s" (capitalize type) name))


(defun current-line-string ()
  "Return current line as string"
  (buffer-substring (point-at-bol) (point-at-eol)))


(defun nd-js-func-args (argstr)
  "Return a list of args by parsing comma separated string
  representing function args in js"
  (if (string= argstr "")
      nil
    (mapcar (lambda (str)
              (replace-regexp-in-string
               "\\`[ \t\n]*" ""
               str))
            (split-string  argstr ","))))


(defun nd-js-match-def (string)
  "Match string to find the type, name and args from a string
representing definition of a variable or a function in js. Return
nil if doesn't match.

The returned value will be a list as (type name args)
eg. (\"function\" \"helloworld\" '(\"x\" \"y\")) or nil if its
not a valid definition in js"
  (cond ((string-match "\s*var\s?\\([a-zA-Z0-9_]+\\)\s?=\s?function\s?(\\(.*\\))\s?{" string)
         (list "function"
               (match-string 1 string)
               (nd-js-func-args (match-string 2 string))))
        ((string-match "\s*\\([a-zA-Z0-9_]+\\.[a-zA-Z0-9_.]+\\)\s?=\s?function\s?(\\(.*\\))\s?{" string)
         (list "function"
               (match-string 1 string)
               (nd-js-func-args (match-string 2 string))))
        ((string-match "\s*function\s?\\([a-zA-Z0-9_]+\\)\s?(\\(.*\\))\s?{" string)
         (list "function"
               (match-string 1 string)
               (nd-js-func-args (match-string 2 string))))
        ((string-match "\\([a-zA-Z0-9_]+\\):\s?function\s?(\\(.*\\))\s?{" string)
         (list "function"
               (match-string 1 string)
               (nd-js-func-args (match-string 2 string))))
        ((string-match "\s*var\s?\\([a-zA-Z0-9_]+\\)\s?" string)
         (list "variable" (match-string 1 string) nil))
        ((string-match "\s*\\([a-zA-Z0-9_]+\\.[a-zA-Z0-9_.]+\\)\s?=" string)
         (list "variable" (match-string 1 string) nil))
        ((string-match "\\([a-zA-Z0-9_]+\\)\s?:\s?.*,?" string)
         (list "variable" (match-string 1 string) nil))))


(defun nd-js-tests ()
  "Unit Testing the functions"
  ;; tests for nd-js-func-args
  (assert (equal (nd-js-func-args "x, y") '("x" "y")))
  (assert (equal (nd-js-func-args "") nil))

  ;; tests for nd-js-match-def
  (assert (equal (nd-js-match-def "var helloworld = 42;")
                 '("variable" "helloworld" nil)))
  (assert (equal (nd-js-match-def "var hello_world = function (x, y) {")
                 '("function" "hello_world" ("x" "y"))))
  (assert (equal (nd-js-match-def "function hw901_88 () {")
                 '("function" "hw901_88" nil)))
  (assert (equal (nd-js-match-def "helloWorld: 42")
                 '("variable" "helloWorld" nil)))
  (assert (equal (nd-js-match-def "helloworld: function (x, y) {")
                 '("function" "helloworld" ("x" "y"))))
  (assert (equal (nd-js-match-def "Greeting.helloworld = function (x, y) {")
                 '("function" "Greeting.helloworld" ("x" "y"))))
  (assert (equal (nd-js-match-def "Greeting.answer = 42")
                 '("variable" "Greeting.answer" nil)))

  (assert (equal (nd-js-match-def "A.B.C = 42")
                 '("variable" "A.B.C" nil)))

  ;; tests with/without whitespace and trailing semi-colon
  (assert (equal (nd-js-match-def "var helloworld=42;")
                 '("variable" "helloworld" nil)))
  (assert (equal (nd-js-match-def "var hello_world = function(x,y){")
                 '("function" "hello_world" ("x" "y"))))
  (assert (equal (nd-js-match-def "function hw901_88(){")
                 '("function" "hw901_88" nil)))
  (assert (equal (nd-js-match-def "helloWorld : 42,")
                 '("variable" "helloWorld" nil)))
  (assert (equal (nd-js-match-def "helloworld:function (x,y) { ")
                 '("function" "helloworld" ("x" "y"))))
  (assert (equal (nd-js-match-def "    var helloworld=42;")
                 '("variable" "helloworld" nil)))
  (assert (equal (nd-js-match-def "    function hw901_88(){")
                 '("function" "hw901_88" nil)))
  (assert (equal (nd-js-match-def "    Greeting.helloworld = function (x, y) {")
                 '("function" "Greeting.helloworld" ("x" "y"))))
  (assert (equal (nd-js-match-def "    Greeting.answer = 42")
                 '("variable" "Greeting.answer" nil)))

  ;; tests for non-relevant lines (should return nil)
  (assert (equal (nd-js-match-def "x = y + 1;") nil))

  (assert (string= (nd-js-doc-topic "variable" "hello") "Variable: hello"))
  (assert (string= (nd-js-doc-topic "function" "doThis") "Function: doThis"))
  )

;; (nd-js-tests)

;; nd-js.el ends here
