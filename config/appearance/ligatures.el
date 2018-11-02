;;; ligatures --- Configure font ligatures -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(defun setup-pragmata-ligatures ()
  "Setup ligatures for Pragmata Pro Liga."
  (setq prettify-symbols-alist
        (append prettify-symbols-alist
                '(
                  ;; ("!!"   . ?)
                  ;; ("!="   . ?)
                  ;; ("!=="  . ?)
                  ;; ("!≡"   . ?)
                  ;; ("!≡≡"  . ?)
                  ;; ("!>"   . ?)
                  ;; ("#("   . ?)
                  ;; ("#_"   . ?)
                  ;; ("#{"   . ?)
                  ;; ("#?"   . ?)
                  ;; ("#>"   . ?)
                  ;; ("%="   . ?)
                  ;; ("%>"   . ?)
                  ;; ("<~"   . ?)
                  ;; ("&%"   . ?)
                  ;; ("&&"   . ?)
                  ;; ("&*"   . ?)
                  ;; ("&+"   . ?)
                  ;; ("&-"   . ?)
                  ;; ("&/"   . ?)
                  ;; ("&="   . ?)
                  ;; ("&&&"  . ?)
                  ;; ("&>"   . ?)
                  ;; ("$>"   . ?)
                  ;; ("~>"   . ?)
                  ;; ;; ("***"  . ?) ; I prefer not to use this one
                  ;; ("*="   . ?)
                  ;; ("*/"   . ?)
                  ;; ("*>"   . ?)
                  ;; ("++"   . ?)
                  ;; ("+++"  . ?)
                  ;; ("+="   . ?)
                  ;; ("+>"   . ?)
                  ;; ("--"   . ?)
                  ;; ("-<"   . ?)
                  ;; ("-<<"  . ?)
                  ;; ("-="   . ?)
                  ;; ("->>"  . ?)
                  ;; ("---"  . ?)
                  ;; ("-->"  . ?)
                  ;; (".."   . ?)
                  ;; ("..."  . ?)
                  ;; ("..<"  . ?)
                  ;; (".>"   . ?)
                  ;; (".~"   . ?)
                  ;; (".="   . ?)
                  ;; ("/*"   . ?)
                  ;; ("//"   . ?)
                  ;; ("/>"   . ?)
                  ;; ("/="   . ?)
                  ;; ("/=="  . ?)
                  ;; ("///"  . ?)
                  ;; ("/**"  . ?)
                  ;; ;; ("::"   . ?)
                  ;; ;; (":="   . ?)
                  ;; ;; (":≡"   . ?)
                  ;; ;; (":>"   . ?)
                  ;; ;; (":=>"  . ?)
                  ;; ;; ("<$>"  . ?)
                  ;; ;; ("<*"   . ?)
                  ;; ;; ("<*>"  . ?)
                  ;; ;; ("<+>"  . ?)
                  ;; ;; ("<-"   . ?) ; I like different arrows (see below)
                  ;; ;; ("<<"   . ?)
                  ;; ;; ("<<<"  . ?)
                  ;; ;; ("<<="  . ?)
                  ;; ;; ("<="   . ?)
                  ;; ;; ("<=>"  . ?) ; I like different arrows (see below)
                  ;; ("<>"   . ?)
                  ;; ("<|>"  . ?)
                  ;; ("<<-"  . ?)
                  ;; ("<|"   . ?)
                  ;; ("<=<"  . ?)
                  ;; ("<~~"  . ?)
                  ;; ("<<~"  . ?)
                  ;; ("<$"   . ?)
                  ;; ("<+"   . ?)
                  ;; ("<!>"  . ?)
                  ;; ("<@>"  . ?)
                  ;; ("<#>"  . ?)
                  ;; ("<%>"  . ?)
                  ;; ("<^>"  . ?)
                  ;; ("<&>"  . ?)
                  ;; ("<?>"  . ?)
                  ;; ("<.>"  . ?)
                  ;; ("</>"  . ?)
                  ;; ("<\>"  . ?)
                  ;; ("<\">" . ?)
                  ;; ("<:>"  . ?)
                  ;; ("<~>"  . ?)
                  ;; ("<**>" . ?)
                  ;; ("<<^"  . ?)
                  ;; ("<!"   . ?)
                  ;; ("<@"   . ?)
                  ;; ("<#"   . ?)
                  ;; ("<%"   . ?)
                  ;; ("<^"   . ?)
                  ;; ("<&"   . ?)
                  ;; ("<?"   . ?)
                  ;; ("<."   . ?)
                  ;; ("</"   . ?)
                  ;; ("<\\"  . ?)
                  ;; ("<\""  . ?)
                  ;; ("<:"   . ?)
                  ;; ("<->"  . ?)
                  ;; ("<!--" . ?)
                  ;; ("<--"  . ?)
                  ;; ("=<<"  . ?)
                  ;; ("=="   . ?)
                  ;; ("==="  . ?)
                  ;; ;; ("==>"  . ?) ; I like different arrows (see below)
                  ;; ;; ("=>"   . ?)  ; I like different arrows (see below)
                  ;; ("=~"   . ?)
                  ;; ("=>>"  . ?)
                  ;; ("≡≡"   . ?)
                  ;; ("≡≡≡"  . ?)
                  ;; ("≡:≡"  . ?)
                  ;; (">-"   . ?)
                  ;; (">="   . ?)
                  ;; (">>"   . ?)
                  ;; (">>-"  . ?)
                  ;; (">>="  . ?)
                  ;; (">>>"  . ?)
                  ;; (">=>"  . ?)
                  ;; (">>^"  . ?)
                  ;; ("??"   . ?)
                  ;; ("?~"   . ?)
                  ;; ("?="   . ?)
                  ;; ("?>"   . ?)
                  ;; ("^="   . ?)
                  ;; ("^."   . ?)
                  ;; ("^?"   . ?)
                  ;; ("^.."  . ?)
                  ;; ("^<<"  . ?)
                  ;; ("^>>"  . ?)
                  ;; ("^>"   . ?)
                  ;; ("\\\\" . ?)
                  ;; ("\\>"  . ?)
                  ;; ("@>"   . ?)
                  ;; ("|="   . ?)
                  ;; ("||"   . ?)
                  ;; ("|>"   . ?)
                  ;; ("|||"  . ?)
                  ;; ("|+|"  . ?)
                  ;; ("~="   . ?)
                  ;; ("~~>"  . ?)
                  ;; ("~>>"  . ?)

                  ;; Personal preference: I like this set of arrows better than default
                  ("<-"   . ?🡐)
                  ("->"   . ?🡒)
                  ("=>"   . ?⇒)
                  ("<=>"  . ?⟺)
                  ("<==>" . ?⟺)
                  ("==>"  . ?⟹)
                  ("<=="  . ?⟸)
                  ("|->"  . ?⟼)
                  ("<-|"  . ?⟻)
                  ("|=>"  . ?⟾)
                  ("<=|"  . ?⟽)))))

(defun refresh-pretty ()
  "Disable and re-enable prettify-symbols mode."
  (interactive)
  (prettify-symbols-mode -1)
  (prettify-symbols-mode +1))

;; Hooks for modes in which to install the Pragmata ligatures
(mapc (lambda (hook)
        (add-hook hook (lambda ()
                         (setup-pragmata-ligatures)
                         (refresh-pretty))))
      '(text-mode-hook
        prog-mode-hook))

(global-prettify-symbols-mode +1)

(provide 'ligatures)
;;; ligatures ends here
