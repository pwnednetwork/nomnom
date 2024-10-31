= Install Guide

** Only for Doom Emacs for now **

Clone repo and make a soft link to ~/.doom/config/local/calc-cal/calc-cal.el

Modify Doom files:

== config.el

...
{
;; calc-cal
(use-package! calc-cal
  :mode ("\\.fud\\'" . calc-cal-fud-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.fud\\'" . calc-cal-fud-mode))
  :commands (calc-cal calc-cal-insert-date calc-cal-fud-mode))


(map! :leader
      :desc "Calculate calorie totals"
      "c t" #'calc-cal)

(map! :leader
      :desc "Insert current date"
      "c i" #'calc-cal-insert-date)
}
...

== packages.el

...
{

(package! calc-cal
  :recipe (:host nil :repo nil :local-repo "local/calc-cal"))
}
...

Then `doom sync` and restart Emacas or SPC h r r
