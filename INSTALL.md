# Install Guide

## Only for Doom Emacs for now

Step 1. Clone repo and make a soft link to ~/.config/doom//local/nomnom/nomnom.el

``` sh
cd ~/
mkdir -p git
mkdir -p ~/.config/doom/local/nomnom/
cd git
git clone https://github.com/pwnednetwork/nomnom.git
ln -s ~/git/nomnom/nomnom.el ~/.config/doom/local/nomnom/nomnom.el

```

Step 2. **Modify Doom files:**

``` emacs-lisp
;; nomnom
(use-package! nomnom
  :mode (("\\.nom\\'" . nomnom-mode)
         ("\\.fud\\'" . nomnom-mode))
  :init
  (add-to-list 'auto-mode-alist '("\\.nom\\'" . nomnom-mode))
  (add-to-list 'auto-mode-alist '("\\.fud\\'" . nomnom-mode))
  :commands (nomnom-calculate nomnom-insert-date nomnom-mode))

(map! :leader
      :desc "Calculate calorie totals"
      "c t" #'nomnom)

(map! :leader
      :desc "Insert current date"
      "c i" #'nomnom-insert-date)

```

File: **packages.el**

``` emacs-lisp
(package! nomnom
  :recipe (:host nil :repo nil :local-repo "local/nomnom"))

```

Step 3. **doom sync** and restart Emacs or **SPC h r r**
