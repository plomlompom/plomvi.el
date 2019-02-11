;;; plomvi.el --- poor man's vim emulation

;; Copyright (C) 2019

;; Author: Christian Heller <plom+plomvi@plomlompom.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A very imperfect simulation of a small subset of Vim behavior, far
;; from the sophistication of packages like evil; an intentionally
;; thin wrapper around native Emacs behavior instead. To the author,
;; it mostly serves to both avoid muscle memory context switches
;; between Emacs and most basic Vim usages, and at the same time avoid
;; the conceptual distances evil puts between the user and native
;; Emacs logic. It does not care though to keep default Emacs
;; keybindings intact, overwriting some common ones with those from
;; the Vim universe.

;;; Instructions:

;; 1. load this script into your init file: (load ...)

;; 2. to start plomvi by default, put this into your init file:
;; (plomvi-global-mode 1)

;; 3. define some otherwise unused keybinding to simulate what would
;; be a jump back from Insert mode to Normal mode in Vim (but is de
;; facto just a plomvi mode activation), such as this:
;; (global-set-key (kbd "<f1>") 'plomvi-activate)



(defun plomvi-half-scroll()
  "Scroll down half a screen width."
  (interactive)
  (scroll-up (/ (window-height) 2)))

(defun plomvi-goto-line (count)
  "Jump to line: on nil count, last one, else count."
  (interactive "P")
  (if (null count)
      (goto-char (point-max))
    (goto-char (point-min))
    (forward-line (1- count))))

(defun plomvi-prefix-zero-or-line-start (prev-prefix)
  "If no prefix so far, jump to start of line, else start new prefix with 0."
  (interactive "P")
  (if (null prev-prefix)
      (beginning-of-line)
    (setq prefix-arg 0)))

(defun plomvi-prompt (prompt-input)
  "Provide super-basic : prompt that only accepts:
q
q!
w
w FILENAME
wq
vsplit
split

If search and replace syntax is detected, it recommends using `query-replace'
instead.
"
  (interactive "M:")
  (cond
   ((string= prompt-input "q!")
    (kill-emacs))
   ((string= prompt-input "q")
    (save-buffers-kill-emacs))
   ((string= prompt-input "w")
    (save-buffer))
   ((string-match "^w [^ ]+" prompt-input)
    (let ((file-name (substring prompt-input 2))) (write-file file-name)))
   ((string-match "^%?s/" prompt-input)
    (message "NOT IMPLEMENTED, consider using query-replace(-regexp)"))
   ((string= prompt-input "wq")
    ((lambda () (save-some-buffers t) (kill-emacs))))
   ((string= prompt-input "vsplit")
    (split-window-horizontally))
   ((string= prompt-input "split")
    (split-window-vertically))
   (t (message "NOT IMPLEMENTED"))))

(defun plomvi-newline-above ()
  "Open and jump into new line above current line, deactivate `plomvi-mode'."
  (interactive)
  (beginning-of-line)
  (insert "\n")
  (previous-line)
  (plomvi-deactivate))

(defun plomvi-newline-below ()
  "Open and jump into new line below current line, deactivate `plomvi-mode'."
  (interactive)
  (end-of-line)
  (insert "\n")
  (plomvi-deactivate))

(defun plomvi-paste-backward ()
  "Paste last kill leftwards in current line, or (if kill ends in \n) above it.

Note that this ignores killed rectangles.
"
  (interactive)
  (if (eq nil (string-match "\n$" (current-kill 0)))
      (yank)
    (beginning-of-line)
    (yank)
    (previous-line)))

(defun plomvi-paste-forward ()
  "Paste last kill rightwards in current line, or (if kill ends in \n) under it.

Note that this ignores killed rectangles."
  (interactive)
  (if (eq nil (string-match "\n$" (current-kill 0)))
      (progn
        (right-char)
        (yank))
    (end-of-line)
    (right-char)
    (yank)
    (previous-line)))

(defun plomvi-region-kill()
  "Kill marked region."
  (interactive)
  (kill-region (region-beginning) (region-end)))

(defun plomvi-x()
  "If rectangle or region marked, kill those; else, kill char after point."
  (interactive)
  (cond
   ((and (boundp 'rectangle-mark-mode) (eq t rectangle-mark-mode))
    (kill-rectangle (region-beginning) (region-end)))
   ((use-region-p)
    (plomvi-region-kill))
   (t
    (delete-char 1))))

(defun plomvi-rectangle-mark()
  "Start marked rectangle, move right one char so a single column is visible."
  (interactive)
  (push-mark (point) nil t)
  (rectangle-mark-mode)
  (right-char))

(defun plomvi-search-forward()
  "Find next occurence of search string last targeted by isearch."
  (interactive)
  (search-forward isearch-string))

(defun plomvi-search-backward()
  "Find previous  occurence of search string last targeted by isearch."
  (interactive)
  (search-backward isearch-string))


(defun plomvi-copy-line()
  "Copy current line into kill buffer."
  (interactive)
  (let ((keep_pos (point)))  ; We sort of cheat: We kill the line, then we
    (kill-whole-line)        ; paste it back, and return point to its
    (plomvi-paste-backward)  ; original position.
    (goto-char keep_pos)))   ;

(defun plomvi-copy-region()
  "Copy marked region."
  (interactive)
  (copy-region-as-kill (region-beginning) (region-end)))

(defun plomvi-replace-char (c)
  "Replace char after point with c."
  (interactive "cplomvi-replace-char")
  (delete-char 1) (insert-char c) (left-char))

(defun plomvi-no-redo()
  "Tell user what to do, since implementing vim redo was too much of a hassle."
  (interactive)
  (message "Vim-style redo not available. Try M-x for Emacs' undo-undo."))

(defun plomvi-activate()
  "Activate `plomvi-mode'."
  (interactive)
  (plomvi-mode))

(defun plomvi-deactivate()
  "Deactivate `plomvi-mode'."
  (interactive)
  (plomvi-mode -1))

(defvar plomvi-mode-basic-map (make-sparse-keymap)
  "Keymap for `plomvi-mode' on read-only buffers.

In contrast to the keymap `plomvi-editable-mode' for editable
buffers, this excludes keybindings for editing text, which thus
become available to be used for other purposes.")
(suppress-keymap plomvi-mode-basic-map t)
(define-key plomvi-mode-basic-map (kbd ":") 'plomvi-prompt)
(define-key plomvi-mode-basic-map (kbd "C-w") 'other-window)
(define-key plomvi-mode-basic-map (kbd "k") 'previous-line)
(define-key plomvi-mode-basic-map (kbd "j") 'next-line)
(define-key plomvi-mode-basic-map (kbd "h") 'left-char)
(define-key plomvi-mode-basic-map (kbd "l") 'right-char)
(define-key plomvi-mode-basic-map (kbd "w") 'forward-word)
(define-key plomvi-mode-basic-map (kbd "b") 'backward-word)
(define-key plomvi-mode-basic-map (kbd "/") 'isearch-forward)
(define-key plomvi-mode-basic-map (kbd "N") 'plomvi-search-backward)
(define-key plomvi-mode-basic-map (kbd "n") 'plomvi-search-forward)
(define-key plomvi-mode-basic-map (kbd "v") 'set-mark-command)
(define-key plomvi-mode-basic-map (kbd "C-v") 'plomvi-rectangle-mark)
(define-prefix-command 'plomvi-g-map)
(define-key plomvi-mode-basic-map (kbd "g") 'plomvi-g-map)
(define-key plomvi-g-map (kbd "g") 'beginning-of-buffer)
(define-key plomvi-mode-basic-map (kbd "G") 'plomvi-goto-line)
(define-key plomvi-mode-basic-map (kbd "$") 'end-of-line)
(define-key plomvi-mode-basic-map (kbd "0") 'plomvi-prefix-zero-or-line-start)
(define-key plomvi-mode-basic-map (kbd "1") 'digit-argument)
(define-key plomvi-mode-basic-map (kbd "2") 'digit-argument)
(define-key plomvi-mode-basic-map (kbd "3") 'digit-argument)
(define-key plomvi-mode-basic-map (kbd "4") 'digit-argument)
(define-key plomvi-mode-basic-map (kbd "5") 'digit-argument)
(define-key plomvi-mode-basic-map (kbd "6") 'digit-argument)
(define-key plomvi-mode-basic-map (kbd "7") 'digit-argument)
(define-key plomvi-mode-basic-map (kbd "8") 'digit-argument)
(define-key plomvi-mode-basic-map (kbd "9") 'digit-argument)
(define-key plomvi-mode-basic-map (kbd "C-b") 'scroll-down)
(define-key plomvi-mode-basic-map (kbd "C-f") 'scroll-up)
(define-key plomvi-mode-basic-map (kbd "C-d") 'plomvi-half-scroll)

(defvar plomvi-mode-editable-map (make-sparse-keymap)
  "Keymap for `plomvi-mode' on editable buffers.

Inherits from `plomvi-mode-basic-map', but adds keybindings for
text editing.")
(set-keymap-parent plomvi-mode-editable-map plomvi-mode-basic-map)
(define-key plomvi-mode-editable-map (kbd "i") 'plomvi-deactivate)
(define-key plomvi-mode-editable-map (kbd "x") 'plomvi-x)
(define-key plomvi-mode-editable-map (kbd "o") 'plomvi-newline-below)
(define-key plomvi-mode-editable-map (kbd "O") 'plomvi-newline-above)
(define-key plomvi-mode-editable-map (kbd "r") 'plomvi-replace-char)
(define-key plomvi-mode-editable-map (kbd "u") 'undo-only)
(define-key plomvi-mode-editable-map (kbd "C-r") 'plomvi-no-redo)
(define-key plomvi-mode-editable-map (kbd "I") 'string-insert-rectangle)
(define-key plomvi-mode-editable-map (kbd "p") 'plomvi-paste-forward)
(define-key plomvi-mode-editable-map (kbd "P") 'plomvi-paste-backward)
(define-key plomvi-mode-editable-map (kbd "Y") 'plomvi-copy-line)
(define-key plomvi-mode-editable-map (kbd "y") 'plomvi-copy-region)
(define-key plomvi-mode-editable-map (kbd "D") 'plomvi-region-kill)
(define-prefix-command 'plomvi-d-map)
(define-key plomvi-mode-editable-map (kbd "d") 'plomvi-d-map)
(define-key plomvi-d-map (kbd "w") 'kill-word)
(define-key plomvi-d-map (kbd "$") 'kill-line)
(define-key plomvi-d-map (kbd "d") 'kill-whole-line)
(defvar plomvi-mode-hook)
(defvar plomvi-mode-basic-hook)
(defvar plomvi-mode-editable-hook)
(defvar plomvi-mode-disable-hook)
(defvar plomvi-mode-basic-disable-hook)
(defvar plomvi-mode-editable-disable-hook)
(defvar-local plomvi-mode nil "mode variable for `plomvi-mode'")
(defvar-local plomvi-mode-basic nil
  "toggles `plomvi-mode-basic-map' in `minor-mode-map-alist' for `plomvi-mode'")
(defvar-local plomvi-mode-editable nil
  "toggles `plomvi-mode-editable-map' in `minor-mode-map-alist' for `plomvi-mode'")

(defun plomvi-mode (&optional arg)
  "Imperfectly emulates a subset of Vim's Normal mode.

Sets mode variable `plomvi-mode' and, on read-only buffers, `plomvi-mode-basic',
or, on editable buffers, `plomvi-mode-editable'. The latter two's values in
`minor-mode-map-alist' toggle either `plomvi-mode-basic-map' or
`plomvi-mode-editable-map'."
  (interactive (list (or current-prefix-arg 'toggle)))
  (let ((enable (if (eq arg 'toggle)                   ; follow suggestions
                    (not plomvi-mode)                  ; from (elisp)Minor
                  (> (prefix-numeric-value arg) 0 )))) ; Mode Conventions
    (if enable
        (unless (minibufferp)
          (if buffer-read-only
              (setq plomvi-mode-basic t)
            (setq plomvi-mode-editable t))
          (setq plomvi-mode t)
          (run-hooks 'plomvi-mode-hook)
          (if plomvi-mode-basic
              (run-hooks 'plomvi-mode-basic-hook)
            (run-hooks 'plomvi-mode-editable-hook)))
      (setq plomvi-mode-editable nil
            plomvi-mode-basic nil
            plomvi-mode nil)
      (run-hooks 'plomvi-mode-editable-disable-hook)
      (run-hooks 'plomvi-mode-basic-disable-hook)
      (run-hooks 'plomvi-mode-disable-hook))))

(define-globalized-minor-mode plomvi-global-mode plomvi-mode plomvi-activate)
(add-to-list 'minor-mode-alist '(plomvi-mode " PV"))
(add-to-list 'minor-mode-map-alist (cons 'plomvi-mode-basic
                                         plomvi-mode-basic-map))
(add-to-list 'minor-mode-map-alist (cons 'plomvi-mode-editable
                                         plomvi-mode-editable-map))
