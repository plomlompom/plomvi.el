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



(defun plomvi-nothing()
  "Do nothing. Used to shadow self-insert bindings in `plomvi-editable-mode-map'."
  (interactive))

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

;;; some attempt at a redo feature, not very successful, documented here for
;;; research purposes
;
;(setq plomvi-in-redo nil)  ; should be made buffer-local
;(setq plomvi-undo-count 0) ; should be made buffer-local
;(defun plomvi-undo()
;  (interactive)
;  (undo-only)
;  (setq plomvi-in-redo nil)
;  (setq plomvi-undo-count (+ plomvi-undo-count 1)))
;(defun plomvi-redo()
;  (interactive)
;  (if (> plomvi-undo-count 0)
;      (progn
;        (if (null plomvi-in-redo)
;            (progn
;              (insert-char ?\s 1)
;              (undo)
;              (setq plomvi-in-redo t)))
;        (progn
;          (undo)
;          (setq plomvi-undo-count (- plomvi-undo-count 1))))))

(defun plomvi-no-redo()
  "Tell user what to do, since implementing vim redo was too much of a hassle."
  (interactive)
  (message "Vim-style redo not available. Try M-x for Emacs' undo-undo."))


(defvar plomvi-basic-mode-map (make-sparse-keymap)
  "Keymap for `plomvi-basic-mode', to be used on read-only buffers.

In contrast to the keymap `plomvi-editable-mode' for editable buffers,
this not only excludes keybindings for editing text, but also does not
shadow keybindings that are bound to `self-insert-command'.

Thus, it on the whole shadows much fewer keybindings of other keymaps
that can therefore be used for other purposes.")
(define-key plomvi-basic-mode-map (kbd ":") 'plomvi-prompt)
(define-key plomvi-basic-mode-map (kbd "C-w") 'other-window)
(define-key plomvi-basic-mode-map (kbd "k") 'previous-line)
(define-key plomvi-basic-mode-map (kbd "j") 'next-line)
(define-key plomvi-basic-mode-map (kbd "h") 'left-char)
(define-key plomvi-basic-mode-map (kbd "l") 'right-char)
(define-key plomvi-basic-mode-map (kbd "w") 'forward-word)
(define-key plomvi-basic-mode-map (kbd "b") 'backward-word)
(define-key plomvi-basic-mode-map (kbd "/") 'isearch-forward)
(define-key plomvi-basic-mode-map (kbd "N") 'plomvi-search-backward)
(define-key plomvi-basic-mode-map (kbd "n") 'plomvi-search-forward)
(define-key plomvi-basic-mode-map (kbd "v") 'set-mark-command)
(define-key plomvi-basic-mode-map (kbd "C-v") 'plomvi-rectangle-mark)
(define-prefix-command 'plomvi-g-map)
(define-key plomvi-basic-mode-map (kbd "g") 'plomvi-g-map)
(define-key plomvi-g-map (kbd "g") 'beginning-of-buffer)
(define-key plomvi-basic-mode-map (kbd "G") 'plomvi-goto-line)
(define-key plomvi-basic-mode-map (kbd "$") 'end-of-line)
(define-key plomvi-basic-mode-map (kbd "0") 'plomvi-prefix-zero-or-line-start)
(define-key plomvi-basic-mode-map (kbd "1") 'digit-argument)
(define-key plomvi-basic-mode-map (kbd "2") 'digit-argument)
(define-key plomvi-basic-mode-map (kbd "3") 'digit-argument)
(define-key plomvi-basic-mode-map (kbd "4") 'digit-argument)
(define-key plomvi-basic-mode-map (kbd "5") 'digit-argument)
(define-key plomvi-basic-mode-map (kbd "6") 'digit-argument)
(define-key plomvi-basic-mode-map (kbd "7") 'digit-argument)
(define-key plomvi-basic-mode-map (kbd "8") 'digit-argument)
(define-key plomvi-basic-mode-map (kbd "9") 'digit-argument)
(define-key plomvi-basic-mode-map (kbd "C-b") 'scroll-down)
(define-key plomvi-basic-mode-map (kbd "C-f") 'scroll-up)
(define-key plomvi-basic-mode-map (kbd "C-d") 'plomvi-half-scroll)
(define-minor-mode plomvi-basic-mode
  "plomvi mode for read-only buffers; uses `plomvi-basic-mode-map' to
implement Vim-Normal-mode-style keybindings."
  nil " PV" plomvi-basic-mode-map)

(defvar plomvi-editable-mode-map (make-sparse-keymap)
  "Keymap for `plomvi-editable-mode'.

Inherits from `plomvi-basic-mode-map', but adds keybindings for text editing
and shadows keybindings bound to `self-insert-command' to avoid accidentally
typing text outside of what would be Vim's Insert mode.")
(set-keymap-parent plomvi-editable-mode-map plomvi-basic-mode-map)
(define-key plomvi-editable-mode-map [remap self-insert-command] 'plomvi-nothing)
(define-key plomvi-editable-mode-map (kbd "i") 'plomvi-deactivate)
(define-key plomvi-editable-mode-map (kbd "x") 'plomvi-x)
(define-key plomvi-editable-mode-map (kbd "o") 'plomvi-newline-below)
(define-key plomvi-editable-mode-map (kbd "O") 'plomvi-newline-above)
(define-key plomvi-editable-mode-map (kbd "r") 'plomvi-replace-char)
(define-key plomvi-editable-mode-map (kbd "u") 'undo-only)
(define-key plomvi-editable-mode-map (kbd "C-r") 'plomvi-no-redo)
;(define-key plomvi-editable-mode-map (kbd "u") 'plomvi-undo)
;(define-key plomvi-editable-mode-map (kbd "C-r") 'plomvi-redo)
(define-key plomvi-editable-mode-map (kbd "I") 'string-insert-rectangle)
(define-key plomvi-editable-mode-map (kbd "p") 'plomvi-paste-forward)
(define-key plomvi-editable-mode-map (kbd "P") 'plomvi-paste-backward)
(define-key plomvi-editable-mode-map (kbd "Y") 'plomvi-copy-line)
(define-key plomvi-editable-mode-map (kbd "y") 'plomvi-copy-region)
(define-key plomvi-editable-mode-map (kbd "D") 'plomvi-region-kill)
(define-prefix-command 'plomvi-d-map)
(define-key plomvi-editable-mode-map (kbd "d") 'plomvi-d-map)
(define-key plomvi-d-map (kbd "w") 'kill-word)
(define-key plomvi-d-map (kbd "$") 'kill-line)
(define-key plomvi-d-map (kbd "d") 'kill-whole-line)
(define-minor-mode plomvi-editable-mode
  "plomvi mode for editable buffers; uses `plomvi-editable-mode-map' to
shadow `self-insert-command' keybindings and implement Vim-Normal-mode-style
keybindings."
  nil " PVe" plomvi-editable-mode-map)

(define-minor-mode plomvi-mode
  "Imperfectly emulates a subset of Vim normal mode.

Actually encapsulates either `plomvi-basic-mode' or `plomvi-editable-mode'.
Use `plomvi-activate' and `plomvi-deactivate' to toggle those.")

(defun plomvi-activate ()
  "Outside mini-buffer, activate `plomvi-mode'.

For read only-buffers, activate `plomvi-basic-mode'; else, `plomvi-editable-mode'."
  (interactive)
  (unless (minibufferp)
    ;(universal-argument)
    (plomvi-mode 1)
    (if buffer-read-only
        (plomvi-basic-mode 1)
      (plomvi-editable-mode 1))))

(defun plomvi-deactivate()
  "Outside mini-buffer, deactivate `plomvi-mode'.

For read only-buffers, deactivate `plomvi-basic-mode'; else, `plomvi-editable-mode'."
  (interactive)
  (plomvi-mode -1)
  (if buffer-read-only
      (plomvi-basic-mode -1)
    (plomvi-editable-mode -1)))

(define-globalized-minor-mode plomvi-global-mode plomvi-mode plomvi-activate)
