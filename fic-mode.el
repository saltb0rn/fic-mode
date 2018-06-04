;;; fic-mode.el --- Show FIXME/TODO/BUG(...) in special face only in comments and strings ;; -*- lexical-binding: t -*-
;;--------------------------------------------------------------------
;;
;; Copyright (C) 2010, Trey Jackson <bigfaceworm(at)gmail(dot)com>
;; Copyright (C) 2010, Ivan Korotkov <twee(at)tweedle-dee(dot)org>
;; Copyright (C) 2012, Le Wang
;;
;; Homepage: https://github.com/lewang/fic-mode
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;; To use, save fic-mode.el to a directory in your load-path.
;;
;;   (require 'fic-mode)
;;
;;   ;;; emacs 24
;;   (add-hook 'prog-mode-hook 'fic-mode)
;;
;;   ;;; emacs 23 needs to add this to all programming modes you want this to be
;;   ;;; enabled for.
;;   ;;;
;;   ;;; e.g.
;;   (add-hook 'c++-mode-hook 'fic-mode)
;;   (add-hook 'emacs-lisp-mode-hook 'fic-mode)
;;
;; or
;;
;; M-x fic-mode

(defgroup fic-mode nil
  "Highlight FIXME/TODO(...) in comments"
  :tag "FIC"
  :group 'tools
  :group 'font-lock
  :group 'faces)

(defcustom fic-highlighted-words '("FIXME" "TODO" "BUG")
  "Words to highlight."
  :group 'fic-mode)

(defcustom fic-author-name-regexp "[-a-zA-Z0-9_.]+"
  "Regexp describing FIXME/TODO author name"
  :group 'fic-mode)

(defcustom fic-activated-faces
  '(font-lock-doc-face font-lock-string-face font-lock-comment-face)
  "Faces to look for to highlight words."
  :group 'fic-mode)

(defface fic-face
  '((((class color))
     (:background "white" :foreground "red" :weight bold))
    (t (:weight bold)))
  "Face to fontify FIXME/TODO words"
  :group 'fic-mode)

(defface fic-author-face
  '((((class color))
     (:background "white" :foreground "orangered" :underline t))
    (t (:underline t)))
  "Face to fontify author/assignee of FIXME/TODO"
  :group 'fic-mode)

(defvar fic-mode-font-lock-keywords '((fic-search-for-keyword
                                       (1 'fic-face t)
                                       (2 'fic-author-face t t))) 
  "Font Lock keywords for fic-mode")

(defvar fic-saved-hash nil
  "(`fic-highlighted-words' . `fic-author-name-regexp')")
(defvar fic-saved-regexp nil
  "Regexp cache for `fic-saved-hash'")

(defvar fic-jump-buffer "*Fic-Jump*"
  "The buffer jump from")

(defun fic-search-re ()
  "Regexp to search for."
  (let ((hash (cons fic-highlighted-words fic-author-name-regexp)))
    (if (and fic-saved-hash
           (equal fic-saved-hash hash))
        fic-saved-regexp
      (let ((fic-words-re (concat "\\<"
                                  (regexp-opt fic-highlighted-words t)
                                  "\\>")))
        (setq fic-saved-hash hash
              fic-saved-regexp (concat fic-words-re "\\(?:(\\(" fic-author-name-regexp "\\))\\)?"))
        fic-saved-regexp))))

(defun fic-in-doc/comment-region (pos)
  (memq (get-char-property pos 'face)
        fic-activated-faces))

(defun fic-search-for-keyword (limit)
  (let (match-data-to-set)
    (save-match-data
      (while (and (null match-data-to-set)
		  (re-search-forward (fic-search-re) limit t))
	(if (and (fic-in-doc/comment-region (match-beginning 0))
		 (fic-in-doc/comment-region (match-end 0)))
	    (setq match-data-to-set (match-data)))))
    (when match-data-to-set
      (set-match-data match-data-to-set)
      (goto-char (match-end 0))
      t)))

(defun fic--keyword-positions (&optional buffer limit)
  "Return the LIMIT positions of keywords in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-match-data
	(let (pos)
	  (goto-char (point-min))
	  (while (re-search-forward (fic-search-re) limit t)
	    (pcase (match-data)
	      (`(,s ,e . ,_)
	       (when (eq (get-char-property s 'face) 'fic-face)
		 (add-to-list 'pos e)))))
	  (reverse pos))))))

(defun fic--content-in-line-in-position (marker)
  "Return the content in line in location MARKER."
  (let ((frombuf (marker-buffer marker))
	(pos (marker-position marker)))
    (if (not (buffer-live-p frombuf))
	(message "Buffer %s is not alive"  (buffer-name frombuf))
      (with-current-buffer frombuf
	(goto-line (line-number-at-pos pos))
	(buffer-substring (line-beginning-position) (line-end-position))))))

(defun fic--lineno-in-position (marker)
  "Return line number in MARKER."
  (let ((buf (marker-buffer marker))
	(pos (marker-position marker)))
    (if (not (buffer-live-p buf))
	(message "Buffer %s is not alive" (buffer-name frombuf))
      (with-current-buffer buf
	(line-number-at-pos pos)))))

(defun fic--jump-to (marker)
  "Jump to the MARKER."
  (let ((tobuf (marker-buffer marker))
	(pos (marker-position marker)))
    (if (not (buffer-live-p tobuf))
	(message "Buffer %s is not alive" (buffer-name tobuf))
      (progn
	(switch-to-buffer tobuf)
	(goto-char pos)))))


(defun fic--append-line-to-buffer (&optional buffer)
  "Append the lines where keywords located in to BUFFER.
By default, BUFFER is named \"*Fic-Jump*\"."
  (let* ((oldbuf (current-buffer))
	 (newbuf (get-buffer-create (or buffer fic-jump-buffer)))
	 (markers (fic--keyword-positions oldbuf)))
    (if (with-current-buffer oldbuf
	  (bound-and-true-p fic-mode))
	(progn
	  (with-current-buffer newbuf
	    (let ((inhibit-read-only t))
	      (dolist (marker markers)
		(let ((beg (point)))
		  (insert (format "Buffer: %s  "(buffer-name (marker-buffer marker))))
		  (insert (format "Line: %s " (fic--lineno-in-position marker)))
		  (insert (format "%s ..." (fic--content-in-line-in-position marker)))
		  (make-button
		   beg (point) 'action
		   ((lambda (mkr) (lambda (x) (fic--jump-to mkr)))
		    marker)))
		(insert "\n"))))
	  (view-buffer (get-buffer newbuf)))
      (message "The fic-mode is disabled in this buffer."))))

(defun fic-jump (&optional buffer)
  "Jump to where keyword located in.
BUFFER is the buffer to list the lines where keywords located in."
  (interactive)
  (let ((bufs (buffer-list))
	(buffer (get-buffer-create (or buffer fic-jump-buffer))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)))
    (dolist (buf bufs)
      (with-current-buffer buf
	(when fic-mode
	  (fic--append-line-to-buffer buffer))))))

;;;###autoload
(define-minor-mode fic-mode
  "Fic mode -- minor mode for highlighting FIXME/TODO in comments"
  :lighter ""
  :group 'fic-mode
  (let ((kwlist fic-mode-font-lock-keywords))
    (if fic-mode
	(font-lock-add-keywords nil kwlist 'append)
      (font-lock-remove-keywords nil kwlist))
    (font-lock-fontify-buffer)))

(provide 'fic-mode)
;;; fic-mode.el ends here
