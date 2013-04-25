;;;; $Id: ba-text-tools.el,v 1.48 2012-09-08 11:47:28 bill Exp $
;;;; Copyright (C) 2005,2006,2012 Bill Alexander. See COPYING below.
;;;; Maintainer: Bill Alexander
;;;; Keywords: text tools
;;;; Purpose: Provide text manipulation tools
;;;;
;;;; Definition Categories:
;;;;   tables, lists, definitions
;;;;     ba-text-insert-alphabet-lc
;;;;     ba-text-insert-alphabet-uc
;;;;   manipulate
;;;;     ba-string-reverse
;;;;   replace
;;;;     ba-string-replace-all
;;;;     ba-text-replace-all
;;;;     ba-text-replace-all-regexp
;;;;   convert
;;;;     ba-text-convert-encondings-siko
;;;;     ba-text-convert-html-entities-to-text
;;;;     ba-text-convert-html-entities-to-text
;;;;     ba-text-convert-integer-to-text
;;;;     ba-text-convert-unicode-punctuation-to-ascii
;;;;     ba-text-rm-html-tags-sicko
;;;;     ba-text-set-hard-newlines
;;;;     ba-text-title-intercap-to-spaces
;;;;     ba-text-title-spaces-to-intercap
;;;;   reformat
;;;;     ba-text-reduce-multiple-blank-lines
;;;;     ba-ext-insert-blank-line-after-2
;;;;     ba-text-insert-blank-line-after-n
;;;;     ba-text-whitespace-cleanup
;;;;   report
;;;;     ba-text-alphabet-lc-list
;;;;     ba-text-alphabet-uc-list
;;;;
;;;; Definitions:
;;;;   ba-string-replace-all
;;;;   ba-string-reverse
;;;;   ba-text-alphabet-lc-list
;;;;   ba-text-alphabet-uc-list
;;;;   ba-text-convert-encondings-siko
;;;;   ba-text-convert-html-entities-to-text
;;;;   ba-text-reduce-multiple-blank-lines
;;;;   ba-text-insert-alphabet-lc
;;;;   ba-text-insert-alphabet-uc
;;;;   ba-text-insert-blank-line-after-2
;;;;   ba-text-insert-blank-line-after-n
;;;;   ba-text-convert-integer-to-text
;;;;   ba-text-replace-all
;;;;   ba-text-replace-all-regexp
;;;;   ba-text-rm-html-tags-sicko
;;;;   ba-text-set-hard-newlines
;;;;   ba-text-title-intercap-to-spaces
;;;;   ba-text-title-spaces-to-intercap
;;;;   ba-text-convert-unicode-punctuation-to-ascii
;;;;   ba-text-whitespace-cleanup
;;;;
;;;; items below this line are for validation

;;;; items above this line are for validation
;;;;
;;;; items below this line are inactive

;; (defun ba-text-rm-html-entities-sicko ()
;;   "This is a sicko solution to removing entities.
;; It does not check for embedded or quoted text"
;;   (interactive)
;;   (ba-text-replace-all-regexp "&nbsp;" " ")
;;   (ba-text-replace-all-regexp "&space;" " "))

;; prior to 2009.04.19
;;  (let ((reverse-char-list (reverse (split-string string "")))
;;      (reversed-string ""))
;;    (while reverse-char-list
;;      (setq reversed-string (concat reversed-string (car reverse-char-list)))
;;      (setq reverse-char-list (cdr reverse-char-list)))
;;    reversed-string))

;;;; items above this line are inactive
;;;;
;;;; items below this line are stable

(defun ba-text-whitespace-cleanup ()
  "Untabify and delete trailing whitespace in a buffer"
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))
    (goto-char (point-min))
    (while (search-forward-regexp "[ \t]+$" nil t)
      (replace-match ""))))

(defun ba-text-title-spaces-to-intercap ()
  "Create intercap title from a space delimited title"
  (interactive)
  (narrow-to-region (line-beginning-position) (line-end-position))
  (let ((orig (buffer-string)))
    ;;initial lower case
    (goto-char (line-beginning-position))
    (downcase-word 1)
    (while (search-forward-regexp " +\\([A-Za-z]\\)" nil t)
      (message (match-string 0))
      (replace-match (upcase (match-string 1))))
    ;;initial cap
    (goto-char (line-end-position))
    (insert "\n" orig)
    (goto-char (line-beginning-position))
    (capitalize-word 1)
    (while (search-forward-regexp " +\\([A-Za-z]\\)" nil t)
      (message (match-string 0))
      (replace-match (upcase (match-string 1))))
    (goto-char (point-max))
    (insert "\n" orig)
    (goto-char (point-min))
    (widen)))

(defun ba-text-title-intercap-to-spaces ()
  "Insert spaces into intercap text"
  ;;note: \1 didn't work for match-string
  (interactive)
  (let ((orig-case-fold-search case-fold-search))
    (setq case-fold-search nil)
    (goto-char (1+ (point-min)))
    (while (search-forward-regexp "\\([0-9]+\\)\\([^0-9]\\)" nil t)
      (message (match-string 0))
      (replace-match  (concat (match-string 1) " " (match-string 2))))
    (goto-char (1+ (point-min)))
    (while (search-forward-regexp "\\([^ \t]\\)\\([A-Z]\\)" nil t)
      (message (match-string 0))
      (replace-match  (concat (match-string 1) " " (match-string 2))))
    (goto-char (point-min))
    (while (search-forward-regexp " *[_.] *" nil t)
      (replace-match " "))
    (setq case-fold-search orig-case-fold-search)))

(defun ba-text-convert-unicode-punctuation-to-ascii ()
  "Convert unicode punctuation to compatible ascii"
  (interactive)
  (ba-text-replace-all (concat (char-to-string ?\302)
                               (char-to-string ?\251)) "(C)") ; copyright
  (ba-text-replace-all (concat (char-to-string ?\302)
                               (char-to-string ?\253)) "<<") ; small <<
  (ba-text-replace-all (concat (char-to-string ?\302)
                               (char-to-string ?\271)) "1") ; sup 1
  (ba-text-replace-all (concat (char-to-string ?\302)
                               (char-to-string ?\262)) "2") ; sup 2
  (ba-text-replace-all (concat (char-to-string ?\302)
                               (char-to-string ?\263)) "3") ; sup 3
  (ba-text-replace-all (concat (char-to-string ?\302)
                               (char-to-string ?\273)) ">>") ; small >>
  (ba-text-replace-all (concat (char-to-string ?\342)
                               (char-to-string ?\200)
                               (char-to-string ?\224)) "--") ; double dash
  (ba-text-replace-all (concat (char-to-string ?\342)
                               (char-to-string ?\200)
                               (char-to-string ?\223)) "--") ; --
  (ba-text-replace-all (concat (char-to-string ?\342)
                               (char-to-string ?\200)
                               (char-to-string ?\230)) "\'") ; curly '
  (ba-text-replace-all (concat (char-to-string ?\342)
                               (char-to-string ?\200)
                               (char-to-string ?\231)) "\'") ; curly '
  (ba-text-replace-all (concat (char-to-string ?\342)
                               (char-to-string ?\200)
                               (char-to-string ?\234)) "\"") ; curly "
  (ba-text-replace-all (concat (char-to-string ?\342)
                               (char-to-string ?\200)
                               (char-to-string ?\235)) "\"") ; curly "
  (ba-text-replace-all (concat (char-to-string ?\342)
                               (char-to-string ?\200)
                               (char-to-string ?\236)) "\"") ; bottom "
  (ba-text-replace-all (concat (char-to-string ?\342)
                               (char-to-string ?\200)
                               (char-to-string ?\240)) "+") ; dagger
  (goto-char (point-min)))

(defun ba-text-convert-encondings-siko ()
  "Convert various nasty encodings by a siko method"
  (interactive)
  (ba-text-convert-unicode-punctuation-to-ascii)
  (ba-text-replace-all         (char-to-string ?\221)  "\'") ; '
  (ba-text-replace-all         (char-to-string ?\222)  "\'") ; '
  (ba-text-replace-all         (char-to-string ?\223)  "\"") ; "
  (ba-text-replace-all         (char-to-string ?\224)  "\"") ; "
  (ba-text-replace-all         (char-to-string ?\225)  "\'") ; '
  (ba-text-replace-all         (char-to-string ?\226)  "--") ; --
  (ba-text-replace-all         (char-to-string ?\240)  "") ; blank
  (ba-text-replace-all (concat (char-to-string ?\342)
                               (char-to-string ?\200)
                               (char-to-string ?\223)) "\"") ; "
  (ba-text-replace-all         (char-to-string ?\256)  "(R)") ; (R)
  (ba-text-replace-all         (char-to-string ?\352)  "^") ; ^
  (message "done"))

(defun ba-text-rm-html-tags-sicko ()
  "This is a sicko solution to removing html tags.
It removes anything between < and > inclusive"
  (interactive)
  (ba-text-replace-all-regexp "<[^>]*>" ""))

(defvar ba-text-alphabet-lc-list
  (let ((i 97)
        (ba-alphabet-lc-list nil))
    (while (<= i 122)
      (setq ba-alphabet-lc-list (cons (char-to-string i)
                                      ba-alphabet-lc-list))
      (setq i (1+ i)))
    (reverse ba-alphabet-lc-list))
  "list of lower case letters in the ascii alphabet")

(defun ba-text-insert-alphabet-lc ()
  "Insert the lower case letters of the alphabet"
  (interactive)
  (let ((alphabet (copy-list ba-text-alphabet-lc-list)))
    (while alphabet
      (insert (car alphabet) "\n")
      (setq alphabet (cdr alphabet)))))

(defvar ba-text-alphabet-uc-list
  (let ((i 65)
        (ba-alphabet-uc-list nil))
    (while (<= i 90)
      (setq ba-alphabet-uc-list (cons (char-to-string i)
                                      ba-alphabet-uc-list))
      (setq i (1+ i)))
    (reverse ba-alphabet-uc-list))
  "list of upper case letters in the ascii alphabet")

(defun ba-text-insert-alphabet-uc ()
  "Insert the lower case letters of the alphabet"
  (interactive)
  (let ((alphabet (copy-list ba-text-alphabet-uc-list)))
    (while alphabet
      (insert (car alphabet) "\n")
      (setq alphabet (cdr alphabet)))))

(defun ba-text-insert-blank-line-after-2 ()
  "Insert blank line after 2 lines"
  (interactive)
  (ba-text-insert-blank-line-after-n 2))

(defun ba-text-insert-blank-line-after-n (n)
  "Insert blank line after N lines"
  (if (not n)
      (setq n 0))
  (while (> n 0)
    (goto-char (1+ (line-end-position)))
    (setq n (1- n)))
  (insert "\n"))

(defun ba-text-set-hard-newlines ()
  "Set hard newlines for paragraph filling."
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\n" nil t)
      (set-hard-newline-properties (1- (point)) (point)))))

(defun ba-text-replace-all-regexp (target-regexp replacement-regexp)
  "Replace each TARGET-REGEXP with REPLACEMENT-REGEXP"
  (interactive)
  (goto-char (point-min))
  (while (search-forward-regexp target-regexp nil t)
    (replace-match replacement-regexp t)))

(defun ba-text-replace-all (original-string replacement-string)
  "Replace ORIGINAL-STRING with REPLACEMENT-STRING in buffer"
  (interactive)
  (let ((orig-case-fold-search case-fold-search))
    (setq case-fold-search nil)
    (goto-char (point-min))
    (while (search-forward original-string nil t)
      (replace-match replacement-string t))
    (setq case-fold-search orig-case-fold-search)))

(defun ba-string-replace-all (original-string replacement-string)
  "alias until all references can be changed"
  ;;should be renamed ba-text-replace-all after all references
  (interactive)
  (ba-text-replace-all original-string replacement-string))

(defun ba-string-reverse (string)
  "Reverse the letters in a string"
  (interactive)
  (concat (nreverse (string-to-list string))))

(defun ba-text-reduce-multiple-blank-lines ()
  "Delete blank lines after cleaning up whitespace"
  (interactive)
  (ba-text-whitespace-cleanup)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\n\n+" nil t)
      (replace-match "\n\n"))))

;;;; items above this line are stable
;;;;
;;;; items below this line are being tested

;;;; items above this line are being tested
;;;;
;;;; items below this line are being developed

(defun ba-text-convert-html-entities-to-text ()
  "Convert html entities to reasonable text equivalents"
  (interactive)
  (ba-text-replace-all "&#39;"   "\'")
  (ba-text-replace-all "&#92;"   "\\\\")
  (ba-text-replace-all "&amp;"   "\&")
  (ba-text-replace-all "&apos;"  "\'")
  (ba-text-replace-all "&quot;"  "\"")
  (ba-text-replace-all "&nbsp;"  " ")
  (ba-text-replace-all "&space;" " "))

(defun ba-text-convert-integer-to-text (i)
  "Return a number as text or a string"
  (cond ((= i 1) "one")
        ((= i 2) "two")
        ((= i 3) "three")
        ((= i 4) "four")
        ((= i 5) "five")
        ((= i 6) "six")
        ((= i 7) "seven")
        (t (number-to-string i))))

(defun ba-text-remove-blank-lines ()
  "Remove blank lines (really)"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\n+" nil t)
      (replace-match "\n"))))

(defun ba-buffer-unique-lines ()
  (interactive)
  (goto-char (point-min))
  (while (< (1+ (line-end-position)) (point-max))
    (let ((line1-begin (point))
          (line1-end (line-end-position))
          (line1 (buffer-substring (line-beginning-position) (line-end-position))))
      (goto-char (1+ (line-end-position)))
      (cond ((string= line1 (buffer-substring (line-beginning-position) (line-end-position)))
             (delete-region (1- (line-beginning-position)) (line-end-position))
             (goto-char line1-begin))
            (t
             (goto-char (1+ line1-end)))))))

;;;; items above this line are being developed
;;;;

;; COPYING

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; $Source: /srv/cvs/LISP/lisp-generic/ba-text-tools.el,v $
