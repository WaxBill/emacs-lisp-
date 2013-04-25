;;;; $Id: ba-date-and-time.el,v 1.32 2013-03-18 03:43:56 bill Exp $
;;;; Purpose: date and time tools for emacs
;;;; Copyright (C) 1999,2005,2006,2010,2011, 2013 Bill Alexander. See COPYING below.
;;;; Maintainer: Bill Alexander
;;;; Keywords: date time emacs lisp
;;;;
;;;; Definitions:
;;;;   ba-date-diary-m+/d+/y+-to-mon_dd,_yyyy
;;;;   ba-date-diary-m/d+/y+-to-mon_dd,_yyyy-buffer
;;;;   ba-date-m+/d+/y+-to-mon_dd,_yyyy-maybe-ok
;;;;   ba-date-m/d+/y+-to-mm/d+/yy+
;;;;   ba-date-m/d/y-to-iso
;;;;   ba-date-mSdSy-to-iso
;;;;   ba-date-mm-dd-yy-to-iso
;;;;   ba-date-mm/dd/yy-to-iso
;;;;   ba-date-mm/dd/yy-to-iso-dotted
;;;;   ba-date-mm/dd/yyyy-to-iso
;;;;   ba-date-mm/dd/yyyy-to-iso-dotted
;;;;   ba-date-mmSddSyy2yy4-to-iso
;;;;   ba-date-mmm_dd_yyyy-to-iso-dotted
;;;;   ba-date-regex-m+/d+/y+
;;;;   ba-date-regex-mmm_d+_yyyy
;;;;   ba-date-rm-initial-weekday-name
;;;;   ba-date-shorten-initial-month-name
;;;;   ba-date-yyyymmdd-to-iso
;;;;   ba-diary-convert-Day_Month_DD,_yyyy-to-mon_dd,_yyyy
;;;;   ba-diary-date-line-holidays-to-mon_dd,_yyyy
;;;;   ba-insert-date
;;;;   ba-insert-date-stamp
;;;;   ba-insert-date-time-stamp
;;;;   ba-insert-iso-date
;;;;   ba-insert-iso-date-dashes
;;;;   ba-insert-iso-date-no-dashes
;;;;   ba-insert-iso-date-numbers-only
;;;;   ba-insert-iso-date-time
;;;;   ba-insert-iso-date-time-stamp
;;;;   ba-insert-time-stamp
;;;;   ba-time-12-AMPM-to-24
;;;;   ba-time-12-no-AMPM-to-24
;;;;
;;;; items below this line are inactive

;;(defun ba-date-mm-dd-yy-to-iso-brute ()
;;  "Convert a date to iso yyyy-mmm-dd format
;;Bug: must be at end of string"
;;  (interactive)
;;  (save-excursion
;;    (let ((point-sav (point)))
;;      (if (not (search-forward-regexp "[-0-9]+" (line-end-position) t))
;;        (message "not in date string") ; does this really work
;;      (if (not (search-backward-regexp
;;                (concat
;;                 "\\([01][0-9]\\)"
;;                 "-"
;;                 "\\([0-3][0-9]\\)"
;;                 "-"
;;                 "\\([0-9][0-9]\\)")
;;                (line-beginning-position) t))
;;          (message "not found: mm-dd-yy")
;;        (replace-match (concat (match-string 3)
;;                               "-"
;;                               (match-string 1)
;;                               "-"
;;                               (match-string 2))))))))

;;;; items above this line are inactive
;;;;
;;;; items below this line are stable

                                        ; date
(defun ba-insert-date ()
  "Insert current date in form like December 13, 1998"
  (interactive "*")
  (insert (format-time-string "%B %d, %Y" (current-time))))

(defun ba-insert-date-stamp ()
  "Insert current date in form like 19981213"
  (interactive "*")
  (insert (format-time-string "%Y%m%d" (current-time))))

(defun ba-insert-iso-date ()
  "Insert current date in form like 1998.12.13"
  (interactive "*")
  (insert (format-time-string "%Y.%m.%d" (current-time))))

(defun ba-insert-iso-date-dashes ()
  "Insert current date in form like 1998-12-13"
  (interactive "*")
  (insert (format-time-string "%Y-%m-%d" (current-time))))

;; 2012.10.28
(defun ba-insert-iso-date-numbers-only ()
  "Insert current date in form like 20121028"
  (interactive "*")
  (insert (format-time-string "%Y%m%d" (current-time))))

                                        ; time

(defun ba-insert-time-stamp ()
  "Insert current time in form like 13:13"
  (interactive "*")
  (insert (format-time-string "%H:%M" (current-time))))

(defun ba-insert-time-stamp-numbers-only ()
  "Insert current time in form like 1313"
  (interactive "*")
  (insert (format-time-string "%H%M" (current-time))))

                                        ; date & time

(defun ba-insert-date-time-stamp ()
  "Insert current date and time in form like 19981213-13:13"
  (interactive "*")
  (ba-insert-date-stamp)
  (insert "-")
  (ba-insert-time-stamp))

(defun ba-insert-iso-date-time ()
  "Insert current date and time in form like 1998.12.13 05:30"
  (interactive "*")
  (ba-insert-iso-date)
  (insert " ")
  (ba-insert-time-stamp))

(defun ba-insert-iso-date-time-stamp ()
  "Insert current date and time in form like 1998.12.13 05:30 "
  (interactive "*")
  (ba-insert-iso-date-time)
  (insert " "))

(defun ba-insert-iso-date-time-for-filenames ()
  "Insert current date and time in form like 19981213-0530"
  (interactive)
  (ba-insert-iso-date-numbers-only)
  (insert "-")
  (ba-insert-time-stamp-numbers-only))

                                        ; convert

(defun ba-date-rm-initial-weekday-name ()
  "Remove initial weekday name from line"
  (interactive)
  (goto-char (line-beginning-position))
  (if (search-forward-regexp "\\(^[A-Za-z]+\\), " (line-end-position) t)
      (if (member (downcase (match-string 1)) ba-calendar-weekday-names-lc)
          (replace-match ""))))

(defun ba-date-diary-m/d+/y+-to-mon_dd,_yyyy-buffer ()
  "Convert every initial date to diary format"
  (interactive)
  (while (ba-date-diary-m/d+/y+-to-mon_dd,_yyyy)
    (goto-char (line-end-position))))

(defvar ba-date-regex-m+/d+/y+
  (concat
   "\\([0-9]+\\)"
   "/"
   "\\([0-9]+\\)"
   "/"
   "\\([0-9][0-9][0-9][0-9]\\)")
  "regexp for dates like 1/24/99 or 11/2/2010")

(defun ba-date-diary-m+/d+/y+-to-mon_dd,_yyyy ()
  "Convert date to diary format"
  (interactive)
  (goto-char (line-beginning-position))
  (if (not (search-forward-regexp (concat
                                   "\\([0-9]+\\)"
                                   "/"
                                   "\\([0-9]+\\)"
                                   "/"
                                   "\\([0-9][0-9][0-9][0-9]\\)")
                                  (line-end-position) t))
      (message "not found: initial date string")
    (let ((month (match-string 1))
          (day (match-string 2))
          (year (match-string 3)))
      (replace-match (concat (nth (1- (string-to-number month))
                                  ba-calendar-month-names-short)
                             " "
                             (if (> 2 (length day))
                                 (concat "0" day)
                               day)
                             ", "
                             year)))))

(defun ba-date-mm/dd/yy-to-iso ()
  "Convert a date to iso yyyy-mmm-dd format"
  (interactive)
  (ba-date-mmSddSyy2yy4-to-iso 2 "/"))

(defun ba-date-mm-dd-yy-to-iso ()
  "Convert a date to iso yyyy-mmm-dd format"
  (interactive)
  (ba-date-mmSddSyy2yy4-to-iso 2 "-"))

(defun ba-date-mm/dd/yyyy-to-iso ()
  "Convert a date to iso yyyy-mm-dd format
Bug: must be at end of string"
  (interactive)
  (save-excursion
    (let ((point-sav (point)))
      (if (not (search-forward-regexp "[/0-9]+" (line-end-position) t))
          (message "not in date string") ; does this really work
        (if (not (search-backward-regexp
                  (concat
                   "\\([01][0-9]\\)"
                   "/"
                   "\\([0-3][0-9]\\)"
                   "/"
                   "\\([12][0-9][0-9][0-9]\\)")
                  (line-beginning-position) t))
            (message "not found: mm/dd/yyyy")
          (replace-match (concat (match-string 3)
                                 "-"
                                 (match-string 1)
                                 "-"
                                 (match-string 2))))))))

(defun ba-date-mmSddSyy2yy4-to-iso (year-length separator)
  "Convert a date to iso yyyy-mmm-dd format
Generalized solution"
  (save-excursion
    (let ((point-sav (point)))
      (if (not (search-forward-regexp
                (concat "[" separator "0-9]+") (line-end-position) t))
          (message "not in date string")
        (let ((my-search-string (concat
                                 "\\([01][0-9]\\)"
                                 separator
                                 "\\([0-3][0-9]\\)"
                                 separator
                                 "\\([0-9]+\\)")))
          (if (not (search-backward-regexp my-search-string
                                           (line-beginning-position) t))
              (message (concat "not found: " my-search-string))
            (replace-match
             (concat (if (= 4 year-length)
                         (match-string 3)
                       (if (< 40 (string-to-number (match-string 3)))
                           (concat "19" (match-string 3))
                         (concat "20" (match-string 3))))
                     "-"
                     (match-string 1)
                     "-"
                     (match-string 2)))))))))

;;;; items above this line are stable
;;;;
;;;; items below this line are being tested

(defun ba-diary-date-line-holidays-to-mon_dd,_yyyy ()
  "Convert 'Day, Month DD, YYYY:' to mon_dd,_yyyy"
  (interactive)
  (goto-char (line-end-position))
  (ba-date-rm-initial-weekday-name)
  (ba-date-shorten-initial-month-name)
  (goto-char (line-beginning-position))
  (if (search-forward-regexp ":" (line-end-position) t)
      (replace-match "")))

(defun ba-diary-convert-Day_Month_DD,_yyyy-to-mon_dd,_yyyy ()
  "Convert holiday and moon buffer 'Day, Month DD, YYYY:' to mon_dd,_yyyy"
  (interactive)
  (goto-char (point-min))
  (while (search-forward-regexp "^[A-Za-z]" nil t)
    (ba-diary-date-line-holidays-to-mon_dd,_yyyy)
    (goto-char (line-end-position))))

(defun ba-date-shorten-initial-month-name ()
  "Shorten initial month name on line"
  (interactive)
  (goto-char (line-beginning-position))
  (if (search-forward-regexp "\\(^[A-Za-z]+\\) " (line-end-position) t)
      (if (member (downcase (match-string 1)) ba-calendar-month-names-long-lc)
          (replace-match (substring (match-string 1) 0 3) nil nil nil 1))))

;;2010.07.21 can't find fist digit of two digit month
;;2010.10.28 forgot + on month regex
(defun ba-date-m+/d+/y+-to-mon_dd,_yyyy-maybe-ok ()
  "Convert date to diary format"
  (interactive)
  (save-excursion
    (if (not (search-forward-regexp "[/0-9]+" (line-end-position) t))
        (message "not in date string")  ; does this really work
      (if (not (search-backward-regexp
                (concat
                 "\\([0-9]+\\)"
                 "/"
                 "\\([0-9]+\\)"
                 "/"
                 "\\([12][0-9][0-9][0-9]\\)")
                (1- (line-beginning-position)) t))
          (message "not found: m+/d+/yyyy")
        (let ((month (match-string 1))
              (day (match-string 2))
              (year (match-string 3)))
          (replace-match (concat (nth (1- (string-to-number month))
                                      ba-calendar-month-names-short)
                                 " "
                                 (if (> 2 (length day))
                                     (concat "0" day)
                                   day)
                                 ", "
                                 year)))))))

(defun ba-date-mSdSy-to-iso (separator)
  "Convert a date to iso yyyy-mmm-dd format
Generalized solution for month date year
Risk: string is not a date"
  (save-excursion
    (let ((point-sav (point)))
      (if (not (search-forward-regexp
                (concat "[" separator "0-9]+") (line-end-position) t))
          (message "not in date string")
        (let ((my-search-string (concat
                                 "\\([^" separator "0-9]\\)"
                                 "\\([0-9]+\\)"
                                 separator
                                 "\\([0-9]+\\)"
                                 separator
                                 "\\([0-9]+\\)")))
          (if (not (search-backward-regexp my-search-string
                                          (line-beginning-position) t))
              (message (concat "not found: " my-search-string))
            (let ((month (match-string 2))
                  (day (match-string 3))
                  (year (match-string 4)))
              (if (or (> (length month) 2)
                      (> (length day) 2)
                      (> (length year) 4)
                      (< (length year) 2))
                  (message "date format invalid")
                (replace-match
                 (concat (match-string 1)
                         (if (= 4 (length year))
                             year
                           (if (< 40 (string-to-number year))
                               (concat "19" year)
                             (concat "20" year)))
                         "-"
                         (if (> 2 (length month))
                             (concat "0" month)
                           month)
                         "-"
                         (if (> 2 (length day))
                             (concat "0" day)
                           day)))))))))))

(defun ba-time-12-AMPM-to-24 ()
  ;; 2009.12.08 does not find 4:21a
  "Convert a time in ampm format to 24hr format"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\\([01]?[0-9]\\):\\([0-5][0-9]\\)"
                                  (point-max) t)
      (let ((time-start (match-beginning 0))
            (hour (match-string 1))
            (min (match-string 2)))
        (delete-region time-start (point))
        (let ((ampm-start (point)))
          (when (search-forward-regexp " *\\([AaPp]\\)[Mm]?"
                                       (line-end-position) t)
            (let ((ampm (downcase (match-string 1))))
              (delete-region ampm-start (point))
              (cond ((string= "a" ampm)
                     (if (= 1 (length hour))
                         (setq hour (concat "0" hour))))
                    ((string= "p" ampm)
                     (let ((hour-number (string-to-number hour)))
                       (if (< hour-number 12)
                           (setq hour (number-to-string (+ 12 hour-number))))))))))
        (insert hour ":" min)))))

(defun ba-time-12-no-AMPM-to-24 ()
  "Convert a single no AMPM 12hr format time to 24hr format"
  (interactive)
  (narrow-to-region (line-beginning-position) (line-end-position))
  (when (search-backward-regexp "[^0-9:]" (line-beginning-position) t)
    (if (search-forward-regexp "\\([01]?[0-9]\\):\\([0-5][0-9]\\)"
                               (point-max) t)
        (let* ((time-start (match-beginning 0))
               (hour (match-string 1))
               (hour-number (string-to-number hour))
               (min (match-string 2)))
          (delete-region time-start (point))
          (if (< hour-number 12)
              (setq hour (number-to-string  (+ 12 hour-number))))
          (insert hour ":" min))))
  (widen))

;;2010.07.21 doesn't work
(defun ba-date-m/d/y-to-iso ()
  "Convert a date to iso yyyy-mmm-dd format"
  (interactive)
  (ba-date-mSdSy-to-iso "/"))

(defun ba-date-mm/dd/yyyy-to-iso-dotted ()
  (interactive)
  (ba-date-mm/dd/yyyy-to-iso)
  (narrow-to-region (point) (+ 10 (point)))
  (while (search-forward "-" nil t)
    (replace-match "."))
  (kill-ring-save (point-min) (point-max))
  (widen))

(defun ba-date-mm/dd/yy-to-iso-dotted ()
  (interactive)
  (ba-date-mm/dd/yy-to-iso)
  (narrow-to-region (point) (+ 10 (point)))
  (while (search-forward "-" nil t)
    (replace-match "."))
  (kill-ring-save (point-min) (point-max))
  (widen))

(defun ba-date-mmm_dd_yyyy-to-iso-dotted ()
  "Replace at beginning of line mmm dd, yyyy with iso"
  (interactive)
  (if (not (= (point) (line-beginning-position)))
      (message "must be at beginning of line")
    (if (not (search-forward-regexp
              (concat
               "\\([A-Za-z][A-Za-z][A-Za-z]\\) "
               "\\([0-9][0-9]\\), "
               "\\([0-9][0-9][0-9][0-9]\\)")
              (line-end-position) t))
        (message "not found: mmm dd, yyyy")
      (let ((month-name (match-string 1))
            (year-string (match-string 3))
            (day-string (format "%02d" (string-to-number (match-string 2)))))
        (replace-match (concat
                        year-string
                        "-"
                        (ba-calendar-month-name-short-to-number month-name)
                        "-"
                        day-string))))))

(defvar ba-date-regex-mmm_d+_yyyy
  (concat
   "\\([A-Za-z][A-Za-z][A-Za-z]\\)"
   " "
   "\\([0-9]+\\)"
   ", "
   "\\([0-9][0-9][0-9][0-9]\\)")
  "regexp for dates like Mar 03, 2010 or Mar 3, 2010")

;;2012.03.03
(defun ba-date-m/d+/y+-to-mm/d+/yy+ ()
  "Convert month to two digits" ; y3k problem
  (interactive)
  (save-excursion
    ;;2012.03.03 won't find string
    (if (not (search-forward-regexp "[1-9]/[0-9]+/[12][0-9]+/" (line-end-position) t))
        (message "not at beginning of date string")
      (if (not (search-backward-regexp
                (concat
                 "\\([0-9]\\)"
                 "/"
                 "\\([0-9]+\\)"
                 "/"
                 "\\([12][0-9]+\\)")
                (1- (line-beginning-position)) t))
          (message "not found: m/d+/yy+")
        (let ((month (match-string 1))
              (day (match-string 2))
              (year (match-string 3)))
          (replace-match (concat "0" month
                                 "/"
                                 day
                                 "/"
                                 year)))))))

;;;; items above this line are being tested
;;;;
;;;; items below this line are being developed

;;2012.06.26 for schwab g/l
(defun ba-date-yyyymmdd-to-iso ()
  "Convert a date from yyyymmdd to iso yyyy-mmm-dd format"
  (interactive)
  (save-excursion
    (let ((point-sav (point)))
      (if (not (search-forward-regexp "[0-9]+" (line-end-position) t))
          (message "not in date string")
        (let ((my-search-string (concat "\\([0-9][0-9][0-9][0-9]\\)"
                                        "\\([0-9][0-9]\\)"
                                        "\\([0-9][0-9]\\)")))
          (if (not (search-backward-regexp my-search-string
                                           (line-beginning-position) t))
              (message (concat "not found: " my-search-string))
            (replace-match (concat (match-string 1)
                                   "-"
                                   (match-string 2)
                                   "-"
                                   (match-string 3)))))))))

;; ;ba-date-yyyymmdd-to-iso
;; ;;2012.06.26 not yet edited
;; (defun ba-date-yyyymmyy-to-iso ()
;;   (interactive)
;;   (ba-date-mm/dd/yyyy-to-iso)
;;   (narrow-to-region (point) (+ 10 (point)))
;;   (while (search-forward "-" nil t)
;;     (replace-match "."))
;;   (kill-ring-save (point-min) (point-max))
;;   (widen))

(defun ba-return-iso-date-time-for-filenames ()
  "Return current date and time in form like 19981213-0530"
  (format-time-string "%Y%m%d-%H%M" (current-time)))

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

;; $Source: /srv/cvs/LISP/lisp-generic/ba-date-and-time.el,v $
