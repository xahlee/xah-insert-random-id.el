;;; xah-insert-random-id.el --- commands to insert random ID. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2013, 2016 by Xah Lee

;; Author: Xah Lee <xah@xahlee.info> ( http://xahlee.info/ )
;; Maintainer: Xah Lee
;; Created: 2013-04-19
;; Package-Requires: ((emacs "24.1"))
;; Version: 0.5.0
;; Keywords: convenience

;; feel free to use this code in anyway you like. Credit and donation is appreciated. Thanks.

;;; Commentary:
;; misc collection of commands to insert hex, digits, of fixed number of digits.

;;; Install:
;; (require 'xah-insert-random-id)

;;; Todo:
;; code needs to be refactored, so that there are a bunch of functions that returns a value, then one interactive command wrapper to them. Also, the command need to accept universal-argument for number of digits to insert.

;;; Change Log:
;; • 0.3 2016-12-20 turned on lexical binding, and misc other changes.
;; • 0.1 2013-04-19 first version. Moved from my personal init.

(random t) ; set seed

;; (defun xah-insert-random-hex (NUM)
;;   "Insert NUM random hexadecimal digits.
;; NUM default to 5.
;; Call `universal-argument' before for different count.
;; URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
;; Version 2017-05-24"
;;   (interactive "P")
;;   (let* (($charset "0123456789abcdef" )
;;          ($baseCount (length $charset)))
;;     (dotimes (_ (if (numberp NUM) (abs NUM) 5 ))
;;       (insert (elt $charset (random $baseCount))))))

(defun xah-insert-random-hex (NUM)
  "Insert NUM random hexadecimal digits.
NUM default to 5.
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2017-08-03"
  (interactive "P")
  (let (($n (if (numberp NUM) (abs NUM) 5 )))
    (insert (format  (concat "%0" (number-to-string $n) "x" ) (random (1- (expt 16 $n)))))))

(defun xah-insert-random-string (NUM)
  "Insert a random alphanumerics string of length 5.
The possible chars are 0 to 9, and a to z (lower case).
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2017-05-24"
  (interactive "P")
  (let* (($charset "1234567890abcdefghijklmnopqrstuvwxyz")
         ($baseCount (length $charset)))
    (dotimes (_ (if (numberp NUM) (abs NUM) 5))
      (insert (elt $charset (random $baseCount))))))

(defun xah-insert-random-number (NUM)
  "Insert NUM random digits.
NUM default to 5.
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2017-05-24"
  (interactive "P")
  (let (($charset "1234567890" )
        ($baseCount 10))
    (dotimes (_ (if (numberp NUM) (abs NUM) 5 ))
      (insert (elt $charset (random $baseCount))))))

(defun xah-insert-random-uuid ()
  "Insert a UUID. This uses a simple hashing of variable data.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

Note: this code uses https://en.wikipedia.org/wiki/Md5, which is not cryptographically safe. I'm not sure what's the implication of its use here.

Version 2015-01-30
URL `http://ergoemacs.org/emacs/elisp_generate_uuid.html'
"
;; by Christopher Wellons, 2011-11-18. Editted by Xah Lee.
;; Edited by Hideki Saito further to generate all valid variants for "N" in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format.
  (interactive)
  (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                            (user-uid)
                            (emacs-pid)
                            (system-name)
                            (user-full-name)
                            (current-time)
                            (emacs-uptime)
                            (garbage-collect)
                            (buffer-string)
                            (random)
                            (recent-keys)))))

    (insert (format "%s-%s-4%s-%s%s-%s"
                    (substring myStr 0 8)
                    (substring myStr 8 12)
                    (substring myStr 13 16)
                    (format "%x" (+ 8 (random 4)))
                    (substring myStr 17 20)
                    (substring myStr 20 32)))))

(provide 'xah-insert-random-id)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xah-insert-random-id.el ends here
