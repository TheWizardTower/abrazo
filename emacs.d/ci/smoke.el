;;; smoke.el --- CI init smoke test for emacs.d  -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded by CI after early-init.el and init.el.  Fails the build
;; (exit 1) if anything ended up in the *Warnings* buffer.  The bar is
;; deliberately strict — every warning we ever shipped here turned out
;; to mean something real (package.el/straight.el conflicts, missing
;; :demand triggers, load-order mistakes).

;;; Code:

(let* ((buf (get-buffer "*Warnings*"))
       (text (when buf
               (with-current-buffer buf
                 (string-trim (buffer-string))))))
  (cond
   ((or (null text) (string-empty-p text))
    (princ "OK: *Warnings* buffer empty\n")
    (kill-emacs 0))
   (t
    (princ "FAIL: *Warnings* buffer is non-empty:\n")
    (princ "----------------------------------------\n")
    (princ text)
    (princ "\n----------------------------------------\n")
    (kill-emacs 1))))

;;; smoke.el ends here
