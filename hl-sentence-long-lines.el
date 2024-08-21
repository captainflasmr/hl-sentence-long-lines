;;; hl-sentence-long-lines.el --- highlights sentences are long
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience
;; URL: https://github.com/captainflasmr/hl-sentence-long-lines
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This Emacs package provides highlighting for the current sentence
;; and optionally highlights sentences that exceed a customizable
;; word limit and is heavily based on =hl-sentence= but with a few
;; additional features.

;;; Code:

(defgroup hl-sentence-long-lines nil
  "Highlight the current sentence."
  :group 'convenience)

;;;###autoload
(defface hl-sentence-long-lines '((t :inherit highlight))
  "The face used to highlight the current sentence."
  :group 'hl-sentence-long-lines)

(defface hl-sentence-long-lines-face '((t :inherit font-lock-warning-face))
  "Face used to highlight sentences that exceed the word limit."
  :group 'hl-sentence-long-lines)

(defcustom hl-sentence-long-lines-word-limit 20
  "The word limit for highlighting long sentences."
  :type 'integer
  :group 'hl-sentence-long-lines)

(defun hl-sentence-long-lines-begin-pos ()
  "Return the point of the beginning of a sentence."
  (save-excursion
    (unless (= (point) (point-max))
      (forward-char))
    (backward-sentence)
    (point)))

(defun hl-sentence-long-lines-end-pos ()
  "Return the point of the end of a sentence."
  (save-excursion
    (unless (= (point) (point-max))
      (forward-char))
    (backward-sentence)
    (forward-sentence)
    (point)))

(defun hl-sentence-long-lines-word-count ()
  "Return the number of words in the current sentence."
  (let ((sentence (buffer-substring-no-properties
                   (hl-sentence-long-lines-begin-pos)
                   (hl-sentence-long-lines-end-pos))))
    (length (split-string sentence "\\W+"))))

(defvar hl-sentence-long-lines-extent nil
  "The location of the hl-sentence-long-lines-mode overlay.")

(defvar-local hl-sentence-long-lines-word-count-str ""
  "String to display the current sentence word count in the mode line.")

(defun hl-sentence-long-lines-update-word-count ()
  "Update the modeline with the current sentence word count."
  (setq hl-sentence-long-lines-word-count-str
        (format " Words: %d" (hl-sentence-long-lines-word-count)))
  (force-mode-line-update))

(defun hl-sentence-long-lines-highlight-long-sentence (start end)
  "Highlight sentence between START and END if it exceeds `hl-sentence-long-lines-word-limit`."
  (save-excursion
    (goto-char start)
    (let ((word-count 0))
      (while (and (< (point) end)
                  (re-search-forward "\\w+" end t))
        (setq word-count (1+ word-count)))
      (if (> word-count hl-sentence-long-lines-word-limit)
          (let ((ov (make-overlay start end)))
            (overlay-put ov 'face 'hl-sentence-long-lines-face)
            (overlay-put ov 'highlight-long-sentences t))))))

;;;###autoload
(define-minor-mode hl-sentence-long-lines-mode
  "Enable highlighting of the current sentence."
  :init-value nil
  :lighter hl-sentence-long-lines-word-count-str
  (if hl-sentence-long-lines-mode
      (add-hook 'post-command-hook 'hl-sentence-long-lines-current nil t)
    (move-overlay hl-sentence-long-lines-extent 0 0 (current-buffer))
    (remove-overlays nil nil 'highlight-long-sentences t)
    (remove-hook 'post-command-hook 'hl-sentence-long-lines-current t)))

(defun hl-sentence-long-lines-current ()
  "Highlight the current sentence and update word count.
Also highlight if the sentence exceeds `hl-sentence-long-lines-word-limit`."
  (and hl-sentence-long-lines-mode (> (buffer-size) 0)
       (boundp 'hl-sentence-long-lines-extent)
       hl-sentence-long-lines-extent
       (let ((beg (hl-sentence-long-lines-begin-pos))
             (end (hl-sentence-long-lines-end-pos)))
         (move-overlay hl-sentence-long-lines-extent beg end (current-buffer))
         (hl-sentence-long-lines-update-word-count)
         (hl-sentence-long-lines-highlight-long-sentence beg end))))

(setq hl-sentence-long-lines-extent (make-overlay 0 0))
(overlay-put hl-sentence-long-lines-extent 'face 'hl-sentence-long-lines)

(provide 'hl-sentence-long-lines)

;;; hl-sentence.el ends here
