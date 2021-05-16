;;; kanji-doc.el --- Kanji pronounciations and meanings -*- lexical-binding: t -*-

;; Author: Léo Paviet Salomon
;; Maintainer: Léo Paviet Salomon
;; Version: 0.1
;; Package-Requires: ((pos-tip))
;; Homepage:
;; Keywords: kanji, japanese


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Requires the Kanjidic2 dictionnary. It can be found here:
;; http://nihongo.monash.edu/kanjidic2/index.html
;; Place this dictionnary in the directory indicated by `kanji-doc-base-dir',
;; or modify this variable accordingly. Make sure that the filename and the
;; variable `kanji-doc-dic-xml-file' correspond.

;;; Code:

(require 'pos-tip)

(defcustom kanji-doc-base-dir "~/Documents/Projects/proj-emacs/kanji-doc/" "Base directory to store kanji-doc-related files"
  :type 'file)

(defcustom kanji-doc-dic-xml-file "kanjidic2.xml" "File containing a XML dictionnary (for now, Kanjidic2)"
  :type 'string)

(defun kanji-doc-dic-xml-file ()
  (concat kanji-doc-base-dir kanji-doc-dic-xml-file))

(defcustom kanji-doc-dic-sexp-file "kanjidic-sexp.el" "File used to store a conversion of the XML dictionnary into S-expressions"
  :type 'string)

(defun kanji-doc-dic-sexp-file ()
  (concat kanji-doc-base-dir kanji-doc-dic-sexp-file))

(defcustom kanji-doc-table-file "kanji-table.el" "File where the lookup-table for all the kanjis is stored"
  :type 'string)

(defun kanji-doc-table-file ()
  (concat kanji-doc-base-dir kanji-doc-table-file))

(defcustom kanji-doc-default-lang "fr" "Language in which to display the kanjis' meanings. Must be encoded in two letters, as specified by ISO 639-1"
  :type 'string)

(defcustom kanji-doc-auto-show-buffer t "Whether to show a buffer on the side that is automatically updated with the kanji at point information."
  :type 'boolean)

(defvar kanji-doc--pos-tip-enabled nil "Whether a pos-tip is already enabled or not")
(defvar kanji-doc--kanji-table nil "The internal table used to find pronounciations and meanings")
(defvar kanji-doc--latest-kanji nil "The last kanji that was looked-up in the table")

(defmacro kanji-doc-mac--reading (lang r)
  (when (and (consp (car lang))
             (member (cdar lang) '("ja_kun" "ja_on")))
    `(quote ,(list 'r (cdar lang) r))))

(defmacro kanji-doc-mac--meaning (lang m &optional arg)
  (let* ((languages (or arg '("en" "fr")))
         (language (if (null lang)
                       (car (member "en" languages))
                     (car (member (cdar lang) languages)))))
    (when language
      `(quote ,(list 'm language m)))))

(defmacro kanji-doc-mac--rmgroup (attr &rest args)
  (when (null attr)
    `(remove nil (list ,@args))))

(defmacro kanji-doc-mac--reading_meaning (attr argrm &rest args)
  (when (null attr)
    argrm))

(defmacro kanji-doc-mac--literal (attr kanji)
  kanji)

(defmacro kanji-doc-mac--codepoint (&rest args)
  nil)

(defmacro kanji-doc-mac--radical (&rest args)
  nil)

(defmacro kanji-doc-mac--misc (&rest args)
  nil)

(defmacro kanji-doc-mac--dic_number (&rest args)
  nil)

(defmacro kanji-doc-mac--query_code (&rest args)
  nil)

(defmacro kanji-doc-mac--character (attr lit &rest args)
  `(cons ,lit (remove nil (list ,@args))))

(defmacro kanji-doc-mac--comment (attr entry)
  nil)

(defmacro kanji-doc-mac--kanjidic2 (attr header &rest args)
  `(remove nil (list ,@args)))


(defun kanji-doc-xml-to-sexp ()
  (save-excursion
    (with-temp-buffer
      (find-file (kanji-doc-dic-xml-file))
      (let ((kanjidic (libxml-parse-xml-region (point-min) (point-max))))
        (with-temp-file (kanji-doc-dic-sexp-file)
          (print kanjidic (current-buffer))
          (goto-char (point-min))
          (let* ((keywords-sym '(reading
                                 meaning
                                 rmgroup
                                 reading_meaning
                                 literal
                                 codepoint
                                 radical
                                 misc
                                 dic_number
                                 query_code
                                 character
                                 comment
                                 kanjidic2))
                 (keywords-str (mapconcat #'symbol-name keywords-sym "\\|")))

            (while (re-search-forward (concat  "(\\("
                                               keywords-str
                                               "\\)")
                                      nil t)  ;; replace keyword by kanji-macro--keyword
              (replace-match "(kanji-doc-macro--\\1")))) ; split in short line: easier to manipulate
        ;; (emacs-lisp-mode)
        ;; (indent-region (point-min) (point-max))
        ;; (delete-trailing-whitespace)
        ))))

(defun kanji-doc-create-kanji-table (kanji-list)
  "Returns a hash-table, whose keys are strings containing one kanji, and whose values are lists of length 3:\n- The first element is a list of kun readings\n- The second is a list of on readings\n- The third is another hash-table:\n    -Keys: strings encoding a language (e.g. \"en\" or \"fr\")\n    -Values: list of meanings"
  (let ((table (make-hash-table :test 'equal :size (length kanji-list))))
    (dolist (kanji kanji-list)
      (let ((kun-list nil)
            (on-list nil)
            (meaning-table (make-hash-table :test 'equal))
            (kanji-val (car kanji)))
        (dolist (rm (cadr kanji))
          (if (equal (car rm) 'r)
              (if (equal (cadr rm) "ja_kun")
                  (setq kun-list (append (cddr rm) kun-list))
                (setq on-list (append (cddr rm) on-list)))
            (puthash (cadr rm)
                     (cons (cddr rm) (gethash (cadr rm) meaning-table))
                     meaning-table)))
        (puthash kanji-val `(,kun-list ,on-list ,meaning-table) table)))
    table))


(defun kanji-doc-sexp-to-table ()
  "Convert a dictionnary made out of S-expressions to a hash-table,
then save this table in the file `kanji-doc-table-file'.
Additionally, sets `kanji-doc--kanji-table' to the computer value.
See `kanji-doc-create-kanji-table' for the format of the hash-table values."
  (with-temp-buffer
    (when (file-exists-p (kanji-doc-dic-sexp-file))
      (insert-file-contents (kanji-doc-dic-sexp-file))
      (let* ((parsed-kanjis (eval (read (buffer-string))))
             (kanji-table (kanji-doc-create-kanji-table parsed-kanjis)))

        (with-temp-file (kanji-doc-table-file)
          (print kanji-table (current-buffer)))
        (setq kanji-doc--kanji-table kanji-table)))))

(defun kanji-doc-load-kanji-table ()
  (interactive)
  "Loads the kanji tabled stored in `kanji-doc-table-file' and sets `kanji-doc--kanji-table' to this value."
  (with-temp-buffer
    (when (file-exists-p (kanji-doc-table-file))
      (insert-file-contents (kanji-doc-table-file))
      (setq kanji-doc--kanji-table (eval (read (buffer-string)))))))

(defun kanji-doc-full-xml-to-table ()
  (interactive)
  "Transforms a XML dictionnary stored in `kanji-doc-dic-xml-file' in a
S-expression-based dictionnary stored in `kanji-doc-dic-sexp-file',
and then to a hash-table stored in `kanji-doc-table-file'.
Sets `kanji-doc--kanji-table' in the process
This functions should only be run once per XML file, as it is very costly and
will produce the same result if the underlying dictionnary has not been modified.
To simply load a table, use `kanji-doc-load-kanji-table' instead of recomputing
everything."
  (kanji-doc-xml-to-sexp)
  (kanji-doc-sexp-to-table))

(defun kanji-doc--kanji-at-point ()
  "Return the character at point or a whitespace if there is none"
  (char-to-string (or (char-after (point)) ?\s))) ; avoids EOF problems

(defun kanji-doc--get-info-kanji (&optional no-cache)
  "Returns a string containing information about the kanji
at point.
The different meanings of the kanji are showed in the language
`kanji-doc-default-lang'. If no meanings are found in this
language, use English instead.
If no-cache is NIL or unspecified, return NIL without computation
if the kanji at point is the same as the last one that was parsed.
Otherwise, always return all the information about it."
  (let ((kanji (kanji-doc--kanji-at-point)))
    (when (or no-cache (not (equal kanji kanji-doc--latest-kanji)))
      (when-let (kanji-entry (gethash kanji kanji-doc--kanji-table))
        (let*  ((language kanji-doc-default-lang)
                (kun (car kanji-entry))
                (on (cadr kanji-entry))
                (pref-meanings (gethash language (caddr kanji-entry)))
                (meanings (or pref-meanings (gethash "en" (caddr kanji-entry))))
                (to-show (concat kanji
                                 "\nKun-yomi: "
                                 (mapconcat #'identity kun ", ")
                                 "\nOn-yomi: "
                                 (mapconcat #'identity on ", ")
                                 "\nMeanings:\n"
                                 (mapconcat #'car meanings ", ")
                                 "\n")))

          (setq kanji-doc--latest-kanji kanji)
          to-show)))))

(defun kanji-doc-show-buffer ()
  "Show a buffer containing information on the kanji at point. If
`kanji-doc-auto-show-buffer' is true, this function is called each time
a new command is performed by adding it to `post-command-hook'."
  (interactive)
  (when-let ((to-show (kanji-doc--get-info-kanji)))
    (let ((cur-buffer (current-buffer))
          (cur-point (point))) ;; save-excursion doesn't work ?
      (switch-to-buffer-other-window "*kanji-doc-show-buffer" t)
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert to-show)
      (switch-to-buffer-other-window cur-buffer)
      (goto-char cur-point))))

(defun kanji-doc-hide-pos-tip ()
  "Hides the pos-tip containing kanji information"
  (interactive)
  (when kanji-doc--pos-tip-enabled
    (x-hide-tip)
    (setq kanji-doc--pos-tip-enabled nil)))

(defun kanji-doc-toggle-pos-tip ()
  "Show a pos-tip containing information on the kanji at point,
with the same information as `kanji-doc-show-buffer'.
If such a pos-tip already exists, hide it before showing the
new one."
  (interactive)
  (kanji-doc-hide-pos-tip)
  (when-let ((to-show (kanji-doc--get-info-kanji t)))
    ;; Poor trick: as Kanjis take up more space, automatic height computation
    ;; is broken, so we add empty lines at the end
    (pos-tip-show (concat to-show "\n\n") nil nil nil 0 nil nil 0 50)
    (setq kanji-doc--pos-tip-enabled t)))

(defvar kanji-doc-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'kanji-doc-toggle-pos-tip)
    (define-key map (kbd "C-c C-b") #'kanji-doc-show-buffer)
    (define-key map (kbd "C-c C-d") #'kanji-doc-hide-pos-tip)
    map)
  "Keymap for `kanji-doc-mode'")

(define-minor-mode kanji-doc-mode
  "Toggle `kanji-doc-mode'.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.
From Lisp, argument omitted or nil enables the mode, `toggle'
toggles the state.

When `kanji-doc-mode' is enabled, the function `kanji-doc-toggle-pos-tip'
will create a child frame showing information on the kanji at
point, such as its pronounciations and its meanings. The function
`kanji-doc-show-buffer' will display this information in a buffer
instead."
  :keymap kanji-doc-map
  (if kanji-doc-mode
      (progn
        ;; Load the table if not already built
        (unless (or kanji-doc--kanji-table (kanji-doc-load-kanji-table))
          (kanji-doc-full-xml-to-table))

        (if kanji-doc-auto-show-buffer
            (add-hook 'post-command-hook #'kanji-doc-show-buffer nil t)
          (remove-hook 'post-command-hook #'kanji-doc-show-buffer t)))
    (remove-hook 'post-command-hook #'kanji-doc-show-buffer t)))

;; Test on some kanjis:
;; 青 荷 物 鬼 飯 晩

;; Test on a Japanese lorem ipsum
;; 挑テアナ幕銀を討転著メスケヒ通進はんい回7取ろえらみ強芸のべつ疾井フヱナホ改禁ヘリソ図化速ろ外3幅げく。強ヨツモ良振ちえしじ非来ー覧7建ヌヨニオ表加けへッ野雑づっ付出ウユ方強般ノリ健問芸イホ学水岳犠眠レえぐ。見ネ指外5任リ掲曜ラづ期進ンにげリ海題んゅ批民連ばは山組カ願取えるど固給購忘礎郷細らフ。

;; 則にとじ常気だ卓開リに済見ハ無57忠誉較9速ミ船人アナ旭簡モミ赤防同フチツ覧任キ葉1内せ急忘状みざ来施丘ぎりおる。女クケラ野合請やのす福要ロヒキカ予有フアユノ魅納ユ専横ドと暮紀状し案連ナエユ響嫌げ授度みげろら好記ろ美車影ゆろゃ。育被住ずつ市六供リムレヌ同本ひ月小ト外質あんドや個運るが予紙へ豊再マ投引るぞラ。

(provide 'kanji-doc)

;;; kanji-doc.el ends here
