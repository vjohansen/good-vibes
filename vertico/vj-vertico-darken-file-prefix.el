;;; vj-vertico-darken-file-prefix.el --- Darken matching path prefixes in Vertico file candidates -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Vagn Johansen

;; Author: Vagn Johansen <https://github.com/vjohansen>
;; URL: https://github.com/vjohansen/good-vibes
;; Package-Requires: ((emacs "27.1") (vertico "0.16"))
;; Version: 0.1.0
;; Keywords: convenience, files, matching, vertico

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:
;;
;; Dim literal path prefixes in file candidates shown by Vertico.  This is
;; especially handy for consult-locate, file browsing, and other file-oriented
;; completion tables where the prefix is visually noisy.
;;
;; Example:
;;
;;   (add-to-list 'vj-file-dark-prefixes "C:/Users/dude/" t)
;;   (add-to-list 'vj-file-dark-prefixes "~/Library/" t)
;;
;;; Code:

(require 'vertico)

(defgroup vj-vertico-darken-file-prefix nil
  "Darken matching path prefixes in Vertico file candidates."
  :group 'convenience)

(defface vj-file-dark
  '((t (:inherit shadow)))
  "Face used to dim matching path prefixes in file candidates."
  :group 'vj-vertico-darken-file-prefix)

(defcustom vj-file-dark-prefixes nil
  "Literal path prefixes to dim at the start of a file candidate.
Not regexps: matched case-insensitively, and `/' or `\\' in an entry
matches either separator in the candidate, so one entry covers both
consult's forward-slash output and consult-locate's backslash output.
The longest match gets the `vj-file-dark' face.

Example entries:
  \"C:/Users/me/\"
  \"~/Library/\"
  \"/usr/local/\""
  :type '(repeat string)
  :group 'vj-vertico-darken-file-prefix)

(defun vj-file-dark--regexp (prefix)
  "Return an anchored regexp for literal PREFIX with separator-agnostic slashes.
Example: \"C:/code/\" becomes \"\\`C:[/\\\\]code[/\\\\]\", i.e.
start of string, then C:, then / or \\, then code, then / or \\.
The returned regexp matches a prefix anchored at the beginning of the
candidate and stops at the end of the longest literal path segment sequence."
  ;; 1. split-string cuts PREFIX at every / or \ into path segments:
  ;;      "C:/code/" -> ("C:" "code" "").  The trailing "" is wanted:
  ;;    it makes the joined regexp end in a separator, so "C:/code/"
  ;;    does not also match "C:/codex/...".
  ;; 2. regexp-quote escapes each segment so characters that mean
  ;;    something in a regexp, e.g. the parens in "Program Files (x86)"
  ;;    or a ".", are matched literally.
  ;; 3. mapconcat joins the quoted segments with the character class
  ;;    [/\\] which matches one separator of either kind.
  ;; 4. \\` anchors the whole thing at the start of the candidate.
  (concat "\\`" (mapconcat #'regexp-quote
                            (split-string prefix "[/\\\\]")
                            "[/\\\\]")))

(defun vj-vertico-darken-file-prefix (args)
  "Give the prefix of file candidates matching `vj-file-dark-prefixes'
the `vj-file-dark' face.
ARGS are the arguments of `vertico--format-candidate'."
  ;; Installed as a :filter-args advice, so ARGS is the argument list
  ;; (CAND PREFIX SUFFIX INDEX START) and we must return it, possibly
  ;; with CAND mutated.  CAND is a fresh copy made by `vertico--hilit',
  ;; so adding a face to it does not touch the real candidate.
  (let* ((cand (car args))
         (cat (vertico--metadata-get 'category)))
    ;; Only act on file completions.  CAT is the category of the whole
    ;; completion table, from its metadata:
    ;; - consult-recent-file, consult-locate, find-file: `file'.
    ;; - consult-buffer: `multi-category'.  It merges several sources
    ;;   (buffers, recent files, bookmarks, our favourites) into ONE
    ;;   table, and a table has only one category, so consult uses the
    ;;   placeholder `multi-category' and instead records each
    ;;   candidate's own source category in a text property, also named
    ;;   `multi-category', on the candidate string.  Its value is
    ;;   (CAT . DATA), e.g. (file . "~/x.el") or (buffer . "*scratch*").
    ;;   We read the property at index 0 and take its car; `car-safe'
    ;;   returns nil instead of erroring if a candidate lacks the
    ;;   property.  Embark uses the same property to pick its actions.
    (when (or (eq cat 'file)
              (and (eq cat 'multi-category)
                   (eq (car-safe (get-text-property 0 'multi-category cand))
                       'file)))
      ;; `case-fold-search' makes `string-match' ignore case, so
      ;; "c:/users/..." matches the entry "C:/Users/...".
      ;; END accumulates the longest match found so far (0 = none).
      (let ((case-fold-search t)
            (end 0))
        (dolist (prefix vj-file-dark-prefixes)
          ;; `string-match' returns the start index of the match, or nil.
          ;; Because the regexp is anchored with `\`' the start is always 0
          ;; when it matches; what we need is where the match ends, which
          ;; `match-end' reads from the match data that `string-match' just set.
          ;; Keep the largest end so "C:/Users/U418662/AppData/Roaming/"
          ;; wins over the shorter "C:/Users/U418662/".
          (when (and (string-match (vj-file-dark--regexp prefix) cand)
                     (> (match-end 0) end))
            (setq end (match-end 0))))
        ;; Prepend (APPEND arg nil) so this face takes priority over the
        ;; consult-file face that consult appended to the string.
        (when (> end 0)
          (add-face-text-property 0 end 'vj-file-dark nil cand))))
    args))

;;;###autoload
(defun vj-vertico-darken-file-prefix-enable ()
  "Enable darkening of matching file prefixes in Vertico completion."
  (interactive)
  (advice-add 'vertico--format-candidate :filter-args
              #'vj-vertico-darken-file-prefix))

;;;###autoload
(defun vj-vertico-darken-file-prefix-disable ()
  "Disable darkening of matching file prefixes in Vertico completion."
  (interactive)
  (advice-remove 'vertico--format-candidate
                 #'vj-vertico-darken-file-prefix))

;;;###autoload
(define-minor-mode vj-vertico-darken-file-prefix-mode
  "Toggle dimming of matching path prefixes in Vertico file candidates.
When enabled, literal prefixes from `vj-file-dark-prefixes' are dimmed."
  :global t
  :group 'vj-vertico-darken-file-prefix
  (if vj-vertico-darken-file-prefix-mode
      (vj-vertico-darken-file-prefix-enable)
    (vj-vertico-darken-file-prefix-disable)))

;; Enable by default for users who load this package intentionally.  This keeps
;; the behavior explicit while still making the package easy to use.
(vj-vertico-darken-file-prefix-enable)

(provide 'vj-vertico-darken-file-prefix)

;;; vj-vertico-darken-file-prefix.el ends here
