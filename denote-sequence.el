;;; denote-sequence.el --- Sequence notes or Folgezettel with Denote -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote-sequence
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (denote "4.0.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Sequence notes extension for Denote.  It uses the SIGNATURE file
;; name component of Denote to establish a hierarchy between notes.
;; As such, with the default numeric `denote-sequence-scheme', note
;; 1=1=2 is the second child of the first child of note 1.  While with
;; the alphanumeric scheme, note 1a2 is the equivalent.  The rest of
;; the Denote file naming scheme continues to apply as described in
;; the manual, as do all the other features of Denote.
;;
;; A new sequence note can be of the type `parent', `child', and
;; `sibling'.  For the convenience of the user, we provide commands to
;; create such "sequence notes", link only between them (as opposed to
;; a link to any other file with the Denote file-naminng scheme), and
;; re-parent them on demand, as well as display them in a Dired buffer
;; in accordance with their inherent order.
;;
;; All the relevant functions we provide take care to automatically
;; use the right number for a given sequence, per the user option
;; `denote-sequence-scheme'.  If, for example, we create a new child
;; for parent 1=1, we make sure that it is the new largest number
;; among any existing children, so if 1=1=1 already exists we use
;; 1=1=2, and so on.
;;
;; This optional extension is not necessary for such a workflow.
;; Users can always define whatever SIGNATURE they want manually.  The
;; purpose of this extension is to streamline that work.

;;; Code:

(require 'denote)

(defgroup denote-sequence ()
  "Sequence notes or Folgezettel with Denote."
  :group 'denote
  :link '(info-link "(denote) top")
  :link '(info-link "(denote-sequence) top")
  :link '(url-link :tag "Denote homepage" "https://protesilaos.com/emacs/denote")
  :link '(url-link :tag "Denote Sequence homepage" "https://protesilaos.com/emacs/denote-sequence"))

(defconst denote-sequence-schemes '(numeric alphanumeric alphanumeric-delimited)
  "The sequence scheme symbols supported by `denote-sequence-scheme'.")

;; TODO 2026-03-24: The `alphanumeric-delimited' is not supporting partial sequences.
;; This will probably be a problem for `denote-sequence-convert'.
(defcustom denote-sequence-scheme 'numeric
  "Sequence scheme to establish file hierarchies.
The value is a symbol among `numeric', `alphanumeric', and
`alphanumeric-delimited'.  Users can change the applicable scheme for
one file or those marked in Dired by calling the command
`denote-sequence-convert'.

Numeric sequences (the default) are the easier to understand but also
are the longest.  Each level of depth in the hierarchy is delimited by
an equals sign: the 1=1=2 thus refers to the second child of the first
child of parent 1.  Each level of depth can be a number of any length,
like 1=40=2=20.

Alphanumeric sequences are more compact than numeric ones.  Their depth
is derived via the alternation from numbers to latin characters, such
that 1a2 refers to the second child of the first child of parent 1.
Because they alternate between numbers and letters, they do not use the
equals sign.  When a number cannot be represented by a single letter,
two or more are used instead, such as the number 50 corresponding to
zx (z is 26 and x is 24).

Alphanumeric delimited sequences combine elements of the aforementioned.
Levels of depth are expressed as alternating numbers and letters, like
with the `alphanumeric' scheme, while they also get the = as a separator
as a visual aid for long sequences.  The sepator is inserted after the
first level of depth and then after every third level of depth, like
1=a2b=a1c.  Note that these are levels of depth, not triplets of letters
and numbers.  As such, 1=zx1zza=1 is valid because zx is one level of
depth as is zza, as noted above."
  :group 'denote-sequence
  :package-version '(denote . "0.3.0")
  :type '(choice (const :tag "Numeric like 1=1=2" numeric)
                 (const :tag "Alphanumeric like 1a2" alphanumeric)
                 (const :tag "Alphanumeric delimited like 1=a2b=a1c" alphanumeric-delimited)))

(defconst denote-sequence-numeric-regexp "=?[0-9]+"
  "Pattern of a numeric sequence.")

(defconst denote-sequence-alphanumeric-regexp "\\([0-9]+\\)\\([[:alpha:]]+\\)?"
  "Pattern of an alphanumeric sequence.")

(defconst denote-sequence-types '(parent child sibling)
  "Types of sequence.")

(defun denote-sequence-numeric-p (sequence)
  "Return SEQUENCE if it is numeric per `denote-sequence-scheme'."
  (when (and (string-match-p denote-sequence-numeric-regexp sequence)
             (not (string-match-p "[[:alpha:]]" sequence))
             (not (string-suffix-p "=" sequence)))
    sequence))

(defun denote-sequence-alphanumeric-p (sequence)
  "Return SEQUENCE if it is alphanumeric per `denote-sequence-scheme'."
  (when (and (string-match-p denote-sequence-alphanumeric-regexp sequence)
             (string-match-p "\\`[0-9]+" sequence)
             (not (string-match-p "=" sequence)))
    sequence))

(defun denote-sequence--alphanumeric-delimited-split (sequence)
  "Split SEQUENCE to test for the alphanumeric delimited scheme."
  (let ((start 0)
        (strings nil))
    (while (string-match "[0-9]+\\|[[:alpha:]]+\\|=" sequence start)
      (push (match-string 0 sequence) strings)
      (setq start (match-end 0)))
    (nreverse strings)))

(defun denote-sequence--alphanumeric-delimited-check-alternation (split-sequence)
  "Return non-nil if SPLIT-SEQUENCE alternates between numbers and letters.

SPLIT-SEQUENCE is an alphanumeric delimited sequence that is split into
separate strings at each level of depth, like this:

    (list \"1\" \"=\" \"a\" \"1\" \"b\" \"=\" \"2\" \"a\" \"1\")"
  (catch 'error
    (let ((last-type nil)
          (current-type nil))
      (dolist (string split-sequence)
        (cond
         ((string-match-p "\\`[0-9]+\\'" string)
          (setq current-type 'numeric))
         ((string-match-p "\\`[[:alpha:]]+\\'" string)
          (setq current-type 'alpha)))
        (unless (string= "=" string)
          (when (eq current-type last-type)
            (throw 'error nil))
          (setq last-type current-type))))
    t))

(defun denote-sequence--alphanumeric-delimited-check-depths (split-sequence)
  "Return non-nil if SPLIT-SEQUENCE is correctly delimited.
More specifically, return non-nil if there is 1 level of depth before
the first delimiter and then up to 3 for every subsequent delimiter.

SPLIT-SEQUENCE is an alphanumeric delimited sequence that is split into
separate strings at each level of depth, like this:

    (list \"1\" \"=\" \"a\" \"1\" \"b\" \"=\" \"2\" \"a\" \"1\")"
  (let ((levels-of-depth nil)
        (current-depth 0))
    (dolist (string split-sequence)
      (if (string= string "=")
          (progn
            (push current-depth levels-of-depth)
            (setq current-depth 0))
        (setq current-depth (+ current-depth 1))))
    (push current-depth levels-of-depth)
    (setq levels-of-depth (nreverse levels-of-depth))
    (catch 'error
      (let ((first-level t))
        (dolist (level levels-of-depth)
          (if first-level
              (progn
                (setq first-level nil)
                (unless (= level 1)
                  (throw 'error nil)))
            (unless (<= level 3)
              (throw 'error nil)))))
      (cond
       ((and (length> levels-of-depth 2)
             (= (car levels-of-depth) 1)
             (seq-every-p
              (lambda (level)
                (= level 3))
              (butlast (cdr levels-of-depth))))
        levels-of-depth)
       ((or (length= levels-of-depth 1)
            (length= levels-of-depth 2))
        levels-of-depth)
       (t
        nil)))))

(defun denote-sequence-alphanumeric-delimited-p (sequence)
  "Return SEQUENCE if it is an alphanumeric and delimited.
Refer to the `denote-sequence-scheme' for the details."
  (cond
   ((string-match-p "\\`[0-9]+\\'" sequence)
    sequence)
   (t
    (when (and (string-match-p "=" sequence)
               ;; TODO 2026-03-24: Probably this should not be here
               ;; due to how we end up with this check.  See, for
               ;; example, `denote-sequence-p' which already checks
               ;; for the numeric before it reaches this one.
               (not (denote-sequence-numeric-p sequence)))
      (let ((strings (denote-sequence--alphanumeric-delimited-split sequence)))
        (when (and (denote-sequence--alphanumeric-delimited-check-alternation strings)
                   (denote-sequence--alphanumeric-delimited-check-depths strings))
          sequence))))))

(defun denote-sequence-user-selected-scheme-p (sequence)
  "Return SEQUENCE if it is consistent with `denote-sequence-scheme'.
Also see `denote-sequence-alphanumeric-p' and `denote-sequence-numeric-p'."
  (pcase denote-sequence-scheme
    ('numeric (denote-sequence-numeric-p sequence))
    ('alphanumeric (denote-sequence-alphanumeric-p sequence))
    ('alphanumeric-delimited (denote-sequence-alphanumeric-delimited-p sequence))
    (_ (error "The sequence `%s' does not have a known scheme among `denote-sequence-schemes'" sequence))))

(defun denote-sequence-p (sequence)
  "Return SEQUENCE string is of a supported scheme.
Also see `denote-sequence-numeric-p' and `denote-sequence-alphanumeric-p'."
  (when (or (denote-sequence-numeric-p sequence)
            (denote-sequence-alphanumeric-p sequence)
            (denote-sequence-alphanumeric-delimited-p sequence))
    sequence))

(defun denote-sequence-with-error-p (sequence)
  "Return SEQUENCE string if it matches `denote-sequence-numeric-regexp'."
  (or (denote-sequence-p sequence)
      (error "The sequence `%s' does not pass `denote-sequence-p'" sequence)))

(defun denote-sequence--numeric-partial-p (string)
  "Return non-nil if STRING likely is part of a numeric sequence."
  (and (string-match-p "[0-9]+" string)
       (not (string-match-p "[[:alpha:][:punct:]]" string))))

(defun denote-sequence--alphanumeric-partial-p (string)
  "Return non-nil if STRING likely is part of an alphanumeric sequence."
  (and (string-match-p "[[:alpha:]]+" string)
       (not (string-match-p "[0-9[:punct:]]+" string))))

(defun denote-sequence--alphanumeric-delimited-partial-p (string)
  "Return non-nil if STRING likely is part of an alphanumeric delimited sequence."
  (or (denote-sequence--numeric-partial-p string)
      (denote-sequence--alphanumeric-partial-p string)))

(defun denote-sequence-and-scheme-p (sequence &optional partial)
  "Return the sequence scheme of SEQUENCE, per `denote-sequence-scheme'.
Return a cons cell of the form (sequence . scheme), where the `car' is
SEQUENCE and the `cdr' is its sequence scheme as a symbol among those
mentioned in `denote-sequence-scheme'.

With optional PARTIAL as a non-nil value, assume SEQUENCE to be a string
that only represents part of a sequence, which itself consists entirely
of numbers or letters.

Produce an error if the sequence scheme cannot be established."
  (cond
   ((and (not partial) (string-match-p "\\`[0-9]+\\'" sequence))
    (cons sequence denote-sequence-scheme))
   ((and (not partial)
         (not (string-match-p "[[:alpha:]]" sequence))
         (eq denote-sequence-scheme 'numeric))
    (cons sequence 'numeric))
   ((or (and partial (denote-sequence--alphanumeric-partial-p sequence))
        (denote-sequence-alphanumeric-p sequence))
    (cons sequence 'alphanumeric))
   ((or (and partial (denote-sequence--numeric-partial-p sequence))
        (denote-sequence-numeric-p sequence))
    (cons sequence 'numeric))
   ((or (and partial (denote-sequence--alphanumeric-delimited-partial-p sequence))
        (denote-sequence-alphanumeric-delimited-p sequence))
    (cons sequence 'alphanumeric-delimited))
   (t (error "The sequence `%s' does not pass `denote-sequence-and-scheme-p'" sequence))))

;; FIXME 2026-03-24: This is technically incorrect because it assumes
;; homogeneity of sequence schemes.  But we never enforce as much.
(defun denote-sequence--scheme-of-strings (strings)
  "Return the sequence scheme of STRINGS, per `denote-sequence-scheme'."
  (cond
   ((seq-every-p #'denote-sequence-numeric-p strings)
    'numeric)
   ((seq-every-p #'denote-sequence-alphanumeric-p strings)
    'alphanumeric)
   ((seq-every-p #'denote-sequence-alphanumeric-delimited-p strings)
    'alphanumeric-delimited)))

(defun denote-sequence-file-p (file)
  "Return the sequence if Denote signature of FILE is a sequence.
A sequence is string that conforms with `denote-sequence-p'."
  (when-let* ((signature (denote-retrieve-filename-signature file)))
    (denote-sequence-p signature)))

(defun denote-sequence-join (strings scheme)
  "Join STRINGS to form a sequence according to SCHEME.
SCHEME is a symbol among those mentioned in `denote-sequence-scheme'.
Return resulting sequence if it conforms with `denote-sequence-p'."
  (pcase scheme
    ('numeric (mapconcat #'identity strings "="))
    ('alphanumeric (apply #'concat strings))
    ('alphanumeric-delimited
     (let* ((result nil)
            (count 0))
       (while strings
         (push (car strings) result)
         (when (and (or (length= result 1)
                        (= (% count 3) 0))
                    (cdr strings))
           (push "=" result))
         (setq count (+ count 1))
         (setq strings (cdr strings)))
       (string-join (nreverse result))))))

;; FIXME 2026-03-23: I think this does not actually work with all sorts of partial sequences.
(defun denote-sequence-split (sequence &optional partial)
  "Split the SEQUENCE string into a list.
SEQUENCE conforms with `denote-sequence-p'.  If PARTIAL is non-nil, it
has the same meaning as in `denote-sequence-and-scheme-p'."
  (pcase-let* ((`(,sequence . ,scheme) (denote-sequence-and-scheme-p sequence partial)))
    (pcase scheme
      ('numeric
       (split-string sequence "=" t))
      ((or 'alphanumeric 'alphanumeric-delimited)
       (let ((strings nil)
             (start 0)
             (sequence-no-delimiters (replace-regexp-in-string "=" "" sequence)))
         (while (string-match denote-sequence-alphanumeric-regexp sequence-no-delimiters start)
           (push (match-string 1 sequence-no-delimiters) strings)
           (when-let* ((two (match-string 2 sequence-no-delimiters)))
             (push two strings)
             (setq start (match-end 2)))
           (setq start (match-end 1)))
         (if strings
             (nreverse strings)
           (split-string sequence-no-delimiters "" :omit-nulls)))))))

(defun denote-sequence--alpha-to-number (string)
  "Convert STRING of alphabetic characters to its numeric equivalent."
  (let* ((strings (denote-sequence-split string :partial))
         (numbers (mapcar
                   (lambda (string)
                     (let ((num (- (string-to-char string) 96)))
                       (cond
                        ((and (> num 0) (<= num 26))
                         num)
                        (t
                         (let ((times (/ num 26)))
                           (if-let* ((mod (% num 26))
                                     ((> mod 0))
                                     (suffix (+ mod 96)))
                               (list (* times 26) suffix)
                             (list (* times 26))))))))
                   strings)))
    (format "%s" (apply #'+ numbers))))

(defun denote-sequence--number-to-alpha (string)
  "Convert STRING of numbers to its alphabetic equivalent."
  (let ((num (string-to-number string)))
    (cond
     ((= num 0)
      (char-to-string (+ num 97)))
     ((and (> num 0) (<= num 26))
      (char-to-string (+ num 96)))
     (t
      (let ((times (/ num 26)))
        (if-let* ((mod (% num 26))
                  ((> mod 0))
                  (prefix (make-string times ?z))
                  (suffix (char-to-string (+ mod 96))))
            (concat prefix suffix)
          (make-string times ?z)))))))

(defun denote-sequence--alpha-to-number-complete (sequence)
  "Like `denote-sequence--alpha-to-number' but for the complete SEQUENCE.
If SEQUENCE conforms with `denote-sequence-numeric-p', return it as-is."
  (if (denote-sequence-numeric-p sequence)
      sequence
    (let* ((parts (denote-sequence-split sequence))
           (converted-parts (mapcar
                             (lambda (string)
                               (if (denote-sequence--numeric-partial-p string)
                                   string
                                 (denote-sequence--alpha-to-number string)))
                             parts)))
      (denote-sequence-join converted-parts 'numeric))))

(defun denote-sequence--number-to-alpha-complete (sequence target-scheme)
  "Like `denote-sequence--number-to-alpha' but for the complete SEQUENCE.
TARGET-SCHEME is either `alphanumeric' or `alphanumeric-delimited'.

If SEQUENCE conforms with `denote-sequence-alphanumeric-p', return it as-is."
  (unless (memq target-scheme '(alphanumeric alphanumeric-delimited))
    (error "The TARGET-SCHEME can only be `alphanumeric' or `alphanumeric-delimited'"))
  (if (denote-sequence-alphanumeric-p sequence)
      sequence
    (let* ((parts (denote-sequence-split sequence))
           (odd-is-numeric 0)
           (converted-parts (mapcar
                             (lambda (string)
                               (setq odd-is-numeric (+ odd-is-numeric 1))
                               (cond
                                ((= (% odd-is-numeric 2) 1)
                                 string)
                                ((denote-sequence--alphanumeric-partial-p string)
                                 string)
                                (t
                                 (denote-sequence--number-to-alpha string))))
                             parts)))
      (denote-sequence-join converted-parts target-scheme))))

(defun denote-sequence-make-conversion (string target-scheme &optional string-is-partial-sequence)
  "Convert STRING to the given sequence TARGET-SCHEME.
With optional STRING-IS-PARTIAL-SEQUENCE interpret STRING accordingly."
  (unless (memq target-scheme denote-sequence-schemes)
    (error "The TARGET-SCHEME can only be one among the `denote-sequence-schemes'"))
  (cond
   (string-is-partial-sequence
    (if (eq target-scheme 'numeric)
        (denote-sequence--alpha-to-number string)
      (denote-sequence--number-to-alpha string)))
   ((eq target-scheme 'numeric)
    (denote-sequence--alpha-to-number-complete string))
   (t
    (denote-sequence--number-to-alpha-complete string target-scheme))))

(define-obsolete-function-alias
  'denote-sequence-increment
  'denote-sequence-increment-partial
  "0.2.0")

(defun denote-sequence-increment-partial (string)
  "Increment number represented by STRING and return it as a string.
STRING is part of a sequence, not the entirety of it.

Also see `denote-sequence-decrement-partial'."
  (cond
   ((denote-sequence--numeric-partial-p string)
    (number-to-string (+ (string-to-number string) 1)))
   ((denote-sequence--alphanumeric-partial-p string)
    (let* ((letters (split-string string "" :omit-nulls))
           (length-1-p (= (length letters) 1))
           (first (car letters))
           (reverse (nreverse (copy-sequence letters)))
           (last (car reverse)))
      (cond
       ((and length-1-p (string= "z" first))
        "za")
       (length-1-p
        (char-to-string (+ (string-to-char first) 1)))
       ((string= "z" last)
        (apply #'concat (append letters (list "a"))))
       (t
        (apply #'concat
               (append (butlast letters)
                       (list (char-to-string (+ (string-to-char last) 1)))))))))
   (t
    (error "The string `%s' must contain only numbers or letters" string))))

(defun denote-sequence-decrement-partial (string)
  "Decrement number represented by STRING and return it as a string.
STRING is part of a sequence, not the entirety of it.

Also see `denote-sequence-increment-partial'."
  (cond
   ((denote-sequence--numeric-partial-p string)
    (let ((number (string-to-number string)))
      (unless (= number 1)
        (number-to-string (- number 1)))))
   ((denote-sequence--alphanumeric-partial-p string)
    (let* ((letters (split-string string "" :omit-nulls))
           (length-1-p (= (length letters) 1))
           (first (car letters))
           (reverse (nreverse (copy-sequence letters)))
           (last (car reverse)))
      (cond
       ((and length-1-p (string= "a" first))
        nil)
       (length-1-p
        (char-to-string (- (string-to-char first) 1)))
       ((string= "a" last)
        (apply #'concat (butlast letters)))
       (t
        (apply #'concat
               (append (butlast letters)
                       (list (char-to-string (- (string-to-char last) 1)))))))))
   (t
    (error "The string `%s' must contain only numbers or letters" string))))

(defun denote-sequence-depth (sequence)
  "Get the depth of SEQUENCE.
For example, 1=2=1 and 1b1 are three levels of depth."
  (length (denote-sequence-split sequence)))

(defun denote-sequence--children-implied-p (sequence)
  "Return non-nil if SEQUENCE implies children.
This does not actually check if there are children in the variable
`denote-directory', but only that SEQUENCE is greater than 1."
  (> (denote-sequence-depth sequence) 1))

(defun denote-sequence--infer-parent (sequence)
  "Return implied parent of SEQUENCE, else nil.
Produce an error if SEQUENCE does not conform with `denote-sequence-p'.
The implied check here has the same meaning as described in
`denote-sequence--children-implied-p'."
  (pcase-let* ((`(,sequence . ,scheme) (denote-sequence-and-scheme-p sequence)))
    (when (and (denote-sequence-with-error-p sequence)
               (denote-sequence--children-implied-p sequence))
      (let ((strings (thread-last
                       (denote-sequence-split sequence)
                       (butlast))))
        (denote-sequence-join strings scheme)))))

(defun denote-sequence--infer-child (sequence)
  "Get likely child of SEQUENCE.
Do not actually try to create a new child, as that is the duty of
`denote-sequence--get-new-child'.  Instead return a greater level of
depth given SEQUENCE."
  (pcase-let* ((`(,sequence . ,scheme) (denote-sequence-and-scheme-p sequence))
               (components (denote-sequence-split sequence))
               (last-component (car (nreverse components)))
               (new-depth (cond
                           ((eq scheme 'numeric) "1")
                           ((denote-sequence--numeric-partial-p last-component) "a")
                           ((denote-sequence--alphanumeric-partial-p last-component) "1")
                           (t (error "Unknown type of sequence for `%s'" last-component)))))
    (denote-sequence-join (list sequence new-depth) scheme)))

(defun denote-sequence--infer-sibling (sequence direction)
  "Get sibling of SEQUENCE in DIRECTION `next' or `previous'.
Do not actually try to create a new sibling nor to test for the
existence of one.  Simply do the work of finding the next or previous
sibling in the sequence."
  (pcase-let* ((`(,sequence . ,scheme) (denote-sequence-and-scheme-p sequence))
               (components (denote-sequence-split sequence))
               (butlast (butlast components))
               (last-component (car (nreverse components)))
               (direction-fn (pcase direction
                               ('next #'denote-sequence-increment-partial)
                               ('previous #'denote-sequence-decrement-partial)
                               (_ (error "Unknown DIRECTION `%s'" direction))))
               (new-number (funcall direction-fn last-component)))
    (when new-number
      (denote-sequence-join (append butlast (list new-number)) scheme))))

(defun denote-sequence--get-files (files)
  "Return list of FILES plus any buffers in the variable `denote-directory'."
  (delete-dups (append (denote--buffer-file-names) files)))

(defun denote-sequence-get-all-files (&optional files as-sequence-path-pairs)
  "Return all files in variable `denote-directory' with a sequence.
A sequence is a Denote signature that conforms with `denote-sequence-p'.

With optional FILES consider only those, otherwise use the return value
of `denote-directory-files'.

With optional AS-SEQUENCE-PATH-PAIRS return all files as a list of pairs
each in the form of (SEQUENCE . PATH).  Otherwise, return a list of
strings each representing a file system path."
  (when-let* ((files (denote-sequence--get-files (or files (denote-directory-files)))))
    (if as-sequence-path-pairs
        (delq nil
              (mapcar
               (lambda (file)
                 (when (denote-sequence-file-p file)
                   (cons
                    (denote-retrieve-filename-signature file)
                    file)))
               files))
      (seq-filter #'denote-sequence-file-p files))))

(defun denote-sequence-get-path (sequence &optional files)
  "Return absolute path of file with SEQUENCE.
Search in the return value of `denote-sequence-get-all-files' or in FILES."
  (let ((files
         (seq-filter
          (lambda (file)
            (string= sequence (denote-retrieve-filename-signature file)))
          (denote-sequence-get-all-files files))))
    (if (length< files 2)
        (car files)
      (seq-find
       (lambda (file)
         (let ((file-extension (denote-get-file-extension-sans-encryption file)))
           (and (denote-file-has-supported-extension-p file)
                (or (string= (denote--file-extension denote-file-type)
                             file-extension)
                    (string= ".org" file-extension)
                    (member file-extension (denote-file-type-extensions))))))
       files))))

(defun denote-sequence--sequence-prefix-p (prefix sequence)
  "Return non-nil if SEQUENCE has prefix sequence PREFIX.

SEQUENCE is a Denote signatures that conforms with `denote-sequence-p'.
PREFIX is a list of strings containing the components of the prefix
sequence, as is returned by `denote-sequence-split'.

If PREFIX is nil, return non-nil as if the SEQUENCE has PREFIX."
  (when (denote-sequence-user-selected-scheme-p sequence)
    (let ((value (denote-sequence-split sequence))
          (depth (length prefix))
          (matched 0))
      (while (and value
                  (< matched depth)
                  (string-equal (pop value) (nth matched prefix)))
        (setq matched (1+ matched)))
      (= matched depth))))

(defun denote-sequence-get-all-files-with-prefix (sequence &optional files)
  "Return all files in variable `denote-directory' with prefix SEQUENCE.
A sequence is a Denote signature that conforms with `denote-sequence-p'.

With optional FILES, operate on them, else use the return value of
`denote-directory-files'."
  (when-let* (((not (string-empty-p sequence)))
              (prefix (denote-sequence-split sequence)))
    (seq-filter
     (lambda (file)
       (when-let* ((file-sequence (denote-sequence-file-p file)))
         (denote-sequence--sequence-prefix-p prefix file-sequence)))
     (denote-sequence-get-all-files files))))

(defun denote-sequence-get-all-files-with-max-depth (depth &optional files)
  "Return all files with sequence depth up to DEPTH (inclusive).
With optional FILES, operate on them, else use the return value of
`denote-sequence-get-all-files'."
  (delq nil
        (mapcar
         (lambda (file)
           (when-let* ((sequence (denote-retrieve-filename-signature file))
                       (components (denote-sequence-split sequence))
                       ((>= depth (length components))))
             file))
         (or files (denote-sequence-get-all-files)))))

(defun denote-sequence-get-all-sequences (&optional files)
  "Return all sequences in `denote-directory-files'.
With optional FILES return all sequences among them instead.

A sequence is a Denote signature that conforms with `denote-sequence-p'."
  (delq nil (mapcar #'denote-sequence-file-p (denote-sequence-get-all-files files))))

(defun denote-sequence-get-all-sequences-with-prefix (sequence &optional sequences)
  "Get all sequences which extend SEQUENCE.
With optional SEQUENCES operate on those, else use the return value of
`denote-sequence-get-all-sequences'.

A sequence is a Denote signature that conforms with `denote-sequence-p'."
  (when-let* (((not (string-empty-p sequence)))
              (prefix (denote-sequence-split sequence)))
    (seq-filter
     (lambda (string)
       (denote-sequence--sequence-prefix-p prefix string))
     (or sequences (denote-sequence-get-all-sequences)))))

(defun denote-sequence-get-all-sequences-with-max-depth (depth &optional sequences)
  "Get sequences up to DEPTH (inclusive).
With optional SEQUENCES operate on those, else use the return value of
`denote-sequence-get-all-sequences'."
  (let* ((strings (or sequences (denote-sequence-get-all-sequences)))
         (lists-all (mapcar #'denote-sequence-split strings))
         (lists (seq-filter (lambda (element) (>= depth (length element))) lists-all)))
    (delete-dups
     (mapcar
      (lambda (strings)
        (denote-sequence-join
         (seq-take strings depth)
         denote-sequence-scheme))
      lists))))

(defun denote-sequence--pad (sequence type)
  "Create a new SEQUENCE with padded spaces for TYPE.
TYPE is a symbol among `denote-sequence-types'.  The special TYPE `all'
means to pad the full length of the sequence."
  (let* ((sequence-separator-p (denote-sequence--children-implied-p sequence))
         (split (denote-sequence-split sequence))
         (s (cond
             ((eq type 'all) split)
             (sequence-separator-p
              (pcase type
                ('parent (car split))
                ('sibling split)
                ('child (car (nreverse split)))
                (_ (error "The type `%s' is not among `denote-sequence-types'" type))))
             (t sequence))))
    (if (listp s)
        (combine-and-quote-strings
         (mapcar
          (lambda (part)
            (string-pad part 5 32 :pad-from-start))
          s)
         "=")
      (string-pad s 32 32 :pad-from-start))))

(defun denote-sequence-sort-sequences (sequences)
  "Sort SEQUENCES according to their sequence.
Also see `denote-sequence-sort-files'."
  (sort
   sequences
   (lambda (sequence1 sequence2)
     (string<
      (denote-sequence--pad sequence1 'all)
      (denote-sequence--pad sequence2 'all)))))

(defun denote-sequence--file-smaller-p (file1 file2)
  "Return non-nil if FILE1 has a smaller sequence than FILE2."
  (let ((sequence1 (denote-retrieve-filename-signature file1))
        (sequence2 (denote-retrieve-filename-signature file2)))
    (string<
     (denote-sequence--pad sequence1 'all)
     (denote-sequence--pad sequence2 'all))))

(defun denote-sequence-sort-files (files-with-sequence)
  "Sort FILES-WITH-SEQUENCE according to their sequence.
Also see `denote-sequence-sort-sequences'."
  (sort files-with-sequence #'denote-sequence--file-smaller-p))

(defun denote-sequence--get-largest-by-order (sequences type)
  "Sort SEQUENCES of TYPE to get largest in order, using `denote-sequence--pad'."
  (car
   (reverse
    (sort
     sequences
     (lambda (sequence1 sequence2)
       (string<
        (denote-sequence--pad sequence1 type)
        (denote-sequence--pad sequence2 type)))))))

(defun denote-sequence--string-length-sans-delimiter (string)
  "Return length of STRING without the equals sign."
  (if (memq denote-sequence-scheme '(numeric alphanumeric-delimited))
      (length (replace-regexp-in-string "=" "" string))
    (length string)))

(defun denote-sequence--get-largest-by-length (sequences)
  "Compare length of SEQUENCES to determine the largest among them.
If there are more than one sequences of equal length, return them."
  (let* ((seqs-with-length (mapcar (lambda (sequence)
                                     (cons (denote-sequence--string-length-sans-delimiter sequence) sequence))
                                   sequences))
         (longest (apply #'max (mapcar #'car seqs-with-length)))
         (largest-sequence (delq nil
                                 (mapcar (lambda (element)
                                           (unless (< (car element) longest)
                                             (cdr element)))
                                         seqs-with-length))))
    (if (= (length largest-sequence) 1)
        (car largest-sequence)
      largest-sequence)))

(defun denote-sequence--get-largest (sequences type)
  "Return largest sequence in SEQUENCES given TYPE.
TYPE is a symbol among `denote-sequence-types'."
  (if (eq type 'child)
      (let ((largest (denote-sequence--get-largest-by-length sequences)))
        (if (listp largest)
            (denote-sequence--get-largest-by-order largest type)
          largest))
    (denote-sequence--get-largest-by-order sequences type)))

(defun denote-sequence--get-start (&optional sequence)
  "Return the start of a new sequence.
With optional SEQUENCE, do so based on the final level of depth therein.
This is usefule only for the alphanumeric `denote-sequence-scheme'."
  ;; TODO 2026-04-03: Rewrite this for clarity.
  (pcase denote-sequence-scheme
    ('numeric "1")
    ((or 'alphanumeric 'alphanumeric-delimited)
     (cond
      ((null sequence) "1")
      ((and sequence (denote-sequence--alphanumeric-partial-p (substring sequence -1))) "1")
      (t "a")))))

(defun denote-sequence--get-new-parent (&optional sequences)
  "Return a new to increment largest among sequences.
With optional SEQUENCES consider only those, otherwise operate on the
return value of `denote-sequence-get-all-sequences'."
  (if-let* ((all (or sequences (denote-sequence-get-all-sequences))))
      (let* ((largest (denote-sequence--get-largest all 'parent))
             (first-component (car (denote-sequence-split largest)))
             (current-number (string-to-number first-component)))
        (number-to-string (+ current-number 1)))
    (denote-sequence--get-start)))

(defun denote-sequence-filter-scheme (sequences &optional scheme)
  "Return list of SEQUENCES that are `denote-sequence-scheme' or SCHEME."
  (let ((predicate (pcase (or scheme denote-sequence-scheme)
                     ('alphanumeric #'denote-sequence-alphanumeric-p)
                     ('numeric #'denote-sequence-numeric-p)
                     ('alphanumeric-delimited #'denote-sequence-alphanumeric-delimited-p))))
    (seq-filter predicate sequences)))

(defun denote-sequence--get-new-child (sequence &optional sequences)
  "Return a new child of SEQUENCE.
Optional SEQUENCES has the same meaning as that specified in the
function `denote-sequence-get-all-sequences-with-prefix'."
  (if-let* ((depth (+ (denote-sequence-depth sequence) 1))
            (all-unfiltered (denote-sequence-get-all-sequences-with-prefix sequence sequences))
            (start-child (denote-sequence--get-start sequence)))
      (if (= (length all-unfiltered) 1)
          (denote-sequence-join (append (denote-sequence-split sequence) (list start-child)) denote-sequence-scheme)
        (if-let* ((all-schemeless (cond
                                   ((denote-sequence-get-all-sequences-with-max-depth depth all-unfiltered))
                                   (t all-unfiltered)))
                  (all (denote-sequence-filter-scheme all-schemeless))
                  (largest (denote-sequence--get-largest all 'child)))
            (if (denote-sequence--children-implied-p largest)
                (pcase-let* ((`(,largest . ,scheme) (denote-sequence-and-scheme-p largest))
                             (components (denote-sequence-split largest))
                             (butlast (butlast components))
                             (last-component (car (nreverse components)))
                             (new-number (denote-sequence-increment-partial last-component)))
                  (denote-sequence-join
                   (if butlast
                       (append butlast (list new-number))
                     (list largest new-number))
                   scheme))
              (denote-sequence-join (append (denote-sequence-split largest) (list start-child)) denote-sequence-scheme))
          (denote-sequence-join (append (denote-sequence-split sequence) (list start-child)) denote-sequence-scheme)))
    (error "Cannot find sequences given sequence `%s' using scheme `%s'" sequence denote-sequence-scheme)))

(defun denote-sequence--get-prefix-for-siblings (sequence)
  "Get the prefix of SEQUENCE such that it is possible to find its siblings."
  (pcase-let ((`(,sequence . ,scheme) (denote-sequence-and-scheme-p sequence)))
    (when (denote-sequence--children-implied-p sequence)
      (denote-sequence-join (butlast (denote-sequence-split sequence)) scheme))))

(defun denote-sequence--get-new-sibling (sequence &optional sequences)
  "Return a new sibling of SEQUENCE.
Optional SEQUENCES has the same meaning as that specified in the
function `denote-sequence-get-all-sequences-with-prefix'."
  (let* ((children-p (denote-sequence--children-implied-p sequence)))
    (if-let* ((depth (denote-sequence-depth sequence))
              (all-unfiltered (if children-p
                                  (denote-sequence-get-all-sequences-with-prefix
                                   (denote-sequence--get-prefix-for-siblings sequence)
                                   sequences)
                                (denote-sequence-get-all-sequences)))
              (all-schemeless (denote-sequence-get-all-sequences-with-max-depth depth all-unfiltered))
              (all (denote-sequence-filter-scheme all-schemeless))
              ((member sequence all))
              (largest (if children-p
                           (denote-sequence--get-largest all 'sibling)
                         (denote-sequence--get-largest all 'parent))))
        (if children-p
            (pcase-let* ((`(,largest . ,scheme) (denote-sequence-and-scheme-p largest))
                         (components (denote-sequence-split largest))
                         (butlast (butlast components))
                         (last-component (car (nreverse components)))
                         (new-number (denote-sequence-increment-partial last-component)))
              (denote-sequence-join (append butlast (list new-number)) scheme))
          (denote-sequence-join (list (number-to-string (+ (string-to-number largest) 1))) denote-sequence-scheme))
      (error "Cannot find sequences given sequence `%s' using scheme `%s'" sequence denote-sequence-scheme))))

(defun denote-sequence-get-new (type &optional sequence sequences)
  "Return a sequence given TYPE among `denote-sequence-types'.
If TYPE is either `child' or `sibling', then optional SEQUENCE must be
non-nil and conform with `denote-sequence-p'.

With optional SEQUENCES consider only those, otherwise operate on the
return value of `denote-sequence-get-all-sequences'."
  (pcase type
    ('parent (denote-sequence--get-new-parent sequences))
    ('child (denote-sequence--get-new-child sequence sequences))
    ('sibling (denote-sequence--get-new-sibling sequence sequences))
    (_ (error "The type `%s' is not among `denote-sequence-types'" type))))

(defun denote-sequence-get-relative (sequence type &optional files)
  "Get files of TYPE given the SEQUENCE.
With optional FILES consider only those, otherwise operate on all files
returned by `denote-sequence-get-all-files'."
  (let* ((depth (denote-sequence-depth sequence))
         (scheme (cdr (denote-sequence-and-scheme-p sequence)))
         (components (denote-sequence-split sequence))
         (filter-common (lambda (comparison prefix)
                          (seq-filter
                           (lambda (file)
                             (funcall comparison (denote-sequence-depth (denote-retrieve-filename-signature file)) depth))
                           (denote-sequence-get-all-files-with-prefix prefix files)))))
    (pcase type
      ('all-parents
       (let ((butlast (butlast components))
             (found-files (denote-sequence-get-all-files files))
             (likely-parents nil))
         (while (>= (length butlast) 1)
           (push (denote-sequence-join butlast scheme) likely-parents)
           (setq butlast (butlast butlast)))
         (seq-filter
          (lambda (file)
            (member (denote-retrieve-filename-signature file) likely-parents))
          found-files)))
      ('parent
       (let ((butlast (denote-sequence-join (butlast components) scheme)))
         (seq-find
          (lambda (file)
            (string= (denote-retrieve-filename-signature file) butlast))
          (denote-sequence-get-all-files files))))
      ('siblings
       (when-let* ((siblings (funcall filter-common '= (denote-sequence-join (butlast components) scheme)))
                   (current-path (denote-sequence-get-path sequence)))
         (delete current-path siblings)))
      ('all-children
       (funcall filter-common '> sequence))
      ('children
       (seq-filter
        (lambda (file)
          (= (denote-sequence-depth (denote-sequence-file-p file)) (+ depth 1)))
        (funcall filter-common '> sequence)))
      (_ (error "The type `%s' is not among the allowed types" type)))))

(defvar denote-sequence-type-history nil
  "Minibuffer history of `denote-sequence-type-prompt'.")

(defun denote-sequence-annotate-types (type)
  "Annotate completion candidate of TYPE for `denote-sequence-type-prompt'."
  (when-let* ((text (pcase type
                      ("parent" "Parent sequence")
                      ("sibling" "Sibling of another sequence")
                      ("child" "Child of another sequence"))))
    (format "%s-- %s"
            (propertize " " 'display '(space :align-to 10))
            (propertize text 'face 'completions-annotations))))

(defun denote-sequence-type-prompt (&optional prompt-text types annotation-fn)
  "Prompt for sequence type among `denote-sequence-types'.
Return selected type as a symbol.

With optional PROMPT-TEXT use it instead of the generic prompt.

With optional TYPES use those instead of the `denote-sequence-types'.

With optional ANNOTATION-FN use it to annotate the completion candidates
instead of the default `denote-sequence-annotate-types'."
  (let ((default (car denote-sequence-type-history))
        (completion-extra-properties
         (list :annotation-function (or annotation-fn #'denote-sequence-annotate-types))))
    (intern
     (completing-read
      (format-prompt (or prompt-text "Select sequence type") default)
      (denote-get-completion-table (or types denote-sequence-types) '(category . denote-sequence-type))
      nil t nil 'denote-sequence-type-history default))))

(defun denote-sequence-file-prompt-affixate (files)
  "Affixate FILES.
Use the identifier as a prefix, the keywords as a suffix, and the title
as the text of the candidate.

Include in the text of the candidate the file extesion.  A group
function can remove it, such as with `denote-file-prompt-group'."
  (mapcar
   (lambda (file)
     (let ((sequence (denote-retrieve-filename-signature file)))
       (list
        file
        (format "%s " (propertize sequence 'face 'completions-annotations))
        (format " %s%s"
                (if (eq completions-format 'one-column)
                    (propertize " " 'display '(space :align-to 90))
                  " ")
                (propertize (or (denote-retrieve-filename-keywords file) "") 'face 'completions-annotations)))))
   files))

(defvar denote-sequence-file-prompt-extra-metadata
  (list
   ;; NOTE 2025-12-15: If we use the `file' category, then we are
   ;; subject to the `completion-category-overrides'.  This is a
   ;; problem because the user will want to, for example, sort
   ;; directories before files, but then we cannot have our sort here.
   (cons 'category 'denote-file)
   (cons 'group-function #'denote-file-prompt-group)
   (cons 'affixation-function #'denote-sequence-file-prompt-affixate)
   (cons 'display-sort-function #'denote-sequence-sort-files))
  "Extra `completion-metadata' for the `denote-file-prompt'.
This is in addition to the completion category, which is constant.")

(defvar denote-sequence-file-history nil
  "Minibuffer history for `denote-sequence-file-prompt'.")

(defun denote-sequence-file-prompt (&optional prompt-text files-with-sequences)
  "Prompt for file with sequence in variable `denote-directory'.
A sequence is a Denote signature that conforms with `denote-sequence-p'.

With optional PROMPT-TEXT use it instead of a generic prompt.

With optional FILES-WITH-SEQUENCES as a list of strings, use them as
completion candidates.  Else use `denote-sequence-get-all-files'."
  (let* ((roots (denote-directories))
         (single-dir-p (null (cdr roots)))
         ;; Some external program may use `default-directory' with the
         ;; relative file paths of the completion candidates.
         (default-directory (if single-dir-p
                                (car roots)
                              (denote-directories-get-common-root))))
    (if-let* ((files (or files-with-sequences (denote-sequence-get-all-files)))
              (relative-files (if single-dir-p
                                  (mapcar #'denote-get-file-name-relative-to-denote-directory files)
                                files))
              (prompt (format-prompt (or prompt-text "Select FILE with sequence") nil))
              (input (completing-read
                      prompt
                      (apply 'denote-get-completion-table relative-files denote-sequence-file-prompt-extra-metadata)
                      nil t nil 'denote-sequence-file-history)))
        (if single-dir-p
            (expand-file-name input default-directory)
          input)
      (error "There are no sequence notes in the `denote-directory'"))))

;;;###autoload
(defun denote-sequence (type &optional file-with-sequence)
  "Create a new sequence note of TYPE among `denote-sequence-types'.
If TYPE is either `child' or `sibling', then it is an extension of
FILE-WITH-SEQUENCE.

When called interactively, prompt for TYPE and, when necessary, for
FILE-WITH-SEQUENCE whose sequence will be used to derive a new sequence.
Files available at the minibuffer prompt are those returned by
`denote-sequence-get-all-files'."
  (interactive
   (let ((selected-type (denote-sequence-type-prompt)))
     (list
      selected-type
      (when (memq selected-type (delq 'parent denote-sequence-types))
        (denote-sequence-file-prompt (format "Make a new %s of SEQUENCE" selected-type))))))
  (let* ((sequence (when file-with-sequence (denote-retrieve-filename-signature file-with-sequence)))
         (new-sequence (denote-sequence-get-new type sequence))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

;;;###autoload
(defun denote-sequence-new-parent ()
  "Like `denote-sequence' to directly create new parent."
  (interactive)
  (let* ((new-sequence (denote-sequence-get-new 'parent))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

;;;###autoload
(defun denote-sequence-new-sibling (sequence)
  "Like `denote-sequence' to directly create new sibling of SEQUENCE.
When called interactively, SEQUENCE is a file among files in the variable
`denote-directory' that have a sequence (per `denote-sequence-file-p').

When called from Lisp, SEQUENCE is a string that conforms with
`denote-sequence-p'."
  (interactive
   (list
    (denote-retrieve-filename-signature
     (denote-sequence-file-prompt "Make a new sibling of SEQUENCE"))))
  (let* ((new-sequence (denote-sequence-get-new 'sibling sequence))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

(defun denote-sequence--get-current-sequence-or-prompt (prompt-text)
  "Get the current sequence or prompt for it with PROMPT-TEXT.
Try to get the current sequence from the file at point in Dired, from the
current Denote sequence file buffer, or at point in the
buffer produced by the command `denote-sequence-view-hierarchy'.

If those fail, prompt for a file."
  (cond
   ((when-let* (((derived-mode-p 'dired-mode))
                (file-at-point (dired-get-filename nil t)))
      (denote-sequence-file-p file-at-point)))
   ((and buffer-file-name (denote-sequence-file-p buffer-file-name)))
   ((derived-mode-p 'denote-sequence-hierarchy-mode)
    (when-let* ((file (get-text-property (point) 'denote-sequence-hierarchy-file)))
      (denote-sequence-file-p file)))
   (t
    (denote-retrieve-filename-signature (denote-sequence-file-prompt prompt-text)))))

;;;###autoload
(defun denote-sequence-new-sibling-of-current (sequence)
  "Create a new sibling sequence of the current file with SEQUENCE.
If the current file does not have a sequence, then behave exactly like
`denote-sequence-new-sibling'."
  (interactive (list (denote-sequence--get-current-sequence-or-prompt "Make a new sibling of SEQUENCE")))
  (let* ((new-sequence (denote-sequence-get-new 'sibling sequence))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

;;;###autoload
(defun denote-sequence-new-child (sequence)
  "Like `denote-sequence' to directly create new child of SEQUENCE.
When called interactively, SEQUENCE is a file among files in the variable
`denote-directory' that have a sequence (per `denote-sequence-file-p').

When called from Lisp, SEQUENCE is a string that conforms with
`denote-sequence-p'."
  (interactive
   (list
    (denote-retrieve-filename-signature
     (denote-sequence-file-prompt "Make a new child of SEQUENCE"))))
  (let* ((new-sequence (denote-sequence-get-new 'child sequence))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

;;;###autoload
(defun denote-sequence-new-child-of-current (sequence)
  "Create a new child sequence of the current file with SEQUENCE.
If the current file does not have a sequence, then behave exactly like
`denote-sequence-new-child'."
  (interactive (list (denote-sequence--get-current-sequence-or-prompt "Make a new child of SEQUENCE")))
  (let* ((new-sequence (denote-sequence-get-new 'child sequence))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

(defun denote-sequence--keep-siblings (lesser-or-greater sequence sequences)
  "Return LESSER-OR-GREATER sequences of SEQUENCE among SEQUENCES.
LESSER-OR-GREATER is the keyword `:lesser' or `:greater'.  SEQUENCES are
siblings of SEQUENCE."
  (let* ((sequences-copy (copy-sequence sequences))
         (all-sequences (delete-dups (push sequence sequences-copy)))
         (sorted (denote-sequence-sort-sequences all-sequences))
         (position (seq-position sorted sequence #'string=)))
    (pcase lesser-or-greater
      (:lesser (seq-take sorted position))
      (:greater (nthcdr (+ position 1) sorted))
      (_ (error "The `%S' is not a known operation" lesser-or-greater)))))

;; NOTE 2025-09-05: The `denote-sequence--keep-sibling-files' will
;; always return :greater if the phony-target is part of the
;; files-with-sequences and is in the last position.  More generally,
;; the :greater lists the phony target if it already is a part of
;; files-with-sequences.  The way we use this function for the
;; `denote-sequence-find-next-sibling' should not be a problem, but we
;; might want to be more strict if you use this elsewhere.
(defun denote-sequence--keep-sibling-files (lesser-or-greater sequence files-with-sequences)
  "Return LESSER-OR-GREATER sequences of SEQUENCE among FILES-WITH-SEQUENCES.
LESSER-OR-GREATER is the keyword `:lesser' or `:greater'.
FILES-WITH-SEQUENCES are siblings of SEQUENCE."
  (if-let* ((phony-target (denote-format-file-name (car (denote-directories)) "00000000T000000" '("keyword") "title" ".org" sequence))
            (_ (denote-sequence-file-p phony-target)))
      (let* ((files-with-sequences-copy (copy-sequence files-with-sequences))
             (all-sequences (delete-dups (push phony-target files-with-sequences-copy)))
             (sorted (denote-sequence-sort-files all-sequences))
             (position (seq-position sorted phony-target #'string=)))
        (pcase lesser-or-greater
          (:lesser (seq-take sorted position))
          (:greater (nthcdr (+ position 1) sorted))
          (_ (error "The `%S' is not a known operation" lesser-or-greater))))
    (error "Cannot have a file path that satisfies `denote-sequence-file-p' while using sequence `%s'" sequence)))

(defun denote-sequence-find-next-prev-sibling-subr (next-or-previous sequence relatives)
  "Find next or previous sibling.
Do the work for `denote-sequence-find-next-sibling' and
`denote-sequence-find-previous-sibling'.  The NEXT-OR-PREVIOUS is the
direction to move towards.  It is the symbol `next' or `previous'.
SEQUENCE is the one to find siblings for.  RELATIVES is a list of files
that are already known to pertain to SEQUENCE."
  (let ((relatives (or relatives (denote-sequence-get-relative sequence 'siblings))))
    (if-let* ((_ relatives)
              (next-in-line (denote-sequence--infer-sibling sequence next-or-previous))
              (path (denote-sequence-get-path next-in-line relatives)))
        (find-file path)
      (if-let* ((_ next-in-line)
                (lesser-or-greater (if (eq next-or-previous 'next) :greater :lesser))
                (remaining-siblings (denote-sequence--keep-sibling-files lesser-or-greater next-in-line relatives)))
          (denote-sequence-find-next-prev-sibling-subr next-or-previous next-in-line remaining-siblings)
        (user-error "No `%s' sibling for sequence `%s'" next-or-previous sequence)))))

;;;###autoload
(defun denote-sequence-find-next-sibling (sequence relatives)
  "Visit the next sibling of file with SEQUENCE.
When called from Lisp RELATIVES is the list of files to search through.
In interactive use, this happens internally when an immediate next
sibling is not available and the search needs to be repeated."
  (interactive (list (denote-sequence--get-current-sequence-or-prompt "Make a new sibling of SEQUENCE") nil))
  (denote-sequence-find-next-prev-sibling-subr 'next sequence relatives))

;;;###autoload
(defun denote-sequence-find-previous-sibling (sequence relatives)
  "Visit the previous sibling of file with SEQUENCE.
When called from Lisp RELATIVES is the list of files to search through.
In interactive use, this happens internally when an immediate previous
sibling is not available and the search needs to be repeated."
  (interactive (list (denote-sequence--get-current-sequence-or-prompt "Make a new sibling of SEQUENCE") nil))
  (denote-sequence-find-next-prev-sibling-subr 'previous sequence relatives))

(defvar denote-sequence-relative-types
  '(all-parents parent siblings children all-children)
  "Types of sequence relatives.")

(defun denote-sequence-annotate-relative-types (type)
  "Annotate completion candidate of TYPE for `denote-sequence-type-prompt'."
  (when-let* ((text (pcase type
                      ("all-parents" "All parent sequences")
                      ("parent" "Immediate parent")
                      ("siblings" "All siblings")
                      ("all-children" "All children")
                      ("children" "Immediate children"))))
    (format "%s-- %s"
            (propertize " " 'display '(space :align-to 15))
            (propertize text 'face 'completions-annotations))))

;;;###autoload
(defun denote-sequence-find (type)
  "Find all relatives of the given TYPE using the current file's sequence.
Prompt for TYPE among `denote-sequence-relative-types' and then prompt
for a file among the matching files."
  (interactive
   (list
    (denote-sequence-type-prompt "Find relatives of TYPE"
                                 '(all-parents parent siblings children all-children)
                                 #'denote-sequence-annotate-relative-types)))
  (if-let* ((sequence (denote-sequence-file-p buffer-file-name)))
      (if-let* ((relatives (denote-sequence-get-relative sequence type)))
          (find-file (if (stringp relatives)
                         relatives
                       (denote-sequence-file-prompt "Select a relative" relatives)))
        (user-error "The sequence `%s' has no relatives of type `%s'" sequence type))
    (user-error "The current file has no sequence")))

;;;###autoload
(defun denote-sequence-link (file &optional id-only)
  "Link to FILE with sequence.
This is like the `denote-link' command but only accepts to link to a
file that conforms with `denote-sequence-file-p'.  When called
interactively, only relevant files are shown for minibuffer completion
from the variable `denote-directory'.

Optional ID-ONLY has the same meaning as the `denote-link' command."
  (interactive (list (denote-sequence-file-prompt "Link to file with sequence")))
  (unless (denote-sequence-file-p file)
    (error "Can only link to file with a sequence; else use `denote-link' and related"))
  (let* ((type (denote-filetype-heuristics buffer-file-name))
         (description (denote-get-link-description file)))
    (denote-link file type description id-only)))

(defvar denote-sequence-history nil
  "Minibuffer history of `denote-sequence-prompt'.")

(defun denote-sequence-prompt (&optional prompt-text sequences)
  "Prompt for a sequence.
With optional PROMPT-TEXT use it instead of a generic prompt.

With optional SEQUENCES as a list of strings, use them as completion
candidates.  Else use the return value of `denote-sequence-get-all-sequences'.
A sequence is a string conforming with `denote-sequence-p'.  Any other string
is ignored."
  (completing-read
   (format-prompt (or prompt-text "Select an existing sequence (empty for all)") nil)
   (or sequences (denote-sequence-get-all-sequences))
   #'denote-sequence-p t nil 'denote-sequence-history))

(defvar denote-sequence-depth-history nil
  "Minibuffer history of `denote-sequence-depth-prompt'.")

(defun denote-sequence-depth-prompt (&optional prompt-text default-value)
  "Prompt for the depth of a sequence.
With optional PROMPT-TEXT use it instead of the generic one.

With optional DEFAULT-VALUE use it as the default minibuffer value, else
use the `car' of `denote-sequence-depth-history', if any."
  (let* ((default (or default-value (car denote-sequence-depth-history)))
         (default-number (if (stringp default)
                             (string-to-number default)
                           default)))
    (read-number
     (or prompt-text
         (format "Get sequences up to this depth %s: "
                 (if (eq denote-sequence-scheme 'alphanumeric)
                     "(e.g. `1a2' is `3' levels of depth)"
                   "(e.g. `1=1=2' is `3' levels of depth)")))
     default-number
     'denote-sequence-depth-history)))

(defun denote-sequence--get-dired-buffer-name (&optional prefix depth)
  "Return a string for `denote-sequence-dired' buffer.
Use optional PREFIX and DEPTH to format the string accordingly."
  (let ((time (format-time-string "%F %T")))
    (cond
     ((and prefix depth)
      (format-message "*Denote sequences of prefix `%s' and depth `%s', %s*" prefix depth time))
     ((and prefix (not (string-empty-p prefix)))
      (format-message "*Denote sequences of prefix `%s', %s*" prefix time))
     (t
      (format "*Denote sequences, %s*" time)))))

(defun denote-sequence--get-interactive-for-prefix-and-depth ()
  "Return interactive list of arguments for `denote-sequence-dired' and related."
  (let ((arg (prefix-numeric-value current-prefix-arg)))
    (cond
     ((= arg 16)
      (list
       (denote-sequence-prompt "Limit to files that extend SEQUENCE (empty for all)")
       (denote-sequence-depth-prompt)))
     ((= arg 4)
      (list
       (denote-sequence-prompt "Limit to files that extend SEQUENCE (empty for all)")))
     (t
      nil))))

;;;###autoload
(defun denote-sequence-dired (&optional prefix depth)
  "Produce a Dired listing of all sequence notes.
Sort sequences from smallest to largest.

With optional PREFIX string, show only files whose sequence matches it.

With optional DEPTH as a number, limit the list to files whose sequence
is that many levels deep.  For example, 1=1=2 is three levels deep.

For a more specialised case, see `denote-sequence-find-relatives-dired'."
  (interactive (denote-sequence--get-interactive-for-prefix-and-depth))
  (let* ((roots (denote-directories))
         (single-dir-p (null (cdr roots)))
         (files-fn (lambda ()
                     (let* ((files (if (and prefix (not (string-blank-p prefix)))
                                       (denote-sequence-get-all-files-with-prefix prefix)
                                     (denote-sequence-get-all-files)))
                            (files-with-depth (if depth
                                                  (denote-sequence-get-all-files-with-max-depth depth files)
                                                files))
                            (files-sorted (denote-sequence-sort-files files-with-depth)))
                       (if single-dir-p
                           (mapcar #'file-relative-name files-sorted)
                         files-sorted)))))
    (dlet ((ls-lisp-use-insert-directory-program (progn (require 'ls-lisp) nil)))
      (if-let* ((directory (if single-dir-p
                               (car roots)
                             (denote-directories-get-common-root)))
                (files (funcall files-fn))
                (buffer-name (denote-format-buffer-name
                              (format-message "prefix `%s'; depth `%s'" (or prefix "ALL") (or depth "ALL"))
                              :is-special-buffer))
                (dired-buffer (dired (cons directory files))))
          (with-current-buffer dired-buffer
            (rename-buffer buffer-name :unique)
            (setq-local revert-buffer-function
                        (lambda (&rest _)
                          (dlet ((ls-lisp-use-insert-directory-program (progn (require 'ls-lisp) nil)))
                            (if-let* ((files (funcall files-fn)))
                                (progn
                                  (setq-local dired-directory (cons directory files))
                                  (dired-revert))
                              (denote-dired-empty-mode))))))
        (message "No matching files")))))

;;;###autoload
(defun denote-sequence-find-dired (type)
  "Like `denote-sequence-find' for TYPE but put the matching files in Dired.
Also see `denote-sequence-dired'."
  (interactive
   (list (denote-sequence-type-prompt "Find relatives of TYPE"
                                      '(all-parents
                                        parent
                                        siblings
                                        all-children
                                        children))))
  (if-let* ((sequence (denote-sequence-file-p buffer-file-name)))
      (if-let* ((default-directory (car (denote-directories)))
                (relatives (ensure-list (denote-sequence-get-relative sequence type)))
                (files-sorted (denote-sequence-sort-files relatives)))
          (dired (cons (format-message "*`%s' type relatives of `%s'" type sequence)
                       (mapcar #'file-relative-name files-sorted)))
        (user-error "The sequence `%s' has no relatives of type `%s'" sequence type))
    (user-error "The current file has no sequence")))

(defun denote-sequence--get-current-file-for-renaming ()
  "Return path to file for a rename operation.
The path is that of the special Org buffer (like `org-capture'), the
file at point in a Dired buffer, or the variable `buffer-file-name'."
  (if (denote--file-type-org-extra-p)
      denote-last-path
    (denote--rename-dired-file-or-current-file-or-prompt)))

(defun denote-sequence-reparent (current-file file-with-sequence &optional recursive)
  "Re-parent CURRENT-FILE to be a child of FILE-WITH-SEQUENCE.

If CURRENT-FILE has a sequence (the Denote file name signature), change
it.  Else create a new one.

If optional RECURSIVE is non-nil, also reparent all children and
descendants of CURRENT-FILE.  When called interactively, RECURSIVE is
the prefix argument (\\[universal-argument] by default).

When called interactively, CURRENT-FILE is either the current file, or a
special Org buffer (like those of `org-capture'), or the file at point
in Dired.

When called interactively, prompt for FILE-WITH-SEQUENCE showing only
the files in the variable `denote-directory' which have a sequence.  If
no such files exist, throw an error.

When called from Lisp, CURRENT-FILE is a string pointing to a file.

When called from Lisp, FILE-WITH-SEQUENCE is either a file with a
sequence (per `denote-sequence-file-p') or the sequence string as
such (per `denote-sequence-p').  In both cases, what matters is to know
the target sequence."
  (interactive
   (list
    (denote-sequence--get-current-file-for-renaming)
    (denote-sequence-file-prompt
     (format "Reparent `%s' to be a child of"
             (propertize
              (denote--rename-dired-file-or-current-file-or-prompt)
              'face 'denote-faces-prompt-current-name)))
    current-prefix-arg))
  (let* ((root-sequence (denote-retrieve-filename-signature current-file))
         (target-sequence (or (denote-sequence-file-p file-with-sequence)
                              (denote-sequence-p file-with-sequence)
                              (user-error "No sequence of `denote-sequence-p' found in `%s'" file-with-sequence)))
         (new-sequence (denote-sequence--get-new-child target-sequence))
         (descendants (when (and recursive root-sequence)
                        (denote-sequence-get-relative root-sequence 'all-children)))
         (rename-fn (lambda (file sequence)
                      (denote-rename-file file 'keep-current 'keep-current sequence 'keep-current 'keep-current))))
    (funcall rename-fn current-file new-sequence)
    (when descendants
      (dolist (child descendants)
        (let* ((child-sequence (denote-retrieve-filename-signature child))
               (child-sequence-suffix (string-remove-prefix root-sequence child-sequence))
               (new-child-sequence (concat new-sequence child-sequence-suffix)))
          (funcall rename-fn child new-child-sequence))))))

(defun denote-sequence-reparent-recursive (current-file file-with-sequence)
  "Re-parent CURRENT-FILE and all its descendants to FILE-WITH-SEQUENCE.
This is a convenience wrapper around `denote-sequence-reparent' to force
the recursive behaviour."
  (interactive
   (list
    (denote-sequence--get-current-file-for-renaming)
    (denote-sequence-file-prompt
     (format "Reparent `%s' (recursively) to be a child of"
             (propertize
              (denote--rename-dired-file-or-current-file-or-prompt)
              'face 'denote-faces-prompt-current-name)))))
  (denote-sequence-reparent current-file file-with-sequence :recursive))

;;;###autoload
(defun denote-sequence-rename-as-parent (current-file)
  "Make CURRENT-FILE a new parent sequence.
If CURRENT-FILE has a sequence abort the operation.

When called interactively, CURRENT-FILE is either the current file, or a
special Org buffer (like those of `org-capture'), or the file at point
in Dired.  When called from Lisp, CURRENT-FILE is a string pointing to a
file."
  (interactive (list (denote-sequence--get-current-file-for-renaming)))
  (when (denote-sequence-file-p current-file)
    (user-error "The `%s' already has a sequence; aborting" current-file))
  (let ((new-sequence (denote-sequence--get-new-parent)))
    (denote-rename-file current-file 'keep-current 'keep-current new-sequence 'keep-current 'keep-current)))

(defvar denote-sequence-scheme-prompt-history nil
  "Minibuffer history for `denote-sequence-scheme-prompt'.")

(defun denote-sequence-scheme-prompt (&optional prompt-text)
  "Prompt for one among the supported `denote-sequence-scheme' symbols.
With optional PROMPT-TEXT, use it for the prompt message.  Else fall
back to a generic prompt message."
  (let ((default (car denote-sequence-scheme-prompt-history)))
    (intern
     (completing-read
      (format-prompt (or prompt-text "Select sequence scheme") default)
      denote-sequence-schemes nil t nil 'denote-sequence-scheme-prompt-history default))))

;;;###autoload
(defun denote-sequence-convert (files &optional target-scheme)
  "Convert the sequence scheme of FILES to match `denote-sequence-scheme'.

With optional TARGET-SCHEME as a prefix argument, prompt for a scheme
among those supported by `denote-sequence-scheme'.  Otherwise, fall back
to the current value of `denote-sequence-scheme'.

When called from inside a Denote file, interpret FILES as just the
current file.

When called from a Dired buffer, FILES are the marked files.

If no files are marked in the Dired buffer, then consider the one at
point.

Do not make any changes if the file among the FILES has no sequence or
if it already matches the value of `denote-sequence-scheme'.  A file has
a sequence when it conforms with `denote-sequence-file-p'.

[ This command is for users who once used a `denote-sequence-scheme' and
  have since decided to switch to another.  IT DOES NOT REPARENT OR
  ANYHOW CHECK THE RESULTING SEQUENCES FOR DUPLICATES: it simply
  performs the conversion from one scheme to another.  ]"
  (interactive
   (list
    (if (derived-mode-p 'dired-mode)
        (dired-get-marked-files)
      buffer-file-name)
    (when current-prefix-arg
      (denote-sequence-scheme-prompt "Select target SCHEME")))
   dired-mode)
  (unless (listp files)
    (setq files (list files)))
  (dolist (file files)
    (when-let* ((old-sequence (denote-sequence-file-p file))
                (new-sequence (denote-sequence-make-conversion old-sequence (or target-scheme denote-sequence-scheme))))
      (denote-rename-file file 'keep-current 'keep-current new-sequence 'keep-current 'keep-current)))
  (denote-update-dired-buffers))

;;;; Display a hierarchy

(defgroup denote-sequence-hierarchy ()
  "Hierarchy view of Denote sequences."
  :group 'denote
  :group 'denote-sequence
  :link '(info-link "(denote) top")
  :link '(info-link "(denote-sequence) top")
  :link '(url-link :tag "Denote homepage" "https://protesilaos.com/emacs/denote")
  :link '(url-link :tag "Denote Sequence homepage" "https://protesilaos.com/emacs/denote-sequence"))

(defcustom denote-sequence-hierarchy-indentation 2
  "Number of spaces to indent by depth in `denote-sequence-view-hierarchy'."
  :type 'natnum
  :package-version '(denote . "0.3.0")
  :group 'denote-sequence-hierarchy)

(defcustom denote-sequence-hierarchy-move-and-open nil
  "When non-nil moving in the hierarchy view also displays the file.
The hierarchy view is the buffer produced by the command
`denote-sequence-view-hierarchy'.

The commands affected by this user option are the following:

- `denote-sequence-hierarchy-outline-forward-same-level'
- `denote-sequence-hierarchy-outline-backward-same-level'
- `denote-sequence-hierarchy-outline-next-visible-heading'
- `denote-sequence-hierarchy-outline-previous-visible-heading'"
  :type 'boolean
  :package-version '(denote . "0.3.0")
  :group 'denote-sequence-hierarchy)

(defun denote-sequence--format-hierarchy-entry (indent sequence title keywords)
  "Format hierarchy entry to include INDENT, SEQUENCE, TITLE, and KEYWORDS."
  (let* ((indent (propertize indent
                             'cursor-sensor-functions
                             (list
                              (lambda (&rest _)
                                (re-search-forward "[[:alnum:]]" nil t)
                                (forward-char -1)))))
         (sequence (propertize sequence 'denote-sequence-hierarchy-sequence-text t))
         (entry (format "%s%s" indent sequence))
         (append-fn (lambda (new prefix property)
                      (when (and new (not (string-blank-p new)))
                        (setq entry (format "%s %s%s" entry prefix (propertize new property t)))))))
    (funcall append-fn title "" 'denote-sequence-hierarchy-title-text)
    (funcall append-fn keywords "_" 'denote-sequence-hierarchy-keywords-text)
    entry))

(defun denote-sequence--hierarchy-insert (file)
  "Insert FILE in the hierarchy with indentation matching the sequence depth."
  (condition-case data
      (let* ((title (denote-retrieve-title-or-filename file (denote-filetype-heuristics file)))
             (keywords (denote-retrieve-filename-keywords file))
             (sequence (denote-retrieve-filename-signature file))
             (depth (denote-sequence-depth sequence))
             (indent (if (eq depth 1)
                         ""
                       (make-string (* (- depth 1) denote-sequence-hierarchy-indentation) ? )))
             (inhibit-read-only t)
             (entry (denote-sequence--format-hierarchy-entry indent sequence title keywords)))
        (insert (propertize entry
                            'denote-sequence-hierarchy-level depth
                            'denote-sequence-hierarchy-file file))
        (insert "\n"))
    (error (message "Failed `denote-sequence--hierarchy-insert' with data: %s" data))))

(defun denote-sequence-hierarchy-get-level ()
  "Return the outline level at point."
  (let ((position (point)))
    (or (get-text-property position 'denote-sequence-hierarchy-level)
        (user-error "No outline level found at position `%s'" position))))

(defun denote-sequence-hierarchy-find-file (position)
  "Find the file at POSITION in `denote-sequence-view-hierarchy' buffer.
When called interactively POSITION is the current `point'."
  (interactive (list (point)))
  (if-let* ((file (get-text-property position 'denote-sequence-hierarchy-file)))
      (funcall denote-open-link-function file)
    (user-error "No file found at position `%s'" position)))

(defun denote-sequence--hierarchy-get-buffer (prefix depth)
  "Return buffer for `denote-sequence-view-hierarchy'.
PREFIX and DEPTH are used to derive the name of the buffer as well as to
set the `revert-buffer-function'."
  (let* ((name (format-message "*denote-sequence-hierarchy with prefix `%s'; depth `%s'*" (or prefix "ALL") (or depth "ALL")))
         (buffer (get-buffer-create name))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _no-confirm)
                    (denote-sequence-view-hierarchy prefix depth))))
    buffer))

(declare-function outline-cycle "outline" (&optional event))
(declare-function outline-cycle-buffer "outline" (&optional level))
(declare-function outline-forward-same-level "outline" (arg))
(declare-function outline-backward-same-level "outline" (arg))
(declare-function outline-next-visible-heading "outline" (arg))
(declare-function outline-previous-visible-heading "outline" (arg))

(defmacro denote-sequence-define-hierarchy-motion-command (outline-motion)
  "Define a command that performs OUTLINE-MOTION.
The command respects the user option `denote-sequence-hierarchy-move-and-open'."
  `(defun ,(intern (format "denote-sequence-hierarchy-%s" outline-motion)) (n)
     ,(format "Perform `%s'.
Then do what `denote-sequence-hierarchy-move-and-open' entails."
              outline-motion)
     (interactive "p")
     (,outline-motion n)
     (when denote-sequence-hierarchy-move-and-open
       (let ((current-window (selected-window)))
         (call-interactively #'denote-sequence-hierarchy-find-file)
         (select-window current-window)))))

(denote-sequence-define-hierarchy-motion-command outline-forward-same-level)
(denote-sequence-define-hierarchy-motion-command outline-backward-same-level)
(denote-sequence-define-hierarchy-motion-command outline-next-visible-heading)
(denote-sequence-define-hierarchy-motion-command outline-previous-visible-heading)

;; TODO 2025-11-19: Review which keybindings we need to cover the
;; basic use-case.  I do not want to have a million options here.
(defvar denote-sequence-hierarchy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'denote-sequence-hierarchy-find-file)
    (define-key map (kbd "TAB") #'outline-cycle)
    (define-key map (kbd "S-TAB") #'outline-cycle-buffer)
    (define-key map (kbd "<backtab>") #'outline-cycle-buffer)
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "f") #'denote-sequence-hierarchy-outline-forward-same-level)
    (define-key map (kbd "b") #'denote-sequence-hierarchy-outline-backward-same-level)
    (define-key map (kbd "n") #'denote-sequence-hierarchy-outline-next-visible-heading)
    (define-key map (kbd "p") #'denote-sequence-hierarchy-outline-previous-visible-heading)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Key map for `denote-sequence-hierarchy-mode'.")

(defun denote-sequence--hierarchy-face-matcher-subr (property)
  "Search forward for PROPERTY and return match data."
  (when-let* ((properties (text-property-search-forward property))
              (beginning (prop-match-beginning properties))
              (end (prop-match-end properties)) )
    (set-match-data (list beginning end))
    (point)))

;; FIXME 2025-12-01: `text-property-search-forward' does not have a
;; concept of LIMIT like `re-search-forward'.  Maybe this approach of
;; putting text properties as anchors and then searching for them is
;; not a good idea.
(defun denote-sequence--hierarchy-face-matcher-sequence (_limit)
  "Font lock matcher for sequences using LIMIT."
  (denote-sequence--hierarchy-face-matcher-subr 'denote-sequence-hierarchy-sequence-text))

(defun denote-sequence--hierarchy-face-matcher-title (_limit)
  "Font lock matcher for titles using LIMIT."
  (denote-sequence--hierarchy-face-matcher-subr 'denote-sequence-hierarchy-title-text))

(defun denote-sequence--hierarchy-face-matcher-keywords (_limit)
  "Font lock matcher for keywords using LIMIT."
  (denote-sequence--hierarchy-face-matcher-subr 'denote-sequence-hierarchy-keywords-text))

(defvar denote-sequence-hierarchy-font-lock-keywords
  '((denote-sequence--hierarchy-face-matcher-sequence
     (0 'denote-faces-signature))
    (denote-sequence--hierarchy-face-matcher-title
     (0 'denote-faces-title))
    (denote-sequence--hierarchy-face-matcher-keywords
     (0 'denote-faces-keywords)))
  "Font lock keywords for `denote-sequence-hierarchy-mode'.")

(define-derived-mode denote-sequence-hierarchy-mode text-mode "Denote Hierarchy"
  "Major mode for `denote-sequence-view-hierarchy' buffers."
  :interactive nil
  (setq-local font-lock-defaults '(denote-sequence-hierarchy-font-lock-keywords))
  (setq-local outline-regexp "[\s[:alnum:]]+")
  (setq-local outline-level #'denote-sequence-hierarchy-get-level)
  (setq-local outline-minor-mode-highlight 'append)
  (setq-local outline-minor-mode-cycle t)
  (setq-local outline-minor-mode-use-buttons nil)
  (setq-local buffer-read-only t)
  (cursor-sensor-mode 1)
  (outline-minor-mode 1))

;; TODO 2026-03-24: We need to document this command, as well as its two user options.
;;;###autoload
(defun denote-sequence-view-hierarchy (&optional prefix depth)
  "Show a hierachy of sequences.
With optional PREFIX string, show only files whose sequence matches it.
When called interactively, prompt for PREFIX, which is a file whose
sequence is used.

With optional DEPTH as a number, limit the list to files whose sequence
is that many levels deep.  For example, 1=1=2 is three levels deep.
When called interactively, prompt for the depth.

In interactive use, PREFIX is the single universal argument, while DEPTH
is the double universal argument.  In this case, PREFIX can be an empty
string, which means to not use a prefix as a restriction."
  (interactive (denote-sequence--get-interactive-for-prefix-and-depth))
  (if-let* ((files-with-prefix (if (and prefix (not (string-blank-p prefix)))
                                   (denote-sequence-get-all-files-with-prefix prefix)
                                 (denote-sequence-get-all-files)))
            (files (if depth
                       (denote-sequence-get-all-files-with-max-depth depth files-with-prefix)
                     files-with-prefix)))
      (let* ((buffer (denote-sequence--hierarchy-get-buffer prefix depth))
             (sorted (denote-sequence-sort-files files)))
        (with-current-buffer buffer
          (dolist (file sorted)
            (denote-sequence--hierarchy-insert file))
          (goto-char (point-min))
          (denote-sequence-hierarchy-mode))
        (display-buffer buffer))
    (user-error "No sequences found")))

(provide 'denote-sequence)
;;; denote-sequence.el ends here
