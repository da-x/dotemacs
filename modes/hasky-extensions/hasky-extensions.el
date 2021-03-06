;;; hasky-extensions.el --- Toggle Haskell language extensions -*- lexical-binding: t; -*-
;;
;; Copyright © 2016 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/hasky-mode/hasky-extensions
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (avy-menu "0.2"))
;; Keywords: programming
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The package provides a way to add and remove Haskell language extensions
;; easily from any place in a file without moving the point.  This is done
;; with help of a menu where the most popular language extensions are
;; assigned just one letter to toggle, while the others require two key
;; strokes.
;;
;; Naturally, when performing toggling of the extensions, they are kept
;; sorted and aligned automatically for you.

;;; Code:

(require 'avy-menu)
(require 'cl-lib)
(require 'simple)

(defgroup hasky-extensions nil
  "Toggle Haskell language extensions."
  :group  'programming
  :tag    "Hasky Extensions"
  :prefix "hasky-extensions-"
  :link   '(url-link :tag "GitHub"
                     "https://github.com/hasky-mode/hasky-extensions"))

(defface hasky-extensions-disabled
  '((t (:inherit font-lock-comment-face)))
  "Face used to print disabled Haskell extensions in the menu.")

(defface hasky-extensions-enabled
  '((t (:inherit font-lock-keyword-face)))
  "Face used to print enabled Haskell extensions in the menu.")

(defcustom hasky-extensions
  '("OverloadedStrings"
    "RecordWildCards"
    "CPP"
    "FlexibleContexts"
    "FlexibleInstances"
    "TemplateHaskell"
    "Arrows"
    "AutoDeriveTypeable"
    "BangPatterns"
    "DataKinds"
    "DeriveAnyClass"
    "DeriveDataTypeable"
    "DeriveFoldable"
    "DeriveFunctor"
    "DeriveGeneric"
    "DeriveTraversable"
    "EmptyDataDecls"
    "ExistentialQuantification"
    "ExplicitForAll"
    "ForeignFunctionInterface"
    "FunctionalDependencies"
    "GADTs"
    "GeneralizedNewtypeDeriving"
    "InstanceSigs"
    "KindSignatures"
    "MagicHash"
    "MultiParamTypeClasses"
    "MultiWayIf"
    "NoImplicitPrelude"
    "OverloadedLists"
    "PolyKinds"
    "QuasiQuotes"
    "RankNTypes"
    "RecursiveDo"
    "ScopedTypeVariables"
    "StandaloneDeriving"
    "TupleSections"
    "TypeFamilies"
    "TypeOperators"
    "TypeSynonymInstances"
    "UndecidableInstances"
    "ViewPatterns")
  "List of commonly used Haskell extensions."
  :tag "List of commonly used Haskell extensions."
  :type '(repeat (string :tag "Extension name")))

(defcustom hasky-extensions-reach 5000
  "Max number of characters from beginning of file to search.

Very large files can either slow down the process of extensions
detection or cause stack overflows, thus we limit number of
characters the package will traverse.  The default value should
be appropriate for most users since language extension pragmas
are typically placed in the beginning of file.  If you wish to
disable the limitation, set this value to NIL (not recommended)."
  :tag "How many characters from beginning of file to scan"
  :type 'integer)

(defcustom hasky-extensions-prettifying-hook nil
  "Hook to run after prettifying of extension section."
  :tag "Hooks to run after prettifying list of extensions"
  :type 'hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The editing itself

(defun hasky-extensions--next-ext (&optional ext)
  "Find and return name of next extensions in the file or NIL.

If EXT is supplied, find this particular extension."
  (re-search-forward
   (concat "^\\s-*{-#\\s-*LANGUAGE\\s-+\\("
           (if ext
               (regexp-quote ext)
             "[[:alnum:]]+")
           "\\)\\s-*#-}\\s-*?$")
   hasky-extensions-reach t))

(defun hasky-extensions-list ()
  "List all active Haskell extensions in current file.

Returned list is always a fresh one (you can perform destructive
operations on it without fear).

This does not take into account extensions enabled in Cabal file
with “default-extensions” or similar settings."
  (let (exts)
    (save-excursion
      (goto-char (point-min))
      (while (hasky-extensions--next-ext)
        (push (match-string-no-properties 1) exts)))
    exts))

(defun hasky-extensions--prettify-exts ()
  "Find, sort, and align extensions in current file if necessary."
  (save-excursion
    (goto-char (point-min))
    (let ((beg (when (hasky-extensions--next-ext)
                 (match-beginning 0))))
      (when beg
        (goto-char beg)
        (while (progn
                 (forward-line)
                 (when (looking-at "\\(^\\s-+\\){-#")
                   (delete-region (match-beginning 1) (match-end 1)))
                 (looking-at "^\\s-*{-#.*?#-}\\s-*?$")))
        (let ((end (point))
              (indent-tabs-mode nil))
          (sort-lines nil beg end)
          (align-regexp beg end "\\(\\s-*\\)#-}")))))
  (run-hooks 'hasky-extensions-prettifying-hook))

(defun hasky-extensions-add (extension)
  "Insert EXTENSION into appropriate place in current file."
  ;; NOTE There are several scenarios we should support.  First, if file
  ;; contains some extensions already, we should add the new one to them.
  ;; If there is no extensions in the file yet, place the new one before
  ;; module declaration (with one empty line between them).  If The file
  ;; contains no module declaration yet, place the new extension after
  ;; header.  If the file has no header, place the new extension at the
  ;; beginning of file.
  (save-excursion
    (let ((ext (format "{-# LANGUAGE %s #-}\n" extension)))
      (cond ((progn
               (goto-char (point-min))
               (hasky-extensions--next-ext))
             (beginning-of-line))
            ((progn
               (goto-char (point-min))
               (re-search-forward
                "^module"
                hasky-extensions-reach
                t))
             (beginning-of-line)
             (open-line 1))
            ((let (going)
               (goto-char (point-min))
               (while (re-search-forward
                       "^--.*?$"
                       hasky-extensions-reach
                       t)
                 (setq going t))
               going)
             (goto-char (match-end 0))
             (forward-line)
             (newline))
            (t (goto-char (point-min))))
      (insert ext)
      (hasky-extensions--prettify-exts))))

(defun hasky-extensions-remove (extension)
  "Remove EXTENSION from current file (if present)."
  (save-excursion
    (goto-char (point-min))
    (when (hasky-extensions--next-ext extension)
      (delete-region (match-beginning 0) (match-end 0))
      (unless (eobp)
        (when (looking-at "$")
          (delete-char 1)))
      (hasky-extensions--prettify-exts))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface

;;;###autoload
(defun hasky-extensions ()
  "Invoke the menu that allows to add and remove Haskell language extensions."
  (interactive)
  (let ((exts (hasky-extensions-list))
        (selected t))
    (while selected
      (setq
       selected
       (avy-menu
        "*hasky-extensions*"
        (cons
         "Haskell Extensions"
         (list
          (cons
           "Pane"
           (mapcar
            (lambda (x)
              (let ((active (cl-find x exts :test #'string=)))
                (cons
                 (propertize
                  x
                  'face
                  (if active
                      'hasky-extensions-enabled
                    'hasky-extensions-disabled))
                 (cons x active))))
            hasky-extensions))))))
      (when selected
        (cl-destructuring-bind (ext . active) selected
          (if active
              (progn
                (hasky-extensions-remove ext)
                (setq exts (cl-delete ext exts :test #'string=)))
            (hasky-extensions-add ext)
            (cl-pushnew ext exts :test #'string=)))))))

(provide 'hasky-extensions)

;;; hasky-extensions.el ends here
