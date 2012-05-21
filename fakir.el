;;; fakir.el --- fakeing bits of Emacs -*- lexical-binding: t -*-
;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 17th March 2012
;; Version: 0.0.6
;; Keywords: lisp, tools

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is an collection of tools to make testing Emacs core functions
;; easier.
;;
;;; Source code
;;
;; fakir's code can be found here:
;;   http://github.com/nicferrier/fakir

;;; Style note
;;
;; This codes uses the Emacs style of:
;;
;;    fakir--private-function
;;
;; for private functions and macros.


;;; Code:

(require 'ert)
(eval-when-compile (require 'cl))

;; Mocking processes

(defvar fakir-mock-process-require-specified-buffer nil
  "Tell `fakir-mock-process' that you require a buffer to be set.

This is used, for example, to make `elnode--filter' testing work
properly. Normally, tests do not need to set the process-buffer
directly, they can just expect it to be there. `elnode--filter',
though, needs to set the process-buffer to work properly.")


(defun fakir--make-hash-table (alist)
  "Make a hash table from the ALIST.

The ALIST looks like a let-list."
  (let ((bindings (make-hash-table :test 'equal)))
    (loop for f in (append
                    (list (list :fakir-mock-process t))
                    alist)
       do
         (if (and f (listp f))
             (puthash (car f) (cadr f) bindings)
             (puthash f nil bindings)))
    bindings))

(ert-deftest fakir--make-hash-table ()
  "Test hash table construction."
  (let ((h (fakir--make-hash-table '((a 10)
                                     (b 20)
                                     (fakir-alist-value "is a string")
                                     fakir-single-value
                                     :self-evaling-symbol-as-well))))
    (should (equal 10 (gethash 'a h)))
    (should (equal 20 (gethash 'b h)))
    (should (equal nil (gethash 'fakir-single-value h)))
    (should (equal nil (gethash ':self-evaling-symbol-as-well h)))))

(defmacro fakir-mock-process (process-bindings &rest body)
  "Allow easier testing by mocking the process functions.

For example:

 (fakir-mock-process (:elnode-http-params
                      (:elnode-http-method \"GET\")
                      (:elnode-http-query \"a=10\"))
   (should (equal 10 (elnode-http-param 't \"a\")))
   )

Causes:

 (process-get anything :elnode-http-method)

to always return \"GET\".

`process-put' is also remapped, currently to swallow any setting.

`process-buffer' is also remapped, to deliver the value of the
key `:buffer' if present and a dummy buffer otherwise.

We return what the BODY returned."
  (declare
   (debug (sexp &rest form))
   (indent defun))
  (let ((pvvar (make-symbol "pv"))
        (pvbuf (make-symbol "buf"))
        (result (make-symbol "result")))
    `(let
         ((,pvvar (fakir--make-hash-table (quote ,process-bindings)))
          ;; For assigning the result of the body
          ,result
          ;; Dummy buffer variable for the process - we fill this in
          ;; dynamically in 'process-buffer
          ,pvbuf)
       ;; Rebind the process function interface
       (flet ((process-get (proc key)
                (gethash key ,pvvar))
              (process-put (proc key value)
                (puthash key value ,pvvar))
              ;; We really need to define a proper fake process
              (processp (proc)
               t)
              (get-or-create-pvbuf
               (proc &optional specified-buf)
               (if (bufferp ,pvbuf)
                   ,pvbuf
                 (setq ,pvbuf
                       (if fakir-mock-process-require-specified-buffer
                           (if (bufferp specified-buf)
                               specified-buf
                             nil)
                         (or specified-buf
                             (get-buffer-create
                              (generate-new-buffer-name
                               "* fakir mock proc buf *")))))
                 ;; If we've got a buffer value then insert it.
                 (when (gethash :buffer ,pvvar)
                   (with-current-buffer ,pvbuf
                     (insert (gethash :buffer ,pvvar))))
                 ,pvbuf))
              (process-send-string
               (proc str)
               (with-current-buffer (get-or-create-pvbuf proc)
                 (save-excursion
                   (goto-char (point-max))
                   (insert str))))
              (process-send-eof
               (proc)
               t)
              (process-contact
               (proc &optional arg)
               (list "localhost" 8000))
              (process-status
               (proc)
               'fake)
              (process-buffer
               (proc)
               (get-or-create-pvbuf proc))
              (set-process-buffer
               (proc buffer)
               (get-or-create-pvbuf proc buffer)))
         (setq ,result (progn ,@body))
         )
       ;; Now clean up
       (when (bufferp ,pvbuf)
         (with-current-buffer ,pvbuf
           (set-buffer-modified-p nil)
           (kill-buffer ,pvbuf)))
       ;; Now return whatever the body returned
       ,result)))

(defun fakir-test-mock-process ()
  "A very quick function to test mocking process macro."
  (fakir-mock-process
    ((a 20)
     (:somevar 15))
    (let ((z 10))
      (let ((a "my string!!!"))
        (setq a (process-get t :somevar))
        a))))

(ert-deftest fakir-mock-process ()
  "Test mock process."
  :tags '(unit)
  (let ((x (fakir-test-mock-process)))
    (should (equal 15 x))))


;; Time utils

(defun fakir-time-encode (time-str)
  "Encode the TIME-STR as an EmacsLisp time."
  ;; FIXME this should be part of Emacs probably; I've had to
  ;; implement this in Elnode as well
  (apply 'encode-time (parse-time-string time-str)))


;; A structure to represent a mock file

(defstruct fakir-file
  filename
  directory
  (content "")
  ;; obviously there should be all the state of the file here
  (mtime "Mon, Feb 27 2012 22:10:19 GMT"))

(defun fakir-file (&rest args)
  "Make a fakir-file, a struct."
  (apply 'make-fakir-file args))

(defun fakir--file-check (file)
  "Implements the type check for FILE is a `fakir--file'."
  (if (not (fakir-file-p file))
      (error "not an fakir--file")))

(defun fakir--file-fqn (file)
  "Return the fully qualified name of FILE, an `fakir--file'."
  (fakir--file-check file)
  (let* ((fqfn
          (concat (fakir-file-directory file)
                  "/"
                  (fakir-file-filename file))))
    fqfn))

(ert-deftest fakir--file-fqn ()
  "Test we can make fully qualified names for files."
  (let ((ef (make-fakir-file
             :filename "somefile"
             :directory "/home/dir")))
    (should (equal "/home/dir/somefile"
                   (fakir--file-fqn ef)))))

(defun fakir--file-mod-time (file &optional raw)
  "Return the encoded mtime of FILE, an `fakir--file'.

If RAW is t then return the raw value, a string."
  (fakir--file-check file)
  (if raw
      (fakir-file-mtime file)
    (fakir-time-encode (fakir-file-mtime file))))

(ert-deftest fakir--file-mod-time ()
  "Test that file mtimes are encoded properly."
  (let ((ef (make-fakir-file
             :filename "somefile"
             :directory "/home/dir"
             :mtime "Mon, Feb 27 2012 22:10:21 GMT")))
    (should (equal (fakir--file-mod-time ef)
                   '(20299 65357)))))

(defun fakir--file-attribs (file)
  "Return an answer as `file-attributes' for FILE.

Currently WE ONLY SUPPORT MODIFIED-TIME."
  (fakir--file-check file)
  (list t t t t t
        (fakir--file-mod-time file)))

(ert-deftest fakir--file-attribs ()
  "Test that we get back file attributes."
  (let ((ef (make-fakir-file
             :filename "somefile"
             :directory "/home/dir"
             :mtime "Mon, Feb 27 2012 22:10:21 GMT")))
    (should (equal
             (list t t t t t '(20299 65357))
             (fakir--file-attribs ef)))))

(defun fakir--file-home (file)
  "Return the home part of FILE or nil.

The home part of FILE is the part that is the home directory of
the user. If it's not a user FILE then it won't have a home
part."
  (fakir--file-check file)
  (let* ((fqn (fakir--file-fqn file))
         (home-root
          (save-match-data
            (when
                (string-match
                 "^\\(/home/[A-Za-z][A-Za-z0-9-]+\\)\\(/.*\\)*"
                 fqn)
              (match-string 1 fqn)))))
    home-root))

(ert-deftest fakir--file-home ()
  "Test the home root stuff."
  (let ((ef (make-fakir-file
             :filename "somefile"
             :directory "/home/dir"))
        (ef2 (make-fakir-file
             :filename "somefile"
             :directory "/var/dir"))
        (ef3 (make-fakir-file
              :filename "somefile"
              :directory "/home/dir/someddir")))
    (should (equal "/home/dir" (fakir--file-home ef)))
    (should (equal "/home/dir" (fakir--file-home ef3)))
    (should (equal nil (fakir--file-home ef2)))))

(defun fakir--file-exists-p (file-name fqfn)
  (equal fqfn file-name))

(defun fakir--expand-file-name (file-name home-root)
  "Simple implementation of .. and ~ handling for FILE-NAME."
  ;; tali713 recomended this as a replacement here
  ;; http://paste.lisp.org/display/128254
  (let* ((fqfn (if (string-match "^\\(~/\\|/\\).*" file-name)
                   file-name
                  (concat home-root "/" file-name)))
         (file-path (replace-regexp-in-string
                     "^~/\\(.\\)"
                     (concat home-root "/" "\\1")
                     fqfn))
         (path (split-string file-path "/" t))
         res)
    (while path
      (if (string= ".." (car path))
          (setq res (cdr res))
        (setq res
              (cons (car path) (if (consp res) res))))
      (setq path (cdr path)))
    (concat
     (when (equal ?\/ (elt file-path 0)) "/")
     (mapconcat 'identity (reverse res) "/"))))

(ert-deftest fakir--expand-file-name ()
  "Test expanding names to absolutes."
  (should
   (equal
    (fakir--expand-file-name
     "blah"
     "/home/emacsuser")
    "/home/emacsuser/blah"))
  (should
   (equal
    (fakir--expand-file-name
     "/home/emacsuser/bladh/qdqnwd/qwdqdq.1"
     "/home/emacsuser")
    "/home/emacsuser/bladh/qdqnwd/qwdqdq.1"))
  (should
   (equal
    (fakir--expand-file-name
     "/home/emacsuser/bladh/../qwdqdq.2"
     "/home/emacsuser")
    "/home/emacsuser/qwdqdq.2"))
  (should
   (equal
    (fakir--expand-file-name
     "qwdqdq.3"
     "/home")
    "/home/qwdqdq.3"))
  (should
   (equal
    (fakir--expand-file-name
     "/qwdqdq.4"
     "/home")
    "/qwdqdq.4"))
  (should
   (equal
    (fakir--expand-file-name
     "/home/emacsuser/bladh/../../../../../../qwdqdq.5"
     "/home")
    "/qwdqdq.5")))

(defun fakir--find-file (fakir-file)
  (let ((buf (get-buffer-create (fakir-file-filename fakir-file))))
    (with-current-buffer buf
      (insert (fakir-file-content fakir-file)))
    buf))

(ert-deftest fakir--find-file ()
  (let ((f (fakir-file :filename "README"
                       :directory "/home/fakir"
                       :content "This is a ReadMe file.")))
    (let ((buf (fakir--find-file f)))
      (unwind-protect
          (with-current-buffer buf
            (should
             (equal "This is a ReadMe file."
                    (buffer-substring (point-min) (point-max)))))
        (kill-buffer buf)))))

(defmacro fakir-mock-file (fakir-file &rest body)
  "Mock the filesystem with the FAKIR-FILE object.

The Emacs Lisp file operations are flet'd so that they operate on
the FAKIR-FILE.

For example:

 (fakir-mock-file (fakir-file
                     :filename \"README\"
                     :directory \"/home/emacs/fakir\")
    (expand-file-name \"~/fakir/README\"))

 => \"/home/emacs/fakir/README\"

The operations that are supported by the fleted functions are:
`file-attributes', `file-exists-p' and `expand-file-name'. Others
will be added as necessary."
  (declare (debug (sexp &rest form))
           (indent defun))
  (let ((fv (make-symbol "fakir-filev")))
    `(let*
         ((,fv ,fakir-file)
          (fqfn (fakir--file-fqn ,fv))
          (home-root (fakir--file-home ,fv))
          (default-directory home-root))
       (flet ((file-attributes
               (file-name)
               (fakir--file-attribs ,fv))
              (file-exists-p
               (file-name)
               (fakir--file-exists-p file-name fqfn))
              (expand-file-name
               (file-name &optional def-dir)
               (fakir--expand-file-name
                file-name
                (or def-dir home-root)))
              (find-file
               (file-name)
               (fakir--find-file ,fv))
              (find-file-noselect
               (file-name)
               (fakir--find-file ,fv)))
         ,@body))))

(ert-deftest fakir-mock-file ()
  "Test the mock file macro."
  (fakir-mock-file (make-fakir-file
                      :filename "somefile"
                      :directory "/home/test"
                      :content "This is a file."
                      :mtime "Mon, Feb 27 2012 22:10:21 GMT")
    (let ((buf (find-file "/home/test/somefile")))
      (unwind-protect
          (with-current-buffer buf
            (should
             (equal "This is a file."
                    (buffer-substring (point-min) (point-max)))))
        (kill-buffer buf)))
    (should (file-exists-p "/home/test/somefile"))
    (should-not (file-exists-p "/home/test/otherfile"))
    (should-not (file-exists-p "/home/dir/somefile"))
    (should (equal
             (expand-file-name "~/somefile")
             "/home/test/somefile"))
    (should (equal
             '(20299 65357)
             (elt (file-attributes "/home/test/somefile") 5)))))

(provide 'fakir)
;
;;; fakir.el ends here
