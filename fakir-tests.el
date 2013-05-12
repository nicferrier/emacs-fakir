;;; Tests for fakir

(require 'fakir)

(ert-deftest flet-overrides ()
  "Test the flet-override stuff."
  (flet ((my-test (x)
                  (and
                   (listp x)
                   (let ((v (car x)))
                     (eq :object v))))
         (my-func (x y)
                  (format "strings: %s %s" x y))
         (my-proc (z)
                  (* 2 x)))
    (flet-overrides
      my-test ; the type predicate we'll use
      ((my-func a (a b)
                (+ (cadr a) b))
       (my-proc y (x y)
                (+ 10 y)))
      (should
       (equal
        '("strings: nic caroline" 7)
        (list
         ;; This call doesn't match the predicate
         (my-func "nic" "caroline")
         ;; This call does match the predicate
         (my-func '(:object 5) 2)))))))


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


(ert-deftest fakir-mock-process ()
  "Basic test of the mock process."
  :tags '(unit)
  (let ((x (fakir-test-mock-process)))
    (should (equal
             '(15 30)
             x))))

(ert-deftest fakir-mock-process-delete ()
  "Test the delete handling."
  :tags '(unit)
  ;; delete-process causes the body to return :mock-process-finished
  (should
   (fakir-mock-process
     :fakeproc
     ((a 20)
      (:somevar "somevar"))
     (let ((x "a string of text"))
       (delete-process :fakeproc))))
  ;; How to use catch inside the BODY to handle delete-process
  (should
   (equal
    "the process finished"
    (fakir-mock-process
      :fakeproc
      ((a 20)
       (:somevar "somevar"))
      (let ((x "a string of text"))
	(when (eq :mock-process-finished
		  (catch :mock-process-finished
		    (delete-process :fakeproc)))
	  "the process finished"))))))



(ert-deftest fakir--file-fqn ()
  "Test we can make fully qualified names for files."
  (let ((ef (make-fakir-file
             :filename "somefile"
             :directory "/home/dir")))
    (should (equal "/home/dir/somefile"
                   (fakir--file-fqn ef)))))

(ert-deftest fakir--file-mod-time ()
  "Test that file mtimes are encoded properly."
  (let ((ef (make-fakir-file
             :filename "somefile"
             :directory "/home/dir"
             :mtime "Mon, Feb 27 2012 22:10:21 GMT")))
    (should (equal (fakir--file-mod-time ef)
                   '(20299 65357)))))

(ert-deftest fakir--file-attribs ()
  "Test that we get back file attributes."
  (let ((ef (make-fakir-file
             :filename "somefile"
             :directory "/home/dir"
             :mtime "Mon, Feb 27 2012 22:10:21 GMT")))
    (should (equal
             (list t t t t t '(20299 65357))
             (fakir--file-attribs ef)))))

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

(ert-deftest fakir--expand-file-name ()
  "Test expanding names to absolutes."
  (should
   (equal
    (fakir--expand-file-name "blah" "/home/emacsuser")
    "/home/emacsuser/blah"))
  (should
   (equal
    (fakir--expand-file-name
     "/home/emacsuser/bladh/qdqnwd/qwdqdq.1" "/home/emacsuser")
    "/home/emacsuser/bladh/qdqnwd/qwdqdq.1"))
  (should
   (equal
    (fakir--expand-file-name
     "/home/emacsuser/bladh/../qwdqdq.2" "/home/emacsuser")
    "/home/emacsuser/qwdqdq.2"))
  (should
   (equal
    (fakir--expand-file-name "qwdqdq.3" "/home")
    "/home/qwdqdq.3"))
  (should
   (equal
    (fakir--expand-file-name "/qwdqdq.4" "/home")
    "/qwdqdq.4"))
  (should
   (equal
    (fakir--expand-file-name
     "/home/emacsuser/bladh/../../../../../../qwdqdq.5" "/home")
    "/qwdqdq.5")))

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

(ert-deftest fakir-mock-file ()
  "Test the mock file macro."
  (let ((fakir--home-root "/home/test"))
    (fakir-mock-file (fakir-file
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
               (elt (file-attributes "/home/test/somefile") 5))))))

(ert-deftest fakir-fake-file/insert-file-contents ()
  (fakir-fake-file
   (fakir-file
    :filename "blah"
    :directory "/tmp"
    :content "blah!")
   (should
    (equal
     (with-temp-buffer
       (insert-file-contents "/tmp/blah")
       (buffer-string))
     "blah!"))
   ;; We should do another test with a real file - this one?
   ))

(ert-deftest fakir-fake-file/expand-file-name ()
  (let ((fakir--home-root "/home/fakir-test"))
    (fakir-fake-file
     (fakir-file
      :filename "blah"
      :directory "/home/fakir-test"
      :content "blah!")
     (let ((real-home-dir
            (file-name-as-directory (getenv "HOME"))))
       (should
        (equal
         (expand-file-name "~/blah")
         "/home/fakir-test/blah"))
       ;; Use a real one
       (should
        (equal
         (expand-file-name "~/.emacs.d")
         (concat real-home-dir ".emacs.d")))))))

;;; fakir-tests.el ends here
