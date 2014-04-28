;;; Tests for fakir

(require 'fakir)

(ert-deftest fakir/make-hash-table ()
  "Test hash table construction."
  (let ((h (fakir/make-hash-table '((a 10)
                                    (b 20)
                                    (fakir-alist-value "is a string")
                                    fakir-single-value
                                    :self-evaling-symbol-as-well))))
    (should (equal 10 (gethash 'a h)))
    (should (equal 20 (gethash 'b h)))
    (should (equal nil (gethash 'fakir-single-value h)))
    (should (equal nil (gethash ':self-evaling-symbol-as-well h)))))


(ert-deftest fakir-mock-proc-properties ()
  "A very quick function to test mocking process macro."
  (let ((somevalue 30))
    (fakir-mock-proc-properties :fakeproc
      (process-put :fakeproc :somevar 10)
      (process-put :fakeproc :othervar 20)
      (should (equal 10 (process-get :fakeproc :somevar)))
      (should (equal 20 (process-get :fakeproc :othervar)))
      (set-process-plist :fakeproc (list :one 1 :two 2))
      (should-not (process-get :fakeproc :somevar))
      (should (equal 1 (process-get :fakeproc :one)))
      (should (equal 2 (plist-get (process-plist :fakeproc) :two))))))

(ert-deftest fakir-make-unix-socket ()
  "Test that we can make unix sockets."
  (let ((pair (fakir-make-unix-socket "nictest1")))
    (unwind-protect
         (progn
           (should (processp (cadr pair)))
           (should (file-exists-p (car pair))))
      ;; Clean up
      (delete-process (cadr pair))
      (delete-file (car pair))))
  (let ((pair (fakir-make-unix-socket)))
    (unwind-protect
         (progn
           (should (processp (cadr pair)))
           (should (file-exists-p (car pair))))
      ;; Clean up
      (delete-process (cadr pair))
      (delete-file (car pair)))))

(ert-deftest fakir-mock-process ()
  "Test that the mock process stuff works.

Includes a test with a real process so that we can establish the
bypass for real processes actually works.  The real process test
requires `make-network-process' with `:family' set to `local' to
work.  That seems better than trying to use a binary."
  (should
   (equal
    (let ((somevalue 10))
      (fakir-mock-process :fakeproc
          ((a 20)
           (:somevar 15)
           (:othervar somevalue))
        (list
         (processp :fakeproc)
         (process-get :fakeproc :somevar)
         (process-get :fakeproc :othervar)
         ;; testing equality of plists sucks
         ;;  (process-plist :fakeproc)
         )))
    '(t 15 10)))
  ;; And now with a real process in the mix - NOTE the name `myproc',
  ;; since the mock-process macro uses the word proc it might be
  ;; dangerous to use proc as a name. FIXME. Yeah. Right.
  (should
   (equal
    (fakir-with-unix-socket (myproc "ert-fakir-mock-process")
      (let ((somevalue 10))
        (unwind-protect
             (append
              (list (processp myproc)) ; the real process outside the mock
              (fakir-mock-process :fakeproc
                  ((a 20)
                   (:somevar 15)
                   (:othervar somevalue))
                (list
                 (processp :fakeproc)
                 (process-get :fakeproc :somevar)
                 (process-get :fakeproc :othervar)
                 ;; testing equality of plists sucks
                 ;;  (process-plist :fakeproc)
                 (processp myproc) ; the real process inside the mock
                 )))
          (when (processp myproc)
            (delete-process myproc)))))
    '(t t 15 10 t))))

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
             (list nil t t t t '(20299 65357))
             (fakir--file-attribs ef))))
  (let ((ef (make-fakir-file
             :filename "somedir"
             :directory "/home/dir"
             :mtime "Mon, Feb 27 2012 22:10:21 GMT"
             :directory-p t)))
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

(ert-deftest fakir--expand ()
  (should
   (equal
    (fakir--expand "/one/../two/../three/four" t)
    "/one/three/four"))
  (should
   (equal
    (fakir--expand "/one/two/../three/../four" t)
    "/one/four")))

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

(ert-deftest fakir/find-file ()
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

(ert-deftest fakir--write-region ()
  "Test writing fake stuff."
  (let ((fl
         (fakir-file :filename "nic" :directory "/tmp/"
                     :content "blah")))
    ;; Overwrite the faked content
    (should
     (equal
      (progn
        (with-temp-buffer
          (insert "hello world!")
          (fakir--write-region
           fl (point-min) (point-max) "/tmp/nic"))
        (fakir-file-content fl))
      "hello world!"))
    ;; Append the faked content
    (should
     (equal
      (progn
        (with-temp-buffer
          (insert " says the computer")
          (fakir--write-region
           fl (point-min) (point-max) "/tmp/nic" t))
        (fakir-file-content fl))
      "hello world! says the computer"))))

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

(ert-deftest fakir-fake-file/creates-parent-directories ()
  (fakir-fake-file
      (fakir-file
       :filename "somefile"
       :directory "/home/fakir-test"
       :content "somecontent")
    (should (equal t (file-directory-p "/")))
    (should (equal t (file-directory-p "/home")))
    (should (equal t (file-directory-p "/home/fakir-test")))))

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

(ert-deftest fakir-fake-file/expand-file-name ()
  (let ((fakir--home-root "/home/fakir-test"))
    (fakir-fake-file
     (list
      (fakir-file
       :filename "blah"
       :directory "/home/fakir-test"
       :content "blah!")
      (fakir-file
       :filename "blah2"
       :directory "/home/fakir-test"
       :content "blah2!")
      (fakir-file
       :filename "blah3"
       :directory "/home/fakir-test"
       :content "NO WAY!")
      (fakir-file
       :filename "blah3"
       :directory "/tmp"
       :content "totally testing!"))
     (let ((real-home-dir
            (file-name-as-directory (getenv "HOME"))))
       (should
        (equal
         (expand-file-name "~/blah")
         "/home/fakir-test/blah"))
       (should
        (equal
         (expand-file-name "~/blah2")
         "/home/fakir-test/blah2"))
       (should
        (equal
         (let ((ctx-dir "/tmp"))
           (expand-file-name "blah3" ctx-dir))
         "/tmp/blah3"))))))

(ert-deftest fakir-fake-file/file-regular-p ()
  (fakir-fake-file
      (list
       (fakir-file
        :filename "testfile"
        :directory "/home/fakir-test"
        :content "file content")
       (fakir-file
        :filename "subdir"
        :directory "/home/fakir-test"
        :directory-p t))
    (should (equal t (file-directory-p "/home/fakir-test/subdir")))
    (should (equal nil (file-directory-p "/home/fakir-test/testfile")))))

(ert-deftest fakir-fake-file/file-directory-p ()
  (fakir-fake-file
      (list
       (fakir-file
        :filename "testfile"
        :directory "/home/fakir-test"
        :content "file content")
       (fakir-file
        :filename "subdir"
        :directory "/home/fakir-test"
        :directory-p t))
    (should (equal t (file-regular-p "/home/fakir-test/testfile")))
    (should (equal nil (file-regular-p "/home/fakir-test/subdir")))))


(ert-deftest fakir-fake-file/directory-files ()
  (fakir-fake-file
      (list
       (fakir-file
        :filename "somefile"
        :directory "/home/fakir-test"
        :content "blah!")
       (fakir-file
        :filename "otherfile"
        :directory "/home/fakir-test/subdir"
        :content "deep")
       (fakir-file
        :filename "otherdir"
        :directory "/home/fakir-test"
        :directory-p ""))
    (should (equal
             (directory-files "/home/fakir-test")
             '("." ".." "otherdir" "somefile" "subdir")))
    (should (equal
             (directory-files "/home/fakir-test" t)
             '("/home/fakir-test/." "/home/fakir-test/.." "/home/fakir-test/otherdir" "/home/fakir-test/somefile" "/home/fakir-test/subdir")))
    (should (equal
             (directory-files "/home/fakir-test" t "otherdir")
             '("/home/fakir-test/otherdir")))))

(ert-deftest fakir-fake-file/directory-files-and-attributes ()
  (fakir-fake-file
      (list
       (fakir-file
        :filename "somefile"
        :directory "/home/fakir-test"
        :content "blah!")
       (fakir-file
        :filename "otherfile"
        :directory "/home/fakir-test/subdir"
        :content "deep")
       (fakir-file
        :filename "otherdir"
        :directory "/home/fakir-test"
        :directory-p ""))
    (should (equal
             (directory-files-and-attributes "/home/fakir-test")
             '(("." t t t t t (20299 65355))
               (".." t t t t t (20299 65355))
               ("otherdir" "" t t t t (20299 65355))
               ("somefile" nil t t t t (20299 65355))
               ("subdir" t t t t t (20299 65355)))))
    (should (equal
             (directory-files-and-attributes "/home/fakir-test" t)
             '(("/home/fakir-test/." t t t t t (20299 65355))
               ("/home/fakir-test/.." t t t t t (20299 65355))
               ("/home/fakir-test/otherdir" "" t t t t (20299 65355))
               ("/home/fakir-test/somefile" nil t t t t (20299 65355))
               ("/home/fakir-test/subdir" t t t t t (20299 65355)))))))

;;; fakir-tests.el ends here
