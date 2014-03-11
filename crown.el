(require 'cl)
(require 'package)
;(require 's)
;; `package' has to be initialized to install pkgs
(package-initialize)

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
;                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defun s-ends-with? (suffix s &optional ignore-case)
  "Does S end with SUFFIX?
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
  (let ((start-pos (- (length s) (length suffix))))
    (and (>= start-pos 0)
         (eq t (compare-strings suffix nil nil
                                s start-pos nil ignore-case)))))

(defun crown-init ()
  "Install or update the prerequisites for Crown.

Prerequisites list:
  -package-build "
  (interactive)
  (let ((temp-dir (make-temp-file "crown" t)))
    ;; if we do not have melpa's package-build,
    ;; get the tar package and install it
    (unless (package-installed-p 'package-build)
      (let* ((archive "http://melpa.milkbox.net/packages/")
             (archive-contents (with-temp-buffer
                                 (url-insert-file-contents
                                  (concat archive "archive-contents"))
                                 (cdr (read (current-buffer)))))
             (archive-entry (assoc 'package-build archive-contents))
             (archive-file-name
              (let* ((name (car archive-entry)) (pkg-info (cdr archive-entry))
                     (version (package-version-join (aref pkg-info 0)))
                     (flavour (aref pkg-info 3)))
                (format "%s-%s.%s" name version (if (eq flavour 'single)
                                                    "el" "tar"))))
             (archive-url (concat archive archive-file-name))
             (file (expand-file-name archive-file-name temp-dir)))
        (url-copy-file archive-url file t)
        (package-install-file file)))
    (require 'package-build "package-build.el")
    (delete-directory temp-dir t)))

(defun crown-get-melpa-package-version (package)
  (let* ((temp-dir (make-temp-file "crown" t))
         (archive "http://melpa.milkbox.net/packages/")
         (archive-contents (with-temp-buffer
                             (url-insert-file-contents
                              (concat archive "archive-contents"))
                             (cdr (read (current-buffer)))))
         (pkg-info (cdr (assoc package archive-contents))))
    (unless (not pkg-info)
      (package-version-join (aref pkg-info 0)))))

(defun crown-get-inst-package-version (package)
  (let ((pkg-info (cdr (assoc package package-alist))))
    (unless (not pkg-info)
      (package-version-join (aref pkg-info 0)))))

(defun crown-update-crown ()
  (interactive)
  (let ((pb-melpa-version (crown-get-melpa-package-version 'package-build))
        (pb-inst-version (crown-get-inst-package-version 'package-build)))
    (if (and pb-inst-version (equal pb-inst-version pb-melpa-version))
        (message "crown package build dependency already up to date")
      (message "crown package build dependency needs to be updated"))))

(defvar crown-jewels-alist ())

(defcustom crown-dir (expand-file-name "crown" user-emacs-directory)
  "Where crown stores its stuff."
  :group 'crown
  :type 'string)

(defcustom crown-jewels-dir (expand-file-name "jewels" crown-dir)
  "Where set jewels are stored."
  :group 'crown
  :type 'string)

;; --> continue to develop the macro
;; put the packages for this crown config etc...
(defmacro crown-cut-jewel (symbol &rest properties)
  ""
  (declare (indent 1))
  (let* ((url (plist-get properties :url))
         (update (plist-get properties :update))
         (jewel-dir (expand-file-name (symbol-name symbol)
                                       crown-jewels-dir))
         (jewel-packages-file (expand-file-name "packages.el"
                                                 jewel-dir)))
    (when (null url)
      (error "Missing :url"))
    (unless (stringp url)
      (error "Invalid url %S" url))
    `(progn 
       ;; fetch or update the jewel
       (if (or (not (file-exists-p ,jewel-packages-file)) ,update)
           (cond
            ((s-ends-with? ".git" ,url t)
             (pb/checkout-git ',symbol '(:url ,url) ,jewel-dir))
            (t
             (error "Unsupported repository with url %s" ,url))))
       (load ,jewel-packages-file)
       (put ',symbol :jewel-url ,url)
       (put ',symbol :jewel-packages packages)
       (add-to-list 'crown-jewels-alist '(,symbol . (:url ,url))))))

(defmacro crown-set-jewel (symbol)
  ""
  (declare (indent defun))
  `(progn
     (let* ((packages (get ',symbol :jewel-packages))
            (not-installed (remove-if 'package-installed-p packages)))
;       (message "jewel packages: %s" ',packages)
       (if (and not-installed
              (y-or-n-p (format "There are %d packages to be installed. %s "
                                (length not-installed) "Install them?" )))
           (progn
             (package-refresh-contents)
             (dolist (package packages)
               (when (not (package-installed-p package))
                 (package-install package))))))))

(defmacro crown-unset-jewel (symbol)
  ""
  (declare (indent defun))
  `(progn
     (let* ((packages (get ',symbol :jewel-packages))
            (installed (remove-if-not 'package-installed-p packages)))
       (message "jewel packages: %s" ',packages)
       (if (and installed
              (y-or-n-p (format "There are %d installed packages. %s "
                                (length installed) "Uninstall them?" )))
           (progn
             (package-refresh-contents)
             (dolist (package packages)
               (when (package-installed-p package)
                 (package-delete package))))))))

(defun crown-config-fetch (name))
(defun crown-config-fetch-all (name))

(defun crown-config-read-packages (name)
)

;; ===========================================================================

;; (require 'package-build "package-build.el")
;; 
;; (defun crown-get-inst-package-version (package) "12235")
;; (package-installed-p 'package-build)
;; 
;; (macroexpand-all '(crown-cut-jewel crown-themes
;;                     :url "http://github.com/syl20bnr/crown-themes.git"
;;                     :update nil))
;; (crown-cut-jewel crown-themes
;;   :url "http://github.com/syl20bnr/crown-themes.git"
;;   :update t)
;; 
;; (macroexpand-all '(crown-set-jewel crown-themes))
;; (crown-set-jewel crown-themes)
;; (macroexpand-all '(crown-unset-jewel crown-themes))
;; (crown-unset-jewel crown-themes)
;; 
;; (message "%s" packages)
;; 
;; (let ((dir (expand-file-name "crown-themes" crown-configs-dir)))
;;   (cond
;;    ((s-ends-with\? ".git" "http://github.com/syl20bnr/crown-themes.git" t)
;;     (pb/checkout-git ... ... dir))
;;    (t
;;     (error "Unsupported repository with url %s" "http://github.com/syl20bnr/crown-themes.git")))
;;   (let ((package-file ...))
;;     (load package-file))
;;   (progn
;;     (put (quote crown-themes) :crown-url))
;;   (add-to-list crown-configs-alist (symbol :url "http://github.com/syl20bnr/crown-themes.git")))
;; 
;; (defun crown-tests ()
;;   (message "%s" package-alist)
;;   (crown-install-crown)
;;   (crown-get-inst-package-version 'package-build)
;;   (crown-get-melpa-package-version 'package-build)
;;   (crown-update-crown)
;;   (crown-use-config 'crown-themes "http://github.com/syl20bnr/crown-themes.git")
;;   (crown-use-config 'crown-themes "http://hub.com/syl20bnr/crown-themes")
;; )


(provide 'crown)
