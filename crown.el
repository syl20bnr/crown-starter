(require 'cl)
(require 'package)
;(require 's)
;; `package' has to be initialized to install pkgs
(package-initialize)

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
;                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defun crown-init ()
  "Install or update the prerequisites for Crown.

Prerequisites list:
  -package-build "
  (interactive)
  (let ((temp-dir (make-temp-file "crown" t)))
    (unless (and (package-installed-p 'package-build)
               (package-installed-p 's))
      (let* ((archive "http://melpa.milkbox.net/packages/")
             (archive-contents (with-temp-buffer
                                 (url-insert-file-contents
                                  (concat archive "archive-contents"))
                                 (cdr (read (current-buffer)))))
             (package-build-entry (assoc 'package-build archive-contents))
             (s-entry (assoc 's archive-contents)))
        (crown-package-install archive package-build-entry)
        (crown-package-install archive s-entry)
        (require 'package-build "package-build.el")
        (require 's)
        (delete-directory temp-dir t)))))

(defun crown-package-install (archive entry)
  ""
  (let*
      ((archive-file-name
        (let* ((name (car entry)) (pkg-info (cdr entry))
               (version (package-version-join (aref pkg-info 0)))
               (flavour (aref pkg-info 3)))
          (format "%s-%s.%s" name version (if (eq flavour 'single)
                                              "el" "tar"))))
       (archive-url (concat archive archive-file-name))
       (file (expand-file-name archive-file-name temp-dir)))
    (url-copy-file archive-url file t)
    (package-install-file file)))

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
       (put ',symbol :jewel-dir ,jewel-dir)
       (put ',symbol :jewel-init-file
            ,(expand-file-name "init.el" jewel-dir))
       (put ',symbol :jewel-functions-file
            ,(expand-file-name "functions.el" jewel-dir))
       (put ',symbol :jewel-keybindings-file
            ,(expand-file-name "keybindings.el" jewel-dir))
       (put ',symbol :jewel-packages-file ,jewel-packages-file)
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
              (y-or-n-p
               (format "Crown: jewel %S will install %d packages. %s "
                       ',symbol (length not-installed) "Proceed?" )))
           (progn
             (package-refresh-contents)
             (dolist (package packages)
               (when (not (package-installed-p package))
                 (package-install package))))))
     (load (get ',symbol :jewel-init-file))
     (load (get ',symbol :jewel-functions-file))
     (load (get ',symbol :jewel-keybindings-file))))

(defmacro crown-cur-and-set-jewel (symbol &rest properties)
  `(progn
     (crown-cut-jewel ,symbol ,properties)
     (crown-set-jewel ,symbol)))

(defmacro crown-unset-jewel (symbol)
  ""
  (declare (indent defun))
  `(progn
     (let* ((packages (get ',symbol :jewel-packages))
            (installed (remove-if-not 'package-installed-p packages)))
       (message "jewel packages: %s" ',packages)
       (if (and installed
              (y-or-n-p
               (format "Crown: jewel %S will uninstall %d packages. %s "
                       ',symbol (length installed) "Proceed?" )))
           (progn
             (package-refresh-contents)
             (dolist (package packages)
               (when (package-installed-p package)
                 (package-delete package))))))))

(defun crown-show-jewel-list (jewels)
  "Display JEWELS in a *Jewels* buffer.
This is similar to `list-jewels', but it does not fetch the
updated list of jewels, and it only displays jewels with
names in JEWELS (which should be a list of symbols)."
  (require 'finder-inf nil t)
  (let ((buf (get-buffer-create "*Jewels*")))
    (with-current-buffer buf
      (jewel-menu-mode)
      (jewel-menu--generate nil jewels))
    (switch-to-buffer buf)))

(crown-show-jewel-list t)

(define-derived-mode jewel-menu-mode tabulated-list-mode "Jewel Menu"
  "Major mode for browsing a list of jewel.
Letters do not insert themselves; instead, they are commands.
\\<package-menu-mode-map>
\\{package-menu-mode-map}"
  (setq tabulated-list-format [("Jewel" 32 jewel-menu--name-predicate)
			       ("Version" 12 nil)
			       ("Status"  10 nil)
			       ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Jewel" nil))
  (tabulated-list-init-header))

(defun jewel-menu--name-predicate (A B)
  (string< (symbol-name (caar A))
	   (symbol-name (caar B))))

(defun jewel-menu--generate (remember-pos jewels)
  "Populate the Jewel Menu.
If REMEMBER-POS is non-nil, keep point on the same entry.
JEWELS should be t, which means to display all known packages,
or a list of jewels names (symbols) to display."
  ;; ;; Construct list of ((JEWELS . VERSION) STATUS DESCRIPTION).
  ;; (let (info-list name)
  ;;   ;; Installed packages:
  ;;   (dolist (elt package-alist)
  ;;     (setq name (car elt))
  ;;     (when (or (eq jewels t) (memq name jewels))
	;; (package--push name (cdr elt)
	;; 	       (if (stringp (cadr (assq name package-load-list)))
	;; 		   "held" "installed")
	;; 	       info-list)))

  ;;   ;; Built-in packages:
  ;;   (dolist (elt package--builtins)
  ;;     (setq name (car elt))
  ;;     (when (and (not (eq name 'emacs)) ; Hide the `emacs' package.
	;; 	 (or (eq jewels t) (memq name jewels)))
  ;;   	(package--push name (cdr elt) "built-in" info-list)))

  ;;   ;; Available and disabled packages:
  ;;   (dolist (elt package-archive-contents)
  ;;     (setq name (car elt))
  ;;     (when (or (eq jewels t) (memq name jewels))
	;; (let ((hold (assq name package-load-list)))
	;;   (package--push name (cdr elt)
	;; 		 (cond
	;; 		  ((and hold (null (cadr hold))) "disabled")
	;; 		  ((memq name package-menu--new-package-list) "new")
	;; 		  (t "available"))
	;; 		 info-list))))

  ;;   ;; Obsolete packages:
  ;;   (dolist (elt package-obsolete-alist)
  ;;     (dolist (inner-elt (cdr elt))
	;; (when (or (eq jewels t) (memq (car elt) jewels))
	;;   (package--push (car elt) (cdr inner-elt) "obsolete" info-list))))

    ;; Print the result.
  (setq info-list nil)
    (push '(crown-themes "1.0" "na" "A big pack of emacs themes") info-list)
    (setq tabulated-list-entries (mapcar 'jewel-menu--print-info info-list))
    (tabulated-list-print remember-pos))
;; )

(defun jewel-menu--print-info (jewel)
  "Return a jewel entry suitable for `tabulated-list-entries'.
JEWEL has the form ((PACKAGE . VERSION) STATUS DOC).
Return (KEY [NAME VERSION STATUS DOC]), where KEY is the
identifier (NAME . VERSION-LIST)."
  ;; (let* ((jwl (caar jewel))
	;;  (version (cdr (car jewel)))
	;;  (status  (nth 1 jewel))
	;;  (doc (or (nth 2 jewel) ""))
	;;  (face (cond
	;; 	((string= status "built-in")  'font-lock-builtin-face)
	;; 	((string= status "available") 'default)
	;; 	((string= status "new") 'bold)
	;; 	((string= status "held")      'font-lock-constant-face)
	;; 	((string= status "disabled")  'font-lock-warning-face)
	;; 	((string= status "installed") 'font-lock-comment-face)
	;; 	(t 'font-lock-warning-face)))) ; obsolete.
  ;;   (list (cons jwl version)
	;;   (vector (list (symbol-name jwl)
	;; 		'face 'link
	;; 		'follow-link t
	;; 		'package-symbol jwl
	;; 		'action 'package-menu-describe-package)
	;; 	  (propertize (package-version-join version)
	;; 		      'font-lock-face face)
	;; 	  (propertize status 'font-lock-face face)
	;; 	  (propertize doc 'font-lock-face face)))))

  (let* ((jwl (car jewel))
         (version (nth 1 jewel))
         (status  (nth 2 jewel))
         (doc (or (nth 3 jewel) ""))
         (face 'default))
    (list jwl
          (vector (list (symbol-name jwl)
                        'face 'link
                        'follow-link t
                        'package-symbol jwl
                        'action 'jewel-menu-describe-jewel)
                  (propertize version 'font-lock-face face)
                  (propertize status 'font-lock-face face)
                  (propertize doc 'font-lock-face face)))))

(defun jewel-menu-describe-jewel (&optional button)
  "Describe the current jewel.
If optional arg BUTTON is non-nil, describe its associated jewel."
  (interactive) nil)

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
