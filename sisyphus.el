;;; sisyphus.el --- Create releases packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;;         Karim Aziiev <karim.aziiev@gmail.com>
;; Homepage: https://github.com/magit/sisyphus
;; Keywords: git tools vc

;; Package-Version: 0.2.1
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.1"))

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Create a release and watch it roll down the hill again.

;; Recommended setup:
;;   (with-eval-after-load 'magit (require 'sisyphus))

;;; Code:

(require 'compat)
(require 'copyright)
(require 'transient)
(require 'magit-tag)
(require 'lisp-mnt)

(defcustom sisyphus-transient-suffixes '(("c" "release commit"
                                        sisyphus-create-release)
                                       ("g" "post release commit"
                                        sisyphus-bump-post-release)
                                       ("y" "bump copyright years"
                                        sisyphus-bump-copyright))
  "Suffixes to add in `magit-tag'."
  :group 'sisyphus
  :type '(repeat (list :tag "Rule"
                       (string :tag "Key")
                       (string :tag "Description")
                       (radio :tag "Command"
                              (function-item sisyphus-create-release)
                              (function-item sisyphus-bump-post-release)
                              (function-item sisyphus-bump-copyright)))))

(defun sisyphus--nth-safe (idx arr-or-list)
  "Return the element at IDX from ARR-OR-LIST, safely handling lists or vectors.

Argument IDX is an integer representing the position of the element to retrieve.

Argument ARR-OR-LIST is either a list or a vector from which the element at
position IDX is retrieved."
  (cond ((listp arr-or-list)
         (nth idx arr-or-list))
        ((vectorp arr-or-list)
         (aref arr-or-list idx))))

(defun sisyphus-transient-suffixes-watcher (_symbol newval _operation _buffer)
  "Variable watcher to update transient prefix `magit-tag' with NEWVAL."
  (let* ((layout (get 'magit-tag 'transient--layout))
         (len (length layout)))
    (when-let* ((suffix-idx (catch 'found
                              (dotimes (idx len)
                                (let ((group (sisyphus--nth-safe
                                              idx
                                              layout)))
                                  (when (and (vectorp group)
                                             (eq :description
                                                 (car-safe (aref group 2)))
                                             (equal
                                              (cadr (aref group 2))
                                              "Sisyphus"))
                                    (throw 'found idx)))))))
      (transient-remove-suffix 'magit-tag (list suffix-idx))))
  (when newval
    (transient-append-suffix 'magit-tag
      (list
       (let ((i (1- (length (get 'magit-tag 'transient--layout)))))
         (if (>= i 0)
             i
           0)))
      (apply #'vector
             (append
              (list "Sisyphus")
              newval)))))


(defun sisyphus-header-multiline (header &optional extra)
  "Return the contents of the header named HEADER, with continuation lines.
The returned value is a list of strings, one per line.

If optional EXTRA is non-nil, then return (LINES BEG END INDENT),
where INDENT is either nil, if the value was specified on a
single line, or the prefix used on continuation lines."
  (save-excursion
    (goto-char (point-min))
    (let ((lines (lm-header header))
          (beg (line-beginning-position))
          (end (1+ (line-end-position)))
          (indent nil))
      (when lines
  (setq lines (list lines))
  (forward-line 1)
  (while (looking-at "^;+\\(\t\\|[\t\s]\\{2,\\}\\)\\(.+\\)")
    (push (match-string-no-properties 2) lines)
          (unless indent
            (setq indent (match-string-no-properties 1)))
    (forward-line 1)
          (setq end (point)))
        (setq lines (nreverse lines))
        (if extra (list lines beg end indent) lines)))))



(defun sisyphus-package-requires (&optional file extra)
  "Extract the value of the Package-Requires header in the FILE.
If optional EXTRA is non-nil, then return (VALUE BEG END INDENT),
where INDENT is either nil, if the value was specified on a
single line, or the prefix used on continuation lines."
  (pcase-let ((`(,lines ,beg ,end ,indent)
               (lm-with-file file
                 (sisyphus-header-multiline "package-requires" t))))
    (and-let* ((lines lines)
               (value
                (when (fboundp 'package--prepare-dependencies)
                  (package--prepare-dependencies
                   (package-read-from-string
                    (mapconcat #'identity lines " "))))))
      (if extra (list value beg end indent) value))))

(defun sisyphus-update-package-requires (&optional file updates noerror)
  "Update Package-Requires with UPDATES in FILE.
If Package-Requires is not found, signal an error, unless NOERROR is non nil."
  (when updates
    (pcase-let* ((`(,value ,beg ,end ,_i)
                  (sisyphus-package-requires file t)))
      (if (not value)
          (unless noerror
            (error "Cannot update Package-Requires; cannot be found"))
        (setq value (sisyphus-update-dependencies value updates))
        (save-excursion
          (goto-char beg)
          (delete-region beg end)
          (insert ";; Package-Requires: " (prin1-to-string updates) "\n"))))))


(defun sisyphus-update-dependencies (value updates)
  "Update dependencies VALUE with UPDATES."
  (pcase-dolist (`(,pkg ,ver) updates)
    (when (alist-get pkg value)
      (setf (alist-get pkg value)
            (list ver))))
  (cl-sort value
           (lambda (a b)
             (pcase (list a b)
               (`(emacs ,_) t)
               (`(,_ emacs) nil)
               (`(compat ,_) t)
               (`(,_ compat) nil)
               (_ (string< a b))))
           :key #'car))

;;; Variables

(defvar sisyphus--non-release-suffix ".50-git"
  "String appended to version strings for non-release revisions.")

;;; Commands

;;;###autoload
(defun sisyphus-create-release (version &optional nocommit)
  "Create a release commit, bumping VERSION strings.
With prefix argument NOCOMMIT, do not create a commit."
  (interactive (list (sisyphus--read-version)))
  (magit-with-toplevel
    (let ((magit-inhibit-refresh t)
          (magit--disable-save-buffers t))
      (sisyphus--bump-changelog version)
      (sisyphus--bump-version version))
    (unless nocommit
      (sisyphus--commit (format "Release version %s" version) t))))

;;;###autoload
(defun sisyphus-bump-post-release (version &optional nocommit)
  "Create a post-release commit, bumping VERSION strings.
With prefix argument NOCOMMIT, do not create a commit."
  (interactive (list (and (sisyphus-resolve-changelog-file)
                          (sisyphus--read-version "Tentative next release"))
                     current-prefix-arg))
  (magit-with-toplevel
    (let ((magit-inhibit-refresh t)
          (magit--disable-save-buffers t))
      (sisyphus--bump-changelog version t)
      (sisyphus--bump-version (concat (sisyphus--previous-version)
                                      sisyphus--non-release-suffix)))
    (unless nocommit
      (sisyphus--commit "Resume development"))))

;;;###autoload
(defun sisyphus-bump-copyright (&optional nocommit)
  "Bump copyright years and commit the result.
With prefix argument NOCOMMIT, do not create a commit."
  (interactive "P")
  (magit-with-toplevel
    (let ((magit-inhibit-refresh t)
          (magit--disable-save-buffers t))
      (sisyphus--bump-copyright))
    (unless nocommit
      (sisyphus--commit "Bump copyright years"))))

;;; Macros

(defmacro sisyphus--with-file (file &rest body)
  "Execute BODY in the FILE buffer."
  (declare (indent 1))
  (let ((file* (gensym "file"))
        (open* (gensym "open")))
    `(let* ((,file* ,file)
            (,open* (find-buffer-visiting ,file*)))
       (with-current-buffer (find-file-noselect ,file*)
         (save-excursion
           (goto-char (point-min))
           (let ((inhibit-read-only t))
             (prog1 (progn ,@body)
               (save-buffer)
               (unless ,open*
                 (kill-buffer)))))))))

;;; Functions

(defun sisyphus--package-name ()
  "Return file name of the current repository sans its directory."
  (file-name-nondirectory (directory-file-name (magit-toplevel))))

(defun sisyphus--previous-version ()
  "Return the highest release version."
  (caar (magit--list-releases)))

(defun sisyphus-resolve-changelog-file ()
  "Return absolute path to changelog in the current VC tree."
  (when-let* ((dir (magit-toplevel))
              (file (car (directory-files dir nil "CHANGELOG"))))
    (expand-file-name file dir)))

(defun sisyphus--get-changelog-version ()
  "Search for latest version in the changelog file."
  (when-let* ((file (sisyphus-resolve-changelog-file)))
    (and (file-exists-p file)
         (sisyphus--with-file file
           (and (re-search-forward "^\\* v\\([^ ]+\\)" nil t)
                (match-string-no-properties 1))))))

(defun sisyphus--read-version (&optional prompt)
  "Read a version from the minibuffer, prompting with string PROMPT."
  (let* ((prev (sisyphus--previous-version))
         (next (sisyphus--get-changelog-version))
         (version (read-string
                   (if prev
                       (format "%s (previous was %s): "
                               (or prompt "Create release")
                               prev)
                     "Create first release: ")
                   (cond ((and next
                               (or (not prev)
                                   (magit--version> next prev)))
                          next)
                         (prev
                          (let ((v (version-to-list prev)))
                            (mapconcat #'number-to-string
                                       (nconc (butlast v)
                                              (list (1+ (car (last v)))))
                                       ".")))))))
    (when (and prev (not (magit--version> version prev)))
      (user-error "Version must increase, but %s is not greater than %s"
                  version prev))
    version))

(defun sisyphus-insert-changelog (version)
  "Insert a changelog entry for the specified VERSION, creating the file if needed.

Argument VERSION is the version string to be inserted into the changelog."
  (interactive (list (sisyphus--read-version "Version: ")))
  (when-let* ((dir (magit-toplevel))
              (file (or (sisyphus-resolve-changelog-file)
                        (when (yes-or-no-p (format
                                            "Create changelog file %s?"
                                            (abbreviate-file-name
                                             (expand-file-name
                                              "CHANGELOG.org"
                                              dir))))
                          (expand-file-name "CHANGELOG.org" dir)))))
    (unless (file-exists-p file)
      (write-region (format "* v%-9sUNRELEASED\n\n" version) nil
                    file))))

(defun sisyphus--bump-changelog (version &optional stub)
  "Update VERSION in changelog.
If STUB is non nil, insert as unreleased."
  (when-let* ((dir (magit-toplevel))
              (file (or (sisyphus-resolve-changelog-file)
                        (when (yes-or-no-p (format
                                            "Create changelog file %s?"
                                            (abbreviate-file-name
                                             (expand-file-name
                                              "CHANGELOG.org"
                                              dir))))
                          (expand-file-name "CHANGELOG.org" dir)))))
    (unless (file-exists-p file)
      (when-let* ((prev (sisyphus--previous-version)))
        (write-region (format "* v%-9sUNRELEASED\n\n" prev) nil
                      file)))
    (when (file-exists-p file)
      (sisyphus--with-file file
        (if (re-search-forward "^\\* v\\([^ ]+\\) +\\(.+\\)$" nil t)
            (let ((vers (match-string-no-properties 1))
                  (date (match-string-no-properties 2))
                  (prev (sisyphus--previous-version))
                  (today (format-time-string "%F")))
              (goto-char (line-beginning-position))
              (cond (stub
                     (insert (format "* v%-9sUNRELEASED\n\n" version)))
                    ((equal vers prev)
                     (insert (format "* v%-9s%s\n\n" version today))
                     (user-error "CHANGELOG entry missing; inserting stub"))
                    ((equal vers version)
                     (unless (equal date today)
                       (replace-match today nil t nil 2)))
                    ((y-or-n-p
                      (format "%sCHANGELOG version is %s, change%s to %s?"
                              (if prev
                                  (format "Previous version is %s, " prev) "")
                              vers
                              (if prev " latter" "")
                              version))
                     (delete-region (point)
                                    (line-end-position))
                     (insert (format "* v%-9s%s" version today)))
                    ((user-error "Abort"))))
          (insert (format "* v%-9sUNRELEASED\n\n" version)))))))

(defun sisyphus--list-files ()
  "Return nested list of elisp libraries, packages and org files."
  (let* ((lisp (if (file-directory-p "lisp") "lisp" "."))
         (docs (if (file-directory-p "docs") "docs" "."))
         (pkgs (nconc (directory-files lisp t "-pkg\\.el\\'")
                      (and (equal lisp "lisp")
                           (directory-files "." t "-pkg\\.el\\'"))))
         (libs (cl-set-difference (directory-files lisp t "\\.el\\'") pkgs))
         (orgs (cl-delete "CHANGELOG.org"
                          (cl-delete "README.org" (directory-files docs t
                                                                   "\\.org\\'")
                                     :test #'equal
                                     :key #'file-name-nondirectory))))
    (list libs pkgs orgs)))

(defun sisyphus--rpartial (fun &rest args)
  "Return a partial application of FUN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a new
function which does the same as FUN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  (lambda (&rest pre-args)
    (apply fun (append pre-args args))))

(defun sisyphus--bump-version (version)
  "Bump VERSION."
  (pcase-let*
      ((`(,libs ,pkgs ,orgs)
        (sisyphus--list-files))
       (updates (mapcar (lambda (lib)
                          (list (intern
                                 (file-name-sans-extension
                                  (file-name-nondirectory lib)))
                                version))
                        (if (> (length libs) 1) libs nil)))
       (pkg-updates (if (string-suffix-p sisyphus--non-release-suffix version)
                        (let ((timestamp (format-time-string "%Y%m%d")))
                          (mapcar (pcase-lambda (`(,pkg ,_))
                                    (list pkg timestamp))
                                  updates))
                      updates)))
    (mapc
     (sisyphus--rpartial #'sisyphus--bump-version-pkg version pkg-updates)
     pkgs)
    (mapc (sisyphus--rpartial #'sisyphus--bump-version-lib version updates)
          libs)
    (mapc (sisyphus--rpartial #'sisyphus--bump-version-org version)
          orgs)))

(defun sisyphus--bump-version-pkg (file version updates)
  "Write `define-package' form in FILE with VERSION and UPDATES.
UPDATES should be the alist of dependencies."
  (sisyphus--with-file file
    (pcase-let* ((`(,_ ,name ,_ ,docstring ,deps . ,props)
                  (read (current-buffer)))
                 (deps (cadr deps)))
      (erase-buffer)
      (insert (format "(define-package %S %S\n  %S\n  '("
                      name version docstring))
      (when deps
        (setq deps (sisyphus-update-dependencies deps updates))
        (let ((dep nil)
              (format
               (format "(%%-%is %%S)"
                       (apply #'max
                              (mapcar (lambda
                                        (%)
                                        (length
                                         (symbol-name
                                          (car %))))
                                      deps)))))
          (while (setq dep (pop deps))
            (indent-to 4)
            (insert (format format (car dep)
                            (cadr dep)))
            (when deps (insert "\n")))))
      (insert ")")
      (when props
        (let (key val)
          (while (setq key (pop props)
                       val (pop props))
            (insert (format "\n  %s %S" key val)))))
      (insert ")\n"))))

(defun sisyphus--bump-version-lib (file version updates)
  "Update VERSION and package requires with UPDATES in the FILE."
  (require 'lisp-mnt)
  (sisyphus--with-file file
    (when (lm-header "Package-Version")
      (delete-region (point)
                     (line-end-position))
      (insert version)
      (goto-char (point-min)))
    (when (lm-header "Version")
      (delete-region (point)
                     (line-end-position))
      (insert version)
      (goto-char (point-min)))
    (when (re-search-forward
           (format "(defconst %s-version \"\\([^\"]+\\)\""
                   (file-name-sans-extension
                    (file-name-nondirectory file)))
           nil t)
      (replace-match version nil t nil 1)
      (goto-char (point-min)))
    (unless (string-suffix-p "-git" version)
      (sisyphus-update-package-requires nil updates t)
      (let ((prev (sisyphus--previous-version)))
        (while (re-search-forward
                ":package-version '([^ ]+ +\\. +\"\\([^\"]+\\)\")" nil t)
          (let ((found (match-string-no-properties 1)))
            (when (and (magit--version> found prev)
                       (version< found version))
              (replace-match version nil t nil 1))))))
    (save-buffer)))

(defun sisyphus--bump-version-org (file version)
  "Update VERSION in org FILE."
  (sisyphus--with-file file
    (when (re-search-forward "^#\\+subtitle: for version \\(.+\\)$" nil t 1)
      (replace-match version t t nil 1))
    (when (re-search-forward "^This manual is for [^ ]+ version \\(.+\\)\\.$"
                             nil t 1)
      (replace-match version t t nil 1))
    (when (directory-files default-directory nil "\\.texi")
      (magit-call-process "make" "texi"))))

(defun sisyphus--bump-copyright ()
  "Update copyright."
  (pcase-let ((`(,libs ,_ ,orgs)
               (sisyphus--list-files)))
    (mapc #'sisyphus--bump-copyright-lib libs)
    (when (and orgs
               (directory-files default-directory nil "\\.texi"))
      (magit-call-process "make" "clean" "texi" "all"))))

(defun sisyphus--bump-copyright-lib (file)
  "Update copyright notice in FILE to indicate the current year."
  (sisyphus--with-file file
    (let ((copyright-update t)
          (copyright-query nil))
      (copyright-update))))

(defun sisyphus--commit (msg &optional allow-empty)
  "Create commit with MSG, possible with ALLOW-EMPTY flag."
  (let ((magit-inhibit-refresh t))
    (magit-stage-1 "-u"))
  (magit-commit-create
   (list "--edit" "--message" msg
         (if (eq transient-current-command 'magit-tag)
             (and-let* ((key (transient-arg-value
                              "--local-user=" (transient-args 'magit-tag))))
               (concat "--gpg-sign=" key))
           (transient-args 'magit-commit))
         (and allow-empty "--allow-empty"))))

;; (add-variable-watcher 'sisyphus-transient-suffixes
;;                       'sisyphus-transient-suffixes-watcher)

;; (sisyphus-transient-suffixes-watcher nil sisyphus-transient-suffixes nil nil)

(provide 'sisyphus)
;;; sisyphus.el ends here
;; Local Variables:
;; checkdoc-verb-check-experimental-flag: nil
;; End:
