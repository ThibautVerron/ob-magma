;;; ob-magma.el --- org-babel functions for magma evaluation

;; Copyright (C) 2015 Thibaut Verron (thibaut.verron@gmail.com)

;; Author: Thibaut Verron
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; These functions provide support for magma evaluation with
;; org-babel. Evaluation is made in a unique magma session by default,
;; explicit naming of sessions is possible.

;; Results type can be either 'output or 'value, in which case magma
;; tries to determine whether the output is a table or not. If your
;; output is a sequence and you do not wish to format it as a table,
;; use 'output for this code block.

;; Tne parameter `:magma-eval t' causes the block to be enclosed in an
;; `eval' form. The output value is given by the `return'
;; statement. At the moment, the return statement is handled like
;; other forms of output (for example calls to `print'). Note that no
;; side-effect is possible in an `eval' form. This is useful if you
;; want to run a test without changing the environment, but don't want
;; to fire up a new session just for this test.

;;; Requirements:

;; Use this section to list the requirements of this language.  Most
;; languages will require that at least the language be installed on
;; the user's system, and the Emacs major mode relevant to the
;; language be installed as well.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 's)

;; possibly require modes required for your language
(require 'magma-mode)


;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("magma" . "m"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:magma '())

(defvar org-babel-magma-eoe "end-of-echo")

(defun org-babel-magma-wrap-in-eval (body)
  "Wraps BODY in an eval form (escapes what needs to be)"
  (concat
   "eval \""
   (s-replace "\"" "\\\"" body)
   "\";"
   ))

(defconst org-babel-magma--scan-output
  "function scanOutput (str)
    try 
        res := eval str;
        if Type(res) eq SeqEnum then
            return \"table\";
        else
            return \"string\";
        end if;
    catch e 
        res := \"output\";
    end try;
    return res;
end function;"
  )

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:magma' function below.
(defun org-babel-expand-body:magma (body params )
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var)))
        (eval (cdr (assoc :magma-eval params))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s := %S;"
                (car pair) (org-babel-magma-var-to-magma (cdr pair))))
      vars "\n")
     "\n"
     (if eval
         (org-babel-magma-wrap-in-eval body)
       body)
     "\n"
     (format "print \"%s\";" org-babel-magma-eoe)
     )))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:magma (body params)
  "Execute a block of Magma code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Magma source code block")
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the session variable is non-nil
         (session (org-babel-magma-initiate-session (cdr (assoc :session params))))
         ;; variables assigned for use in the block
         ;(vars (cdr (assoc :vars params)))
         (result-params (cdr (assoc :result-params params)))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (cdr (assoc :result-type params)))
         ;; expand the body with `org-babel-expand-body:magma'
         (full-body (org-babel-expand-body:magma
                     body params ))
         (results
          (nth 0 (org-babel-comint-with-output
              (session org-babel-magma-eoe t full-body)
            (funcall #'insert full-body)
            (funcall #'comint-send-input)
            ;(funcall #'insert org-babel-magma-eoe)
            )))
         (results-wo-eoe (s-join "\n" (butlast (split-string results "\n") 2)))
         )
    ;; actually execute the source-code block either in a session or
    ;; possibly by dropping it to a temporary file and evaluating the
    ;; file.
    ;; 
    ;; for session based evaluation the functions defined in
    ;; `org-babel-comint' will probably be helpful.
    ;;
    ;; for external evaluation the functions defined in
    ;; `org-babel-eval' will probably be helpful.
    ;;
    ;; when forming a shell command, or a fragment of code in some
    ;; other language, please preprocess any file names involved with
    ;; the function `org-babel-process-file-name'. (See the way that
    ;; function is used in the language files)
    (if (or (eq result-type 'value) (eq result-type 'eval))
        (let* ((scan-body
                (concat org-babel-magma--scan-output
                        "\n"
                       "scanOutput(\""
                       results-wo-eoe
                       "\");\n"
                       (format "print \"%s\";\n" org-babel-magma-eoe)
                       ))
               (scan-res (nth 0 (org-babel-comint-with-output
                                (session org-babel-magma-eoe nil nil)
                              (funcall #'insert scan-body)
                              (funcall #'comint-send-input))))
               (type (car (split-string scan-res "\n"))))
         (if (s-matches? "^.*table[ \n]?$" type)
             (org-babel-script-escape results-wo-eoe)
           results-wo-eoe))
      results-wo-eoe))
    ;; (org-babel-reassemble-table
    ;;  results
    ;;  (org-babel-pick-name (cdr (assoc :colname-names params))
    ;;     		  (cdr (assoc :colnames params)))
    ;;  (org-babel-pick-name (cdr (assoc :rowname-names params))
    ;;     		  (cdr (assoc :rownames params))))
    )

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:magma (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-magma-var-to-magma (var)
  "Convert an elisp var into a string of magma source code
specifying a var of the same value."
  var
  ;(format "%s" var)
  )

(defun org-babel-magma-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-script-escape results)
  )

(defun org-babel-magma-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (let ((magma-interactive-use-comint t))
    (magma-comint-run (or session "org"))))

(provide 'ob-magma)
;;; ob-magma.el ends here
