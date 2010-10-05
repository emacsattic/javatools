;;; javatools.el --- java functions to compile and run small java programs
;;;
;;; Author: Halil Oezdemir <haLiLLix@googlemail.com>
;;;         
;;; Keywords: java, programming
;;; Copyright: (C) 2008 Halil Oezdemir
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; Please send suggestions and bug reports to <haLiLLix@googlemail.com>. 

;;; Overview ==========================================================
;; These java functions help you to compile and run small java
;; programs. For instance if you want to test if a java program compiles
;; or if it produces the right output than this is the right tool. 
;; For larger programs with hundreds of classes eclipse or some other
;; dedicated IDE would be the better choice. 
;; The provided important functions are: 
;;   java-compile
;;     This compiles a java program. Note that the sourcepath is 
;;     automatically determined by looking at the package statement
;;     if exists. If there is no package statement there is no need
;;     need to set the sourcepath either. Also the classpath is set 
;;     to the classpath envirenment variable content. 
;;   java-compile-without-prompt
;;     Same as java-compile, but the user is not asked to confirm. 
;;     While using java-compile the user can modify the compile command
;;     parameters this is not possible here.
;;   java-run
;;     Runs the java program. If source is not compiled then a compile
;;     command will first be suggested. The classpath and sourcepath
;;     are set like in java-compile.
;;   java-run-without-prompt
;;     Similar to java-compile-without-prompt: No prompt.
;;   scaffoldApplet
;;     A html-file for the applet is generated and started via
;;     jdk-appletviewer. Of course the generated html-file can
;;     also be viewed a java-enabled web browser.
;;
;;
;; Some convenience functions while editing java code:
;;   sop
;;     Insert "System.out.println(" into buffer.
;;   main
;;     Insert main method (with empty content) into buffer.
;;   buildClassScaffold
;;     Build the scaffold of a class. Usually used if buffer contents
;;     is empty. For example if you use this command in buffer test.java 
;;     then following will be inserted: 
;;                public class test
;;                {
;;                    public static void main(String argv[])
;;                    {
;;        	
;;                    }
;;                }
;;   surroundTryCatch
;;     The marked block is surrounded by a try-catch-block
;;     (.. catch(Exception e){} )
;;     
;; Installation ========================================================
;;
;; Place this file into your HOME directory (same directory where 
;; your .emacs (or _emacs)-file resideds). Then insert the line:
;;     (load "~/javatools.el")
;; into your .emacs-File.
;; Me have following keybindings in my .emacs-File:
;;   (global-set-key [(meta p)] 'indentWholeBuffer)     ; [Esc]-[p]
;;   (global-set-key [f9]  'java-compile)
;;   (global-set-key [f10] 'java-compile-without-prompt)
;;   (global-set-key [f11] 'java-run)
;;   (global-set-key [f12] 'java-run-without-prompt)
;;   (global-set-key [f4]  'grep1)
;;   (global-set-key [f5]  'sop)
;;   (global-set-key [f6]  'main)
;;   (global-set-key [f7]  'buildClassScaffold)
;;   (global-set-key [f8]  'scaffoldApplet)
;; 
;; That means all important java functions are bound to the 
;; function keys so that they are easily accessible.
;; 
;;
;; javatools.el is only tested on emacs >= 22 
;;
;;

;;; History

;;   
;;;   0.1: first release

;;; Code:


(defun scaffoldApplet()
  "scaffolding applets (create html automatically and start appletviewer afterwards)" 
  (interactive)
  (let (contents bufName fileName codebase stem file-name-length class-file-name)
    (progn
      (setq file-name-length (- (length (buffer-name)) 5))
      (setq stem (substring (buffer-name) 0 file-name-length))
      (setq class-file-name (concat (file-name-nondirectory stem) ".class"))
      (save-some-buffers)
    
      (setq saveBufferName (buffer-name))
      (if (file-newer-than-file-p buffer-file-name class-file-name)
	  (let ((compile-buffer (java-compile)))
	    ;; we need to compile the file before running java
	    (let ((proc (get-buffer-process compile-buffer)))
	      (set-process-sentinel proc 'java-compile-sentinel2)))
      
	(scaffoldAppletInternal)
	))))

;; This comp.finished-sentinel is called from scaffoldApplet
(defun java-compile-sentinel2 (proc msg)
  "Sentinel for compilation buffers."
  (if (memq (process-status proc) '(signal exit))
      (progn
	(set-buffer (compilation-find-buffer))
	(beginning-of-buffer)
	(condition-case err
	    (compilation-next-error 1)   
	   
	  ;; This is the handler; it is not a form.
	  (error (princ (format "The error was: %s" err))
		 2))
      
	(if (> (point) 1)		;errors
	    (message "There are errors!")
	  (progn
	    (scaffoldAppletInternal)
	    )
	  )
	)
    )
  )


(defun scaffoldAppletInternal()
  "scaffolding applets (create html automatically and start appletviewer afterwards)"
  (let (contents bufName fileName codebase stem file-name-length class-file-name compile-command)
    (progn
      (switch-to-buffer saveBufferName)
      (setq file-name-length (- (length (buffer-name)) 5))
      (setq stem (substring (buffer-name) 0 file-name-length))
      (setq class-file-name (concat (file-name-nondirectory stem) ".class"))

      (setq appletName 
	    (read-from-minibuffer "Scaffold Applet class name:" stem nil nil  nil nil nil))
    
      (setq contents (concat "<HTML><APPLET " (getClassPath 3)))
      (setq contents (concat contents appletName))
      (setq contents (concat contents ".class\" "))
      (setq contents 
	    (concat contents " WIDTH=600 HEIGHT=600 ALIGN=middle></APPLET></HTML>"))
      (setq fileName (concat appletName ".html"))
      (with-temp-buffer
	(insert contents)
	(setq fileName (concat "GNUEmacs_AutoGen_" fileName))
	(when (file-writable-p fileName)
	  (write-region (point-min)
			(point-max)
			fileName)))
       (setq compile-command (concat "appletviewer " fileName))
       (compile compile-command)
      )
    ))


(defun getClassPath (javac_or_java_or_plain)
  "Returns the user current directory setting and the classpath flag along with the package prefix. These are determined by the package statement at the beginning of the java code. If there is no package statement the returned string is empty. Also the directorys from system CLASSPATH are added to the classpath. For example if the package statement is \"package ext.games\;\" and the current directory is \"D:\\progs\" then the string which is returned is \" -Duser.dir=\"D:\\progs\\..\\..\" -classpath D:\\progs\\..\\.. ext.games.\". If javac_or_java_plain is 1 then only the sourcepath is returned, for example: \" -sourcepath D:\\progs\\..\\..\". If it is 3 it means that the result is in a form which can be put in an APPLET tag."
  (save-excursion
    (beginning-of-buffer)
    (setq retval "")
    (setq regstr "^[ \t]*package[ \t]+[a-zA-ZÄäüÜöÖ_]+[a-zA-Z0-9_\.ÄäüÜöÖ]*[ \t]*[;]+")
    (setq mymatchstring "")
    (setq found 1)

    (if (re-search-forward regstr nil t)
	(progn
	  (let (list1, list2, packageName, slashes, tmpstr, 
		       sourcepath, tmpstr2, delims, counter)
	    (setq mymatchstring (match-string 0))
	    (setq delims "[ \f\t\n\r\v;]+")
	    (setq list1 (split-string mymatchstring delims))
	    (setq counter 0)
	    (while (and (< counter 1) (car list1))
	      (if(> (length (car list1)) 0)
		  (setq counter (1+ counter)))
	      (setq list1 (cdr list1))
	      )
	    
	    (setq list1 (car list1))
	    (setq list2 (split-string list1 "\\."))
	    (setq slashes "")
	    (setq packageName "")
	    (while (car list2) 
	      (setq packageName (concat packageName (car list2)))
	      (setq packageName (concat packageName "."))
	      (setq list2 (cdr list2))
	      (setq slashes (concat slashes ".."))
	      (if (car list2)
		  (setq slashes (concat slashes "/")));geandert
	      )

	    ; need only full path for java and javac but not for applet
	    (if (< javac_or_java_or_plain 3)
		(progn
		  (setq tmpstr2 (substring (buffer-file-name) (- (length (buffer-file-name))) (- (length (buffer-name)))))
		  (setq slashes (concat tmpstr2 slashes))
		  )
	      )
	    
	    (if (= 3 javac_or_java_or_plain)
		(progn
		  (setq slashes (concat " codebase=\"" slashes))
		  (setq slashes (concat slashes "\" CODE=\""))
		  (setq slashes (concat slashes packageName))
		  (setq retval slashes)
		  )
	      (progn
		(setq workingDir slashes)
		(if (getenv "CLASSPATH")
			(setq slashes (concat slashes ";"))
		  )
		(setq slashes (concat "\"" slashes))
		
		(setq slashes (concat slashes (getenv "CLASSPATH")))	
		(setq slashes (concat slashes "\""))
		(setq slashes (concat  " -classpath " slashes))

		(if (= 1 javac_or_java_or_plain)
		    (progn
		      (setq sourcepath (concat " -sourcepath \"" workingDir))
		      (setq sourcepath (concat sourcepath "\" "))
		      (setq retval (concat slashes sourcepath))
		      )
		  (progn
		    (setq packageName (concat  " " packageName))
		    (setq retval  (concat slashes packageName))
		    )
		  )
		)
	      )
	    )
	  )
       ; No package statement?? So we have to take info from somewhere else
      (if (or (= 0 javac_or_java_or_plain) (= 1 javac_or_java_or_plain))
	  (let (list2 tmpstr tmpstr2)
	    (progn
	      (setq tmpstr2 (substring (buffer-file-name) (- (length (buffer-file-name))) (- (+ 1 (length (buffer-name))))))
	      (setq workingDir tmpstr2)
	      (setq retval " -classpath \"")
	      (setq retval (concat retval tmpstr2))
	      (if (getenv "CLASSPATH")
	      	(progn
	      	  (setq retval (concat retval ";"))
	          (setq retval (concat retval (getenv "CLASSPATH")))
		  ))
	       (setq retval (concat retval "\" "))
	      )
	    )
	(if (= 3 javac_or_java_or_plain)
	    (progn
	      (setq retval " codebase=\".\" CODE=\"")
	      )
	  )
	)
      )
    retval 
    )
  )


(add-hook 'java-mode-hook 'java-menu)

(defun java-menu ()
  (progn
  (define-key java-mode-map [menu-bar compile]
    (cons "Compile" (make-sparse-keymap "Compile")))

  (define-key java-mode-map "\em" 'compile)
  (define-key java-mode-map [menu-bar compile make]
    '("Make Project" . compile))

  (define-key java-mode-map "\ec" 'java-compile)
  (define-key java-mode-map [menu-bar compile java-compile]
    '("Compile" . java-compile))

  (define-key java-mode-map "\er" 'java-run)
  (define-key java-mode-map [menu-bar compile java-run]
    '("Run" . java-run))
  ))

(defun java-compile ()
  "Compile Java program in current buffer"
  (interactive)
  (let (compile-command)
  (setq compile-command (concat "javac " (concat (getClassPath 1) (buffer-name))))
  (setq java-compile-command (read-from-minibuffer "Compile command: "
						   compile-command nil nil
						   '(java-compile-history . 1))))

  (compile java-compile-command)
  )

(defun java-compile-without-prompt ()
  "Compile Java program in current buffer"
  (interactive)
    (let (compile-command)
    (setq compile-command (concat "javac " (concat (getClassPath 1) (buffer-name))))
    (compile compile-command)
  )
)


(defun java-run-without-prompt ()
  "java run without prompt"
  (interactive)
  (let (file-name-length
	stem
	file-name
	class-file-name
	compile-buffer)
    (progn
      ;; strip .java from name - shuld check it is there!
      (setq file-name-length (- (length buffer-file-name) 5))
      (setq stem (substring buffer-file-name 0 file-name-length))
      (setq file-name (file-name-nondirectory stem))
      
      ;; this won't work if a package with '.' fields is executed!
      (setq java-shell-args (concat "java " (concat (getClassPath 0) file-name)))
      
      (set (java-explicit-shell-args) (cons "-c" (cons java-shell-args ())))
      (java-run-internal)
      )))

(defun java-run ()
  "Run Java program, using current buffer for name of class to run"
  (interactive)
  (let (file-name-length
	stem
	file-name
	class-file-name
	compile-buffer)
  (progn
    ;; strip .java from name - shuld check it is there!
    (setq file-name-length (- (length buffer-file-name) 5))
    (setq stem (substring buffer-file-name 0 file-name-length))
    (setq file-name (file-name-nondirectory stem))

    ;; this won't work if a package with '.' fields is executed!
    (setq java-shell-args (concat "java " (concat (getClassPath 0) file-name)))

    ;; make sure class file is up-to-date
    (save-some-buffers)
    (setq class-file-name (concat file-name ".class"))
    (if (file-newer-than-file-p buffer-file-name class-file-name)
	(let ((compile-buffer (java-compile)))
	  ;; we need to compile the file before running java
	  (let ((proc (get-buffer-process compile-buffer)))
	    (set-process-sentinel proc 'java-compile-sentinel)))
      (progn
        (set (java-explicit-shell-args) (java-run-command))
        (java-run-internal))))))

(defun java-run-internal ()
 "internal function handling java running process"
 (let (do-it)
   (setq do-it nil)
        (if (get-process "shell")
            (if (y-or-n-p "shell already running: kill it && start new java?")
                (progn
                  (delete-process "shell")
                  (setq do-it t)
                  )
              (setq do-it nil)
              )
          (setq do-it t))
        
          (if do-it
              (progn
                ;;no compile needed, just run java
                ;;the value of explicit-<shell>-args is used as
                ;;the command to be run by the shell
                (switch-to-buffer-other-window "*shell*")
                (erase-buffer)
                (cd workingDir)
                (shell)))))
   

(defun java-explicit-shell-args ()
  "returns the symbol explicit-<shell>-args where <shell>
is the name of the current shell e.g. returns
'explicit-bash-args if your favoured shell is bash"
  (let* ((full-shell (or (getenv "ESHELL")
			 (getenv "SHELL")
			 "sh"))
	 (shell (file-name-nondirectory full-shell)))
  (intern (concat "explicit-" (concat shell "-args")))))

(defun java-run-command ()
  "return the java run command for the shell to execute"
  (let ((args java-shell-args))
  (setq java-command (read-from-minibuffer "Run command: "
					   args nil nil
					   '(java-run-history . 1))))
  (cons "-c" (cons java-command ())))

(defun java-shell-sentinel (proc msg)
  "sentinel for java run completion"
  (switch-to-buffer (other-buffer)))

;; sentinel called when compilation is finished
(defun java-compile-sentinel (proc msg)
  "Sentinel for compilation buffers."
  (if (memq (process-status proc) '(signal exit))
  (progn
    (set-buffer (compilation-find-buffer))
    (beginning-of-buffer)
    (condition-case err
	(compilation-next-error 1)   
	   
      ;; This is the handler; it is not a form.
      (error (princ (format "The error was: %s" err))
	     2))
      
    (if (> (point) 1)			;errors
	(message "There are errors!")
      (progn
	(set (java-explicit-shell-args) (java-run-command))
	(java-run-internal))))))


(defun buildClassScaffold()
  "builds the scaffold of class"
  (interactive) 
  (let (file-name-length stem savepoint)
    (progn
      (setq file-name-length (- (length (buffer-name)) 5))
      (setq stem (substring (buffer-name) 0 file-name-length))
      (insert "public class ")
      (insert stem)
      (insert "\n{\n")
      (setq savepoint (point))
      (insert "\n}\n")
      (goto-char savepoint)
      (main)
      )
    )
  )


(defun main()
  "inserts main function declaration into buffer"
  (interactive) 
  (let (start end)
    (progn
      (setq start (point))
      (insert "public static void main(String argv[])\n{\n\n}")
      (setq end (point))      
      (backward-char 2)
      (indent-region start end nil)
      (indent-according-to-mode)
      )
    )
  )

(defun sop()
  "inserts << System.out.println( >>  into buffer"
  (interactive) 
  (insert "System.out.println(")
  )

 (defun openAllJavaInDir()
  "opens all java files in current directory"
  (interactive)  
  (find-file-noselect "*.java" nil nil 1)
  )


(defun surroundTryCatch()
  "surround marked block with try/catch"
  (interactive)
  (save-excursion
    (let (savepoint savepoint2)
      (progn
	(setq savepoint  (region-beginning))
	(setq savepoint2 (region-end))
	(goto-char (region-end))
	(insert "\n}\n")
	(insert "catch(Exception e)\n")
	(insert "{\n")
	(insert "e.printStackTrace();\n")
	(insert "}\n")
	(goto-char savepoint)
	(insert "try\n")
	(insert "{\n")
	(deactivate-mark)
	(setq savepoint2 (+ savepoint2 55))
	(indent-region savepoint savepoint2)
	))))
