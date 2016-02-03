;; Coding System
(prefer-coding-system 'utf-8-dos)
(setq coding-system-for-read 'utf-8-dos)
(setq coding-system-for-write 'utf-8-dos)

;; test tls connection on windows for successfull download of packages
;; makes sure this returns t in the echo area
(gnutls-available-p)
;; ;; test the installed certificates

;; (condition-case e
;;     (delete-process
;;      (gnutls-negotiate
;;       :process (open-network-stream "test" nil "www.google.com" 443)
;;       :hostname "www.google.com"
;;       :verify-error t))
;;   (error e))

(setenv "PATH" (concat (getenv "PATH") ";H:\\Win7PoolData\\Desktop\\emacs\\bin"))

;; (getenv "PATH")
;; university pcs cant use https for some reason (error: cant reach melpa:443)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))

;; git settings
;; requires portable-git on the desktop
(add-to-list 'exec-path "H:/Win7PoolData/Desktop/PortableGit/mingw64/bin")
