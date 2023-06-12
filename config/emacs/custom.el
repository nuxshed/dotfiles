(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)
     ((:application eshell)
      eshell-connection-default-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))
     (eshell-connection-default-profile
      (eshell-path-env-list))))
 '(custom-safe-themes
   '("7e96e1959f1f2b7c25d6c2c73fbe13c3da44a283b8c7d8aa507220b5d602c001" "e6e1e7889014d00c9450aea465132e4975be190cac707facc7420795685eeb36" "b715f940ead28ee389d8e6d01ec467c348df9e2cc3a85ee17059db272ba0e835" "141b75f9e00389252006bbeb6fcdfd97e3ec886530d07202c4ae7a5cfb4a26f0" "f38610e0722380c9bd547fc278acdfe1ae662a8c29699be061379c053af13785" "ec272d0c0899a09e3907f72d9843bebe343d6b00f6717d0fcb67ad5e8224b35f" "1dce2af69dcc8e23a5cb22a1e6bab6c80eb7464e9db75a8a7a04c05b661ff59a" "a97786ffd82fdf64b2893931132e440b023f895f4c1e6439cbe249ed78cf8423" "4a38cdecf203b24ff5301de3498a6c4ca6bc506ec07c70698a2be736e96e104e" "b83d0ad7f8a39227c306815f5225cfbc9b2ae2d04d457523c8f63f6018f05ad5" "33a116ca3cf47b5c44704e22c7a5b847ef62147bb4bf62c2a72bbfc409cb847a" "94e9e3734a42b1a92f3c7b46c797581fe00a9c1f40aa56037dd03b9fc9e4b716" "22baba3cedadbfddcb20b1c9d04da1cbc395d670050772855fb4557a4db2c93b" "38b5009e5b9bfc523129ed1f85aa36df178399b19ff5eb7c980378fe8aa54967" "dfa2cc9d420af81b30ee9d2304d9e17a27631c77d11c285a2571deca22cad863" default))
 '(package-selected-packages
   '(rainbow-mode marginalia hotfuzz vertico company-box company-quickhelp company-statistics company format-all consult-flycheck flycheck cider lispy lua-mode nix-mode clojure-mode smartparens which-key vterm consult good-scroll doom-themes all-the-icons-ibuffer ace-window deft htmlize org-bullets org-contrib evil-leader evil-collection evil gcmh)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
