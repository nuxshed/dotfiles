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
   '("df43432fa6a79723d60f8f1b9aee6322662d8ede2d18630f36a25558ec607f0e" "34bc2ac34ebe59f37e65951c77e617447a2dd9bb6e253feaa9819525134a4719" "bfcf8e7579b035eedf49f833d62be3e344b9fd70072f38310b6f74a6a992554b" "8bfc122b28bac3a8cde3a40ef84e02a0f2f9de7caaa95f78c87474df5aab87f8" "df8d372c7ddafc022ddc90e38e447191d812d5d217a23d24884c1d7e92f29b2b" "da3c546b1ac2df85576eb9f52e35e6da2fc335c8cddc536ed9e78d367c04d3d3" "2a2bfb765b3653bb5f21457ab7accbd8fc9b112febae2269424579449d546b22" "4a42ee8fb02d081eb4a1649508a87ae269d923e6f6588752405cb8344e696ab9" "00222825d0af66de87009a039babe13fb382b58a4feed4eb76dd6ee738052617" "c60f5193853be9ffdc49c14bd142014017a3893b2adf1e0a9456183608e67b61" "f11f2053cb8dd708aa178acc4708aeddf17968069c2600b17f7bb80b0ebd3022" "8ade8ae13ff409dde88d21aefde850b5d6868bc18477441fbbaf6d6ef1986fe6" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "dd2233164362df95ee2e5de3a9737c16d06da0a3488ee4a8ae3f71026304e6d7" "b37af2a85a264c57f2c93cc7cb19b2bd463039cb11eb587e17d48d14f72e298b" "a6b6fd1540953c0e5f077f3c3604e22d063c67e5bf4c316b3e3835e9d5bb1700" "a559531ec54e57aa6c864efb99b06b92a566207c7d887d4782e6d9f313f17118" "7e96e1959f1f2b7c25d6c2c73fbe13c3da44a283b8c7d8aa507220b5d602c001" "e6e1e7889014d00c9450aea465132e4975be190cac707facc7420795685eeb36" "b715f940ead28ee389d8e6d01ec467c348df9e2cc3a85ee17059db272ba0e835" "141b75f9e00389252006bbeb6fcdfd97e3ec886530d07202c4ae7a5cfb4a26f0" "f38610e0722380c9bd547fc278acdfe1ae662a8c29699be061379c053af13785" "ec272d0c0899a09e3907f72d9843bebe343d6b00f6717d0fcb67ad5e8224b35f" "1dce2af69dcc8e23a5cb22a1e6bab6c80eb7464e9db75a8a7a04c05b661ff59a" "a97786ffd82fdf64b2893931132e440b023f895f4c1e6439cbe249ed78cf8423" "4a38cdecf203b24ff5301de3498a6c4ca6bc506ec07c70698a2be736e96e104e" "b83d0ad7f8a39227c306815f5225cfbc9b2ae2d04d457523c8f63f6018f05ad5" "33a116ca3cf47b5c44704e22c7a5b847ef62147bb4bf62c2a72bbfc409cb847a" "94e9e3734a42b1a92f3c7b46c797581fe00a9c1f40aa56037dd03b9fc9e4b716" "22baba3cedadbfddcb20b1c9d04da1cbc395d670050772855fb4557a4db2c93b" "38b5009e5b9bfc523129ed1f85aa36df178399b19ff5eb7c980378fe8aa54967" "dfa2cc9d420af81b30ee9d2304d9e17a27631c77d11c285a2571deca22cad863" default))
 '(package-selected-packages
   '(rust-mode rainbow-mode marginalia hotfuzz vertico company-box company-quickhelp company-statistics company format-all consult-flycheck flycheck cider lispy lua-mode nix-mode clojure-mode smartparens which-key vterm consult good-scroll doom-themes all-the-icons-ibuffer ace-window deft htmlize org-bullets org-contrib evil-leader evil-collection evil gcmh)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
