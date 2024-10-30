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
   '("e47335cb55ccd2e363ec19b63470d1a59a06b31d526c4d75253275fa838199d5" "23bf8ef95269268a8588b9a7b778fd6208d10cf9f27a6e29477a4613a4cfabc9" "8f8f2461fbeb969fce55def4facda7306059044af964d4a430e2a9049acf5840" "7978f2b1b4f6bdd95913104d73c8ac8bbe4b76218c40d79b9fc3144f9c72dba1" "9088b7fb8084ecd7bdd69c94a01bf336de611bd12dc44c5940ff6de0d941e3cd" "e906e35aec7ea0371984a3ce42053e8b50626c663d1772e180be3a7f88301a3d" "357b33b39a96493ac5dc5eb3563b84dafdb3fb8578a079e8fde9b7d46c845e3a" "4bc6ac5884376521bf7c45591d9678b5aa6e0896c14e47a1153ab8fc263a70a9" "383b5172bd3096e74a85cde308945f50bb42af410fe4f16ae6a9cd5acaa18c8c" "b1b64ea4b108e985fa3a6e49d81e528ca10fa75f31aa7ab6e27e618f81c821b9" "0ad7c47c433811dcde791bf0f9c57cf856bc732f90a631d6350a571c419ddd9c" "575d5a4539561321eeaab001cf48b85e020e53d44595071b8c9f8a23fedee057" "784a860ab855adc7c4fa53d1c090314c623142341a0ac6e8bf3d49f0c108e806" "e3833cb275c54eed4eed8c9bb4d323f27c67aa1c8163b2700dc1cfd8d577e5f9" "ebfac9a16a781db5b3e7de662b7633d530f6dd88081ae021f78928165facd500" "dc68b051a9818ae4a1860144cf84447ad3af62444f6609d55eea545c373b205f" "10ddfad9e7a14c670dc98ad77ac80bc7f8d694c1b29000d142f3610dcb25b3f3" "5084228f42f500a8916f1da84552aee90badd33c03684fb0e4c53d076c29069e" "ea745cf83322bc21b7a4003d67d6520bd77f7c2c391e5d677fb0b7f7f1861577" "53a81c94484d4edccc534c7df9758ce7c9cd2d9493f8b0f019254b6f963b119a" "57fdb62ce7c8adb99a3a2b4b876b3ac83aea2fddd55c988ec3f73a0b5969c71b" "2113c4277baf502b9ce0c1dc246a01c349888101e37080829ac2a6e2a37cf913" "0425e6a9f024f9356cde53d74607bbdc661723a39ff1e5bf2390fea75e4c62c2" "ca5d79419eeb657c02caeb7440d2d992759e67792306f5cfd2aba977b0176d68" "e64618c1b072f41acf7ee54a2471d4774a2e582f69000a936c090dabd901bc0a" "bbfce276bcb0f23601a2bab5231f4c8efe0dcab9ff110ad23ebfda5538f70563" "b31547b3df491fe1df89315c5f37fefc190d419b7b27cbea1af9a075962e6c20" "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb" "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "37b6695bae243145fa2dfb41440c204cd22833c25cd1993b0f258905b9e65577" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290" "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd" "c8b3d9364302b16318e0f231981e94cbe4806cb5cde5732c3e5c3e05e1472434" "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434" "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40" "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2" "32f22d075269daabc5e661299ca9a08716aa8cda7e85310b9625c434041916af" "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1" "6f96a9ece5fdd0d3e04daea6aa63e13be26b48717820aa7b5889c602764cf23a" "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0" "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "e5fe2821284a2ebeef973ce98d226e3cfcd1cc9db93edac428465074a0728e5b" "92050cf108c301c40dd09d807c8b22b00c0bfd072c8ee5314d9c2e9fecc82389" "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641" "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1" "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87" "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "e8ceeba381ba723b59a9abc4961f41583112fc7dc0e886d9fc36fa1dc37b4079" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d" "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478" "df43432fa6a79723d60f8f1b9aee6322662d8ede2d18630f36a25558ec607f0e" "34bc2ac34ebe59f37e65951c77e617447a2dd9bb6e253feaa9819525134a4719" "bfcf8e7579b035eedf49f833d62be3e344b9fd70072f38310b6f74a6a992554b" "8bfc122b28bac3a8cde3a40ef84e02a0f2f9de7caaa95f78c87474df5aab87f8" "df8d372c7ddafc022ddc90e38e447191d812d5d217a23d24884c1d7e92f29b2b" "da3c546b1ac2df85576eb9f52e35e6da2fc335c8cddc536ed9e78d367c04d3d3" "2a2bfb765b3653bb5f21457ab7accbd8fc9b112febae2269424579449d546b22" "4a42ee8fb02d081eb4a1649508a87ae269d923e6f6588752405cb8344e696ab9" "00222825d0af66de87009a039babe13fb382b58a4feed4eb76dd6ee738052617" "c60f5193853be9ffdc49c14bd142014017a3893b2adf1e0a9456183608e67b61" "f11f2053cb8dd708aa178acc4708aeddf17968069c2600b17f7bb80b0ebd3022" "8ade8ae13ff409dde88d21aefde850b5d6868bc18477441fbbaf6d6ef1986fe6" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "dd2233164362df95ee2e5de3a9737c16d06da0a3488ee4a8ae3f71026304e6d7" "b37af2a85a264c57f2c93cc7cb19b2bd463039cb11eb587e17d48d14f72e298b" "a6b6fd1540953c0e5f077f3c3604e22d063c67e5bf4c316b3e3835e9d5bb1700" "a559531ec54e57aa6c864efb99b06b92a566207c7d887d4782e6d9f313f17118" "7e96e1959f1f2b7c25d6c2c73fbe13c3da44a283b8c7d8aa507220b5d602c001" "e6e1e7889014d00c9450aea465132e4975be190cac707facc7420795685eeb36" "b715f940ead28ee389d8e6d01ec467c348df9e2cc3a85ee17059db272ba0e835" "141b75f9e00389252006bbeb6fcdfd97e3ec886530d07202c4ae7a5cfb4a26f0" "f38610e0722380c9bd547fc278acdfe1ae662a8c29699be061379c053af13785" "ec272d0c0899a09e3907f72d9843bebe343d6b00f6717d0fcb67ad5e8224b35f" "1dce2af69dcc8e23a5cb22a1e6bab6c80eb7464e9db75a8a7a04c05b661ff59a" "a97786ffd82fdf64b2893931132e440b023f895f4c1e6439cbe249ed78cf8423" "4a38cdecf203b24ff5301de3498a6c4ca6bc506ec07c70698a2be736e96e104e" "b83d0ad7f8a39227c306815f5225cfbc9b2ae2d04d457523c8f63f6018f05ad5" "33a116ca3cf47b5c44704e22c7a5b847ef62147bb4bf62c2a72bbfc409cb847a" "94e9e3734a42b1a92f3c7b46c797581fe00a9c1f40aa56037dd03b9fc9e4b716" "22baba3cedadbfddcb20b1c9d04da1cbc395d670050772855fb4557a4db2c93b" "38b5009e5b9bfc523129ed1f85aa36df178399b19ff5eb7c980378fe8aa54967" "dfa2cc9d420af81b30ee9d2304d9e17a27631c77d11c285a2571deca22cad863" default))
 '(package-selected-packages
   '(lsp-ui lsp-mode flycheck-rust rust-mode rainbow-mode marginalia hotfuzz vertico company-box company-quickhelp company-statistics company format-all consult-flycheck flycheck cider lispy lua-mode nix-mode clojure-mode smartparens which-key vterm consult good-scroll doom-themes all-the-icons-ibuffer ace-window deft htmlize org-bullets org-contrib evil-leader evil-collection evil gcmh)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
