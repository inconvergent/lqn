(defpackage #:lqn
  (:use #:common-lisp)
  (:nicknames #:cl-lqn)
  (:export #:d? #:v?
    #:qry #:qryd #:jsnqryf #:qryl #:proc-qry
    #:jsnloads #:jsnloadf #:jsnout #:ldnout #:ldnload #:fmt #:out #:jsnstr #:@* #:@@ #:??
    #:some? #:none? #:all? #:empty? #:size? #:is?
    #:path? #:subdir #:subfiles #:ls #:dir? #:file? #:cwd #:now #:cmd
    #:some? #:all? #:none? #:cd #:keys?
    #:new* #:new$ #:cat$ #:cat* #:head #:tail #:apply* #:range #:linspace #:psh* #:pop*
    #:flatn* #:compct #:flatall* #:flatn$ #:uniq
    #:pnum #:inum #:cnt
    #:noop #:kv? #:kw? #:ssym? #:msym? #:trim #:sym!
    #:num!? #:num? #:flt!? #:flt? #:int!? #:int?
    #:lst? #:lst! #:lst!? #:seq? #:seq!? #:str! #:str? #:str!? #:vec! #:vec? #:vec!?
    #:grp #:ind* #:sel #:seq
    #:sup #:sdwn #:strcat #:repl #:splt #:symb #:join
    #:pref? #:suf? #:sub? #:subx? #:ipref? #:isuf? #:isub? #:isubx?
    #:srt #:lpad #:rpad #:nstr #:ascii #:bar #:clmp
    #:dat-read-file #:dat-read-files #:dat-read-one #:dat-read-stream #:dat-export
    #:txt-read-file #:txt-read-stream #:txt-export))

