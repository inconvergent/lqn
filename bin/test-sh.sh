#!/bin/bash

set -e

echo '
#### running terminal tests:'

check () {
  if [ "$r" = "$a" ]; then echo "   - ok" ; else echo "fail!
wants: $a
got  : $r"; exit 20; fi; };

###### LQN #####################################################################
echo '## running LQN terminal tests:'

a='[1,2]'
r=`echo '1 2' | sbcl --script ./lqn-sh.lisp -jm '_'`; check;

a='#(1 2)'
r=`echo '1 2' | sbcl --script ./lqn-sh.lisp -lm '_'`; check;

a='1
2'
r=`echo '1 2' | sbcl --script ./lqn-sh.lisp -t '_'`; check;

a='"- 0:0x- 1:1x- 2:1x- 3:2x- 4:3x- 5:5x- 6:8x- 7:13x- 8:21x- 9:34x- 10:55x- 11:89x- 12:144x- 13:233x- 14:377x- 15:610x- 16:987x- 17:1597x- 18:2584x- 19:4181x- 20:6765x- 21:10946"'
r=`echo '#(0 1)' |\
    sbcl --script ./lqn-sh.lisp -l \
    '(?rec (< (@ -1) 10000)
           (cat* _ (apply* + (tail* _ 2))))
           #((str! "- " (cnt) ":" _)) (join _ "x")'`; check;

a='[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946]'
r=`echo '#(0 1)' |\
    sbcl --script ./lqn-sh.lisp -jm \
    '(?rec (< (@ -1) 10000)
           (cat* _ (apply* + (tail* _ 2))))'`; check;

###### TQN #####################################################################
echo '## running TQN terminal tests:'

a='["1","2"]'
r=`echo '1
2' | sbcl --script ./tqn-sh.lisp -jm '_'`; check;

a='1
2'
r=`echo '1
2' | sbcl --script ./tqn-sh.lisp -t '_'`; check;

a='#(1 2)'
r=`echo '1
2' | sbcl --script ./tqn-sh.lisp -l 'int!?'`; check;

a='109'
r=`echo '1 x 1 x 7 x 100' |\
    sbcl --script ./tqn-sh.lisp '(splt _ :x) int!? (*fld 0 +)'`; check;

a='[{"v":1},{"v":1},{"v":7},{"v":100}]'
r=`echo '1 x 1 x 7 x 100' |\
    sbcl --script ./tqn-sh.lisp -jm '(splt _ :x) int!? #((new$ :v _))'`; check;

a='((:V . 1))
((:V . 3))'
r=`echo '1 x 3 x ' |\
    sbcl --script ./tqn-sh.lisp '(splt _ :x) int!? [is?] #((new$ :v _))'`; check;

a='#(((:V . 1)) ((:V . 1)) ((:V . 7)) ((:V . 100)))'
r=`echo '1 x 1 x 7 x 100' |\
    sbcl --script ./tqn-sh.lisp -lm '(splt _ :x) int!? #((new$ :v _))'`; check;

a='["x","dkef","kkkk1","uu33"]'
r=`echo 'abk c x dkef x kkkk1 x uu33' |\
    sbcl --script ./tqn-sh.lisp -jm '(splt _ :x) trim (?txpr +@str!? "+@ab" :x)'`
check;

###### JQN #####################################################################
echo '## running JQN terminal tests:'

a='1
2'
r=`echo '1 2' | sbcl --script ./jqn-sh.lisp -j '_'`; check;
r=`echo '1 2' | sbcl --script ./jqn-sh.lisp -l '_'`; check;
r=`echo '1 2' | sbcl --script ./jqn-sh.lisp -t '_'`; check;

a='{"_id":1}'
r=`echo '{"_id": 1}' | sbcl --script ./jqn-sh.lisp -jm '{:_id}'`; check;

a='((:_ID . 1))'
r=`echo '{"_id": 1}' | sbcl --script ./jqn-sh.lisp -lm '{:_id}'`; check;

a='[{"_id":"65679","things":[{"id":10}]}]
[{"_id":"6AABB"}]'
r=`echo '{ "_id": "65679", "things": [ { "id": 10 } ] }
         { "_id": "6AABB" }' |\
    sbcl --script ./jqn-sh.lisp -m '#{:_id :?@things}'`; check;

echo '## done! all clear!'

