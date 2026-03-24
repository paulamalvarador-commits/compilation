#!/bin/bash

for f in t1_basic_push.pfx t2_add.pfx t3_swap_sub.pfx t4_div.pfx t5_mul_comments.pfx t6_rem.pfx t7_stack_errors.pfx t8_div_zero.pfx t9_args_add.pfx
do
  echo "===== $f ====="
  dune exec ./pfx/pfxVM.exe -- "$f"
  echo
done
