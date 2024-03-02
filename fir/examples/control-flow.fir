fun u32 mul(u32, u32) {
; symbol table:
;   a: { %0 }
;   b: { %1, %3, %6, %8 }
;   c: { %2, %4, %5, %7 }
bb0(u32 %0, u32 %1):
  br bb1(0, %1);
bb1(u32 %2, u32 %3):
  %4 = icmp i1 gt %3, 0;
  condbr %4, bb2(%2, %3), bb3;
bb2(u32 %5, u32 %6):
  %7 = add u32 %5, %0;
  %8 = sub u32 %6, 1;
  br bb1(%7, %8);
bb3:
  ret %2;
}

fun u32 mul(u32, u32) {
; symbol table:
;   a: { %0 }
;   b: { %1, %3, %6, %8 }
;   c: { %2, %4, %5, %7 }
bb0(u32 %0, u32 %1):
  br bb1(0, %1);
bb1(u32 %2, u32 %3):
  %4 = icmp gt %3, 0;
  condbr %4, bb2(%2, %3), bb3;
bb2(u32 %5, u32 %6):
  %7 = add %5, %0;
  %8 = sub %6, 1;
  br bb1(%7, %8);
bb3:
  ret %2;
}
