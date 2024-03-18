fun u32 @t(u32) {
bb0(u32 v1):
v2 = add u32 v1, 1;
v6 = icmp bool eq v1, v2;
condbr bool v6, bb1, bb2(v1);
bb1:
br bb2(v2);
bb2(u32 v7):
v8 = add u32 v7, 1;
ret u32 v8;
}
