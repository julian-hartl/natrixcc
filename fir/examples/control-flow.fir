fun u32 @t(u32) {
bb0(u32 v1):
v2 = add u32 v1, 1;
v6 = icmp bool eq v1, v2;
br bb1(v2, 7);
bb1(u32 v3, u32 v4):
v5 = add u32 v4, v3;
ret u32 v5;
}
