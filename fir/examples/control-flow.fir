fun u32 @t(u32) {
bb0(u32 v0):
    br bb1(0, v0);
bb1(u32 v3, u32 v4):
    v5 = icmp gt u32 v4, 0;
    condbr v5 bb6, bb2(v3);
bb2(u32 v7):
    v9 = icmp gt u32 v0, 20;
    condbr v9 bb3, bb4;
bb3:
    v10 = add u32 v7, 2;
    br bb5(v10);
bb4:
    v11 = add u32 v7, 1;
    br bb5(v11);
bb5(u32 v12):
    v13 = sub u32 v4, 1;
    br bb1(v12, v13);
bb6:
    ret u32 v3;
}
