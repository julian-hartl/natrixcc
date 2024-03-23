fun i64 @t(u8, u16, u32, u64, i8, i16) {
bb0(u8 v33, u16 v34, u32 v0, u64 v35, i8 v36, i16 v37):
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
    v13 = sub u32 v0, 1;
    v14 = sub u32 v4, 16;
    v15 = add u32 v12, 1;
    v16 = sub u32 v4, v15;
    br bb1(v12, v16);
bb6:
    v17 = u8 0;
    v25 = u8 v17;
    v18 = u16 1;
    v26 = u16 v18;
    v19 = u32 2;
    v27 = u32 v19;
    v20 = u64 3;
    v28 = u64 v20;
    v21 = i8 4;
    v29 = i8 v21;
    v22 = i16 5;
    v30 = i16 v22;
    v23 = i32 6;
    v31 = i32 v23;
    v24 = i64 7;
    v32 = i64 v24;
    ret i64 v32;
}
