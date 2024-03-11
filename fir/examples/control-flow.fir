fun u32 @mul(u32, u32) {
bb0(u32 v0, u32 v1):
  br bb1(0, v1);
bb1(u32 v2, u32 v3):
  br bb2(v2, v3);
}
