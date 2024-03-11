fun i32 @add(i32, i32) {
bb0(i32 v0, i32 v1):
  v2 = add i32 v0, v1;
  ret i32 v2;
}

fun i32 @sub(i32, i32) {
bb0(i32 v0, i32 v1):
  v2 = sub i32 v0, v1;
  v3 = i32 v0;
  v4 = i32 v1;
  ret i32 v2;
}
