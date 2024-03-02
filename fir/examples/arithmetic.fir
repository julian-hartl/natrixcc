fun i32 @add(i32, i32) {
bb0(i32 %0, i32 %1):
  %c = add i32 %0, %1;
  ret %c;
}

fun i32 @sub(i32, i32):
entry(i32 %a, i32 %b):
  %c = sub i32 %a, %b;
  ret %c;
