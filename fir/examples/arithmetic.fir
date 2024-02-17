fun i32 @add(i32 %a, i32 %b):
.entry:
  %c = add i32 %a, %b;
  ret %c;

fun i32 @sub(i32 %a, i32 %b):
.entry:
  %c = sub i32 %a, %b;
  ret %c;
