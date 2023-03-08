fn routine(a: i32, ) {
  if OpGtInt(var_0, 4) {
    return OpAddInt(var_0, 1);
  } else {
    return 1;
  };
  2;
}

fn test() {
  OpAddInt(routine(5), routine(2));
}

