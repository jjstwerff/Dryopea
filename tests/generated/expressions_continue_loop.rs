fn test() {
  var_0 = 0;
  {
    var_2 = 0;
    loop {
      var_1 = if OpGeInt(var_2, 10) {Null} else {
        var_3 = var_2;
        var_2 = OpAddInt(var_2, 1);
        var_3;
      };
      if OpEqInt(var_1, Null) {Break(0)} else {Null};
      {
        if OpEqInt(var_1, 2) {
          Continue(0);
        } else {Null};
        if OpGtInt(var_1, 5) {
          return var_0;
        } else {Null};
        var_0 = OpAddInt(var_0, var_1);
      };
    };
  };
  var_0;
}

