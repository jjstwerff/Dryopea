fn test() {
  {
    var_0 = Text("a");
    var_0 = OpAddText(var_0, OpFormatLong(OpAddLong(Long(1), OpConvLongFromInt(1)), 10, 4, 32, 1, 0));
    var_0 = OpAddText(var_0, Text("b"));
    var_0 = OpAddText(var_0, OpFormatInt(OpCastIntFromLong(Long(12)), 10, 0, 32, 0, 0));
    var_0 = OpAddText(var_0, Text("c"));
    var_0 = OpAddText(var_0, OpFormatText(if OpGeLong(OpMulLong(Long(2), OpRemLong(Long(4), Long(6))), OpConvLongFromInt(8)) {Text("true")} else {Text("false")}, 0, -1, 32));
    var_0 = OpAddText(var_0, Text("d"));
    var_0;
  };
}

