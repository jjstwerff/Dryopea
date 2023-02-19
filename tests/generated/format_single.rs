fn test() {
  {
    var_0 = Text("a");
    var_0 = OpAddText(var_0, OpFormatSingle(OpAddSingle(Single(0.1), OpMulSingle(OpConvSingleFromInt(2), Single(1.0))), 0, 0));
    var_0 = OpAddText(var_0, Text("b"));
    var_0;
  };
}

