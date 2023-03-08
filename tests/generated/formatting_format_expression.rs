fn test() {
  {
    var_0 = Text("ab");
    var_0 = OpAddText(var_0, OpFormatInt(OpAddInt(OpAddInt(1, 2), 32), 16, 0, 32, 0, 1));
    var_0 = OpAddText(var_0, Text("c"));
    var_0 = OpAddText(var_0, OpFormatInt(12, 8, 0, 32, 0, 0));
    var_0 = OpAddText(var_0, Text("d"));
    var_0 = OpAddText(var_0, OpFormatInt(391, 10, 0, 32, 0, 0));
    var_0 = OpAddText(var_0, Text("e"));
    var_0 = OpAddText(var_0, OpFormatInt(12, 10, 4, 32, 1, 0));
    var_0 = OpAddText(var_0, Text("f"));
    var_0 = OpAddText(var_0, OpFormatInt(1, 10, 3, 48, 0, 0));
    var_0 = OpAddText(var_0, Text("g"));
    var_0 = OpAddText(var_0, OpFormatInt(42, 2, 0, 32, 0, 0));
    var_0 = OpAddText(var_0, Text("h"));
    var_0;
  };
}

