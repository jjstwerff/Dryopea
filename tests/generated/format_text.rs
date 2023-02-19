fn test() {
  var_0 = Text("abc");
  {
    var_1 = Text("1");
    var_1 = OpAddText(var_1, OpFormatText(var_0, OpAddInt(2, 3), -1, 32));
    var_1 = OpAddText(var_1, Text("2"));
    var_1 = OpAddText(var_1, OpFormatText(var_0, 0, -1, 32));
    var_1 = OpAddText(var_1, Text("3"));
    var_1 = OpAddText(var_1, OpFormatText(var_0, 6, -1, 32));
    var_1 = OpAddText(var_1, Text("4"));
    var_1 = OpAddText(var_1, OpFormatText(var_0, 7, 1, 32));
    var_1;
  };
}

