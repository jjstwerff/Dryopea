fn test() {
  {
    var_0 = Text("a");
    var_0 = OpAddText(var_0, OpFormatFloat(Float(1.2), 4, 2));
    var_0 = OpAddText(var_0, Text("b"));
    var_0 = OpAddText(var_0, OpFormatFloat(Float(1.34), 0, 0));
    var_0 = OpAddText(var_0, Text("c"));
    var_0 = OpAddText(var_0, OpFormatFloat(Float(1.4), 5, 0));
    var_0 = OpAddText(var_0, Text("d"));
    var_0 = OpAddText(var_0, OpFormatFloat(Float(334.1), 0, 2));
    var_0 = OpAddText(var_0, Text("e"));
    var_0;
  };
}

