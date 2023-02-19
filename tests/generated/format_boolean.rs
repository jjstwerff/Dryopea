fn test() {
  {
    var_0 = Text("1");
    var_0 = OpAddText(var_0, OpFormatText(if 1 {Text("true")} else {Text("false")}, 7, 0, 32));
    var_0 = OpAddText(var_0, Text("2"));
    var_0;
  };
}

