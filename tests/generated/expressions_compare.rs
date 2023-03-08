{
  let d = Definition::new("T");
  d.add_attribute("A", Type::T mutable = 1);
  d.add_attribute("C", Type::T mutable = 2);
  d.add_attribute("B", Type::T mutable = 3);
  d.returned = Type::T;
  d.size = 0;
}
{
  let d = Definition::new("A");
  d.returned = Type::T;
  d.size = 0;
}
{
  let d = Definition::new("C");
  d.returned = Type::T;
  d.size = 0;
}
{
  let d = Definition::new("B");
  d.returned = Type::T;
  d.size = 0;
}
fn count(v: u8, ) {
  if OpGtEnum(var_0, 2) {
    2;
  } else {
    1;
  };
}

fn test() {
  OpAddInt(OpAddInt(count(1), count(3)), count(3));
}

