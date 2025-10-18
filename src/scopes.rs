use crate::data::{Block, Data, DefType, Type, Value, v_set};
use crate::variables::Function;
use std::collections::{BTreeMap, HashSet};

struct Scopes {
    d_nr: u32,
    max_scope: u16,
    scope: u16,
    stack: Vec<u16>,
    var_scope: BTreeMap<u16, u16>,
    loops: Vec<u16>,
}

pub fn check(data: &mut Data) {
    for d_nr in 0..data.definitions() {
        if !matches!(data.def(d_nr).def_type, DefType::Function) || data.def(d_nr).variables.done {
            continue;
        }
        let mut scopes = Scopes {
            d_nr,
            max_scope: 1,
            scope: 0,
            stack: Vec::new(),
            var_scope: BTreeMap::new(),
            loops: vec![],
        };
        let mut function = Function::copy(&data.def(d_nr).variables);
        for a in function.arguments() {
            scopes.var_scope.insert(a, 0);
        }
        let code = scopes.scan(&data.definitions[d_nr as usize].code, &mut function, data);
        data.definitions[d_nr as usize].code = code;
        data.definitions[d_nr as usize].variables = function;
        for (v_nr, scope) in scopes.var_scope {
            data.definitions[d_nr as usize]
                .variables
                .set_scope(v_nr, scope);
        }
    }
}

impl Scopes {
    fn enter_scope(&mut self) -> u16 {
        self.stack.push(self.scope);
        self.scope = self.max_scope;
        self.max_scope += 1;
        self.scope
    }

    fn exit_scope(&mut self) {
        if let Some(scope) = self.stack.pop() {
            self.scope = scope;
        }
    }

    fn scan(&mut self, val: &Value, function: &mut Function, data: &Data) -> Value {
        match val {
            Value::Set(v, value) => {
                if self.var_scope.contains_key(v) && **value == Value::Null {
                    return Value::Insert(Vec::new());
                }
                // remember scope of variable
                let mut depend = Vec::new();
                for d in function.tp(*v).depend() {
                    if !self.var_scope.contains_key(&d) {
                        depend.push(d);
                        self.var_scope.insert(d, self.scope);
                    }
                }
                if !self.var_scope.contains_key(v) {
                    self.var_scope.insert(*v, self.scope);
                }
                if depend.is_empty() {
                    Value::Set(*v, Box::new(self.scan(value, function, data)))
                } else {
                    let mut ls = Vec::new();
                    for d in depend {
                        if d == *v {
                            continue;
                        }
                        if matches!(function.tp(d), Type::Text(_)) {
                            ls.push(v_set(d, Value::Text(String::new())));
                        } else {
                            ls.push(v_set(d, Value::Null));
                        }
                        self.var_scope.insert(d, self.scope);
                    }
                    ls.push(Value::Set(*v, Box::new(self.scan(value, function, data))));
                    Value::Insert(ls)
                }
            }
            Value::Loop(lp) => {
                let scope = self.enter_scope();
                self.loops.push(scope);
                let ls = self.convert(lp, function, data);
                self.loops.pop();
                self.exit_scope();
                Value::Loop(Box::new(Block {
                    operators: ls,
                    result: Type::Void,
                    name: lp.name,
                    scope,
                }))
            }
            Value::If(test, t_val, f_val) => Value::If(
                Box::new(self.scan(test, function, data)),
                Box::new(self.scan(t_val, function, data)),
                Box::new(self.scan(f_val, function, data)),
            ),
            Value::Break(lv) => {
                let to_scope = self.loops[self.loops.len() - *lv as usize - 1];
                let mut ls = self.get_free_vars(function, data, to_scope, &Type::Void);
                if ls.is_empty() {
                    Value::Break(*lv)
                } else {
                    ls.push(Value::Break(*lv));
                    Value::Insert(ls)
                }
            }
            Value::Continue(lv) => {
                let to_scope = self.loops[self.loops.len() - *lv as usize - 1];
                let mut ls = self.get_free_vars(function, data, to_scope, &Type::Void);
                if ls.is_empty() {
                    Value::Continue(*lv)
                } else {
                    ls.push(Value::Continue(*lv));
                    Value::Insert(ls)
                }
            }
            Value::Return(v) => {
                let expr = self.scan(v, function, data);
                Value::Insert(self.free_vars(
                    true,
                    &expr,
                    function,
                    data,
                    &data.def(self.d_nr).returned,
                    self.scope - 1,
                ))
            }
            Value::Block(bl) => {
                let scope = self.enter_scope();
                let ls = self.convert(bl, function, data);
                self.exit_scope();
                Value::Block(Box::new(Block {
                    operators: ls,
                    result: bl.result.clone(),
                    name: bl.name,
                    scope,
                }))
            }
            Value::Call(d_nr, args) => {
                let mut ls = Vec::new();
                for v in args {
                    ls.push(self.scan(v, function, data));
                }
                Value::Call(*d_nr, ls)
            }
            _ => val.clone(),
        }
    }

    fn convert(&mut self, bl: &Block, function: &mut Function, data: &Data) -> Vec<Value> {
        let mut ls = Vec::new();
        for v in &bl.operators {
            let sv = self.scan(v, function, data);
            if let Value::Insert(to_insert) = sv {
                for i in to_insert {
                    ls.push(i.clone());
                }
            } else {
                ls.push(sv);
            }
        }
        let expr = if ls.is_empty() || bl.result == Type::Void {
            Value::Null
        } else {
            ls.pop().unwrap()
        };
        for v in self.free_vars(false, &expr, function, data, &bl.result, self.scope) {
            ls.push(v);
        }
        ls
    }

    #[must_use]
    fn variables(&self, to_scope: u16) -> Vec<u16> {
        let mut scopes = HashSet::new();
        let mut sc = self.scope;
        let mut scope_pos = self.stack.len();
        loop {
            if sc == 0 {
                // never return function arguments
                break;
            }
            scopes.insert(sc);
            if sc == to_scope {
                break;
            }
            if scope_pos == 0 {
                break;
            }
            scope_pos -= 1;
            sc = self.stack[scope_pos];
        }
        let mut res = Vec::new();
        for (v_nr, sc) in &self.var_scope {
            if scopes.contains(sc) {
                res.push(*v_nr);
            }
        }
        res
    }

    fn free_vars(
        &mut self,
        is_return: bool,
        expr: &Value,
        function: &mut Function,
        data: &Data,
        tp: &Type,
        to_scope: u16,
    ) -> Vec<Value> {
        let mut ls = self.get_free_vars(function, data, to_scope, tp);
        if ls.is_empty() || matches!(expr, Value::Null | Value::Var(_)) {
            if is_return {
                ls.push(Value::Return(Box::new(expr.clone())));
            } else if !matches!(expr, Value::Null) {
                ls.push(expr.clone());
            }
        } else if let Value::Block(bl) = expr {
            ls.clear();
            for o in self.convert(bl, function, data) {
                ls.push(o);
            }
        } else {
            let v = function.add_unique("res", tp, to_scope);
            ls.insert(0, v_set(v, expr.clone()));
            if is_return {
                ls.push(Value::Return(Box::new(Value::Var(v))));
            } else {
                ls.push(Value::Var(v));
            }
        }
        ls
    }

    fn get_free_vars(
        &mut self,
        function: &mut Function,
        data: &Data,
        to_scope: u16,
        tp: &Type,
    ) -> Vec<Value> {
        let mut ls = Vec::new();
        for v in self.variables(to_scope) {
            if matches!(function.tp(v), Type::Text(_)) {
                ls.push(call("OpFreeText", v, data));
            }
            if let Type::Reference(_, dep) | Type::Vector(_, dep) = function.tp(v)
                && dep.is_empty()
                && !tp.depend().contains(&v)
            {
                ls.push(call("OpFreeRef", v, data));
            }
        }
        ls
    }
}

fn call(to: &'static str, v: u16, data: &Data) -> Value {
    Value::Call(data.def_nr(to), vec![Value::Var(v)])
}
