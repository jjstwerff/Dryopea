# Code Rules

- **Naming:** names of functions, variables, arguments, and fields must be self-documenting — short but unambiguous.
- **Functions:** one algorithm per function; extract helpers to avoid duplication; group fields that always travel together into a struct.
- **Doc comments:** describe *why* to call the function (when, preconditions, trade-offs), not *what* it does. Inline comments only where the algorithm is non-obvious.
- **Evaluation:** no clippy warnings (cognitive_complexity enabled); formatted with rustfmt; `tests/suite/` files are concise end-user examples with doc comments.
