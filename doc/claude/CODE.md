# Code Rules

## Naming

- Function names should indicate what a function does. Keep them short but prevent ambiguity.
- Variable, argument, and field names should indicate what data they hold. Keep them short but prevent ambiguity.

**Goal:** Code should be self-documenting. A reader should understand what a function does and what data a variable holds just by reading the name, without needing comments or context.

## Functions

- A function should do one thing. Split it up when different algorithms are involved.
- Try to prevent code duplication by creating helper functions.
- When the same fields are passed as arguments to many functions, move them to a common struct.

**Goal:** Functions become small, testable, and easy to reason about. A function named `parse_header` that only parses a header is trivially verifiable; one that also validates and stores is not. Shared logic lives in one place, so a bug fix only needs to happen once. When a group of fields always travel together, they belong together — this reduces function signatures and makes it obvious where to look when that data needs to change.

## Documentation

- Documentation above a function should describe why it should be used, not what it does.
- Only write documentation inside a function when the algorithm is unclear.

**Goal:** If the name already says what a function does, repeating it in a docstring adds noise. The useful information is *when* and *why* to call it — preconditions, trade-offs, or intent that can't fit in a name. Inline comments should only appear where the algorithm is non-obvious (e.g. a specific formula, a non-trivial optimization, or a workaround for an external constraint). Comments that describe obvious logic go stale and mislead.
