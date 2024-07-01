# PurrLang :rocket:
PurrLang is a programming language that compiles to [Scratch](https://scratch.mit.edu/) blocks.

This is very experimental and is not guaranteed to be supported in the future.

# Installation
Currently there is no simple installation method like package managers/installator so
you need to clone and compile this repo manually.

1. Clone the repository into any directory that you like using
`git clone https://github.com/olix3001/PurrLang`.
2. Compile It using `cargo build --release` (this will compile `catnip` binary).
3. Add `PurrLang/target/release` to your PATH.
4. Go to your project directory and use `catnip build`. (You can check out [examples](./examples)).

# Syntax
PurrLang's syntax is inspired by rust, but a bit easier as there is no borrow checker :smile:.

### Triggers
Triggers are main building blocks of PurrLang. In scratch there are blocks like `when green flag clicked`,
those are triggers I'm talking about. To use them you write `@<trigger name> { ... }`.

Currently implemented triggers are:
- `@green_flag { ... }`
- `@key_pressed("space") { ... }`
- `@sprite_clicked { ... }`
- `@backdrop_switch("my_backdrop") { ... }`

### Variables
Variables are defined using `let` keyword. There are plans to implement `const` and `global` in the future, so
please DO NOT use those keywords at the moment.

Examples:
```
let name = "olix3001";
```

### Functions
Functions can be defined using `def` keyword. Those produce scratch's procedures.
Last expression in the block if not followed by `;` will act as a return.

Examples:
```
def greet(name: text) -> text {
    "Hi, " + name + "!"
}
```

### Imports
Imports allow you to access other modules from your script.
If a module is not imported no triggers and functions from It will be available.

Import syntax is a tree which allows for more concise imports.

For example:
```
import scratch::{
    math::sqrt, 
    looks::{say, say_for}
};
```

### Structs
Sometimes you need more complex structures than just plain `number`, `text` or `bool`.
You could pass each part of let's say vector separately, but where Is the fun?
That's exactly why PurrLang supports structures (and impls in the future).

Structures can be defined using the following syntax:
```
struct Vec2 { x: number, y: number }
```

and then can be used in custom functions and variables.

Also... writing `let a = Vec2 { x: 1, y: 1 }` is equal to `let a: Vec2 = .{ x: 1, y: 2 }` as
anonymous structs (`.{ ... }`) will automatically infer as other matching structures.
It is however recommended to write `Vec2 { x: 1, y: 2 }` for better error messages.

### Loops and Conditionals
Loops and conditionals syntax is very similar to other languages.
In terms of conditionals there is `if` and `else` that can be stacked together to form `else if`.
If's condition is always followed by `->` and block, but else is only followed by a block or another if.
Like this:
```
if a == 0 -> {
    ...
} else if a == 1 -> {
    ...
} else {
    ...
}
```

Currently implemented loops are `repeat` and `while` which are both followed by `->` and block.

```
repeat 10 -> {
    ...
}

while true -> {
    ...
}
```

# Builtin libraries
Currently available builtin libraries are `scratch` which provides basic scratch blocks and `std` for
more complex tasks. You can see which blocks/functions are in them by exploring [source code](./libraries).

# Planned features
PurrLang is currently pretty simple, but this is not supposed to be forever.
There are many features that will work in the future, and some of those are:

- Match expressions
- Macros
- Implementations
- Enums
- Closures
- Break/Continue (if possible to implement)

# Issues
If you find any issues please report It on this repositories issues page.
