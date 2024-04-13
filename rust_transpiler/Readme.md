# Running the Project in Development Mode

Welcome to our Rust project! To ensure a smooth development experience, we use `cargo-watch` to automatically recompile and rerun the project whenever source files change. This approach allows you to see the effects of your changes in real-time, without manually restarting the build process.

## Prerequisites

Before you begin, ensure you have Rust and Cargo installed on your system. If you haven't installed Rust yet, follow the instructions on the [official Rust website](https://www.rust-lang.org/tools/install) to set it up.

## Installation

First, you need to install the project dependencies. Run the following command in the root directory of the project:

```sh
cargo install
```

```sh
cargo install cargo-watch
```

```sh
cargo watch -x check
```

```sh
cargo watch -x run
```

```sh
cargo-modules generate tree --fns
```
