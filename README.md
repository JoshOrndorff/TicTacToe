# Substrate Tic Tac Toe

A peer-to-peer electronic tic tac toe game. This blockchain allows users to play many simultaneous games of tic tac toe that cannot be censored or reverted by as long as there are properly incentivized validators running nodes. While the game of tic tac toe itself is just for fun, this project demonstrates many common design principals for building blockchains in general and building blockchains with substrate in particular.

## How to use this
### Build the node


```bash
# Install rust
curl https://sh.rustup.rs -sSf | sh

# Install required tools
./scripts/init.sh

# build project
cargo build --release
```

### Run the node

```bash
# Read the help page
./target/release/tictactoe --help

# Start an instance
./target/release/tictactoe <parameters>
```

### Run the user interface
While you're welcome to interact with the node in any way you see fit #SovereignUIs, a convenient user interface exists at https://github.com/JoshOrndorff/tictactoe-frontend

## Lessons Learned
* Only verify on chain. UI finds the winner, runtime just checks it.
* Help user not submit incorrect transactions. Runtime will check for all sorts of bad conditions, but the UI should help non-malicious users avoid making them. Eg, if you go out of turn, the runtime will handle it, but the UI will only submit transactions for you if it is your turn.
* Don't make the user do extra work. Although claiming a win is a separate extrinsic call, the UI submits it automagically when it detects the current user has won.
* Verify then mutate. Runtime does not make any changes to storage until it is sure those changes will be successful. For example, it does not update the squares until it checks it is the caller's turn and the square is available.
