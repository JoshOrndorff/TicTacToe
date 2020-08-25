### The Builder Stage Compiles the code
FROM parity/rust-builder as builder

# Copy the code from this repo
COPY . .

# Build the node
RUN cargo build --release

### The final stage just copies binary and chainspecs

# Choose the base image. Same one used in main Polkadot repo
FROM debian:stretch-slim

# Copy the node into the image
COPY --from=builder /builds/target/release/tictactoe .


# Open default ports. User is responsible for re-mapping these
# or using host or overlay networking.
EXPOSE 30333 9933 9944

ENTRYPOINT ["./tictactoe"]
