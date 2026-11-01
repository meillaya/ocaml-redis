> Archived from the repository root on 2026-11-01. Links below use the
> original root-relative paths.

# ocaml-redis

[![CI](https://github.com/Meillaya/ocaml-redis/actions/workflows/ci.yml/badge.svg)](https://github.com/Meillaya/ocaml-redis/actions/workflows/ci.yml) [![Release](https://github.com/Meillaya/ocaml-redis/actions/workflows/release.yml/badge.svg)](https://github.com/Meillaya/ocaml-redis/actions/workflows/release.yml)

A correctness-first Redis learning implementation in OCaml.

This project is not trying to be a full Redis clone yet. It is a deliberately scoped, test-driven implementation of a substantial Redis-compatible subset, built to study protocol parsing, server design, data-structure semantics, replication basics, and how OCaml's type system and module structure support correctness.

## What this project is

`ocaml-redis` is a small Redis-compatible server with a real TCP interface, RESP2 parser/encoder, multi-client handling, blocking operations, replication basics, RDB loading, Pub/Sub, and an authentication subset.

The guiding idea is simple: implement only behavior that is explicitly understood, documented, and tested. Breadth matters less than keeping the current subset principled and explainable.

## Theory and design goals

Redis is interesting because it combines several distinct concerns in one system: a compact wire protocol, command dispatch, mutable in-memory state, blocking operations, session-local behavior, and replication-oriented transport semantics.

This implementation separates those concerns into small OCaml modules so each layer has a narrow job. Parsing is kept separate from execution. Execution is kept separate from transport. Transport owns sockets and client session state, while the engine owns command semantics, and the store owns data-family behavior.

The project also leans on classic functional-programming discipline even where mutation is required. Pure logic is extracted when possible, side effects are pushed to the edges, invariants are represented in types and variants, and behavior is locked down with regression tests before compatibility work moves forward.

## OCaml correctness techniques used here

OCaml is helpful in this kind of system because it makes illegal states harder to express and localized mutation easier to reason about.

Short version:

- algebraic data types encode protocol frames, replies, commands, and blocking states
- `result` values make parse and semantic failures explicit
- module boundaries keep protocol, store, engine, and server responsibilities separate
- deterministic clock injection avoids sleep-based tests for expiry logic
- single-owner serialized execution avoids data races inside the authoritative store
- narrow mutable records keep session and replication state explicit instead of hidden in globals

A few examples:

```ocaml
type reply =
  | Simple_string of string
  | Error of string
  | Prefixed_error of { prefix : string; message : string }
  | Integer of int64
  | Bulk_string of string option
  | Array of reply list
  | Null_array
```

The reply surface is explicit and closed. That helps keep wire-shape decisions local and reviewable.

```ocaml
type 'a lookup = [ `Missing | `Value of 'a | `Wrong_type ]
```

Missing-key and wrong-type behavior are intentionally distinguished. That matters a lot for Redis compatibility because empty replies, nil replies, and wrong-type errors are different protocol outcomes.

```ocaml
val handle : now_ms:int64 -> t -> Command.t -> outcome
```

The engine is parameterized by time. That makes expiry-sensitive behavior testable without introducing nondeterministic sleeps into the semantic core.

## High-level architecture

The codebase is organized as a pipeline from bytes to semantics to bytes again.

```text
TCP socket
  -> RESP parser
  -> command parser
  -> engine
  -> store
  -> RESP reply encoder
  -> TCP socket
```

There are two important architectural ideas underneath that simple picture.

First, there is one authoritative in-process store owner. The server multiplexes many clients, but command execution is serialized through one execution path, which keeps behavior understandable and avoids concurrency bugs.

Second, session-local state lives in the server layer, not in the store. Transactions, WATCH state, subscription state, auth state, replica offsets, and blocking wait metadata are connection concerns, so they are tracked per client.

![ocaml-redis request and replication architecture](docs/figures/architecture.png)

The hand-drawn scene is reproducible from
[`docs/figures/architecture.excalidraw`](docs/figures/architecture.excalidraw)
with the generator in
[`docs/figures/generate_architecture.py`](docs/figures/generate_architecture.py).

## Module-by-module implementation details

### `Clock` and `Expiry`

These modules isolate time-sensitive logic.

`Clock` provides the current time abstraction. `Expiry` encapsulates expiry checks. This makes it possible to test expiration deterministically instead of relying on real-time sleeps.

```text
Clock -> current notion of time
Expiry -> comparison helpers for expiring entries
```

### `Resp`

`Resp` implements the RESP2 wire format used by clients and by the server's own replication command propagation.

It is responsible for:

- encoding replies
- encoding command arrays
- incrementally parsing requests from buffered input
- incrementally parsing frames from upstream replication streams

This layer is intentionally syntax-level only. It does not know Redis command semantics.

### `Command`

`Command` translates arrays of bulk strings into typed command variants.

It also centralizes:

- arity validation
- integer/float parsing
- command-specific option parsing
- textual error normalization
- command re-encoding for replication propagation when supported

That means protocol-level parsing and command-shape parsing stay distinct.

```ocaml
type set_expiry = Relative_ms of int64 | Absolute_ms of int64

type t =
  | Ping of string option
  | Set of { key : string; value : string; expiry : set_expiry option }
  | Blpop of { key : string; timeout_ms : int64 option }
  | Wait of { numreplicas : int64; timeout_ms : int64 option }
  | Xread of { block_ms : int64 option; queries : (string * string) list }
  | ...
```

### `Store`

`Store` is the in-memory data layer.

It owns data-family semantics for:

- strings
- lists
- streams
- sorted sets
- geospatial values

It also owns watch-version tracking, which lets the server evaluate optimistic-lock semantics without pushing WATCH logic into each command implementation.

A key design choice is that the store returns explicit semantic categories like missing, wrong-type, invalid stream ID, not-an-integer, etc. That keeps wire decisions out of the store and lets the engine map semantic results to exact protocol replies.

### `Engine`

`Engine` is the semantic core. It turns typed commands into typed outcomes.

An `outcome` can include:

- an immediate reply
- a blocking instruction (`BLPOP`, `XREAD`)
- keys that should wake blocked clients

```ocaml
type outcome = {
  reply : Resp.reply option;
  block : blocked option;
  wake_keys : string list;
}
```

This is a useful boundary: the engine decides *what should happen*, and the server decides *how blocked clients and sockets are managed*.

### `Server`

`Server` owns the runtime behavior of the process.

Responsibilities include:

- listening on TCP
- accepting clients
- buffering partial RESP input
- tracking per-client transaction state
- tracking WATCH state
- tracking Pub/Sub subscriptions
- tracking auth state
- managing blocked clients
- managing replica peers and upstream replication
- enforcing read-only replica restrictions
- coordinating `WAIT`

The server uses `Unix.select`, keeping the implementation simple and explicit.

```text
server loop
  -> process readable sockets
  -> parse buffered input
  -> dispatch commands
  -> handle wakeups/timeouts
  -> continue
```

### `Rdb`

`Rdb` implements the currently supported startup-load subset.

This is not a full Redis persistence implementation. It supports enough Redis-style RDB parsing to load a bounded subset of string entries and expiries at startup from `--rdb-file`.

### `app/server_main.ml`

This is the CLI entrypoint. It parses:

- `--port`
- `--replicaof HOST:PORT`
- `--rdb-file PATH`
- `--requirepass PASSWORD`

and then starts the server.

## Runtime model

The runtime model is intentionally conservative.

- one OS process
- one authoritative in-memory store
- one event loop using `Unix.select`
- many client sockets multiplexed in the same loop
- explicit blocking-client registries for `BLPOP`, `XREAD`, and `WAIT`

This is not the most scalable architecture possible, but it is excellent for clarity. It keeps the main correctness questions local: parsing, state transitions, wakeups, and protocol shapes.

## Implemented command surface so far

### Core strings

Implemented and tested:

- `PING`
- `PING <message>`
- `ECHO`
- `TYPE`
- `SET`
- `GET`
- `INCR`
- `SET ... EX|PX ...`

Example:

```text
> SET greeting hello
+OK
> GET greeting
$5
hello
> INCR counter
:1
```

### Lists

Implemented and tested:

- `RPUSH`
- `LPUSH`
- `LRANGE`
- `LLEN`
- `LREM`
- `BLPOP`

Blocking list pop is integrated into the event loop, not implemented as a thread-per-client special case.

### Transactions and optimistic locking

Implemented and tested:

- `MULTI`
- `EXEC`
- `DISCARD`
- `WATCH`
- `UNWATCH`

Behavior covered includes:

- queued command execution
- empty transactions
- dirty transaction aborts
- optimistic aborts on watched-key changes
- watch clearing on `DISCARD`, successful `EXEC`, and `UNWATCH`

### Streams

Implemented and tested:

- `XADD`
- `XRANGE`
- `XREAD`

Covered behaviors include:

- explicit IDs
- partial and full auto-generated IDs
- monotonic ID validation
- multi-stream reads
- blocking `XREAD`
- `BLOCK 0` indefinite blocking
- `$` semantics for blocking reads

### Sorted sets

Implemented and tested:

- `ZADD`
- `ZRANK`
- `ZRANGE`
- `ZCARD`
- `ZSCORE`
- `ZREM`

Current ordering rule is the Redis-like subset this project intended: score ascending, then member lexicographically ascending.

### Geospatial subset

Implemented and tested:

- `GEOADD`
- `GEOPOS`
- `GEODIST`
- `GEOSEARCH`

The current implementation preserves an intentionally explicit boundary:

- geo-created keys report `TYPE => zset`
- `Z*` on geo-created keys is currently wrong-type
- `GEO*` on plain zset-created keys is currently wrong-type

That boundary is documented rather than hidden.

### Replication subset

Implemented and tested:

- `INFO replication`
- `REPLCONF`
- `PSYNC ? -1`
- empty full-resync RDB transfer from this server
- replica startup with `--replicaof HOST:PORT`
- replica authentication against an auth-enabled master with `--masterauth`
- propagation to one or multiple replicas
- replica ACK tracking via `REPLCONF ACK`
- replica reconnect every second, with a 4 MiB master backlog for `+CONTINUE`
  partial resync
- non-empty full-resync RDB import for supported string values
- `MULTI`/`EXEC` propagation so replicas apply transactions atomically
- `WAIT`

Replication preserves the things that are easy to get subtly wrong:

- `SET ... PX` is propagated as an absolute `PXAT` deadline, so replication
  delay does not extend a key's life.
- `XADD ... *` is propagated with the concrete ID generated by the master.
- a blocking `BLPOP` is propagated as the `LPOP` it performed, including pops
  served by the wakeup path.
- `XREAD` waiters are fanned out to every blocked client, not just the first.
- transaction writes are wrapped in `MULTI`/`EXEC`; a replica queues them and
  applies the batch before waking local readers.

This is a replication-learning subset, not a full Redis replication
implementation. Non-empty full-resync RDB payloads from another master are
imported when they contain the supported string value type; unsupported value
types fail the sync instead of being discarded. This server still emits an
empty full-resync RDB, so a replica of this server does not receive its
pre-existing local data.

### RDB loading subset

Implemented and tested:

- startup loading via `--rdb-file`
- multiple string keys
- expiry-bearing string keys

### Pub/Sub subset

Implemented and tested:

- `SUBSCRIBE`
- `UNSUBSCRIBE`
- `PUBLISH`
- subscribed-mode `PING`
- multi-channel subscription handling
- correct wire-shape handling for unsubscribe-all edges checked against a live local Valkey instance

### Authentication subset

Implemented and tested:

- `--requirepass`
- `--masterauth`
- `AUTH <password>`
- `ACL WHOAMI`
- `ACL GETUSER default`
- per-client auth state
- pre-auth allowlist for `AUTH` and `PING` only
- constant-time password comparison over SHA-256 digests
- `ACL GETUSER` reports a SHA-256 digest, like Redis, never the cleartext
- `PSYNC` requires authentication when `requirepass` is configured, closing the
  pre-auth replication leak
- `--requirepass` and `--replicaof` can be combined; use `--masterauth` when the
  upstream master requires a password

## What has been achieved so far

The most important achievement is not just feature count. It is that the current subset has been implemented with a stable internal architecture and a substantial regression suite.

That includes:

- a real socket server
- a typed command model
- deterministic expiry testing
- blocking operation support without introducing threads everywhere
- transaction and WATCH semantics
- a meaningful replication subset
- documented compatibility boundaries
- live-checked Pub/Sub wire-shape comparison against a local Redis-compatible server

In other words, this is already well beyond a toy parser or an in-memory map with a few commands.

## Current compatibility posture

Compatibility in this repository means: the documented and tested subset should behave like Redis for the intended cases, while deliberate non-goals remain explicit.

The detailed matrix lives in:

- [`docs/compatibility-matrix.md`](docs/compatibility-matrix.md)

That document separates:

- supported-compatible behavior
- deliberate incompatibilities
- partial-subset implementations

## Deliberate subset limitations

This project intentionally does **not** claim full Redis compatibility.

Important current limitations include:

- no cluster mode
- no AOF
- no general persistence engine
- no full RDB compatibility
- no Lua / scripting
- no RESP3 server mode
- no pattern Pub/Sub
- no multi-user ACL model
- auth subset is not production-secure
- geospatial and sorted-set interoperability is intentionally partial

For auth specifically:

- the model is single-user: `default` plus `requirepass`, with no runtime ACL
  mutation
- `ACL GETUSER default` reports the SHA-256 password digest, matching Redis
- `--requirepass` and `--masterauth` are visible in the process argument list;
  prefer a supervised configuration if that matters for your environment
- client TLS is available via `--tls-cert`/`--tls-key`; replication links and
  client-certificate authentication are not covered

## Next major milestones

The post-roadmap plan is phase-based.

### 1. Finish Phase 2 compatibility work

This is the current active frontier.

Focus:

- tighten already-implemented semantics only
- keep improving wire-shape parity
- revisit remaining edge cases in transactions, streams, Pub/Sub, replication, auth, and sorted-set/geo interactions
- keep documenting intentional differences rather than leaving them accidental

### 2. Phase 3 broader command coverage

Only after Phase 2 is stable.

Priority order:

- fill gaps in existing families/subsystems first
- extend replication and Pub/Sub where it fits the architecture cleanly
- then extend strings/lists/streams/zsets/geo with family-local commands
- only consider a truly new family after those extensions are exhausted

### 3. Phase 4 packaging, docs, and examples

This README expansion is part of that direction, but there is more to do.

Remaining work includes:

- richer examples
- contributor workflow docs
- small runnable demos
- smoother fresh-clone instructions
- polish around packaging and CI expectations

## Building

Requirements:

- OCaml 5.x (the pinned formatter is `ocamlformat` 0.29.0)
- `dune` 3.22 or newer
- `unix` from the OCaml distribution
- the `tls` and `x509` OCaml packages for TLS
- `ocamlformat` for formatting

### Reproducible development shell

The repository ships a `devenv.nix` that pins the compiler, dune, formatter,
`redis-cli`, and the convenience scripts used below.

```sh
direnv allow        # if direnv is installed
# or
devenv shell
```

Inside the shell:

```sh
build       # dune build
tests       # dune test --display=short
fmt         # format in place
fmt-check   # fail if formatting is stale
server      # dune exec app/server_main.exe
live-test   # opt-in conformance lane against a local Redis/Valkey
```

Without devenv, use any OCaml 5.x toolchain:

```sh
dune build
```

Build and run the main executable through dune:

```sh
dune exec app/server_main.exe
```

## Running

Run on the default port:

```sh
dune exec app/server_main.exe
```

Run on a custom port:

```sh
dune exec app/server_main.exe -- --port 6380
```

Start as a replica:

```sh
dune exec app/server_main.exe -- --port 6381 --replicaof 127.0.0.1:6379
```

Start as a replica of a password-protected master:

```sh
dune exec app/server_main.exe -- --port 6381 \
  --replicaof 127.0.0.1:6379 --masterauth secret
```

The server binds only to loopback. It is a learning server, not a hardened
internet-facing daemon; see the security posture section below.

Load an RDB file at startup:

```sh
dune exec app/server_main.exe -- --rdb-file ./dump.rdb
```

Require a password:

```sh
dune exec app/server_main.exe -- --requirepass secret
```

Serve TLS on the client port:

```sh
dune exec app/server_main.exe -- \
  --tls-cert ./cert.pem --tls-key ./key.pem
```

Then connect with a TLS-capable client:

```sh
redis-cli --tls --cacert ./cert.pem -p 6379 PING
```

TLS covers client connections. `--replicaof` replication links are still
plaintext, and the server does not request client certificates.

Example manual session with `redis-cli` or `valkey-cli`:

```sh
redis-cli -p 6379
PING
SET greeting hello
GET greeting
MULTI
INCR counter
EXEC
```

## Testing

The TLS test generates a short-lived self-signed certificate with `openssl`, so
keep `openssl` on `PATH` when running the suite.

Run the full regression suite:

```sh
dune test --display=short
```

If you run plain `dune test`, Dune may show very little output when everything passes. That is expected in this repository because the test runner is a simple executable that raises on failure and prints `PASS: ...` lines on success, while Dune often suppresses successful stdout by default.

If you want to see the individual passing test names, use one of these forms:

```sh
dune test --display=short
dune runtest --no-buffer
dune exec test/test_main.exe
```

The direct executable form now works from the repository root as well as through Dune's normal test flow.

The GitHub Actions CI workflow runs the same deterministic lane across a small OCaml version matrix. The first uncached run on a fresh hosted runner can take several minutes because opam may need to build the OCaml compiler switch from source; later runs should be faster when the workflow caches hit.

The automated test suite covers:

- deterministic expiry behavior
- parser and protocol edges
- multi-client server behavior
- blocking commands and timeout behavior
- transactions and WATCH semantics
- replication and `WAIT`
- RDB loading
- Pub/Sub edge shapes
- auth gating and ACL subset behavior
- wrong-type and missing-key shape distinctions
- protocol length ceilings, strict decimal parsing, and error-frame injection
- replication of absolute expiries, generated stream IDs, and blocked pops
- blocked-client resumption, XREAD fan-out, and SIGPIPE-safe disconnects
- authentication gates for `PSYNC`/`REPLCONF` and SHA-256 ACL digests
- `maxclients`, output-buffer, and `maxmemory` enforcement
- full-resync RDB import, reconnect, and `+CONTINUE` partial resync
- replica transaction isolation and `MULTI`/`EXEC` propagation
- TLS handshakes and encrypted RESP round trips

### Opt-in live conformance lane

The default test suite is the authoritative regression lane and does **not** require a running external daemon.

There is also an **opt-in** live conformance lane that compares selected already-implemented behaviors against a real local Redis-compatible server. It is deliberately kept out of the default `dune test` path so normal development and CI remain deterministic.

Run the deterministic suite plus the live conformance probes:

```sh
OCAML_REDIS_LIVE=1 dune exec test/test_main.exe
```

Override the live target host or port if needed:

```sh
OCAML_REDIS_LIVE=1 OCAML_REDIS_LIVE_HOST=127.0.0.1 OCAML_REDIS_LIVE_PORT=6379 dune exec test/test_main.exe
```

The live lane currently checks a curated subset only:

- exact Pub/Sub wire shapes
- selected transaction reply shapes
- selected sorted-set / geo nil-vs-bulk-vs-array distinctions
- narrow replication interoperability in the already-supported subset

Important boundaries:

- live checks are **off by default**
- the live lane assumes an unauthenticated daemon unless you explicitly provide a different setup
- tests use unique prefixes and self-seeded state so they do not depend on an empty DB
- if live mode is enabled but the daemon is unreachable, the runner fails clearly and intentionally

Example of the expected unreachable-daemon failure mode:

```sh
OCAML_REDIS_LIVE=1 OCAML_REDIS_LIVE_PORT=6399 dune exec test/test_main.exe
```

Expected tail output:

```text
LIVE: using 127.0.0.1:6399
Fatal error: exception Failure("failed connecting to live server 127.0.0.1:6399 (Connection refused)")
```

### Manual replication smoke test

If you have a live Redis-compatible server already running on port `6379`, you can test replica startup and propagation manually.

Important current limitation: this server's own full-resync payload is still **empty**, so a replica of this server does not import its pre-existing local dataset at connect time. Only writes made **after** the replica is connected are expected to appear on the replica. A replica pointed at another master does import that master's non-empty RDB when it uses the supported string value type.

With a real Redis or Valkey upstream, a pre-existing string key is imported during full resync:

```sh
redis-cli -p 6379 SET preexisting value
dune exec app/server_main.exe -- --port 6381 --replicaof 127.0.0.1:6379
redis-cli -p 6381 GET preexisting
```

Expected output:

```text
value
```

An `ocaml-redis` upstream still sends an empty full-resync payload, so the same
test against it returns `(nil)` for pre-existing data. Unsupported RDB value
types fail the replica sync rather than importing a partial dataset.

First confirm the upstream server is reachable:

```sh
redis-cli -p 6379 PING
```

Or, if you are using Valkey tools:

```sh
valkey-cli -p 6379 PING
```

Expected output:

```text
PONG
```

Start `ocaml-redis` as a replica on a different port:

```sh
dune exec app/server_main.exe -- --port 6381 --replicaof 127.0.0.1:6379
```

In a second terminal, inspect the replica's replication state:

```sh
redis-cli -p 6381 INFO replication
```

Expected fields include:

```text
role:slave
master_host:127.0.0.1
master_port:6379
master_link_status:up
```

Write values to the upstream server:

```sh
redis-cli -p 6379 SET hello world
redis-cli -p 6379 SET counter 1
```

Read them from the replica:

```sh
redis-cli -p 6381 GET hello
redis-cli -p 6381 GET counter
```

Expected output:

```text
world
1
```

Test continuing propagation:

```sh
redis-cli -p 6379 INCR counter
redis-cli -p 6381 GET counter
```

Expected output:

```text
2
```

Test read-only enforcement on the replica:

```sh
redis-cli -p 6381 SET nope value
```

Expected output:

```text
READONLY You can't write against a read only replica.
```

Test `WAIT` from the upstream server:

```sh
redis-cli -p 6379 SET sync-key value
redis-cli -p 6379 WAIT 1 1000
```

Expected output:

```text
1
```

## Security posture and operational limits

The server is a learning implementation. Its defaults are deliberately
conservative, and its remaining operational limits are explicit:

- it binds to loopback only; there is no `--bind`
- client connections can use TLS via `--tls-cert`/`--tls-key`; replication
  upstream links are still plaintext
- TLS does not request or verify client certificates; it is server
  authentication only
- command execution is serialized in one `Unix.select` event loop; client
  output is nonblocking and bounded, but a blocking upstream reconnect attempt
  can still delay other clients
- connection, output-buffer, and approximate memory limits are configurable
  with `--maxclients` (default 1024), `--max-output-buffer` (default 64 MiB),
  and `--maxmemory` (default 256 MiB, `0` disables it); memory policy is
  `noeviction`
- keys expire lazily on access; there is no active expiry cycle
- RDB checksums are not verified, and unsupported RDB value types fail startup
- this server's full resync still sends an empty RDB; it does not serialize its
  own dataset for first-time replicas
- a replica importing from another master accepts supported string RDB values
  and fails sync on unsupported value types
- the replica reconnects after a link drop and uses a 4 MiB master backlog for
  partial resync; a longer gap falls back to full resync
- `--requirepass` and `--masterauth` appear in the process argument list
- inline commands, `SELECT`, `QUIT`, RESP3, Lua, AOF, cluster mode, and pattern
  Pub/Sub are not implemented
- TLS handshakes time out after ten seconds

The authentication path was hardened in the audit: `PSYNC` is no longer
pre-auth, password comparison is constant-time over SHA-256 digests,
`ACL GETUSER` never returns cleartext, and `--masterauth` lets a replica
authenticate instead of relying on an unauthenticated replication exception.

See [`docs/AUDIT.md`](docs/AUDIT.md) for the full findings and residual-risk
register.

## Formatting

Format the OCaml sources with the pinned formatter:

```sh
dune build @fmt --auto-promote
```

Check formatting without rewriting files:

```sh
dune build @fmt
```

## Repository layout

```text
app/
  server_main.ml        CLI entrypoint
lib/
  clock.*              time abstraction
  expiry.*             expiry helper logic
  integer.*            strict decimal integer parsing
  sha256.*             SHA-256 for ACL password digests
  resp.*               RESP parsing and encoding
  command.*            typed command parsing and re-encoding
  store.*              in-memory data-family semantics
  engine.*             command execution semantics
  tls_transport.*     nonblocking TLS record and handshake transport
  server.*             event loop, clients, blocking, replication, auth, pubsub
  rdb.*                startup RDB loading subset
  redis.*              public library aggregation
test/
  test_main.ml         regression and integration-style tests
docs/
  AUDIT.md             audit findings and residual-risk register
  compatibility-matrix.md
  figures/
    architecture.excalidraw
    architecture.png
    generate_architecture.py
devenv.nix             pinned OCaml/dune/ocamlformat development shell
devenv.yaml
devenv.lock
```

## Dependency policy

Runtime/build baseline:

- OCaml stdlib
- `unix`
- `dune`
- `tls` and `x509` for the optional TLS transport

The TLS dependency is justified by the security requirement. Everything else
remains in the standard library.

Any additional dependency should be justified by correctness, deterministic testing, or a substantial reduction in protocol/networking risk.

## References

The following resources are the core reading set for building a Redis-like server from scratch in OCaml with a correctness-first mindset.

- **Redis serialization protocol specification (official)**  
  https://redis.io/docs/latest/develop/reference/protocol-spec/  
  Required for implementing RESP request parsing, reply encoding, Pub/Sub push-mode semantics, and exact wire-shape reasoning.

- **Redis command documentation (official)**  
  https://redis.io/docs/latest/commands/  
  Required for command arity, reply shapes, nil-vs-empty distinctions, blocking semantics, transaction constraints, and replication-related command surfaces.

- **Redis Pub/Sub documentation (official)**  
  https://redis.io/docs/latest/develop/pubsub/  
  Required for subscribed-mode command restrictions and pushed-message formats.

- **OCaml Manual (official)**  
  https://ocaml.org/manual/  
  Required for language semantics, modules, exceptions, standard compilation model, and precise behavior of the language/runtime used in the implementation.

- **OCaml stdlib / API documentation (official)**  
  https://ocaml.org/docs  
  Required for `Unix`, `Hashtbl`, `Queue`, `Result`, string handling, and the standard library primitives used to keep the implementation dependency-light.

- **OCaml Programming: Correct + Efficient + Beautiful (CS 3110 textbook)**  
  https://cs3110.github.io/textbook/cover.html  
  Strong reference for functional design, structural induction, persistent data-structure reasoning, specification writing, and proof-oriented thinking useful when validating semantics.

- **Real World OCaml, 2nd edition**  
  https://dev.realworldocaml.org/  
  Strong reference for practical module design, error handling, data modeling, and building larger OCaml programs that still stay readable.

- **Redis source code itself**  
  https://github.com/redis/redis  
  Required eventually for deeper fidelity work, especially around replication details, RDB specifics, and edge-case behavior that is not fully captured in the command reference alone.

- **Redis RDB internals / implementation references**  
  Start from the Redis source tree, especially the persistence-related implementation and format handling.  
  Required because exact RDB compatibility quickly moves beyond high-level docs into implementation-defined details.

- **Functional-correctness techniques to keep in mind while extending this project**  
  Structural induction, equational reasoning, explicit sum types for state machines, small pure functions around mutable boundaries, and deterministic time injection are not optional style choices here; they are the main tools that keep a stateful network server understandable.
