# Changelog

All notable changes to this project will be documented in this file.

The format is inspired by Keep a Changelog, adapted for this learning-project repository.

## [Unreleased]

### Security
- Require authentication for `PSYNC` and `REPLCONF` when `requirepass` is
  configured; the previous pre-auth allowlist let any client request a
  replication stream from an auth-enabled master.
- Add `--masterauth` so replicas can authenticate instead of relying on the
  unauthenticated replication exception.
- Report `ACL GETUSER` password digests as SHA-256, never cleartext, and compare
  passwords in constant time over fixed-width digests.
- Flatten CR/LF and cap diagnostics in error replies so a hostile argument
  cannot inject protocol frames or amplify a request into a huge reply.
- Ignore `SIGPIPE` and close fds on write failures; a disconnected client can no
  longer terminate the server or leak its descriptor.

### Fixed
- Reject RESP bulk lengths that overflow `int`, exceed 64 MiB, or are negative
  values other than `-1`; cap requests at 1,048,576 arguments and 128 MiB of
  buffered input per client.
- Propagate a blocked `BLPOP` as the `LPOP` it performed, including the wakeup
  path, so replicas converge.
- Propagate the master-generated `XADD` ID instead of the `*` template.
- Rewrite relative `SET ... PX` expiries as absolute `PXAT` deadlines before
  replication.
- Wake every blocked `XREAD` waiter on a new entry instead of only the first.
- Resume commands pipelined behind a blocking command after the block clears.
- Reject `INCR` overflow and accept only canonical decimal integers on the wire.
- Keep `WAIT` replica counts in `int64` rather than narrowing them to `int`.
- Accept case-insensitive subcommands and keywords for ACL, REPLCONF,
  GEOSEARCH/GEODIST, XREAD, and INFO.
- Validate `GEOSEARCH` center coordinates and reject negative radii.
- Execute `PUBLISH` and `INFO` inside `MULTI`; reject stateful commands at queue
  time.
- Make `XADD` append O(1) instead of O(n) per entry.
- Cap RDB files at 256 MiB and warn when a non-empty full-resync payload is
  discarded.
- Validate `--port`, resolve replica hostnames, support bracketed IPv6, and
  print clean startup errors instead of backtraces.

### Added
- `SET ... EXAT|PXAT`, `LPOP`, and the `--masterauth` option.
- `lib/integer.ml` for strict decimal parsing and `lib/sha256.ml` for ACL
  digests.
- `docs/AUDIT.md`, including a residual-risk register.
- An Excalidraw architecture figure with a reproducible generator.
- `devenv.nix`, `devenv.yaml`, and `devenv.lock` for a pinned development shell.
- A CI formatting check and a pinned `ocamlformat` version.

### Documentation
- Updated the README for the hardened auth model, replication guarantees,
  security posture, devenv workflow, and architecture figure.
- Reworked the compatibility matrix around the current subset and its explicit
  protocol/resource limits.

## [0.1.0] - 2026-04-15

### Added
- A correctness-first Redis learning server in OCaml with a real TCP interface.
- RESP2 request parsing and reply encoding.
- Command parsing layer with typed command variants and normalized arity/value errors.
- Deterministic clock and expiry helpers for testable time-dependent behavior.
- In-memory store and semantic engine for implemented command families.
- A multiplexed `Unix.select` server loop with explicit per-client session state.
- String command support:
  - `PING`, `ECHO`, `TYPE`, `SET`, `GET`, `INCR`
  - `SET ... EX|PX ...`
- List command support:
  - `RPUSH`, `LPUSH`, `LRANGE`, `LLEN`, `LREM`, `BLPOP`
- Transaction and optimistic-locking support:
  - `MULTI`, `EXEC`, `DISCARD`, `WATCH`, `UNWATCH`
- Stream support:
  - `XADD`, `XRANGE`, `XREAD`
  - stream ID validation
  - blocking `XREAD`, including `BLOCK 0` and `$` semantics
- Sorted-set support:
  - `ZADD`, `ZRANK`, `ZRANGE`, `ZCARD`, `ZSCORE`, `ZREM`
- Geospatial subset:
  - `GEOADD`, `GEOPOS`, `GEODIST`, `GEOSEARCH`
- Replication subset:
  - `INFO replication`, `REPLCONF`, `PSYNC ? -1`
  - replica startup via `--replicaof`
  - propagation for supported writes
  - replica ACK tracking and `WAIT`
- Standalone RDB loading subset via `--rdb-file`.
- Pub/Sub subset:
  - `SUBSCRIBE`, `UNSUBSCRIBE`, `PUBLISH`
  - subscribed-mode `PING`
- Authentication subset:
  - `--requirepass`
  - `AUTH <password>`
  - `ACL WHOAMI`
  - `ACL GETUSER default`
- A substantial deterministic regression suite covering parser, engine, server, replication, auth, Pub/Sub, wrong-type, and reply-shape behavior.
- An opt-in live conformance lane for selected comparisons against a real local Redis/Valkey server.
- Project documentation describing theory, architecture, implementation details, testing workflow, and compatibility boundaries.
- A GitHub Actions CI workflow for deterministic build/test validation.

### Changed
- Hardened reply-shape and timeout wording compatibility for already-implemented commands.
- Improved compatibility around transaction edge shapes, `WAIT`, `XREAD BLOCK 0`, and selected sorted-set / geo reply distinctions.
- Improved Pub/Sub compatibility for subscribed-mode `PING` and `UNSUBSCRIBE` edge shapes.
- Improved replica handshake robustness against live Redis-compatible server framing.
- Improved the direct test runner so `dune exec test/test_main.exe` works reliably from the repository root.

### Documentation
- Added a compatibility matrix describing supported-compatible behavior, deliberate incompatibilities, and partial-subset boundaries.
- Expanded the README with:
  - theory and architecture
  - module-by-module implementation notes
  - build/run/test instructions
  - manual replication smoke tests
  - deterministic vs live test-lane documentation
  - references for Redis and OCaml correctness-oriented study

### Known limitations
- Initial full-sync dataset import is not implemented; replica startup still uses an empty full-resync RDB payload.
- The live conformance lane is intentionally opt-in and covers a curated subset only.
- Auth live conformance against a dedicated auth-configured daemon is not included yet.
- Geospatial and sorted-set interoperability remains intentionally partial and documented as such.
- This remains a learning-project subset, not a full Redis-compatible production server.

[0.1.0]: https://github.com/Meillaya/ocaml-redis/releases/tag/v0.1.0
