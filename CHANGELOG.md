# Changelog

All notable changes to this project will be documented in this file.

The format is inspired by Keep a Changelog, adapted for this learning-project repository.

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
