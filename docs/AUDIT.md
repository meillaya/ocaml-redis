# Audit: ocaml-redis

Date: 2026-11-01

## Scope and method

This audit treated `ocaml-redis` as a correctness-first learning server rather
than a production Redis replacement. The review covered:

- protocol parsing and reply encoding (`lib/resp.ml`)
- command typing and option parsing (`lib/command.ml`)
- store semantics, expiry, and versioning (`lib/store.ml`)
- engine dispatch (`lib/engine.ml`)
- the socket loop, sessions, blocking, Pub/Sub, replication, and auth
  (`lib/server.ml`)
- RDB loading (`lib/rdb.ml`)
- the CLI (`app/server_main.ml`)
- the regression suite, CI, and the claims in `README.md` and
  `docs/compatibility-matrix.md`

The method was adversarial reading plus executable probes: every confirmed
behavioral defect was reproduced with the server's own socket protocol before a
fix was written. The remediations below are covered by regression tests in
`test/test_main.ml`, including two timing-sensitive replication tests that pause
a replica with `SIGSTOP` to make propagation delay observable.

## Executive summary

The codebase has a clear module boundary and an unusually good deterministic
test lane for a learning project. The audit nevertheless found several defects
that undermined its two central claims: Redis-compatible wire behavior and
replication correctness.

The most serious issues were:

1. A malicious RESP bulk length could overflow `int` arithmetic or force
   unbounded buffering.
2. An auth-enabled master still accepted unauthenticated `PSYNC`, which let any
   client stream subsequent writes and bypass `AUTH` entirely.
3. Replication diverged for three common operations: a blocking `BLPOP` pop was
   not propagated, `XADD ... *` was re-generated on the replica, and relative
   `SET ... PX` deadlines were extended by replication delay.
4. A blocked `XREAD` woke only one waiter, and commands pipelined behind a
   blocking command stalled until the client wrote again.
5. Error replies echoed attacker-controlled bytes, including CR/LF, allowing
   protocol-frame injection.
6. `ACL GETUSER` returned the configured password in cleartext, and `AUTH`
   compared passwords with an early-exit string comparison.

All six are fixed and tested. A follow-up hardening pass then added bounded
nonblocking output queues, connection and memory limits, client-facing TLS,
full-resync RDB import, replica reconnect with partial resync, and atomic
transaction propagation. The remaining risk is concentrated in deliberate
scope, mainly single-process operation and the unsupported parts of the Redis
surface.

## Findings and disposition

Severity is assigned for the intended audience of this repository (a learning
server that should still be internally honest), not for an internet-facing
deployment.

### Critical

| ID | Finding | Evidence | Disposition |
|---|---|---|---|
| C1 | RESP bulk lengths were parsed directly into `int`; a huge `$...` length could overflow `after_length + length + 2`, bypass the completeness check, and index outside the buffer. The server also buffered an unbounded client stream. | `lib/resp.ml` old `parse_bulk_string`; handmade `*1\r\n$999...` probe | Fixed: 64 MiB bulk ceiling, 1M argument ceiling, strict decimal parsing, 128 MiB per-client buffer cap, and `Integer.to_int_opt` range checks. Test: `RESP limits reject oversized frames`. |
| C2 | `auth_exempt_command` allowed `PSYNC` and `REPLCONF` before authentication. On a `--requirepass` master, an unauthenticated client could send `PSYNC ? -1`, become a replica peer, and receive every subsequent write. | `lib/server.ml` old allowlist; `PSYNC` probe without `AUTH` | Fixed: only `AUTH` and `PING` are exempt. Replicas authenticate with the new `--masterauth` option before `REPLCONF`/`PSYNC`. Tests: `auth gate matrix is enforced`, `auth-enabled master still allows replica sync`. |
| C3 | The server did not ignore `SIGPIPE`. A write to a closed subscriber/client could terminate the process. Write failures also marked a client closed without closing its fd, and a closed client could be read again in the same `select` batch. | `lib/server.ml` old `send_raw`, `handle_client`, loop | Fixed: `SIGPIPE` ignored, all `Unix_error` write failures close the fd, `EINTR` retried, closed clients are filtered and re-checked, and `accept`/`read` are exception-safe. Test: `writes to disconnected clients do not kill the server`. |

### High

| ID | Finding | Evidence | Disposition |
|---|---|---|---|
| H1 | A `BLPOP` served by the wakeup path popped the list locally but propagated nothing. The replica kept the element, so the datasets diverged. | `wake_list_waiters_for_key` old path | Fixed: the pop is propagated as `LPOP` after the producer's `RPUSH`. Test: `replication propagates the pop performed by a blocked BLPOP`. |
| H2 | `XADD key * ...` was propagated verbatim, so each replica generated its own ID from its own clock. | `Command.to_arguments`; propagation path | Fixed: the master's returned ID is propagated as an explicit `XADD key <id> ...`. Test: `replication preserves generated stream ids` (paused replica makes the clocks differ). |
| H3 | `SET key value PX n` was propagated as the same relative option. Replication delay extended the key's lifetime on the replica. | `Command.to_arguments`; replica `SET` path | Fixed: relative expiries are rewritten to absolute `PXAT` deadlines using the execution timestamp; the parser accepts `EXAT`/`PXAT`. Test: `replication preserves absolute expiry deadlines` (paused replica). |
| H4 | `wake_stream_waiters_for_key` popped exactly one waiter per `XADD`; every other blocked `XREAD` stayed blocked until another write. | old wake function | Fixed: one snapshot pass wakes every eligible waiter, since `XREAD` is non-consuming. Test: `blocking XREAD fans out to every waiter`. |
| H5 | Commands pipelined after `BLPOP`/`XREAD`/`WAIT` were left in the client buffer after the block cleared because nothing resumed `process_buffer`. | `process_buffer` old exit condition | Fixed: a `resume_client` hook runs buffered commands after every wakeup and timeout. Test: `commands buffered behind a blocking pop run after the wakeup`. |
| H6 | Error messages echoed raw arguments. Because command arguments are bulk strings, an argument containing CR/LF could inject an extra RESP frame. Huge arguments were also echoed back, amplifying one request into a large reply. | `Command.to_error_message`, engine error strings | Fixed with a global error sanitizer (CR/LF flattened, 512-byte cap) plus 64-byte source truncation. Test: `error replies cannot inject extra protocol frames`. |
| H7 | `ACL GETUSER default` returned the cleartext `requirepass`, unlike Redis. `AUTH` used `=` on strings, short-circuiting at the first differing byte. | `lib/server.ml` old ACL/auth helpers | Fixed: a local SHA-256 implementation reports the digest Redis reports, and password checks compare fixed-width digests in constant time. Tests: `auth and acl subset works`, plus the strict test lane. |
| H8 | `INCR` wrapped at `Int64.max_int`, and integer arguments accepted OCaml forms Redis rejects (`0x10`, `+1`, `1_000`). A stored `0x10` incremented to 17. | `lib/store.ml`, `lib/command.ml` | Fixed: strict decimal parser (`lib/integer.ml`) used at every wire boundary, plus a checked overflow error. Tests: `strict integer and expiry edge cases`, `engine reports INCR overflow`. |
| H9 | `WAIT` narrowed `int64` replica counts to `int`; values above `max_int` wrapped negative and could satisfy the wait immediately. | `Int64.to_int required_replicas` | Fixed: all comparisons stay in `int64`. Covered by `WAIT` regression tests and code review. |
| H10 | RDB loading read the whole file without a size check, and non-empty full-resync payloads were discarded silently, leaving an operator believing a replica had imported data. | `lib/rdb.ml`, `read_rdb_payload_blocking` | Fixed in the follow-up pass: 256 MiB file cap plus in-memory RDB parsing and import. Unsupported value types fail the sync instead of being discarded. This master's own resync payload remains empty. |

### Medium

| ID | Finding | Disposition |
|---|---|---|
| M1 | Subcommands and keywords were case-sensitive (`ACL whoami`, lowercase `GEOSEARCH`/`XREAD` keywords, uppercase units, `INFO REPLICATION`, lowercase `REPLCONF` arguments). Redis is case-insensitive here. | Fixed and tested by `lowercase subcommands and keywords are accepted`. |
| M2 | Every `XADD` appended with `@`, making repeated appends quadratic. | Fixed by storing entries newest-first and reversing once on read. Existing `XRANGE`/`XREAD` tests lock the order. |
| M3 | `PUBLISH` and `INFO` inside `MULTI` were queued but failed at `EXEC` with a server error. | Fixed: both execute through `EXEC`; stateful commands (`AUTH`, ACL, `SUBSCRIBE`, `REPLCONF`, `PSYNC`) are rejected at queue time. Test: `transactions support PUBLISH and INFO`. |
| M4 | `SET ... EX|PX` multiplied seconds by 1000 without an overflow check; a huge value wrapped into a negative expiry. | Fixed with checked multiplication and saturating `now + ttl`. Tested by the strict-expiry test. |
| M5 | `--port` was not validated and startup failures printed raw backtraces. | Fixed: range validation and clean `Failure`/`Unix_error` diagnostics with exit status 1. |
| M6 | A closed blocked client stayed in waiter queues until the watched key was pushed. Blocked sockets are not polled, so disconnects are not noticed promptly. | Partially mitigated: wake loops prune closed clients, and queues are removed when empty. The residual is documented below. |
| M7 | The formatter version in `.ocamlformat` was correct but CI never checked formatting. A stale format could merge unnoticed. | CI check is part of this change (`dune build @fmt`). |
| M8 | `GEOSEARCH` accepted out-of-range center coordinates and negative radii, returning an empty result instead of Redis's validation error. | Fixed: center and radius validation is covered by `server enforces geospatial validation and wrong-type boundaries`. |

### Low / hygiene

- No `LICENSE` file. The repository is public, so this is a legal and
  distribution ambiguity that the owner must resolve; no license was invented
  during remediation.
- No `.opam` package or public library name, despite `dune-project` declaring a
  project. The code builds, but cannot be consumed as an opam package.
- `docs/ocaml-language-reference.txt` and
  `docs/ocaml-stdlib-reference.txt` add about 1.5 MiB of generated reference
  text to the repository. They are not referenced by the README and could move
  to a release artifact or external link.
- The CI matrix still tests OCaml 5.1.1 and 5.2.1. Both are older than the
  toolchain the project is developed against; a future change should move the
  matrix forward and test the formatter version explicitly.
- Error wording remains close to, but not byte-identical with, Redis in several
  edges. The compatibility matrix marks these areas as partial subsets.

## Follow-up hardening pass

The second pass added these controls and guarantees:

- **Nonblocking output**: every client has a bounded output buffer, flush events
  are driven by `select`, and `maxclients`, `max-output-buffer`, and `maxmemory`
  are configurable. `INFO memory` reports usage and policy.
- **RDB import**: non-empty full-resync payloads are parsed and loaded, the
  replica store is cleared first, and unsupported RDB value types fail the sync
  instead of silently discarding data.
- **Reconnect and partial resync**: the master keeps a 4 MiB backlog and answers
  a matching `PSYNC` with `+CONTINUE`; the replica reconnects every second and
  preserves its offset and replid.
- **Transaction atomicity**: `EXEC` writes are wrapped in `MULTI`/`EXEC` on the
  wire; the replica queues them and applies the whole batch before waking local
  readers.
- **TLS**: `--tls-cert`/`--tls-key` enable a pure, nonblocking TLS engine for
  client connections. Ciphertext uses the same bounded output queue as plaintext.

## Verification performed

```sh
dune build
dune build @all
dune test --display=short
dune build @fmt
OCAML_REDIS_LIVE=1 dune exec test/test_main.exe
```

The opt-in live lane also passed against Redis 8.10.2, including Pub/Sub wire
shapes, transaction/geo reply shapes, and the replica-interoperability probe.
The deterministic lane also covers a TLS handshake and encrypted RESP round
trips using a generated self-signed certificate.

The deterministic suite now includes regression tests for:

- protocol ceilings and negative/overflowing lengths
- strict decimal arguments and `INCR` overflow
- absolute expiry semantics and replication across a paused replica
- generated stream-ID replication across a paused replica
- `BLPOP` wakeup propagation
- `XREAD` fan-out to multiple waiters
- resumption of pipelined commands after a blocking wakeup
- error-frame injection and oversized diagnostics
- `SIGPIPE`-safe writes to disconnected subscribers
- case-insensitive subcommands and keywords
- `PUBLISH`/`INFO` inside `MULTI`
- `PSYNC`/`REPLCONF` authentication gates and SHA-256 ACL digests
- `maxclients`, output-buffer, and `maxmemory` enforcement
- non-empty full-resync import, reconnect, and `+CONTINUE` partial resync
- replica transaction isolation and master-side `MULTI`/`EXEC` propagation
- TLS handshake and encrypted RESP round trips

## Residual risk register

These are accepted for the current learning scope, not silently ignored.

| Risk | Impact | Why it remains |
|---|---|---|
| Single event loop | A blocking upstream reconnect attempt can still delay other clients | Output writes are nonblocking and bounded; reconnect attempts are not fully offloaded |
| TLS is client-facing only | Replication upstream links remain plaintext, and clients are not certificate-authenticated | Server-side TLS is implemented; mutual TLS and upstream TLS are future work |
| Loopback-only bind | Remote clients cannot connect without a proxy | Deliberate safety default; `--bind` is not implemented |
| `maxmemory` is approximate and noeviction | One write can cross the ceiling before later writes are rejected; eviction is unavailable | Accounting ignores allocator overhead by design; eviction is a larger policy feature |
| This master still sends an empty full-resync RDB | A replica that first connects to this server does not receive pre-existing local data | The replica imports non-empty RDBs from other masters; serializing this store back to RDB is future work |
| Reconnect uses a 4 MiB backlog | A longer disconnect falls back to full resync, which is empty from this master | Backlog sizing and RDB serialization are future work |
| TLS handshake deadline | Incomplete handshakes are closed after ten seconds | The deadline bounds slowloris-style handshakes; CPU cost per handshake is not accounted separately |
| No cascading replication | A replica cannot serve downstream replicas | Out of the current replication model |
| No active expiry cycle | Expired keys occupy memory until accessed | Redis uses active plus lazy expiry; this subset uses lazy only |
| Wall-clock timeouts | A system clock jump can change blocking timeouts | Expiry needs wall time; separating a monotonic timeout clock is future work |
| RDB checksum not verified | Corrupt RDB payloads may load partially | CRC64 verification is not implemented |
| Single-user auth | No per-user permissions or ACL mutation | Deliberate subset |
| `--requirepass` / `--masterauth` in argv | Visible to local process inspection | A config-file or environment source would be a larger CLI change |
| Passwords capped at 1,024 bytes | Extremely long configured passwords are rejected at startup | Prevents hashing arbitrary client input; documented as a safety limit |
| Custom SHA-256 implementation | A digest bug could weaken the ACL/auth check | Verified against published vectors, including a multi-block one-million-byte input; not formally audited |
| No fuzzing or property tests | Parser edge cases may remain | The regression lane is deterministic and targeted, not exhaustive |
| No LICENSE / opam package | Redistribution and consumption are ambiguous | Owner decisions, not code changes |
