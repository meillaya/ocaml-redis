# ocaml-redis

A correctness-first Redis learning implementation in OCaml.

## Current status

Implemented and verified so far:
- Dune project bootstrap
- library/test layout
- deterministic clock abstraction
- pure expiry helper logic
- RESP2 request parsing and reply encoding for supported commands
- serialized execution core with one authoritative store owner
- multiplexed TCP server using `Unix.select`
- string commands: `PING`, `ECHO`, `TYPE`, `SET`, `GET`, `INCR`
- string expiry via `SET ... EX|PX ...`
- list commands: `RPUSH`, `LPUSH`, `LRANGE`, `LLEN`, `LREM`, `BLPOP`
- blocking list pop with timeout
- transaction commands: `MULTI`, `EXEC`, `DISCARD`
- optimistic locking commands: `WATCH`, `UNWATCH`
- sorted-set commands: `ZADD`, `ZRANK`, `ZRANGE`, `ZCARD`, `ZSCORE`, `ZREM`
- deterministic zset ordering: score ascending, then member lexicographically ascending
- stream commands: `XADD`, `XRANGE`, `XREAD`
- stream ID validation plus partial/full auto-generated IDs
- blocking `XREAD` with `$` semantics and timeout
- geospatial commands: `GEOADD`, `GEOPOS`, `GEODIST`, `GEOSEARCH`
- geo keys currently report `TYPE => zset`
- temporary non-goal boundary for M9:
  - `Z*` on geo-created keys => wrong-type
  - `GEO*` on plain zset-created keys => wrong-type
- replication basics:
  - `INFO replication`
  - `REPLCONF`
  - `PSYNC ? -1`
  - empty RDB transfer on full resync
  - replica startup via `--replicaof HOST:PORT`
  - single and multi-replica command propagation for supported writes
  - replica ACK tracking via `REPLCONF ACK`
  - `WAIT` for replica acknowledgements
- standalone RDB file-load subset:
  - startup load via `--rdb-file PATH`
  - string keys/values
  - multiple keys
  - expiry-bearing keys
- Pub/Sub basics:
  - `SUBSCRIBE`, `UNSUBSCRIBE`, `PUBLISH`
  - subscribed-mode `PING`
  - multi-channel subscribe
  - delivery to subscribed clients
- authentication subset:
  - startup-only `--requirepass PASSWORD`
  - `AUTH <password>`
  - `ACL WHOAMI`
  - `ACL GETUSER default`
  - per-client authenticated state
  - pre-auth allowlist: `AUTH`, `PING`, `REPLCONF`, `PSYNC`
  - fail-fast rejection for `--requirepass` + `--replicaof`

## Deliberate subset limitations

This is a learning-project subset, not a secure production auth implementation.
In particular:
- `ACL GETUSER default` returns the configured password in cleartext
- an auth-enabled master still allows unauthenticated replication handshake control
  commands (`REPLCONF`, `PSYNC`) so replicas can sync in this subset
- no runtime ACL mutation, no username auth, and no multi-user ACL model

## Dependency policy

Runtime/build baseline:
- OCaml stdlib
- `unix`
- `dune`

Any additional dependency must have a written justification tied to correctness,
deterministic testing, or substantial reduction in protocol/networking risk.

## Commands

Build:

```sh
dune build
```

Test:

```sh
dune test
```

Format:

```sh
find app lib test -type f \( -name '*.ml' -o -name '*.mli' \) -print0 | xargs -0 -n1 ocamlformat -i
```

## Compatibility notes

See `docs/compatibility-matrix.md` for the current supported-compatible behavior,
deliberate incompatibilities, and partial-subset boundaries relative to Redis.
