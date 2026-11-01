# Redis Compatibility Matrix

This document describes the current compatibility posture of `ocaml-redis`
relative to real Redis for the implemented subset.

## Categories

- **Supported-compatible**: intended to behave like Redis for the documented
  supported cases
- **Deliberate incompatibility**: intentionally different in this
  learning-project subset
- **Partial subset**: only part of the Redis command/family surface is
  implemented

## Core strings

| Area | Status | Notes |
|---|---|---|
| `PING`, `PING <message>` | Supported-compatible | Includes subscribed-mode pong array behavior for current subset |
| `ECHO` | Supported-compatible | Basic wire behavior covered |
| `GET`, `SET`, `INCR` | Supported-compatible | For supported cases only |
| Expiry on strings | Supported-compatible | `SET` with `EX`, `PX`, `EXAT`, and `PXAT` |
| Integer parsing | Supported-compatible | Canonical decimal only; hexadecimal, `+`, and `_` separators are rejected |
| `INCR` overflow | Supported-compatible | Reports overflow instead of wrapping |

## Lists

| Area | Status | Notes |
|---|---|---|
| `RPUSH`, `LPUSH`, `LRANGE`, `LLEN`, `LREM`, `LPOP` | Supported-compatible | For documented subset only |
| `BLPOP` | Partial subset | Blocking plus timeout; single-key form only |
| Replicated `BLPOP` | Supported-compatible | Propagated as the `LPOP` it performed, including wakeup-path pops |

## Transactions / WATCH

| Area | Status | Notes |
|---|---|---|
| `MULTI`, `EXEC`, `DISCARD` | Supported-compatible | For current subset semantics |
| `WATCH`, `UNWATCH` | Supported-compatible | For supported optimistic-lock cases |
| `PUBLISH`, `INFO` inside `MULTI` | Supported-compatible | Executed at `EXEC` and returned in the reply array |
| Blocking commands inside `MULTI` | Deliberate incompatibility | Explicitly rejected |
| `AUTH`, ACL, `SUBSCRIBE`, `REPLCONF`, `PSYNC` inside `MULTI` | Deliberate incompatibility | Rejected instead of queued |

## Streams

| Area | Status | Notes |
|---|---|---|
| `XADD`, `XRANGE`, `XREAD` | Partial subset | Supported for implemented query/id shapes only |
| Stream ID validation | Supported-compatible | For implemented rules |
| Blocking `XREAD` with `$` | Supported-compatible | In current supported shape |
| Multiple blocked `XREAD` waiters | Supported-compatible | Every waiter on the key receives new entries |
| Replicated auto-generated IDs | Supported-compatible | The master's concrete ID is propagated |

## Replication

| Area | Status | Notes |
|---|---|---|
| `INFO replication` | Partial subset | Replication-focused section only |
| `INFO memory` | Partial subset | Reports `used_memory`, `maxmemory`, policy, and client counts |
| `REPLCONF`, `PSYNC ? -1` | Partial subset | Enough for the current master/replica handshake |
| Replica authentication | Supported-compatible | `--masterauth` sends `AUTH` before `REPLCONF`/`PSYNC` |
| Authenticated `PSYNC` | Supported-compatible | Required when `--requirepass` is configured |
| Empty RDB full resync | Partial subset | This server sends an empty payload |
| Non-empty RDB full resync (replica side) | Partial subset | Parsed and imported for supported string values; unsupported value types fail the sync |
| This master's full-resync payload | Deliberate incompatibility | Empty; local data is not serialized for first-time replicas |
| Single/multi-replica propagation | Supported-compatible | For currently implemented write commands |
| Replicated relative expiries | Supported-compatible | Rewritten as absolute `PXAT` deadlines |
| Transaction atomicity on replicas | Supported-compatible | `EXEC` writes are wrapped in `MULTI`/`EXEC`; the replica queues them and applies the batch before waking readers |
| `WAIT` | Partial subset | Implemented for the current replication model |
| Replica reconnect / partial resync | Supported-compatible | One-second reconnect and a 4 MiB master backlog; longer gaps use full resync |
| Transaction propagation | Supported-compatible | Wrapped in `MULTI`/`EXEC`; replicas apply the batch before waking readers |
| Cascading replication | Deliberate incompatibility | Not implemented |

## RDB loading

| Area | Status | Notes |
|---|---|---|
| Startup `--rdb-file` | Partial subset | String keys/values only |
| Expiry-bearing RDB entries | Partial subset | Supported for the current string subset |
| Large files / checksums | Deliberate incompatibility | Files are capped at 256 MiB and the checksum is not verified |
| General RDB compatibility | Deliberate incompatibility | Not a full Redis RDB implementation |

## Pub/Sub

| Area | Status | Notes |
|---|---|---|
| `SUBSCRIBE`, `UNSUBSCRIBE`, `PUBLISH` | Partial subset | Core channel pub/sub only |
| `UNSUBSCRIBE` without arguments | Supported-compatible | Includes RESP2 nil/no-subscription shape and unsubscribe-all acknowledgements in subscription order |
| `PING` in subscribed mode | Supported-compatible | Includes empty-payload `pong` array shape for bare `PING` |
| Replication of `PUBLISH` | Deliberate incompatibility | Messages are not forwarded to replicas |
| Pattern subscriptions / broader pubsub surface | Deliberate incompatibility | Not implemented |

## Sorted sets

| Area | Status | Notes |
|---|---|---|
| `ZADD`, `ZRANK`, `ZRANGE`, `ZCARD`, `ZSCORE`, `ZREM` | Partial subset | Only the implemented option-free forms |
| Ordering | Supported-compatible | Score ascending, then member lexicographically ascending |
| Infinite scores | Deliberate incompatibility | `inf` / `-inf` scores are rejected |

## Geospatial

| Area | Status | Notes |
|---|---|---|
| `GEOADD`, `GEOPOS`, `GEODIST`, `GEOSEARCH` | Partial subset | Only the implemented `FROMLONLAT ... BYRADIUS` shape |
| `GEOSEARCH` center/radius validation | Supported-compatible | Invalid centers and negative radii are rejected |
| Units | Partial subset | `m` and `km` only |
| `TYPE geo_key => zset` | Deliberate compatibility choice | External family identity preserved |
| `Z*` on geo-created keys | Deliberate incompatibility | Wrong-type in current subset |
| `GEO*` on plain zset-created keys | Deliberate incompatibility | Wrong-type in current subset |

## Authentication

| Area | Status | Notes |
|---|---|---|
| `AUTH <password>` | Partial subset | Default user only; constant-time digest comparison |
| `ACL WHOAMI` | Supported-compatible | Returns `default` |
| `ACL GETUSER default` | Partial subset | Returns `flags` and `passwords` with SHA-256 digests, never cleartext; other Redis ACL fields are omitted |
| `PSYNC` authorization | Supported-compatible | Requires authentication when `requirepass` is set |
| Multi-user/username ACL model | Deliberate incompatibility | Not implemented |
| TLS | Supported-compatible | Client connections via `--tls-cert`/`--tls-key`; upstream replication and client certificates are not supported |

## Protocol and resource limits

| Area | Status | Notes |
|---|---|---|
| Bulk string ceiling | Deliberate safety limit | 64 MiB per bulk string |
| Array ceiling | Deliberate safety limit | 1,048,576 arguments per request |
| Client input buffer ceiling | Deliberate safety limit | 128 MiB per connection |
| `--maxclients` | Operational control | Default 1024; excess connections receive an error |
| `--max-output-buffer` | Operational control | Default 64 MiB per client; over-limit readers are closed |
| `--maxmemory` | Partial subset | Default 256 MiB, approximate accounting, `noeviction` policy; `0` disables |
| Password length ceiling | Deliberate safety limit | 1,024 bytes for `AUTH`, `requirepass`, and `masterauth` |
| Error replies | Hardened | Embedded CR/LF is flattened and diagnostics are truncated |
| Inline commands | Deliberate incompatibility | Only RESP arrays are accepted |
| `SELECT`, `QUIT`, `HELLO`, RESP3 | Deliberate incompatibility | Not implemented |

## Guiding rule

Where behavior is listed as supported-compatible, it means compatibility is
intended only for the documented, tested cases in this repo, not for every
Redis edge case.
