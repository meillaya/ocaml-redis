# Redis Compatibility Matrix

This document describes the current compatibility posture of `ocaml-redis` relative to real Redis for the implemented subset.

## Categories
- **Supported-compatible**: intended to behave like Redis for the documented supported cases
- **Deliberate incompatibility**: intentionally different in this learning-project subset
- **Partial subset**: only part of the Redis command/family surface is implemented

## Core strings
| Area | Status | Notes |
|---|---|---|
| `PING`, `PING <message>` | Supported-compatible | Includes subscribed-mode pong array behavior for current subset |
| `ECHO` | Supported-compatible | Basic wire behavior covered |
| `GET`, `SET`, `INCR` | Supported-compatible | For supported cases only |
| Expiry on strings | Supported-compatible | `SET` with `EX` / `PX` supported |

## Lists
| Area | Status | Notes |
|---|---|---|
| `RPUSH`, `LPUSH`, `LRANGE`, `LLEN`, `LREM` | Supported-compatible | For documented subset only |
| `BLPOP` | Partial subset | Blocking + timeout supported |

## Transactions / WATCH
| Area | Status | Notes |
|---|---|---|
| `MULTI`, `EXEC`, `DISCARD` | Supported-compatible | For current subset semantics |
| `WATCH`, `UNWATCH` | Supported-compatible | For supported optimistic-lock cases |
| Blocking commands inside `MULTI` | Deliberate incompatibility | Explicitly rejected in this subset |

## Streams
| Area | Status | Notes |
|---|---|---|
| `XADD`, `XRANGE`, `XREAD` | Partial subset | Supported for implemented query/id shapes only |
| Stream ID validation | Supported-compatible | For implemented rules |
| Blocking `XREAD` with `$` | Supported-compatible | In current supported shape |

## Replication
| Area | Status | Notes |
|---|---|---|
| `INFO replication` | Partial subset | Replication-focused section only |
| `REPLCONF`, `PSYNC ? -1` | Partial subset | Enough for current master/replica handshake |
| Empty RDB full resync | Partial subset | Empty payload only |
| Single/multi-replica propagation | Supported-compatible | For currently implemented write commands |
| `WAIT` | Partial subset | Implemented for current replication model |
| Auth-enabled master allows unauthenticated `REPLCONF`/`PSYNC` | Deliberate incompatibility | Kept so replicas can sync in this subset |

## RDB loading
| Area | Status | Notes |
|---|---|---|
| Startup `--rdb-file` | Partial subset | String keys/values only |
| Expiry-bearing RDB entries | Partial subset | Supported for current string subset |
| General RDB compatibility | Deliberate incompatibility | Not a full Redis RDB implementation |

## Pub/Sub
| Area | Status | Notes |
|---|---|---|
| `SUBSCRIBE`, `UNSUBSCRIBE`, `PUBLISH` | Partial subset | Core channel pub/sub only |
| `UNSUBSCRIBE` without arguments | Supported-compatible | Includes RESP2 nil/no-subscription shape and reverse-order unwind of active channel subscriptions |
| `PING` in subscribed mode | Supported-compatible | Includes empty-payload `pong` array shape for bare `PING` |
| Pattern subscriptions / broader pubsub surface | Deliberate incompatibility | Not implemented |

## Sorted sets
| Area | Status | Notes |
|---|---|---|
| `ZADD`, `ZRANK`, `ZRANGE`, `ZCARD`, `ZSCORE`, `ZREM` | Partial subset | Exact milestone semantics documented in PRD addendum |
| Ordering | Supported-compatible | Score asc, then member lexicographically asc |

## Geospatial
| Area | Status | Notes |
|---|---|---|
| `GEOADD`, `GEOPOS`, `GEODIST`, `GEOSEARCH` | Partial subset | Exact command shapes from M9 addendum only |
| `TYPE geo_key => zset` | Deliberate compatibility choice | External family identity preserved |
| `Z*` on geo-created keys | Deliberate incompatibility | Wrong-type in current subset |
| `GEO*` on plain zset-created keys | Deliberate incompatibility | Wrong-type in current subset |

## Authentication
| Area | Status | Notes |
|---|---|---|
| `AUTH <password>` | Partial subset | Default user only |
| `ACL WHOAMI` | Supported-compatible | Returns `default` |
| `ACL GETUSER default` | Deliberate incompatibility | Returns cleartext configured password in this learning subset |
| Multi-user/username ACL model | Deliberate incompatibility | Not implemented |
| `--requirepass` + `--replicaof` | Deliberate incompatibility | Explicit startup rejection |

## Guiding rule
Where behavior is listed as supported-compatible, it means compatibility is intended only for the documented, tested cases in this repo — not for every Redis edge case.
