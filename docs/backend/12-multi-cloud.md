# Multi-Cloud Architecture

## Overview

Namma Yatri operates across AWS and GCP clouds. Understanding data flow between clouds is critical for debugging.

## Cloud Model

| Component | AWS | GCP |
|-----------|-----|-----|
| BAP DB | `atlas_app_v2` | `atlas_app_v1` |
| BPP DB | `atlas_driver_offer_bpp_v2` | `atlas_driver_offer_bpp_v1` |
| Redis | Independent instance | Independent instance |

## Key Architectural Rules

1. **Drivers poll the cloud they're registered in** — AWS driver always hits AWS BPP
2. **BAP makes cross-cloud calls to BPP** using BPP URLs (rider in AWS can book driver in GCP)
3. **Updates only happen in the Redis where data is found** — no cross-Redis sync on writes
4. **Secondary Redis checked only on MISS** — stale cache hits bypass secondary! This is a common source of bugs
5. **DB replicates AWS→GCP via logical replication** — Redis does NOT replicate

## Data Replication

| Data Store | Replication | Direction |
|-----------|-------------|-----------|
| PostgreSQL | Logical replication | AWS → GCP |
| Redis | None | Independent per cloud |
| ClickHouse | — | Per cloud |

## KV Connector

The KV connector layer abstracts DB and Redis operations. It's implemented in the `euler-hs` package.

### Read Logic
1. Check primary Redis
2. On MISS → check secondary Redis (if configured)
3. On MISS → query PostgreSQL
4. Cache result in primary Redis

### Write Logic
1. Write to PostgreSQL
2. Write to the Redis where data was found (or primary if new)
3. **Never writes to both Redis instances**

### Key Functions

Core KV functions are in:
- `shared-kernel`: `lib/mobility-core/src/Kernel/Beam/Functions.hs`
- `euler-hs`: `src/EulerHS/KVConnector/Flow.hs`

| Function | Purpose |
|----------|---------|
| `findOneWithKV` | Find single record (Redis → DB fallback) |
| `findAllWithKV` | Find multiple records |
| `findAllWithOptionsKV` | Find with sort/pagination |
| `updateWithKV` | Update record (DB + Redis) |
| `deleteWithKV` | Delete record |
| `createWithKV` | Create record (DB + Redis) |

## N-Cloud Redis (AWS / GCP / NAMMA)

Redis is no longer limited to a primary and one secondary. Every cloud other than the local one is a peer.

| Piece | Where |
|-------|-------|
| `CloudType` (`AWS`, `GCP`, `NAMMA`, `UNAVAILABLE`) and codes 0/1/2 | `Kernel.Types.Version` (`cloudTypeCode`) |
| Cloud tag on ids | `Kernel.Types.CloudTag` (`tagLocalGuid`), `EulerHS.KVConnector.CloudTag` |
| Peer config | `hedisPeerClusterCfgs :: [HedisPeerCfg]` in the app config, `additionalHedisClusterEnvs` in the env |
| KV probing | `EulerHS.KVConnector.Flow` / `Utils` (`meshPeerConns`, `findOneFromRedisAcrossClouds`) |

### Cloud-tagged ids
`generateGUID` (FlowR) replaces the last two characters of the UUID with two decimal digits `v = 10*spread + cloudCode`, using `CLOUD_TYPE` of the generating pod. The cloud is read back as `v % 10`, so adding a cloud never changes how existing ids decode. Ids that are not UUIDs, or have no tag, simply have no hint.

### Read and write behaviour
1. `findOne`: the cloud named by the tag on the row's own primary key is probed first; on a miss every other cloud is probed in parallel. Without a tag the local cloud goes first.
2. `findAll` / `findAllWithOptions` / update-all / delete-all: all clouds are read in parallel (plus DB where applicable) and merged by primary key.
3. Updates and deletes are applied in the cloud where the row was found. Creates go to the local cloud.
4. The tag is only a hint. A wrong or missing tag falls back to the parallel search, so correctness never depends on it.

### Config
- `hedisPeerClusterCfgs = []` keeps the old behaviour (`hedisSecondaryClusterCfg` is the single peer; AWS and GCP are mapped to each other from `CLOUD_TYPE`).
- A non-empty list replaces the secondary. The first entry is the legacy `KVRedisSecondary` connection, the rest connect as `KVRedis<Cloud>`.
- Each entry is `{ peerCloud, peerHedisCfg }`. See `dhall-configs/generic/common.dhall` (`HedisPeerCfg`, `noHedisPeerClusterCfgs`).
- `runInMasterCloudRedisCell` and the LTS redis helpers still use the legacy secondary only.

## Multi-Cloud Callback Routing

For FRFS (public transport), `mkCloudBapUri` in `Beckn/ACL/FRFS/Utils.hs` replaces the host of `subscriberUrl` with runtime `nwAddress` for correct multi-cloud callback routing.

## Shared Redis Keys

`lib/utils/src/Tools/SharedRedisKeys.hs` — utilities for managing Redis keys that need to be consistent across services.

## Common Multi-Cloud Issues

| Issue | Cause | Fix |
|-------|-------|-----|
| Stale data after update | Cache hit in wrong Redis (rule #4) | Check which Redis the data is cached in |
| Driver not seeing update | Driver polling wrong cloud | Verify driver's registration cloud |
| Cross-cloud booking fails | BPP URL incorrect | Check BAP→BPP URL configuration |
| Data missing after failover | Redis doesn't replicate | Data only in one Redis; DB has it via replication |

## Related Docs

- Database patterns: `08-database-patterns.md`
- Architecture overview: `01-architecture-overview.md`
- FRFS multi-cloud: `10-frfs-public-transport.md`
