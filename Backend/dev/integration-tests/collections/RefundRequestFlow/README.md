# RefundRequestFlow collections

Two independent families of integration tests, run via the local test dashboard.

## 00–07 — refund regression suite (each STANDALONE)

Every collection creates its own completed Stripe-charged ride with fresh random driver and rider
numbers, so they run in any order and any subset. They talk only to backend APIs and to scoped
`/mock/override` rules; nothing reads or writes the database or Redis directly. Driver-side
ledger legs are read through `wallet/ledgerHistory?entityId=<refund request id | ride id>`;
rider-side money is asserted through refund requests, `refunds[]` on the booking and Refund invoices.

| # | covers |
|---|---|
| 00 | smoke: real card-charged ride, commission and refundable caps |
| 01 | driver-deducted refund, clawback accounting |
| 02 | platform-absorbed refund |
| 03 | async resolution |
| 04 | failure + retry |
| 05 | admin entry points |
| 06 | caps + multi-component refunds |
| 07 | customer-surface validation |

## CF1–CF3 — cancellation-fee flows (each STANDALONE)

Each collection onboards its own driver/rider, flips its own feature flag
(`enable_payment_refunds`) through a ConfigPilot rollout on the dashboards, seeds its own cancellation-dues JsonLogic (fee 5.00 + 1.20 VAT;
CF2 adds overdue 4.00 + 0.96) and cleans up after itself. **Run individually — any order,
any subset.** There is no separate commission enable flag: commission books iff the JL
commission orders (and, for route c, `fare_policy.cancellation_commission_charge_config`)
are seeded — absent config means no commission. CF2 and CF3 run TWO cancelled/completed
rides because they prove a *contrast* a single ride can't show; CF1 runs one.

### CF1-CancellationSettleNow — the immediate-charge road
Fee is charged to the card immediately at cancel (driver cancels CUSTOMER_NO_SHOW after
the no-show window).
- Fee 6.20 charged; commission **0.93** books at cancel (driven by the seeded JL commission
  orders); then two half-cap refunds (3.10 + 3.10, driver-deducted) each claw back 0.465; a
  third refund is cap-rejected; the AggregatedCommission job nets booked − clawed to
  **exactly 0**.

### CF2-CancellationOverdue — the two overdue endings
In both rides the card charge FAILS at cancel, so the fee goes overdue: the driver is owed
only the reduced 4.00 + 0.96 and the platform would keep the 1.00 + 0.24 difference
("benefit"). The rides differ in what happens *before* the customer finally pays.
- **Ride A — settled AFTER overdue (reversal road):** customer clears dues *before* any
  driver payout → the overdue + benefit ledger legs are **reversed**, the full fee re-books,
  and the FULL 0.93 commission books. Refunds then show plain full-fee math with **zero**
  benefit-refund legs — proving the reversal netting is honored.
- **Ride B — overdue STANDS (payout road):** the driver is paid out first through a real
  instant wallet payout (overdue legs reach PAID_OUT) → clearing dues keeps the overdue split; only **0.744** commission
  (15% of the reduced 4.96) books. Refunds return **three buckets** pro-rata: driver share +
  commission clawback + the platform giving back its kept benefit
  (`CancellationOverdueBenefitRefund(+Tax)` 1.00 + 0.24). Conservation: all buckets sum to
  the 6.20 the customer gets back.

### CF3-CancellationDueOnNextRide — the deferred fee
One scenario that *needs* two rides: the due born on ride 1 is collected on ride 2
(`settle_cancellation_fee_before_next_ride = false`).

> **⚠ INTERIM ASSERTIONS — gross-carry, not the final tax behavior.** The due is born
> split (the JL books 5.00 fee + 1.20 VAT into `cancellation_dues_details`), but the
> current build **carries it into ride 2 as one GROSS 6.20 with tax 0** — the BPP-side
> re-split was reverted in review ("do it properly later"; see the revert commit on
> `fix-cancellation-ledger-and-overdue`). CF3 asserts what the build does, so today it
> expects: `cancelFeeBase = 6.20`, `cancelFeeTax = 0.00`, and **zero**
> `CancellationFeeRefundVAT` legs (their sums assert to 0). This is knowingly
> tax-imprecise — ride 2's fee slot holds a VAT-inclusive amount marked tax-free.
>
> **When the proper split ships, updating CF3 back is ONE change:** in the step
> *"R2: fareBreakup exposes RIDE_FARE + CANCELLATION_FEE (fee cap 6.20)"*, set the env
> pair back to `cancelFeeBase = 5.00` / `cancelFeeTax = 1.20`. Every assertion computes
> its expected value from that pair, so they all become correct automatically (including
> expecting real `CancellationFeeRefundVAT` legs again). Test names are written
> generically (no build-specific numbers), so nothing else needs editing — just also
> delete this note, which describes the interim build only.
- **Ride 1 — cancelled, fee defers:** nothing is charged, **no** ledger legs book; the dues
  record sits PENDING with the frozen 0.93 commission candidate.
- **Ride 2 — completes and carries the fee:** ONE card charge = ride fare + the carried
  6.20 (fareBreakup shows RIDE_FARE + CANCELLATION_FEE). At EndRide the commission books
  against **ride 2's** booking; the dues record flips PAID. Refunds target ride 2's
  CANCELLATION_FEE component and the clawback reads **ride 2's** ledger — the hard case
  where the commission lives on a different booking than the cancelled ride. Aggregation
  carries ride 2's ride commission while netting the cancellation slice to zero.

## How to run

1. Full local stack up with synced config (`./run-tests.sh sync-config`).
2. `, run-local-test-dashboard` → UI on **:7070**.
3. Pick the collection + the `Local_BF_Helsinki` environment → Run.
