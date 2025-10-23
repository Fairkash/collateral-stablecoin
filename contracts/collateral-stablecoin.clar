;; collateral-stablecoin.clar
;; Collateralized stablecoin (simple implementation)
;; - Collateral: STX (microSTX)
;; - Stablecoin: internal SIP-010-like token "cUSD" (6 decimals)
;; - Admin sets STX price (microSTX per 1 cUSD)
;; - Users deposit STX collateral and mint cUSD respecting collateral ratio
;; - Liquidation allowed when undercollateralized

(define-constant BPS u10000)
(define-constant STABLE_DECIMALS u1000000) ;; cUSD has 6 decimals (micro-cUSD)

;; -------------------------
;; Errors
;; -------------------------
(define-constant ERR_NOT_ADMIN u100)
(define-constant ERR_PAUSED u101)
(define-constant ERR_BAD_AMOUNT u102)
(define-constant ERR_INSUFFICIENT_COLLATERAL u103)
(define-constant ERR_INSUFFICIENT_DEBT u104)
(define-constant ERR_INSUFFICIENT_BALANCE u105)
(define-constant ERR_TRANSFER_FAIL u106)
(define-constant ERR_NO_POSITION u107)
(define-constant ERR_NOT_UNDERCOLLATERALIZED u108)
(define-constant ERR_ARITH u109)

;; -------------------------
;; Metadata for stablecoin (cUSD)
;; -------------------------
(define-constant token-name "Collateralized USD")
(define-constant token-symbol "cUSD")
(define-constant token-decimals u6)

;; -------------------------
;; Admin & config
;; -------------------------
(define-data-var admin principal tx-sender)
(define-data-var paused bool false)

;; Price oracle: price = microSTX per 1 cUSD (i.e., how many microSTX equals 1 cUSD)
;; Admin must update this frequently from an oracle source in real deployment.
(define-data-var price-microstx-per-cusd uint u1000000) ;; default 1 STX = 1 cUSD (for testing)

;; Collateralization ratio in percent (e.g., 150 means 150%)
(define-data-var collateral-ratio uint u150)

;; Liquidation bonus in percent (e.g., 5 -> liquidator gets 5% bonus on seized collateral)
(define-data-var liquidation-bonus uint u5)

;; -------------------------
;; Token supply & balances (internal SIP-010-like)
;; -------------------------
(define-data-var total-supply uint u0)
(define-map balances { account: principal } { balance: uint })

;; -------------------------
;; Positions mapping
;; Each position keyed by owner principal
;; { collateral: uint (microSTX), debt: uint (micro-cUSD) }
;; -------------------------
(define-map positions
  { owner: principal }
  { collateral: uint, debt: uint })

;; -------------------------
;; Events (print for indexers)
;; -------------------------
(define-private (ev-deposit (who principal) (amount uint))
  (print { event: "deposit-collateral", who: who, amount: amount }))

(define-private (ev-withdraw (who principal) (amount uint))
  (print { event: "withdraw-collateral", who: who, amount: amount }))

(define-private (ev-mint (who principal) (amount uint))
  (print { event: "mint-cusd", who: who, amount: amount }))

(define-private (ev-burn (who principal) (amount uint))
  (print { event: "burn-cusd", who: who, amount: amount }))

(define-private (ev-liquidate (target principal) (liquidator principal) (repaid uint) (collateral-seized uint))
  (print { event: "liquidation", target: target, liquidator: liquidator, repaid: repaid, collateral_seized: collateral-seized }))

(define-private (ev-param-update (name (string-ascii 32)) (value uint))
  (print { event: "param-update", name: name, value: value }))

;; -------------------------
;; Helpers / read-only views
;; -------------------------
(define-read-only (is-admin (p principal)) (is-eq p (var-get admin)))
(define-read-only (is-paused) (var-get paused))

(define-read-only (get-price) (var-get price-microstx-per-cusd))
(define-read-only (get-collateral-ratio) (var-get collateral-ratio))
(define-read-only (get-liquidation-bonus) (var-get liquidation-bonus))

(define-read-only (get-total-supply) (ok (var-get total-supply)))
(define-read-only (get-name) (ok token-name))
(define-read-only (get-symbol) (ok token-symbol))
(define-read-only (get-decimals) (ok token-decimals))



(define-read-only (get-position (who principal))
  (map-get? positions { owner: who }))

;; -------------------------
;; Internal token helpers (internal FT implemented here)
;; -------------------------
(define-private (balance-of (who principal))
  (default-to u0 (get balance (map-get? balances { account: who }))))

(define-private (set-balance (who principal) (amt uint))
  (map-set balances { account: who } { balance: amt }))

(define-private (mint-token (to principal) (amount uint))
  (begin
    (var-set total-supply (+ (var-get total-supply) amount))
    (set-balance to (+ (balance-of to) amount))
    (ev-mint to amount)
    (ok true)))

(define-private (burn-token (from principal) (amount uint))
  (let ((bal (balance-of from)))
    (asserts! (>= bal amount) (err ERR_INSUFFICIENT_BALANCE))
    (set-balance from (- bal amount))
    (var-set total-supply (- (var-get total-supply) amount))
    (ev-burn from amount)
    (ok true)))

;; SIP-010 style transfer (amount, sender, recipient, memo)
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) (err ERR_TRANSFER_FAIL))
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    (let ((sbal (balance-of sender))
          (rbal (balance-of recipient)))
      (asserts! (>= sbal amount) (err ERR_INSUFFICIENT_BALANCE))
      (asserts! (<= (+ rbal amount) (var-get total-supply)) (err ERR_ARITH))
      (set-balance sender (- sbal amount))
      (set-balance recipient (+ rbal amount))
      (ok true))))

;; get-balance (SIP-010)
(define-read-only (get-balance (who principal))
  (ok (balance-of who)))

;; -------------------------
;; Core math: collateral value & limits
;; -------------------------
;; collateral C (microSTX), price P (microSTX per 1 cUSD)
;; collateral_value_in_microcusd = C * STABLE_DECIMALS / P
(define-private (collateral-value-microcusd (collateral uint) (price uint))
  (if (or (is-eq collateral u0) (is-eq price u0))
    u0
    (/ (* collateral STABLE_DECIMALS) price)))

;; max debt allowed (micro-cUSD) at collateral ratio R (percent)
;; max_debt = collateral_value_microcusd * 100 / R
(define-private (max-debt-for-collateral (collateral uint) (price uint) (ratio uint))
  (let ((collv (collateral-value-microcusd collateral price)))
    (if (is-eq collv u0) u0
      (/ (* collv u100) ratio))))

;; check if position is healthy: collateral and debt (both uint), price, ratio
(define-private (is-position-healthy (collateral uint) (debt uint) (price uint) (ratio uint))
  (let ((maxd (max-debt-for-collateral collateral price ratio)))
    (>= maxd debt)))

;; -------------------------
;; User flows
;; -------------------------

;; deposit-collateral: user transfers STX to contract in same call
(define-public (deposit-collateral (amount uint))
  (begin
    (asserts! (not (var-get paused)) (err ERR_PAUSED))
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (let ((pos (map-get? positions { owner: tx-sender })))
      (if (is-none pos)
          (map-set positions { owner: tx-sender } { collateral: amount, debt: u0 })
          (let ((p (unwrap-panic pos)))
            (map-set positions { owner: tx-sender } { collateral: (+ (get collateral p) amount), debt: (get debt p) }))))
    (ev-deposit tx-sender amount)
    (ok true)))

;; withdraw-collateral: withdraw up to allowed amount while keeping position healthy
(define-public (withdraw-collateral (amount uint))
  (begin
    (asserts! (not (var-get paused)) (err ERR_PAUSED))
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    (let ((pos-opt (map-get? positions { owner: tx-sender })))
      (asserts! (is-some pos-opt) (err ERR_NO_POSITION))
      (let ((p (unwrap-panic pos-opt)))
        (let ((coll (get collateral p)) (debt (get debt p)) (price (var-get price-microstx-per-cusd)) (ratio (var-get collateral-ratio)))
          (asserts! (>= coll amount) (err ERR_INSUFFICIENT_COLLATERAL))
          ;; compute remaining collateral and ensure healthy
          (let ((new-coll (- coll amount)))
            (asserts! (is-position-healthy new-coll debt price ratio) (err ERR_INSUFFICIENT_COLLATERAL))
            ;; update and send STX back
            (map-set positions { owner: tx-sender } { collateral: new-coll, debt: debt })
            (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
            (ev-withdraw tx-sender amount)
            (ok true)))))))

;; mint-cusd: mint stablecoin up to allowed debt given collateral
;; amount is in micro-cUSD (i.e., 1 cUSD = 1_000_000)
(define-public (mint-cusd (amount uint))
  (begin
    (asserts! (not (var-get paused)) (err ERR_PAUSED))
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    (let ((pos-opt (map-get? positions { owner: tx-sender })))
      (asserts! (is-some pos-opt) (err ERR_NO_POSITION))
      (let ((p (unwrap-panic pos-opt)))
        (let ((coll (get collateral p))
              (debt (get debt p))
              (price (var-get price-microstx-per-cusd))
              (ratio (var-get collateral-ratio)))
          (let ((maxd (max-debt-for-collateral coll price ratio)))
            (let ((available (if (>= maxd debt) (- maxd debt) u0)))
              (asserts! (>= available amount) (err ERR_INSUFFICIENT_COLLATERAL))
              ;; increase debt and mint tokens
              (map-set positions { owner: tx-sender } { collateral: coll, debt: (+ debt amount) })
              (unwrap! (mint-token tx-sender amount) (err ERR_TRANSFER_FAIL))
              (ok true))))))))

;; burn-cusd: burn stablecoin to reduce debt; user must have balance (transfer then burn)
;; amount in micro-cUSD
(define-public (burn-cusd (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    ;; first burn tokens from caller balance
    (try! (burn-token tx-sender amount))
    ;; reduce debt in position
    (let ((pos-opt (map-get? positions { owner: tx-sender })))
      (asserts! (is-some pos-opt) (err ERR_NO_POSITION))
      (let ((p (unwrap-panic pos-opt)))
        (let ((debt (get debt p)) (coll (get collateral p)))
          (asserts! (>= debt amount) (err ERR_INSUFFICIENT_DEBT))
          (map-set positions { owner: tx-sender } { collateral: coll, debt: (- debt amount) })
          (ok true))))))

;; repay-cusd: alternative flow where user approves contract to pull tokens then contract calls transfer? is not available for internal tokens.
;; Since token is internal, user should call transfer to contract then call repay-from-contract to apply to debt.

;; repay-from-contract: if user has transferred cUSD to contract (i.e., increased contract balance), apply that amount to reduce debt
;; amount in micro-cUSD
(define-public (repay-from-contract (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    ;; ensure contract holds enough cUSD (internal balance)
    (asserts! (>= (balance-of (as-contract tx-sender)) amount) (err ERR_INSUFFICIENT_BALANCE))
    ;; reduce contract balance and total-supply? no: we'll burn then adjust debt
    (try! (burn-token (as-contract tx-sender) amount))
    ;; apply to user's debt
    (let ((pos-opt (map-get? positions { owner: tx-sender })))
      (asserts! (is-some pos-opt) (err ERR_NO_POSITION))
      (let ((p (unwrap-panic pos-opt)))
        (let ((debt (get debt p)) (coll (get collateral p)))
          (asserts! (>= debt amount) (err ERR_INSUFFICIENT_DEBT))
          (map-set positions { owner: tx-sender } { collateral: coll, debt: (- debt amount) })
          (ok true))))))

;; -------------------------
;; Liquidation
;; Anyone can liquidate undercollateralized position.
;; Liquidator repays up to target debt amount and receives equivalent collateral + bonus.
;; repay-amount in micro-cUSD
;; -------------------------
(define-public (liquidate (target principal) (repay-amount uint))
  (begin
    (asserts! (not (var-get paused)) (err ERR_PAUSED))
    (asserts! (> repay-amount u0) (err ERR_BAD_AMOUNT))
    (let ((pos-opt (map-get? positions { owner: target })))
      (asserts! (is-some pos-opt) (err ERR_NO_POSITION))
      (let ((p (unwrap-panic pos-opt)))
        (let ((coll (get collateral p)) (debt (get debt p)) (price (var-get price-microstx-per-cusd)) (ratio (var-get collateral-ratio)) (bonus (var-get liquidation-bonus)))
          ;; check undercollateralized
          (let ((maxd (max-debt-for-collateral coll price ratio)))
            ;; ensure position is undercollateralized (debt > maxd)
            (asserts! (> debt maxd) (err ERR_NOT_UNDERCOLLATERALIZED))
            ;; ensure repay amount not greater than debt
            (asserts! (<= repay-amount debt) (err ERR_BAD_AMOUNT))
            ;; liquidator must provide repay-amount cUSD to contract: caller should transfer internal cUSD to contract first
            ;; Because token is internal, require liquidator burned tokens and we will accept burn from liquidator and use that to reduce debt.
            ;; Simpler UX: liquidator should transfer cUSD to contract by calling transfer(repay-amount, liquidator, as-contract, none) before calling this function.
            ;; So verify contract balance increased:
            (asserts! (>= (balance-of (as-contract tx-sender)) repay-amount) (err ERR_INSUFFICIENT_BALANCE))
            ;; burn the repay-amount from contract balance (it was transferred in by liquidator)
            (try! (burn-token (as-contract tx-sender) repay-amount))
            ;; compute collateral equivalent (microSTX) = repay-amount * price / STABLE_DECIMALS
            (let ((collateral-value (/ (* repay-amount price) STABLE_DECIMALS)))
              ;; apply bonus: seized = collateral-value * (100 + bonus) / 100
              (let ((seized (/ (* collateral-value (+ u100 bonus)) u100)))
                (asserts! (<= seized coll) (err ERR_ARITH)) ;; ensure enough collateral
                ;; ensure arithmetic operations are safe
                (asserts! (<= seized coll) (err ERR_ARITH))
                (asserts! (<= repay-amount debt) (err ERR_ARITH))
                ;; update target position: reduce debt and collateral
                (map-set positions { owner: target } { collateral: (- coll seized), debt: (- debt repay-amount) })
                ;; transfer seized collateral STX to liquidator
                (try! (stx-transfer? seized (as-contract tx-sender) tx-sender))
                (ev-liquidate target tx-sender repay-amount seized)
                (ok { repaid: repay-amount, seized: seized })))))))))

;; -------------------------
;; Admin functions
;; -------------------------
(define-public (set-admin (p principal))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (var-set admin p)
    (ok true)))

(define-public (set-price (price uint))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (asserts! (> price u0) (err ERR_BAD_AMOUNT))
    (var-set price-microstx-per-cusd price)
    (ev-param-update "price-microstx-per-cusd" price)
    (ok true)))

(define-public (set-collateral-ratio (ratio uint))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (asserts! (>= ratio u100) (err ERR_BAD_AMOUNT)) ;; must be >= 100%
    (var-set collateral-ratio ratio)
    (ev-param-update "collateral-ratio" ratio)
    (ok true)))

(define-public (set-liquidation-bonus (bonus uint))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (var-set liquidation-bonus bonus)
    (ev-param-update "liquidation-bonus" bonus)
    (ok true)))

(define-public (set-paused (p bool))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (var-set paused p)
    (ev-param-update "paused" (if p u1 u0))
    (ok true)))

;; rescue STX stuck in contract (only admin)
(define-public (admin-rescue-stx (to principal) (amount uint))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    (try! (stx-transfer? amount (as-contract tx-sender) to))
    (ok true)))

;; rescue cUSD (internal) from contract to admin (only admin) - burns the tokens and reduces supply
(define-public (admin-rescue-cusd (amount uint))
  (begin
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (asserts! (>= (balance-of (as-contract tx-sender)) amount) (err ERR_INSUFFICIENT_BALANCE))
    (try! (burn-token (as-contract tx-sender) amount))
    (ok true)))

;; -------------------------
;; Read-only convenience view: position-health metrics
;; returns map-like tuple with collateral, debt, collateral_value_microcusd, max_debt, healthy?
;; -------------------------
(define-read-only (position-stats (who principal))
  (let ((p (map-get? positions { owner: who })))
    (if (is-none p)
        (err ERR_NO_POSITION)
        (let ((rec (unwrap-panic p)))
          (let ((coll (get collateral rec))
                (debt (get debt rec))
                (price (var-get price-microstx-per-cusd))
                (ratio (var-get collateral-ratio)))
            (let ((collv (collateral-value-microcusd coll price))
                  (maxd (max-debt-for-collateral coll price ratio)))
              (ok { collateral: coll,
                   debt: debt,
                   collateral_value_microcusd: collv,
                   max_debt_microcusd: maxd,
                   healthy: (is-position-healthy coll debt price ratio) })))))))