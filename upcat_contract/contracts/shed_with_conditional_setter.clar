;; Enhanced Conditional Storage Smart Contract
;; Advanced storage with multiple conditions, history tracking, and access controls

;; Data storage
(define-data-var stored-value uint u0)
(define-data-var previous-value uint u0)
(define-data-var total-updates uint u0)
(define-data-var last-update-block uint u0)
(define-data-var last-updater (optional principal) none)
(define-data-var is-paused bool false)
(define-data-var min-block-interval uint u1)
(define-data-var max-value uint u1000000)
(define-data-var condition-mode (string-ascii 20) "greater-than")

;; Access control mappings
(define-map authorized-users principal bool)
(define-map user-update-counts principal uint)
(define-map user-permissions principal (string-ascii 20))

;; Event logs
(define-map update-events uint { 
    old-value: uint, 
    new-value: uint, 
    updater: principal, 
    block-height: uint,
    timestamp: uint
})

;; Error constants
(define-constant ERR-CONDITION-NOT-MET (err u100))
(define-constant ERR-UNAUTHORIZED (err u101))
(define-constant ERR-CONTRACT-PAUSED (err u102))
(define-constant ERR-TOO-SOON (err u103))
(define-constant ERR-VALUE-TOO-HIGH (err u104))
(define-constant ERR-INVALID-MODE (err u105))
(define-constant ERR-HISTORY-FULL (err u106))
(define-constant ERR-USER-LIMIT-REACHED (err u107))

;; Contract owner
(define-constant contract-owner tx-sender)
(define-constant max-user-updates u50)

;; Read-only functions
(define-read-only (get-stored-value)
    (var-get stored-value)
)

(define-read-only (get-contract-info)
    {
        value: (var-get stored-value),
        previous-value: (var-get previous-value),
        total-updates: (var-get total-updates),
        last-update-block: (var-get last-update-block),
        last-updater: (var-get last-updater),
        is-paused: (var-get is-paused),
        condition-mode: (var-get condition-mode),
        max-value: (var-get max-value)
    }
)

(define-read-only (get-value-change)
    {
        current: (var-get stored-value),
        previous: (var-get previous-value),
        change: (if (>= (var-get stored-value) (var-get previous-value))
                    (- (var-get stored-value) (var-get previous-value))
                    u0),
        is-increase: (> (var-get stored-value) (var-get previous-value))
    }
)

(define-read-only (get-user-stats (user principal))
    {
        is-authorized: (default-to false (map-get? authorized-users user)),
        update-count: (default-to u0 (map-get? user-update-counts user)),
        permission-level: (default-to "none" (map-get? user-permissions user))
    }
)

(define-read-only (is-authorized (user principal))
    (or 
        (is-eq user contract-owner)
        (default-to false (map-get? authorized-users user))
    )
)

(define-read-only (can-update-now)
    (and 
        (not (var-get is-paused))
        (>= block-height (+ (var-get last-update-block) (var-get min-block-interval)))
    )
)

(define-read-only (would-accept-value (test-value uint))
    (and
        (<= test-value (var-get max-value))
        (let ((mode (var-get condition-mode))
              (current-val (var-get stored-value)))
            (if (is-eq mode "greater-than")
                (> test-value current-val)
                (if (is-eq mode "greater-equal")
                    (>= test-value current-val)
                    (if (is-eq mode "double")
                        (>= test-value (* current-val u2))
                        (if (is-eq mode "increment-10")
                            (>= test-value (+ current-val u10))
                            (if (is-eq mode "any")
                                true
                                false
                            )
                        )
                    )
                )
            )
        )
    )
)

(define-read-only (get-update-event (event-id uint))
    (map-get? update-events event-id)
)

;; Internal helper functions
(define-private (record-update (old-val uint) (new-val uint))
    (let ((event-id (var-get total-updates)))
        (map-set update-events event-id {
            old-value: old-val,
            new-value: new-val,
            updater: tx-sender,
            block-height: block-height,
            timestamp: (unwrap-panic (get-block-info? time block-height))
        })
        (var-set total-updates (+ event-id u1))
        (var-set last-update-block block-height)
        (var-set last-updater (some tx-sender))
        (var-set previous-value old-val)
        (map-set user-update-counts tx-sender 
            (+ (default-to u0 (map-get? user-update-counts tx-sender)) u1))
    )
)

(define-private (check-user-limits (user principal))
    (let ((user-updates (default-to u0 (map-get? user-update-counts user))))
        (< user-updates max-user-updates)
    )
)

;; Core setter functions
(define-public (set-value (new-value uint))
    (if (not (is-authorized tx-sender))
        ERR-UNAUTHORIZED
        (if (var-get is-paused)
            ERR-CONTRACT-PAUSED
            (if (not (can-update-now))
                ERR-TOO-SOON
                (if (not (check-user-limits tx-sender))
                    ERR-USER-LIMIT-REACHED
                    (if (not (would-accept-value new-value))
                        ERR-CONDITION-NOT-MET
                        (let ((old-value (var-get stored-value)))
                            (var-set stored-value new-value)
                            (record-update old-value new-value)
                            (ok new-value)
                        )
                    )
                )
            )
        )
    )
)

(define-public (set-value-force (new-value uint))
    (if (not (is-eq tx-sender contract-owner))
        ERR-UNAUTHORIZED
        (let ((old-value (var-get stored-value)))
            (var-set stored-value new-value)
            (record-update old-value new-value)
            (ok new-value)
        )
    )
)

(define-public (increment-value (amount uint))
    (set-value (+ (var-get stored-value) amount))
)

(define-public (multiply-value (factor uint))
    (set-value (* (var-get stored-value) factor))
)

;; Access control functions
(define-public (authorize-user (user principal) (permission-level (string-ascii 20)))
    (if (not (is-eq tx-sender contract-owner))
        ERR-UNAUTHORIZED
        (begin
            (map-set authorized-users user true)
            (map-set user-permissions user permission-level)
            (ok true)
        )
    )
)

(define-public (revoke-user (user principal))
    (if (not (is-eq tx-sender contract-owner))
        ERR-UNAUTHORIZED
        (begin
            (map-delete authorized-users user)
            (map-delete user-permissions user)
            (ok true)
        )
    )
)

;; Configuration functions
(define-public (set-condition-mode (mode (string-ascii 20)))
    (if (not (is-eq tx-sender contract-owner))
        ERR-UNAUTHORIZED
        (begin
            (var-set condition-mode mode)
            (ok mode)
        )
    )
)

(define-public (set-max-value (max uint))
    (if (not (is-eq tx-sender contract-owner))
        ERR-UNAUTHORIZED
        (begin
            (var-set max-value max)
            (ok max)
        )
    )
)

(define-public (set-min-block-interval (interval uint))
  (if (not (is-eq tx-sender contract-owner))
    ERR-UNAUTHORIZED
    (begin
        (var-set min-block-interval interval)
        (ok interval)
    )
  )
)

(define-public (pause-contract)
    (if (not (is-eq tx-sender contract-owner))
        ERR-UNAUTHORIZED
        (begin
            (var-set is-paused true)
            (ok true)
        )
    )
)

(define-public (unpause-contract)
    (if (not (is-eq tx-sender contract-owner))
        ERR-UNAUTHORIZED
        (begin
            (var-set is-paused false)
            (ok true)
        )
    )
)

;; Batch operations
(define-public (batch-set-values (values (list 10 uint)))
    (if (not (is-authorized tx-sender))
        ERR-UNAUTHORIZED
        (fold batch-set-helper values (ok u0))
    )
)

(define-private (batch-set-helper (value uint) (previous-result (response uint uint)))
    (match previous-result
        success-count (match (set-value value)
            success-val (ok (+ success-count u1))
            error-val (err error-val))
        error-val (err error-val)
    )
)

;; Helper functions for min/max calculations
(define-private (find-min (a uint) (b uint))
    (if (< a b) a b)
)

(define-private (find-max (a uint) (b uint))
    (if (> a b) a b)
)

;; Statistical functions
(define-read-only (get-total-change)
    (if (> (var-get total-updates) u0)
        (if (>= (var-get stored-value) (var-get previous-value))
            (- (var-get stored-value) (var-get previous-value))
            u0)
        u0
    )
)

;; Utility functions
(define-public (reset-user-stats (user principal))
    (if (not (is-eq tx-sender contract-owner))
        ERR-UNAUTHORIZED
        (begin
            (map-delete user-update-counts user)
            (ok true)
        )
    )
)



;; Initialization
(define-public (initialize (initial-value uint))
    (if (and (is-eq tx-sender contract-owner) (is-eq (var-get stored-value) u0))
        (begin
            (var-set stored-value initial-value)
            (var-set previous-value initial-value)
            (ok initial-value)
        )
        ERR-UNAUTHORIZED
    )
)