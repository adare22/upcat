;; Multi-Send Payment Splitter Contract
;; Automatically splits incoming payments among multiple recipients based on shares

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-shares (err u101))
(define-constant err-no-recipients (err u102))
(define-constant err-transfer-failed (err u103))
(define-constant err-recipient-not-found (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-insufficient-balance (err u106))

;; Data Variables
(define-data-var total-shares uint u0)
(define-data-var recipient-count uint u0)

;; Data Maps
(define-map recipients principal uint)
(define-map recipient-index uint principal)

;; Private Functions
(define-private (calculate-share (amount uint) (shares uint))
  (/ (* amount shares) (var-get total-shares)))

(define-private (distribute-to-recipient (recipient-data {index: uint, amount: uint}) (prev-result (response bool uint)))
  (match prev-result
    success-value
    (let ((index (get index recipient-data))
          (amount (get amount recipient-data))
          (recipient-opt (map-get? recipient-index index)))
      (match recipient-opt
        recipient-principal
        (let ((shares (default-to u0 (map-get? recipients recipient-principal))))
          (if (> shares u0)
            (let ((share-amount (calculate-share amount shares)))
              (match (as-contract (stx-transfer? share-amount tx-sender recipient-principal))
                success (ok true)
                error err-transfer-failed))
            (ok true)))
        (ok true)))
    error-value (err error-value)))

;; Public Functions

;; Add a recipient with their share allocation
(define-public (add-recipient (recipient principal) (shares uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> shares u0) err-invalid-shares)
    
    ;; Check if recipient already exists
    (match (map-get? recipients recipient)
      existing-shares
      (begin
        ;; Update existing recipient's shares
        (var-set total-shares (- (+ (var-get total-shares) shares) existing-shares))
        (map-set recipients recipient shares)
        (ok true))
      ;; Add new recipient
      (begin
        (map-set recipients recipient shares)
        (map-set recipient-index (var-get recipient-count) recipient)
        (var-set recipient-count (+ (var-get recipient-count) u1))
        (var-set total-shares (+ (var-get total-shares) shares))
        (ok true)))))

;; Remove a recipient
(define-public (remove-recipient (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (match (map-get? recipients recipient)
      shares
      (begin
        (map-delete recipients recipient)
        (var-set total-shares (- (var-get total-shares) shares))
        (ok true))
      err-recipient-not-found)))

;; Update recipient shares
(define-public (update-shares (recipient principal) (new-shares uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> new-shares u0) err-invalid-shares)
    (match (map-get? recipients recipient)
      old-shares
      (begin
        (var-set total-shares (+ (- (var-get total-shares) old-shares) new-shares))
        (map-set recipients recipient new-shares)
        (ok true))
      err-recipient-not-found)))

;; Distribute payment to all recipients
(define-public (distribute-payment (amount uint))
  (let ((recipient-list (list 
          {index: u0, amount: amount}
          {index: u1, amount: amount}
          {index: u2, amount: amount}
          {index: u3, amount: amount}
          {index: u4, amount: amount}
          {index: u5, amount: amount}
          {index: u6, amount: amount}
          {index: u7, amount: amount}
          {index: u8, amount: amount}
          {index: u9, amount: amount})))
    (begin
      (asserts! (> amount u0) err-invalid-amount)
      (asserts! (> (var-get recipient-count) u0) err-no-recipients)
      (asserts! (>= (stx-get-balance tx-sender) amount) err-insufficient-balance)
      
      ;; Transfer STX to contract first
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      
      ;; Distribute to all recipients
      (fold distribute-to-recipient recipient-list (ok true)))))

;; Multi-send to specific recipients with custom amounts
(define-public (multi-send (send-list (list 10 {to: principal, amount: uint})))
  (begin
    (asserts! (> (len send-list) u0) err-no-recipients)
    (fold multi-send-iter send-list (ok true))))

(define-private (multi-send-iter (send-data {to: principal, amount: uint}) (result (response bool uint)))
  (match result
    success
    (match (stx-transfer? (get amount send-data) tx-sender (get to send-data))
      success-transfer (ok true)
      error-transfer err-transfer-failed)
    error (err error)))

;; Read-only functions

;; Get recipient shares
(define-read-only (get-recipient-shares (recipient principal))
  (default-to u0 (map-get? recipients recipient)))

;; Get total shares
(define-read-only (get-total-shares)
  (var-get total-shares))

;; Get recipient count
(define-read-only (get-recipient-count)
  (var-get recipient-count))

;; Calculate what a recipient would receive from a given amount
(define-read-only (calculate-recipient-share (recipient principal) (amount uint))
  (let ((shares (get-recipient-shares recipient)))
    (if (and (> shares u0) (> (var-get total-shares) u0))
      (calculate-share amount shares)
      u0)))

;; Get recipient by index
(define-read-only (get-recipient-by-index (index uint))
  (map-get? recipient-index index))