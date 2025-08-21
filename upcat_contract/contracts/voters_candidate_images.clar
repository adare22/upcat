;; Advanced Voting Smart Contract with Candidate Images (IPFS Hash)
;; Enhanced version with multiple voting rounds, delegation, and advanced features

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-CANDIDATE-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-VOTED (err u102))
(define-constant ERR-VOTING-CLOSED (err u103))
(define-constant ERR-CANDIDATE-EXISTS (err u104))
(define-constant ERR-INVALID-CANDIDATE-ID (err u105))
(define-constant ERR-INVALID-ROUND (err u106))
(define-constant ERR-SELF-DELEGATION (err u107))
(define-constant ERR-DELEGATION-LOOP (err u108))
(define-constant ERR-MINIMUM-VOTE-THRESHOLD (err u109))
(define-constant ERR-VOTING-NOT-STARTED (err u110))
(define-constant ERR-INVALID-TIME (err u111))
(define-constant ERR-PROPOSAL-EXISTS (err u112))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u113))
(define-constant ERR-INSUFFICIENT-STAKE (err u114))

;; Contract owner and admin roles
(define-constant CONTRACT-OWNER tx-sender)
(define-data-var emergency-admin (optional principal) none)

;; Voting configuration
(define-data-var voting-active bool false)
(define-data-var current-round uint u1)
(define-data-var voting-start-time uint u0)
(define-data-var voting-end-time uint u0)
(define-data-var min-vote-threshold uint u10) ;; Minimum votes required to validate election
(define-data-var require-registration bool true)

;; Candidate management
(define-data-var candidate-counter uint u0)
(define-data-var max-candidates-per-round uint u20)

;; Proposal system for governance
(define-data-var proposal-counter uint u0)

;; Token-based voting (optional)
(define-data-var token-voting-enabled bool false)
(define-data-var voting-token-contract (optional principal) none)

;; Data structures

;; Enhanced candidate information
(define-map candidates
  { candidate-id: uint, round: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    image-hash: (string-ascii 100),
    profile-url: (optional (string-ascii 200)),
    manifesto-hash: (optional (string-ascii 100)), ;; IPFS hash for detailed manifesto
    vote-count: uint,
    weighted-vote-count: uint, ;; For token-based voting
    active: bool,
    created-at: uint,
    creator: principal,
    category: (string-ascii 30) ;; e.g., "president", "senator", "mayor"
  }
)

;; Voting records with more details
(define-map vote-records
  { voter: principal, round: uint }
  { 
    voted: bool, 
    candidate-id: uint,
    vote-weight: uint,
    timestamp: uint,
    vote-hash: (optional (string-ascii 64)) ;; For vote verification
  }
)

;; Vote delegation system
(define-map delegations
  { delegator: principal, round: uint }
  {
    delegate: principal,
    active: bool,
    created-at: uint
  }
)

;; Voter registration system
(define-map registered-voters
  { voter: principal }
  {
    registered: bool,
    registration-date: uint,
    voter-category: (string-ascii 20), ;; e.g., "citizen", "resident", "member"
    kyc-verified: bool,
    stake-amount: uint
  }
)

;; Proposal system for governance changes
(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    votes-for: uint,
    votes-against: uint,
    voting-deadline: uint,
    executed: bool,
    proposal-type: (string-ascii 30), ;; "config", "candidate", "emergency"
    target-value: uint,
    active: bool
  }
)

;; Round configuration
(define-map voting-rounds
  { round: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    start-time: uint,
    end-time: uint,
    min-participation: uint,
    voting-type: (string-ascii 20), ;; "simple", "weighted", "ranked"
    active: bool,
    finalized: bool
  }
)

;; Candidate lists per round
(define-map round-candidates
  { round: uint }
  { candidate-ids: (list 100 uint) }
)

;; Vote verification and audit trail
(define-map vote-audit-trail
  { vote-id: uint }
  {
    voter: principal,
    candidate-id: uint,
    round: uint,
    timestamp: uint,
    verification-hash: (string-ascii 64),
    ip-hash: (optional (string-ascii 64)) ;; Hashed IP for fraud detection
  }
)

(define-data-var vote-audit-counter uint u0)

;; Admin and Setup Functions

;; Set emergency admin
(define-public (set-emergency-admin (admin principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set emergency-admin (some admin))
    (ok true)
  )
)

;; Create new voting round
(define-public (create-voting-round (title (string-ascii 100))
                                  (description (string-ascii 300))
                                  (start-time uint)
                                  (end-time uint)
                                  (min-participation uint)
                                  (voting-type (string-ascii 20)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> end-time start-time) ERR-INVALID-TIME)
    
    (let ((next-round (+ (var-get current-round) u1)))
      (map-set voting-rounds
        { round: next-round }
        {
          title: title,
          description: description,
          start-time: start-time,
          end-time: end-time,
          min-participation: min-participation,
          voting-type: voting-type,
          active: false,
          finalized: false
        }
      )
      
      ;; Initialize empty candidate list for round
      (map-set round-candidates
        { round: next-round }
        { candidate-ids: (list) }
      )
      
      (ok next-round)
    )
  )
)

;; Start voting round
(define-public (start-voting-round (round uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    (match (map-get? voting-rounds { round: round })
      round-data
      (begin
        (asserts! (>= block-height (get start-time round-data)) ERR-VOTING-NOT-STARTED)
        (var-set current-round round)
        (var-set voting-active true)
        (var-set voting-start-time (get start-time round-data))
        (var-set voting-end-time (get end-time round-data))
        
        (map-set voting-rounds
          { round: round }
          (merge round-data { active: true })
        )
        (ok true)
      )
      ERR-INVALID-ROUND
    )
  )
)

;; Enhanced candidate management

;; Add candidate with enhanced features
(define-public (add-candidate (name (string-ascii 50)) 
                             (description (string-ascii 200)) 
                             (image-hash (string-ascii 100))
                             (profile-url (optional (string-ascii 200)))
                             (manifesto-hash (optional (string-ascii 100)))
                             (category (string-ascii 30))
                             (round uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Check round exists
    (asserts! (is-some (map-get? voting-rounds { round: round })) ERR-INVALID-ROUND)
    
    ;; Check candidate limit
    (let ((current-candidates (default-to (list) (get candidate-ids (map-get? round-candidates { round: round })))))
      (asserts! (< (len current-candidates) (var-get max-candidates-per-round)) ERR-CANDIDATE-EXISTS)
    )
    
    (var-set candidate-counter (+ (var-get candidate-counter) u1))
    
    (let ((new-candidate-id (var-get candidate-counter)))
      (map-set candidates
        { candidate-id: new-candidate-id, round: round }
        {
          name: name,
          description: description,
          image-hash: image-hash,
          profile-url: profile-url,
          manifesto-hash: manifesto-hash,
          vote-count: u0,
          weighted-vote-count: u0,
          active: true,
          created-at: block-height,
          creator: tx-sender,
          category: category
        }
      )
      
      ;; Add to round candidates
      (let ((current-candidates (default-to (list) (get candidate-ids (map-get? round-candidates { round: round })))))
        (map-set round-candidates
          { round: round }
          { candidate-ids: (unwrap! (as-max-len? (append current-candidates new-candidate-id) u100) ERR-INVALID-CANDIDATE-ID) }
        )
      )
      
      (ok new-candidate-id)
    )
  )
)

;; Voter Registration System

;; Register voter
(define-public (register-voter (voter-category (string-ascii 20)) (stake-amount uint))
  (begin
    (asserts! (is-none (map-get? registered-voters { voter: tx-sender })) ERR-ALREADY-VOTED)
    
    (map-set registered-voters
      { voter: tx-sender }
      {
        registered: true,
        registration-date: block-height,
        voter-category: voter-category,
        kyc-verified: false,
        stake-amount: stake-amount
      }
    )
    (ok true)
  )
)

;; Verify KYC for voter (admin only)
(define-public (verify-voter-kyc (voter principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    (match (map-get? registered-voters { voter: voter })
      voter-data
      (begin
        (map-set registered-voters
          { voter: voter }
          (merge voter-data { kyc-verified: true })
        )
        (ok true)
      )
      ERR-CANDIDATE-NOT-FOUND
    )
  )
)

;; Vote Delegation System

;; Delegate vote to another address
(define-public (delegate-vote (delegate principal) (round uint))
  (begin
    (asserts! (not (is-eq tx-sender delegate)) ERR-SELF-DELEGATION)
    (asserts! (is-eligible-voter tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; Check for delegation loops
    (asserts! (not (has-delegation-loop tx-sender delegate round)) ERR-DELEGATION-LOOP)
    
    (map-set delegations
      { delegator: tx-sender, round: round }
      {
        delegate: delegate,
        active: true,
        created-at: block-height
      }
    )
    (ok true)
  )
)

;; Revoke delegation
(define-public (revoke-delegation (round uint))
  (begin
    (match (map-get? delegations { delegator: tx-sender, round: round })
      delegation-data
      (begin
        (map-set delegations
          { delegator: tx-sender, round: round }
          (merge delegation-data { active: false })
        )
        (ok true)
      )
      ERR-CANDIDATE-NOT-FOUND
    )
  )
)

;; Enhanced voting system

;; Cast vote with enhanced features
(define-public (vote-for-candidate (candidate-id uint) (vote-weight uint) (verification-hash (string-ascii 64)))
  (begin
    (asserts! (var-get voting-active) ERR-VOTING-CLOSED)
    (asserts! (<= block-height (var-get voting-end-time)) ERR-VOTING-CLOSED)
    (asserts! (is-eligible-voter tx-sender) ERR-NOT-AUTHORIZED)
    
    (let ((current-round-val (var-get current-round)))
      ;; Check if already voted in this round
      (asserts! (is-none (map-get? vote-records { voter: tx-sender, round: current-round-val })) ERR-ALREADY-VOTED)
      
      ;; Verify candidate exists and is active
      (match (map-get? candidates { candidate-id: candidate-id, round: current-round-val })
        candidate-data
        (begin
          (asserts! (get active candidate-data) ERR-CANDIDATE-NOT-FOUND)
          
          ;; Calculate effective vote weight
          (let ((effective-weight (calculate-vote-weight tx-sender vote-weight)))
            ;; Record the vote
            (map-set vote-records
              { voter: tx-sender, round: current-round-val }
              {
                voted: true,
                candidate-id: candidate-id,
                vote-weight: effective-weight,
                timestamp: block-height,
                vote-hash: (some verification-hash)
              }
            )
            
            ;; Update candidate vote counts
            (map-set candidates
              { candidate-id: candidate-id, round: current-round-val }
              (merge candidate-data { 
                vote-count: (+ (get vote-count candidate-data) u1),
                weighted-vote-count: (+ (get weighted-vote-count candidate-data) effective-weight)
              })
            )
            
            ;; Add to audit trail
            (add-to-audit-trail tx-sender candidate-id current-round-val verification-hash)
            
            (ok effective-weight)
          )
        )
        ERR-CANDIDATE-NOT-FOUND
      )
    )
  )
)

;; Proposal System for Governance

;; Create governance proposal
(define-public (create-proposal (title (string-ascii 100))
                               (description (string-ascii 500))
                               (proposal-type (string-ascii 30))
                               (target-value uint)
                               (voting-deadline uint))
  (begin
    (asserts! (is-eligible-voter tx-sender) ERR-NOT-AUTHORIZED)
    
    (var-set proposal-counter (+ (var-get proposal-counter) u1))
    
    (let ((proposal-id (var-get proposal-counter)))
      (map-set proposals
        { proposal-id: proposal-id }
        {
          title: title,
          description: description,
          proposer: tx-sender,
          votes-for: u0,
          votes-against: u0,
          voting-deadline: voting-deadline,
          executed: false,
          proposal-type: proposal-type,
          target-value: target-value,
          active: true
        }
      )
      (ok proposal-id)
    )
  )
)

;; Vote on proposal
(define-public (vote-on-proposal (proposal-id uint) (support bool))
  (begin
    (asserts! (is-eligible-voter tx-sender) ERR-NOT-AUTHORIZED)
    
    (match (map-get? proposals { proposal-id: proposal-id })
      proposal-data
      (begin
        (asserts! (get active proposal-data) ERR-PROPOSAL-NOT-FOUND)
        (asserts! (<= block-height (get voting-deadline proposal-data)) ERR-VOTING-CLOSED)
        
        (let ((vote-weight (get-voter-weight tx-sender)))
          (if support
            (map-set proposals
              { proposal-id: proposal-id }
              (merge proposal-data { votes-for: (+ (get votes-for proposal-data) vote-weight) })
            )
            (map-set proposals
              { proposal-id: proposal-id }
              (merge proposal-data { votes-against: (+ (get votes-against proposal-data) vote-weight) })
            )
          )
          (ok true)
        )
      )
      ERR-PROPOSAL-NOT-FOUND
    )
  )
)

;; Private helper functions

;; Check if voter is eligible
(define-private (is-eligible-voter (voter principal))
  (if (var-get require-registration)
    (match (map-get? registered-voters { voter: voter })
      voter-data (and (get registered voter-data) (get kyc-verified voter-data))
      false
    )
    true
  )
)

;; Calculate vote weight based on token holdings or stake
(define-private (calculate-vote-weight (voter principal) (base-weight uint))
  (if (var-get token-voting-enabled)
    ;; Token-based voting weight calculation would go here
    base-weight
    ;; Simple 1-person-1-vote for now
    u1
  )
)

;; Get voter weight for proposals
(define-private (get-voter-weight (voter principal))
  (match (map-get? registered-voters { voter: voter })
    voter-data (+ u1 (/ (get stake-amount voter-data) u1000)) ;; 1 base vote + bonus for stake
    u1
  )
)

;; Check for delegation loops
(define-private (has-delegation-loop (delegator principal) (delegate principal) (round uint))
  ;; Simple check - in production, this would need recursive loop detection
  (match (map-get? delegations { delegator: delegate, round: round })
    delegate-data (is-eq (get delegate delegate-data) delegator)
    false
  )
)

;; Add vote to audit trail
(define-private (add-to-audit-trail (voter principal) (candidate-id uint) (round uint) (verification-hash (string-ascii 64)))
  (begin
    (var-set vote-audit-counter (+ (var-get vote-audit-counter) u1))
    (map-set vote-audit-trail
      { vote-id: (var-get vote-audit-counter) }
      {
        voter: voter,
        candidate-id: candidate-id,
        round: round,
        timestamp: block-height,
        verification-hash: verification-hash,
        ip-hash: none
      }
    )
  )
)

;; Enhanced Read-only functions

;; Get candidate with full details
(define-read-only (get-candidate-full (candidate-id uint) (round uint))
  (map-get? candidates { candidate-id: candidate-id, round: round })
)

;; Get all candidates for a round
(define-read-only (get-round-candidates (round uint))
  (default-to (list) (get candidate-ids (map-get? round-candidates { round: round })))
)

;; Get voting round details
(define-read-only (get-voting-round (round uint))
  (map-get? voting-rounds { round: round })
)

;; Get voter registration status
(define-read-only (get-voter-status (voter principal))
  (map-get? registered-voters { voter: voter })
)

;; Get vote record for voter in specific round
(define-read-only (get-vote-record (voter principal) (round uint))
  (map-get? vote-records { voter: voter, round: round })
)

;; Get delegation details
(define-read-only (get-delegation (delegator principal) (round uint))
  (map-get? delegations { delegator: delegator, round: round })
)

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

;; Get election statistics for a round
(define-read-only (get-election-stats (round uint))
  (let ((candidate-list (get-round-candidates round)))
    {
      total-candidates: (len candidate-list),
      total-votes: (fold + (map get-candidate-vote-count candidate-list) u0),
      round-info: (map-get? voting-rounds { round: round }),
      participation-rate: u0 ;; Would calculate based on registered voters
    }
  )
)

;; Helper to get candidate vote count
(define-private (get-candidate-vote-count (candidate-id uint))
  (match (map-get? candidates { candidate-id: candidate-id, round: (var-get current-round) })
    candidate-data (get vote-count candidate-data)
    u0
  )
)

;; Get top candidates by vote count
(define-read-only (get-top-candidates (round uint) (limit uint))
  ;; This would need a more complex implementation to sort candidates
  ;; For now, returns all candidates in the round
  (get-round-candidates round)
)

;; Emergency functions

;; Emergency pause (only emergency admin or owner)
(define-public (emergency-pause)
  (begin
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                  (is-eq (some tx-sender) (var-get emergency-admin))) ERR-NOT-AUTHORIZED)
    (var-set voting-active false)
    (ok true)
  )
)

;; Get current voting status
(define-read-only (get-voting-status)
  {
    active: (var-get voting-active),
    current-round: (var-get current-round),
    start-time: (var-get voting-start-time),
    end-time: (var-get voting-end-time),
    total-candidates: (var-get candidate-counter),
    require-registration: (var-get require-registration)
  }
)

;; Configuration functions

;; Update voting configuration
(define-public (update-config (min-threshold uint) (max-candidates uint) (require-reg bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set min-vote-threshold min-threshold)
    (var-set max-candidates-per-round max-candidates)
    (var-set require-registration require-reg)
    (ok true)
  )
)