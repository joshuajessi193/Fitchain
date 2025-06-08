(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_GOAL_NOT_FOUND (err u101))
(define-constant ERR_GOAL_ALREADY_EXISTS (err u102))
(define-constant ERR_GOAL_NOT_ACTIVE (err u103))
(define-constant ERR_INSUFFICIENT_BALANCE (err u104))
(define-constant ERR_INVALID_AMOUNT (err u105))
(define-constant ERR_GOAL_EXPIRED (err u106))
(define-constant ERR_ALREADY_COMPLETED (err u107))
(define-constant ERR_INVALID_VERIFICATION (err u108))
(define-constant ERR_VERIFICATION_EXPIRED (err u109))
(define-constant ERR_ALREADY_VERIFIED (err u110))

(define-fungible-token fitchain-token)

(define-data-var goal-id-nonce uint u0)
(define-data-var total-rewards-distributed uint u0)
(define-data-var verification-window uint u144)

(define-map goals
  { goal-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    target-value: uint,
    reward-amount: uint,
    deadline: uint,
    is-active: bool,
    created-at: uint,
    goal-type: (string-ascii 20)
  }
)

(define-map user-goals
  { user: principal, goal-id: uint }
  {
    joined-at: uint,
    progress: uint,
    is-completed: bool,
    reward-claimed: bool,
    verification-hash: (optional (buff 32)),
    verified-at: (optional uint)
  }
)

(define-map user-stats
  { user: principal }
  {
    total-goals: uint,
    completed-goals: uint,
    total-rewards: uint,
    current-streak: uint,
    last-completion: uint
  }
)

(define-map goal-participants
  { goal-id: uint }
  {
    participant-count: uint,
    completion-count: uint,
    total-rewards-pool: uint
  }
)

(define-map verifications
  { verification-id: (buff 32) }
  {
    user: principal,
    goal-id: uint,
    submitted-at: uint,
    verified: bool,
    verifier: (optional principal)
  }
)

(define-public (mint-tokens (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (ft-mint? fitchain-token amount recipient)
  )
)

(define-public (create-goal (title (string-ascii 100)) (description (string-ascii 500)) (target-value uint) (reward-amount uint) (duration uint) (goal-type (string-ascii 20)))
  (let
    (
      (goal-id (+ (var-get goal-id-nonce) u1))
      (deadline (+ stacks-block-height duration))
    )
    (asserts! (> target-value u0) ERR_INVALID_AMOUNT)
    (asserts! (> reward-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> duration u0) ERR_INVALID_AMOUNT)
    
    (map-set goals
      { goal-id: goal-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        target-value: target-value,
        reward-amount: reward-amount,
        deadline: deadline,
        is-active: true,
        created-at: stacks-block-height,
        goal-type: goal-type
      }
    )
    
    (map-set goal-participants
      { goal-id: goal-id }
      {
        participant-count: u0,
        completion-count: u0,
        total-rewards-pool: reward-amount
      }
    )
    
    (var-set goal-id-nonce goal-id)
    (ok goal-id)
  )
)

(define-public (join-goal (goal-id uint))
  (let
    (
      (goal (unwrap! (map-get? goals { goal-id: goal-id }) ERR_GOAL_NOT_FOUND))
      (user-goal-key { user: tx-sender, goal-id: goal-id })
      (participants (unwrap! (map-get? goal-participants { goal-id: goal-id }) ERR_GOAL_NOT_FOUND))
    )
    (asserts! (get is-active goal) ERR_GOAL_NOT_ACTIVE)
    (asserts! (< stacks-block-height (get deadline goal)) ERR_GOAL_EXPIRED)
    (asserts! (is-none (map-get? user-goals user-goal-key)) ERR_GOAL_ALREADY_EXISTS)
    
    (map-set user-goals
      user-goal-key
      {
        joined-at: stacks-block-height,
        progress: u0,
        is-completed: false,
        reward-claimed: false,
        verification-hash: none,
        verified-at: none
      }
    )

    (map-set goal-participants
      { goal-id: goal-id }
      (merge participants { participant-count: (+ (get participant-count participants) u1) })
    )

    (unwrap! (update-user-stats tx-sender u1 u0 u0) ERR_NOT_AUTHORIZED)
    (ok true)
  )
)

(define-public (update-progress (goal-id uint) (progress-value uint))
  (let
    (
      (goal (unwrap! (map-get? goals { goal-id: goal-id }) ERR_GOAL_NOT_FOUND))
      (user-goal-key { user: tx-sender, goal-id: goal-id })
      (user-goal (unwrap! (map-get? user-goals user-goal-key) ERR_GOAL_NOT_FOUND))
    )
    (asserts! (get is-active goal) ERR_GOAL_NOT_ACTIVE)
    (asserts! (< stacks-block-height (get deadline goal)) ERR_GOAL_EXPIRED)
    (asserts! (not (get is-completed user-goal)) ERR_ALREADY_COMPLETED)
    
    (map-set user-goals
      user-goal-key
      (merge user-goal { progress: progress-value })
    )
    
    (if (>= progress-value (get target-value goal))
      (mark-goal-ready-for-verification goal-id)
      (ok true)
    )
  )
)

(define-public (submit-verification (goal-id uint) (verification-hash (buff 32)))
  (let
    (
      (goal (unwrap! (map-get? goals { goal-id: goal-id }) ERR_GOAL_NOT_FOUND))
      (user-goal-key { user: tx-sender, goal-id: goal-id })
      (user-goal (unwrap! (map-get? user-goals user-goal-key) ERR_GOAL_NOT_FOUND))
    )
    (asserts! (>= (get progress user-goal) (get target-value goal)) ERR_INVALID_VERIFICATION)
    (asserts! (is-none (get verification-hash user-goal)) ERR_ALREADY_VERIFIED)
    (asserts! (< stacks-block-height (+ (get deadline goal) (var-get verification-window))) ERR_VERIFICATION_EXPIRED)
    
    (map-set user-goals
      user-goal-key
      (merge user-goal { 
        verification-hash: (some verification-hash),
        verified-at: (some stacks-block-height)
      })
    )
    
    (map-set verifications
      { verification-id: verification-hash }
      {
        user: tx-sender,
        goal-id: goal-id,
        submitted-at: stacks-block-height,
        verified: false,
        verifier: none
      }
    )
    
    (ok true)
  )
)


(define-private (mark-goal-ready-for-verification (goal-id uint))
  (ok true)
)



(define-private (update-user-stats (user principal) (goals-increment uint) (completed-increment uint) (rewards-increment uint))
  (let
    (
      (current-stats (default-to 
        { total-goals: u0, completed-goals: u0, total-rewards: u0, current-streak: u0, last-completion: u0 }
        (map-get? user-stats { user: user })
      ))
      (new-streak (if (> completed-increment u0)
        (if (< (- stacks-block-height (get last-completion current-stats)) u1008)
          (+ (get current-streak current-stats) u1)
          u1)
        (get current-streak current-stats)))
    )
    (map-set user-stats
      { user: user }
      {
        total-goals: (+ (get total-goals current-stats) goals-increment),
        completed-goals: (+ (get completed-goals current-stats) completed-increment),
        total-rewards: (+ (get total-rewards current-stats) rewards-increment),
        current-streak: new-streak,
        last-completion: (if (> completed-increment u0) stacks-block-height (get last-completion current-stats))
      }
    )
    (ok true)
  )
)

(define-public (deactivate-goal (goal-id uint))
  (let
    (
      (goal (unwrap! (map-get? goals { goal-id: goal-id }) ERR_GOAL_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator goal)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active goal) ERR_GOAL_NOT_ACTIVE)
    
    (map-set goals
      { goal-id: goal-id }
      (merge goal { is-active: false })
    )
    (ok true)
  )
)

(define-public (transfer-tokens (amount uint) (recipient principal))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (ft-transfer? fitchain-token amount tx-sender recipient)
  )
)

(define-read-only (get-goal (goal-id uint))
  (map-get? goals { goal-id: goal-id })
)

(define-read-only (get-user-goal (user principal) (goal-id uint))
  (map-get? user-goals { user: user, goal-id: goal-id })
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats { user: user })
)

(define-read-only (get-goal-participants (goal-id uint))
  (map-get? goal-participants { goal-id: goal-id })
)

(define-read-only (get-verification (verification-hash (buff 32)))
  (map-get? verifications { verification-id: verification-hash })
)

(define-read-only (get-token-balance (user principal))
  (ft-get-balance fitchain-token user)
)

(define-read-only (get-total-supply)
  (ft-get-supply fitchain-token)
)

(define-read-only (get-contract-stats)
  {
    total-goals: (var-get goal-id-nonce),
    total-rewards-distributed: (var-get total-rewards-distributed),
    current-block: stacks-block-height,
    verification-window: (var-get verification-window)
  }
)

(define-read-only (is-goal-expired (goal-id uint))
  (match (map-get? goals { goal-id: goal-id })
    goal (>= stacks-block-height (get deadline goal))
    false
  )
)

(define-read-only (get-active-goals-count)
  (var-get goal-id-nonce)
)

(define-read-only (calculate-streak-bonus (user principal))
  (match (map-get? user-stats { user: user })
    stats (let ((streak (get current-streak stats)))
      (if (>= streak u10) u50
        (if (>= streak u5) u25
          (if (>= streak u3) u10
            u0))))
    u0
  )
)