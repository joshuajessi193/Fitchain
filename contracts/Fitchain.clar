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

(define-constant ERR_INVALID_LEAGUE (err u111))
(define-constant ERR_ACHIEVEMENT_NOT_FOUND (err u112))
(define-constant ERR_ALREADY_CLAIMED (err u113))
(define-constant ERR_REQUIREMENTS_NOT_MET (err u114))
(define-constant ERR_LEAGUE_FULL (err u115))
(define-constant ERR_SEASON_ENDED (err u116))
(define-constant ERR_INVALID_BADGE (err u117))

(define-data-var current-season uint u1)
(define-data-var season-duration uint u4320)
(define-data-var season-start-block uint u0)
(define-data-var achievement-id-nonce uint u0)
(define-data-var badge-id-nonce uint u0)
(define-data-var league-entry-fee uint u100)

(define-map leagues
  { league-id: uint }
  {
    name: (string-ascii 50),
    tier: uint,
    max-participants: uint,
    current-participants: uint,
    season: uint,
    entry-fee: uint,
    reward-pool: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map league-participants
  { league-id: uint, user: principal }
  {
    joined-at: uint,
    season-score: uint,
    rank: uint,
    rewards-claimed: bool,
    goals-completed: uint,
    best-streak: uint
  }
)

(define-map user-leagues
  { user: principal, season: uint }
  {
    current-league: uint,
    tier-progress: uint,
    season-achievements: uint,
    total-season-rewards: uint,
    rank-history: (list 10 uint)
  }
)

(define-map achievements
  { achievement-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    category: (string-ascii 30),
    requirement-type: (string-ascii 20),
    requirement-value: uint,
    reward-amount: uint,
    badge-id: uint,
    is-active: bool,
    rarity: (string-ascii 20)
  }
)

(define-map user-achievements
  { user: principal, achievement-id: uint }
  {
    earned-at: uint,
    season-earned: uint,
    reward-claimed: bool,
    progress: uint
  }
)

(define-map badges
  { badge-id: uint }
  {
    name: (string-ascii 50),
    symbol: (string-ascii 10),
    description: (string-ascii 200),
    rarity: (string-ascii 20),
    created-at: uint
  }
)

(define-map user-badges
  { user: principal, badge-id: uint }
  {
    earned-at: uint,
    season-earned: uint,
    display-order: uint
  }
)

(define-map season-leaderboard
  { season: uint, rank: uint }
  {
    user: principal,
    total-score: uint,
    achievements-earned: uint,
    goals-completed: uint,
    final-league: uint
  }
)

(define-public (create-league (name (string-ascii 50)) (tier uint) (max-participants uint) (entry-fee uint))
  (let
    (
      (league-id (+ (var-get goal-id-nonce) u1))
      (current-season-val (var-get current-season))
    )
    (asserts! (> tier u0) ERR_INVALID_LEAGUE)
    (asserts! (> max-participants u0) ERR_INVALID_LEAGUE)
    (asserts! (>= entry-fee u0) ERR_INVALID_AMOUNT)
    
    (map-set leagues
      { league-id: league-id }
      {
        name: name,
        tier: tier,
        max-participants: max-participants,
        current-participants: u0,
        season: current-season-val,
        entry-fee: entry-fee,
        reward-pool: u0,
        is-active: true,
        created-at: stacks-block-height
      }
    )
    
    (var-set goal-id-nonce league-id)
    (ok league-id)
  )
)

(define-public (join-league (league-id uint))
  (let
    (
      (league (unwrap! (map-get? leagues { league-id: league-id }) ERR_INVALID_LEAGUE))
      (participant-key { league-id: league-id, user: tx-sender })
      (current-season-val (var-get current-season))
      (user-balance (ft-get-balance fitchain-token tx-sender))
    )
    (asserts! (get is-active league) ERR_INVALID_LEAGUE)
    (asserts! (is-eq (get season league) current-season-val) ERR_SEASON_ENDED)
    (asserts! (< (get current-participants league) (get max-participants league)) ERR_LEAGUE_FULL)
    (asserts! (>= user-balance (get entry-fee league)) ERR_INSUFFICIENT_BALANCE)
    (asserts! (is-none (map-get? league-participants participant-key)) ERR_GOAL_ALREADY_EXISTS)
    
    (if (> (get entry-fee league) u0)
      (unwrap! (ft-burn? fitchain-token (get entry-fee league) tx-sender) ERR_INSUFFICIENT_BALANCE)
      true
    )
    
    (map-set league-participants
      participant-key
      {
        joined-at: stacks-block-height,
        season-score: u0,
        rank: u0,
        rewards-claimed: false,
        goals-completed: u0,
        best-streak: u0
      }
    )
    
    (map-set leagues
      { league-id: league-id }
      (merge league { 
        current-participants: (+ (get current-participants league) u1),
        reward-pool: (+ (get reward-pool league) (get entry-fee league))
      })
    )
    
    (map-set user-leagues
      { user: tx-sender, season: current-season-val }
      {
        current-league: league-id,
        tier-progress: u0,
        season-achievements: u0,
        total-season-rewards: u0,
        rank-history: (list u0)
      }
    )
    
    (ok true)
  )
)

(define-public (create-achievement (name (string-ascii 50)) (description (string-ascii 200)) (category (string-ascii 30)) (requirement-type (string-ascii 20)) (requirement-value uint) (reward-amount uint) (rarity (string-ascii 20)))
  (let
    (
      (achievement-id (+ (var-get achievement-id-nonce) u1))
      (badge-id (+ (var-get badge-id-nonce) u1))
    )
    (asserts! (> requirement-value u0) ERR_INVALID_AMOUNT)
    (asserts! (> reward-amount u0) ERR_INVALID_AMOUNT)
    
    (map-set badges
      { badge-id: badge-id }
      {
        name: name,
        symbol: (unwrap-panic (as-max-len? (unwrap-panic (slice? name u0 u10)) u10)),
        description: description,
        rarity: rarity,
        created-at: stacks-block-height
      }
    )
    
    (map-set achievements
      { achievement-id: achievement-id }
      {
        name: name,
        description: description,
        category: category,
        requirement-type: requirement-type,
        requirement-value: requirement-value,
        reward-amount: reward-amount,
        badge-id: badge-id,
        is-active: true,
        rarity: rarity
      }
    )
    
    (var-set achievement-id-nonce achievement-id)
    (var-set badge-id-nonce badge-id)
    (ok achievement-id)
  )
)

(define-public (claim-achievement (achievement-id uint))
  (let
    (
      (achievement (unwrap! (map-get? achievements { achievement-id: achievement-id }) ERR_ACHIEVEMENT_NOT_FOUND))
      (user-achievement-key { user: tx-sender, achievement-id: achievement-id })
      (user-stats-data (unwrap! (map-get? user-stats { user: tx-sender }) ERR_GOAL_NOT_FOUND))
      (current-season-val (var-get current-season))
    )
    (asserts! (get is-active achievement) ERR_ACHIEVEMENT_NOT_FOUND)
    (asserts! (is-none (map-get? user-achievements user-achievement-key)) ERR_ALREADY_CLAIMED)
    
    (let
      (
        (requirement-met (check-achievement-requirement achievement user-stats-data))
      )
      (asserts! requirement-met ERR_REQUIREMENTS_NOT_MET)
      
      (map-set user-achievements
        user-achievement-key
        {
          earned-at: stacks-block-height,
          season-earned: current-season-val,
          reward-claimed: true,
          progress: (get requirement-value achievement)
        }
      )
      
      (map-set user-badges
        { user: tx-sender, badge-id: (get badge-id achievement) }
        {
          earned-at: stacks-block-height,
          season-earned: current-season-val,
          display-order: u0
        }
      )
      
      (unwrap! (ft-mint? fitchain-token (get reward-amount achievement) tx-sender) ERR_INSUFFICIENT_BALANCE)
      (ok true)
    )
  )
)

(define-public (update-league-score (user principal) (score-increment uint))
  (let
    (
      (current-season-val (var-get current-season))
      (user-league-data (map-get? user-leagues { user: user, season: current-season-val }))
    )
    (match user-league-data
      user-league
      (let
        (
          (league-id (get current-league user-league))
          (participant-key { league-id: league-id, user: user })
          (participant-data (unwrap! (map-get? league-participants participant-key) ERR_GOAL_NOT_FOUND))
        )
        (map-set league-participants
          participant-key
          (merge participant-data { 
            season-score: (+ (get season-score participant-data) score-increment),
            goals-completed: (+ (get goals-completed participant-data) u1)
          })
        )
        (ok true)
      )
      (ok false)
    )
  )
)

(define-public (distribute-season-rewards (season uint) (league-id uint))
  (let
    (
      (league (unwrap! (map-get? leagues { league-id: league-id }) ERR_INVALID_LEAGUE))
      (reward-pool (get reward-pool league))
      (participant-count (get current-participants league))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> reward-pool u0) ERR_INVALID_AMOUNT)
    (asserts! (> participant-count u0) ERR_INVALID_LEAGUE)
    
    (let
      (
        (first-place-reward (/ (* reward-pool u50) u100))
        (second-place-reward (/ (* reward-pool u30) u100))
        (third-place-reward (/ (* reward-pool u20) u100))
      )
      (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) reward-pool))
      (ok true)
    )
  )
)

(define-public (start-new-season)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set current-season (+ (var-get current-season) u1))
    (var-set season-start-block stacks-block-height)
    (ok (var-get current-season))
  )
)

(define-private (check-achievement-requirement (achievement {name: (string-ascii 50), description: (string-ascii 200), category: (string-ascii 30), requirement-type: (string-ascii 20), requirement-value: uint, reward-amount: uint, badge-id: uint, is-active: bool, rarity: (string-ascii 20)}) (user-stats-data {total-goals: uint, completed-goals: uint, total-rewards: uint, current-streak: uint, last-completion: uint}))
  (let
    (
      (req-type (get requirement-type achievement))
      (req-value (get requirement-value achievement))
    )
    (if (is-eq req-type "goals-completed")
      (>= (get completed-goals user-stats-data) req-value)
      (if (is-eq req-type "current-streak")
        (>= (get current-streak user-stats-data) req-value)
        (if (is-eq req-type "total-rewards")
          (>= (get total-rewards user-stats-data) req-value)
          false
        )
      )
    )
  )
)

(define-read-only (get-league (league-id uint))
  (map-get? leagues { league-id: league-id })
)

(define-read-only (get-league-participant (league-id uint) (user principal))
  (map-get? league-participants { league-id: league-id, user: user })
)

(define-read-only (get-user-league (user principal) (season uint))
  (map-get? user-leagues { user: user, season: season })
)

(define-read-only (get-achievement (achievement-id uint))
  (map-get? achievements { achievement-id: achievement-id })
)

(define-read-only (get-user-achievement (user principal) (achievement-id uint))
  (map-get? user-achievements { user: user, achievement-id: achievement-id })
)

(define-read-only (get-badge (badge-id uint))
  (map-get? badges { badge-id: badge-id })
)

(define-read-only (get-user-badge (user principal) (badge-id uint))
  (map-get? user-badges { user: user, badge-id: badge-id })
)

(define-read-only (get-season-leaderboard (season uint) (rank uint))
  (map-get? season-leaderboard { season: season, rank: rank })
)

(define-read-only (get-current-season)
  (var-get current-season)
)

(define-read-only (get-season-info)
  {
    current-season: (var-get current-season),
    season-duration: (var-get season-duration),
    season-start-block: (var-get season-start-block),
    blocks-remaining: (if (> (+ (var-get season-start-block) (var-get season-duration)) stacks-block-height)
                        (- (+ (var-get season-start-block) (var-get season-duration)) stacks-block-height)
                        u0)
  }
)

(define-read-only (get-achievement-stats)
  {
    total-achievements: (var-get achievement-id-nonce),
    total-badges: (var-get badge-id-nonce),
    league-entry-fee: (var-get league-entry-fee)
  }
)