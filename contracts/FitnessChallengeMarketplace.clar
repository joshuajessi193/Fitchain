;; Fitness Challenge Marketplace for Fitchain
;; Enables users to create, buy, and participate in custom fitness challenges

(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-CHALLENGE-NOT-FOUND (err u301))
(define-constant ERR-INSUFFICIENT-BALANCE (err u302))
(define-constant ERR-ALREADY-PURCHASED (err u303))
(define-constant ERR-CHALLENGE-INACTIVE (err u304))
(define-constant ERR-INVALID-DIFFICULTY (err u305))
(define-constant ERR-INVALID-PRICE (err u306))
(define-constant ERR-ALREADY-REVIEWED (err u307))
(define-constant ERR-CHALLENGE-EXPIRED (err u308))
(define-constant ERR-NOT-PARTICIPANT (err u309))
(define-constant ERR-CHALLENGE-FULL (err u310))

(define-data-var next-challenge-id uint u1)
(define-data-var next-review-id uint u1)
(define-data-var platform-fee-percentage uint u10) ;; 10% platform fee
(define-data-var featured-challenge-fee uint u500) ;; Cost to feature a challenge

;; Custom fitness challenges created by users
(define-map challenges
  { challenge-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    category: (string-ascii 30), ;; "strength", "cardio", "flexibility", "nutrition", etc.
    difficulty: uint, ;; 1-5 scale
    base-price: uint,
    duration-days: uint,
    max-participants: uint,
    current-participants: uint,
    reward-pool: uint,
    creator-earnings: uint,
    is-active: bool,
    is-featured: bool,
    created-at: uint,
    completion-rate: uint, ;; Percentage of successful completions
    average-rating: uint,
    total-reviews: uint
  }
)

;; Track challenge purchases and participation
(define-map challenge-participants
  { challenge-id: uint, participant: principal }
  {
    purchased-at: uint,
    started-at: (optional uint),
    completed-at: (optional uint),
    progress-score: uint,
    difficulty-chosen: uint,
    amount-paid: uint,
    eligible-for-refund: bool,
    completion-verified: bool
  }
)

;; Challenge reviews and ratings
(define-map challenge-reviews
  { review-id: uint }
  {
    challenge-id: uint,
    reviewer: principal,
    rating: uint, ;; 1-5 stars
    review-text: (string-ascii 300),
    difficulty-accuracy: uint, ;; How accurate was the difficulty rating
    would-recommend: bool,
    created-at: uint,
    helpful-votes: uint
  }
)

;; Creator statistics and reputation
(define-map challenge-creators
  { creator: principal }
  {
    total-challenges-created: uint,
    total-participants: uint,
    total-earnings: uint,
    average-challenge-rating: uint,
    successful-completion-rate: uint,
    featured-challenges: uint,
    creator-level: uint, ;; 1-5 reputation level
    last-payout: uint
  }
)

;; Difficulty tier multipliers for rewards
(define-map difficulty-multipliers
  { difficulty: uint }
  { reward-multiplier: uint, price-multiplier: uint }
)

;; Featured challenges rotation
(define-map featured-challenges
  { slot: uint }
  { challenge-id: uint, featured-until: uint }
)

;; Initialize difficulty multipliers
(map-set difficulty-multipliers { difficulty: u1 } { reward-multiplier: u80, price-multiplier: u80 })   ;; Beginner: 0.8x
(map-set difficulty-multipliers { difficulty: u2 } { reward-multiplier: u100, price-multiplier: u100 }) ;; Intermediate: 1.0x
(map-set difficulty-multipliers { difficulty: u3 } { reward-multiplier: u130, price-multiplier: u120 }) ;; Advanced: 1.3x
(map-set difficulty-multipliers { difficulty: u4 } { reward-multiplier: u160, price-multiplier: u150 }) ;; Expert: 1.6x
(map-set difficulty-multipliers { difficulty: u5 } { reward-multiplier: u200, price-multiplier: u180 }) ;; Elite: 2.0x

;; Create a new fitness challenge
(define-public (create-challenge
  (title (string-ascii 100))
  (description (string-ascii 500))
  (category (string-ascii 30))
  (difficulty uint)
  (base-price uint)
  (duration-days uint)
  (max-participants uint))
  (let (
    (challenge-id (var-get next-challenge-id))
    (creator tx-sender)
  )
    ;; Validate inputs
    (asserts! (> difficulty u0) ERR-INVALID-DIFFICULTY)
    (asserts! (<= difficulty u5) ERR-INVALID-DIFFICULTY)
    (asserts! (> base-price u0) ERR-INVALID-PRICE)
    (asserts! (> duration-days u0) ERR-INVALID-PRICE)
    (asserts! (> max-participants u0) ERR-INVALID-PRICE)
    (asserts! (<= max-participants u1000) ERR-INVALID-PRICE)
    
    ;; Create the challenge
    (map-set challenges
      { challenge-id: challenge-id }
      {
        creator: creator,
        title: title,
        description: description,
        category: category,
        difficulty: difficulty,
        base-price: base-price,
        duration-days: duration-days,
        max-participants: max-participants,
        current-participants: u0,
        reward-pool: u0,
        creator-earnings: u0,
        is-active: true,
        is-featured: false,
        created-at: stacks-block-height,
        completion-rate: u0,
        average-rating: u0,
        total-reviews: u0
      }
    )
    
    ;; Update creator stats
    (let (
      (creator-stats (default-to 
        { total-challenges-created: u0, total-participants: u0, total-earnings: u0, average-challenge-rating: u0, 
          successful-completion-rate: u0, featured-challenges: u0, creator-level: u1, last-payout: u0 }
        (map-get? challenge-creators { creator: creator })))
    )
      (map-set challenge-creators
        { creator: creator }
        (merge creator-stats { 
          total-challenges-created: (+ (get total-challenges-created creator-stats) u1)
        })
      )
    )
    
    (var-set next-challenge-id (+ challenge-id u1))
    (ok challenge-id)
  )
)

;; Purchase and join a challenge
(define-public (purchase-challenge (challenge-id uint) (chosen-difficulty uint))
  (let (
    (challenge-data (unwrap! (map-get? challenges { challenge-id: challenge-id }) ERR-CHALLENGE-NOT-FOUND))
    (participant tx-sender)
    (participant-key { challenge-id: challenge-id, participant: participant })
    (difficulty-data (unwrap! (map-get? difficulty-multipliers { difficulty: chosen-difficulty }) ERR-INVALID-DIFFICULTY))
    (final-price (/ (* (get base-price challenge-data) (get price-multiplier difficulty-data)) u100))
    (platform-fee (/ (* final-price (var-get platform-fee-percentage)) u100))
    (creator-payment (- final-price platform-fee))
  )
    ;; Validate purchase
    (asserts! (get is-active challenge-data) ERR-CHALLENGE-INACTIVE)
    (asserts! (< (get current-participants challenge-data) (get max-participants challenge-data)) ERR-CHALLENGE-FULL)
    (asserts! (is-none (map-get? challenge-participants participant-key)) ERR-ALREADY-PURCHASED)
    (asserts! (>= chosen-difficulty (get difficulty challenge-data)) ERR-INVALID-DIFFICULTY)
    
    ;; Transfer payment (simplified - in real implementation would use STX or FIT tokens)
    (asserts! (>= final-price u1) ERR-INSUFFICIENT-BALANCE)
    
    ;; Record participation
    (map-set challenge-participants
      participant-key
      {
        purchased-at: stacks-block-height,
        started-at: (some stacks-block-height),
        completed-at: none,
        progress-score: u0,
        difficulty-chosen: chosen-difficulty,
        amount-paid: final-price,
        eligible-for-refund: true,
        completion-verified: false
      }
    )
    
    ;; Update challenge stats
    (map-set challenges
      { challenge-id: challenge-id }
      (merge challenge-data {
        current-participants: (+ (get current-participants challenge-data) u1),
        reward-pool: (+ (get reward-pool challenge-data) platform-fee),
        creator-earnings: (+ (get creator-earnings challenge-data) creator-payment)
      })
    )
    
    ;; Update creator stats
    (let (
      (creator-stats (unwrap! (map-get? challenge-creators { creator: (get creator challenge-data) }) ERR-CHALLENGE-NOT-FOUND))
    )
      (map-set challenge-creators
        { creator: (get creator challenge-data) }
        (merge creator-stats {
          total-participants: (+ (get total-participants creator-stats) u1),
          total-earnings: (+ (get total-earnings creator-stats) creator-payment)
        })
      )
    )
    
    (ok true)
  )
)

;; Complete a challenge and claim rewards
(define-public (complete-challenge (challenge-id uint) (completion-proof (buff 32)))
  (let (
    (challenge-data (unwrap! (map-get? challenges { challenge-id: challenge-id }) ERR-CHALLENGE-NOT-FOUND))
    (participant tx-sender)
    (participant-key { challenge-id: challenge-id, participant: participant })
    (participation-data (unwrap! (map-get? challenge-participants participant-key) ERR-NOT-PARTICIPANT))
    (difficulty-data (unwrap! (map-get? difficulty-multipliers { difficulty: (get difficulty-chosen participation-data) }) ERR-INVALID-DIFFICULTY))
    (base-reward (/ (get reward-pool challenge-data) (get current-participants challenge-data)))
    (final-reward (/ (* base-reward (get reward-multiplier difficulty-data)) u100))
  )
    ;; Validate completion
    (asserts! (is-some (get started-at participation-data)) ERR-NOT-PARTICIPANT)
    (asserts! (is-none (get completed-at participation-data)) ERR-ALREADY-REVIEWED)
    
    ;; Check if within time limit (simplified validation)
    (let (
      (start-block (unwrap! (get started-at participation-data) ERR-NOT-PARTICIPANT))
      (duration-blocks (* (get duration-days challenge-data) u144)) ;; Approximate blocks per day
    )
      (asserts! (<= stacks-block-height (+ start-block duration-blocks)) ERR-CHALLENGE-EXPIRED)
    )
    
    ;; Mark as completed
    (map-set challenge-participants
      participant-key
      (merge participation-data {
        completed-at: (some stacks-block-height),
        progress-score: u100,
        completion-verified: true,
        eligible-for-refund: false
      })
    )
    
    ;; Update challenge completion stats
    (let (
      (new-completion-count (+ (get current-participants challenge-data) u1))
      (new-completion-rate (/ (* new-completion-count u100) (get current-participants challenge-data)))
    )
      (map-set challenges
        { challenge-id: challenge-id }
        (merge challenge-data { completion-rate: new-completion-rate })
      )
    )
    
    ;; Award completion reward (simplified - would mint FIT tokens)
    (ok final-reward)
  )
)

;; Submit a challenge review
(define-public (submit-review
  (challenge-id uint)
  (rating uint)
  (review-text (string-ascii 300))
  (difficulty-accuracy uint)
  (would-recommend bool))
  (let (
    (review-id (var-get next-review-id))
    (reviewer tx-sender)
    (challenge-data (unwrap! (map-get? challenges { challenge-id: challenge-id }) ERR-CHALLENGE-NOT-FOUND))
    (participation-data (unwrap! (map-get? challenge-participants { challenge-id: challenge-id, participant: reviewer }) ERR-NOT-PARTICIPANT))
  )
    ;; Validate review
    (asserts! (>= rating u1) ERR-INVALID-PRICE)
    (asserts! (<= rating u5) ERR-INVALID-PRICE)
    (asserts! (>= difficulty-accuracy u1) ERR-INVALID-PRICE)
    (asserts! (<= difficulty-accuracy u5) ERR-INVALID-PRICE)
    (asserts! (is-some (get completed-at participation-data)) ERR-NOT-PARTICIPANT)
    
    ;; Create review
    (map-set challenge-reviews
      { review-id: review-id }
      {
        challenge-id: challenge-id,
        reviewer: reviewer,
        rating: rating,
        review-text: review-text,
        difficulty-accuracy: difficulty-accuracy,
        would-recommend: would-recommend,
        created-at: stacks-block-height,
        helpful-votes: u0
      }
    )
    
    ;; Update challenge rating
    (let (
      (new-total-reviews (+ (get total-reviews challenge-data) u1))
      (current-rating-sum (* (get average-rating challenge-data) (get total-reviews challenge-data)))
      (new-rating-sum (+ current-rating-sum rating))
      (new-average-rating (/ new-rating-sum new-total-reviews))
    )
      (map-set challenges
        { challenge-id: challenge-id }
        (merge challenge-data {
          average-rating: new-average-rating,
          total-reviews: new-total-reviews
        })
      )
    )
    
    (var-set next-review-id (+ review-id u1))
    (ok review-id)
  )
)

;; Feature a challenge (paid promotion)
(define-public (feature-challenge (challenge-id uint) (slot uint))
  (let (
    (challenge-data (unwrap! (map-get? challenges { challenge-id: challenge-id }) ERR-CHALLENGE-NOT-FOUND))
    (feature-fee (var-get featured-challenge-fee))
  )
    ;; Validate authorization and payment
    (asserts! (is-eq tx-sender (get creator challenge-data)) ERR-NOT-AUTHORIZED)
    (asserts! (>= slot u1) ERR-INVALID-PRICE)
    (asserts! (<= slot u5) ERR-INVALID-PRICE) ;; Max 5 featured slots
    (asserts! (>= feature-fee u1) ERR-INSUFFICIENT-BALANCE)
    
    ;; Feature the challenge for 7 days
    (map-set featured-challenges
      { slot: slot }
      { challenge-id: challenge-id, featured-until: (+ stacks-block-height u1008) }
    )
    
    ;; Mark challenge as featured
    (map-set challenges
      { challenge-id: challenge-id }
      (merge challenge-data { is-featured: true })
    )
    
    ;; Update creator stats
    (let (
      (creator-stats (unwrap! (map-get? challenge-creators { creator: (get creator challenge-data) }) ERR-CHALLENGE-NOT-FOUND))
    )
      (map-set challenge-creators
        { creator: (get creator challenge-data) }
        (merge creator-stats { featured-challenges: (+ (get featured-challenges creator-stats) u1) })
      )
    )
    
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-challenge (challenge-id uint))
  (map-get? challenges { challenge-id: challenge-id })
)

(define-read-only (get-challenge-participation (challenge-id uint) (participant principal))
  (map-get? challenge-participants { challenge-id: challenge-id, participant: participant })
)

(define-read-only (get-challenge-review (review-id uint))
  (map-get? challenge-reviews { review-id: review-id })
)

(define-read-only (get-creator-stats (creator principal))
  (map-get? challenge-creators { creator: creator })
)

(define-read-only (get-difficulty-pricing (challenge-id uint) (difficulty uint))
  (match (map-get? challenges { challenge-id: challenge-id })
    challenge-data
      (match (map-get? difficulty-multipliers { difficulty: difficulty })
        multipliers
          (some {
            base-price: (get base-price challenge-data),
            difficulty-price: (/ (* (get base-price challenge-data) (get price-multiplier multipliers)) u100),
            reward-multiplier: (get reward-multiplier multipliers)
          })
        none
      )
    none
  )
)

(define-read-only (get-featured-challenge (slot uint))
  (map-get? featured-challenges { slot: slot })
)

(define-read-only (is-challenge-available (challenge-id uint))
  (match (map-get? challenges { challenge-id: challenge-id })
    challenge-data
      (and
        (get is-active challenge-data)
        (< (get current-participants challenge-data) (get max-participants challenge-data))
      )
    false
  )
)

(define-read-only (calculate-completion-reward (challenge-id uint) (difficulty uint))
  (match (map-get? challenges { challenge-id: challenge-id })
    challenge-data
      (match (map-get? difficulty-multipliers { difficulty: difficulty })
        multipliers
          (let (
            (base-reward (/ (get reward-pool challenge-data) (get current-participants challenge-data)))
          )
            (some (/ (* base-reward (get reward-multiplier multipliers)) u100))
          )
        none
      )
    none
  )
)

(define-read-only (get-marketplace-stats)
  {
    total-challenges: (var-get next-challenge-id),
    total-reviews: (var-get next-review-id),
    platform-fee-percentage: (var-get platform-fee-percentage),
    featured-challenge-fee: (var-get featured-challenge-fee)
  }
)
