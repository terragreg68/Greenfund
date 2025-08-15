(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-already-voted (err u103))
(define-constant err-project-not-active (err u104))
(define-constant err-voting-ended (err u105))
(define-constant err-minimum-not-met (err u106))
(define-constant err-unauthorized (err u107))
(define-constant err-milestone-not-found (err u108))
(define-constant err-milestone-already-completed (err u109))
(define-constant err-milestone-insufficient-votes (err u110))
(define-constant err-milestone-not-ready (err u111))
(define-constant err-invalid-milestone-order (err u112))
(define-constant err-invalid-impact-data (err u113))
(define-constant err-credits-not-available (err u114))
(define-constant err-insufficient-credits (err u115))
(define-constant err-invalid-trade-amount (err u116))
(define-constant err-cannot-trade-with-self (err u117))

(define-data-var next-project-id uint u1)
(define-data-var next-milestone-id uint u1)
(define-data-var milestone-voting-period uint u144)
(define-data-var dao-treasury uint u0)
(define-data-var voting-period uint u1008)
(define-data-var minimum-votes uint u10)
(define-data-var total-carbon-credits uint u0)
(define-data-var credit-verification-period uint u720)

(define-map projects
  { project-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    funding-goal: uint,
    current-funding: uint,
    votes-for: uint,
    votes-against: uint,
    created-at: uint,
    status: (string-ascii 20),
    funded: bool
  }
)

(define-map project-votes
  { project-id: uint, voter: principal }
  { vote: bool, amount: uint }
)

(define-map member-stakes
  { member: principal }
  { stake: uint, voting-power: uint }
)

(define-map project-backers
  { project-id: uint, backer: principal }
  { amount: uint }
)

(define-map project-milestones
  { milestone-id: uint }
  {
    project-id: uint,
    title: (string-ascii 100),
    description: (string-ascii 300),
    funding-amount: uint,
    order-index: uint,
    completed: bool,
    completion-votes-for: uint,
    completion-votes-against: uint,
    submission-hash: (optional (buff 32)),
    submitted-at: (optional uint),
    verified-at: (optional uint)
  }
)

(define-map milestone-completion-votes
  { milestone-id: uint, voter: principal }
  { vote: bool, voting-power: uint }
)

(define-map project-milestone-count
  { project-id: uint }
  { count: uint }
)

;; Environmental impact tracking for funded projects
(define-map project-impact-data
  { project-id: uint }
  {
    co2-reduced: uint,        ;; CO2 reduction in kg
    trees-planted: uint,      ;; Number of trees planted
    energy-saved: uint,       ;; Energy saved in kWh
    verified: bool,           ;; Impact data verified by DAO
    verification-votes-for: uint,
    verification-votes-against: uint,
    submitted-at: (optional uint),
    verified-at: (optional uint)
  }
)

;; Carbon credits generated from verified projects
(define-map project-carbon-credits
  { project-id: uint }
  {
    total-credits: uint,      ;; Total credits minted for project
    distributed-credits: uint, ;; Credits already distributed to backers
    available-credits: uint   ;; Credits available for distribution
  }
)

;; Individual carbon credit balances
(define-map carbon-credit-balances
  { holder: principal }
  { credits: uint }
)

;; Carbon credit trading orders
(define-map credit-trade-orders
  { order-id: uint, seller: principal }
  {
    credits-offered: uint,
    price-per-credit: uint,   ;; Price in micro-STX
    active: bool,
    created-at: uint
  }
)

;; Impact data verification votes
(define-map impact-verification-votes
  { project-id: uint, voter: principal }
  { vote: bool, voting-power: uint }
)

(define-public (join-dao (stake-amount uint))
  (let (
    (current-stake (default-to u0 (get stake (map-get? member-stakes { member: tx-sender }))))
  )
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (var-set dao-treasury (+ (var-get dao-treasury) stake-amount))
    (map-set member-stakes
      { member: tx-sender }
      {
        stake: (+ current-stake stake-amount),
        voting-power: (/ (+ current-stake stake-amount) u1000)
      }
    )
    (ok true)
  )
)

(define-public (submit-project (title (string-ascii 100)) (description (string-ascii 500)) (funding-goal uint))
  (let (
    (project-id (var-get next-project-id))
  )
    (map-set projects
      { project-id: project-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        funding-goal: funding-goal,
        current-funding: u0,
        votes-for: u0,
        votes-against: u0,
        created-at: stacks-block-height,
        status: "voting",
        funded: false
      }
    )
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (vote-on-project (project-id uint) (vote-for bool))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
    (member-data (unwrap! (map-get? member-stakes { member: tx-sender }) err-unauthorized))
    (voting-power (get voting-power member-data))
    (existing-vote (map-get? project-votes { project-id: project-id, voter: tx-sender }))
  )
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (is-eq (get status project) "voting") err-project-not-active)
    (asserts! (< stacks-block-height (+ (get created-at project) (var-get voting-period))) err-voting-ended)
    
    (map-set project-votes
      { project-id: project-id, voter: tx-sender }
      { vote: vote-for, amount: voting-power }
    )
    
    (map-set projects
      { project-id: project-id }
      (merge project {
        votes-for: (if vote-for (+ (get votes-for project) voting-power) (get votes-for project)),
        votes-against: (if vote-for (get votes-against project) (+ (get votes-against project) voting-power))
      })
    )
    (ok true)
  )
)

(define-public (finalize-voting (project-id uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
    (total-votes (+ (get votes-for project) (get votes-against project)))
    (votes-for (get votes-for project))
    (votes-against (get votes-against project))
  )
    (asserts! (is-eq (get status project) "voting") err-project-not-active)
    (asserts! (>= stacks-block-height (+ (get created-at project) (var-get voting-period))) err-voting-ended)
    (asserts! (>= total-votes (var-get minimum-votes)) err-minimum-not-met)
    
    (let (
      (approved (> votes-for votes-against))
      (new-status (if approved "approved" "rejected"))
    )
      (map-set projects
        { project-id: project-id }
        (merge project { status: new-status })
      )
      (ok approved)
    )
  )
)

(define-public (fund-project (project-id uint) (amount uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
    (current-backing (default-to u0 (get amount (map-get? project-backers { project-id: project-id, backer: tx-sender }))))
  )
    (asserts! (is-eq (get status project) "approved") err-project-not-active)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set project-backers
      { project-id: project-id, backer: tx-sender }
      { amount: (+ current-backing amount) }
    )
    
    (let (
      (new-funding (+ (get current-funding project) amount))
      (funding-complete (>= new-funding (get funding-goal project)))
    )
      (map-set projects
        { project-id: project-id }
        (merge project {
          current-funding: new-funding,
          status: (if funding-complete "funded" "approved"),
          funded: funding-complete
        })
      )
      (ok funding-complete)
    )
  )
)

(define-public (withdraw-funds (project-id uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
  )
    (asserts! (is-eq tx-sender (get creator project)) err-unauthorized)
    (asserts! (get funded project) err-project-not-active)
    
    (let (
      (funding-amount (get current-funding project))
    )
      (try! (as-contract (stx-transfer? funding-amount tx-sender (get creator project))))
      (map-set projects
        { project-id: project-id }
        (merge project { status: "completed" })
      )
      (ok funding-amount)
    )
  )
)

(define-public (leave-dao)
  (let (
    (member-data (unwrap! (map-get? member-stakes { member: tx-sender }) err-not-found))
    (stake-amount (get stake member-data))
  )
    (asserts! (> stake-amount u0) err-insufficient-funds)
    (asserts! (<= stake-amount (var-get dao-treasury)) err-insufficient-funds)
    
    (try! (as-contract (stx-transfer? stake-amount tx-sender tx-sender)))
    (var-set dao-treasury (- (var-get dao-treasury) stake-amount))
    (map-delete member-stakes { member: tx-sender })
    (ok stake-amount)
  )
)

(define-public (create-milestone (project-id uint) (title (string-ascii 100)) (description (string-ascii 300)) (funding-amount uint) (order-index uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
    (milestone-id (var-get next-milestone-id))
    (current-count (default-to u0 (get count (map-get? project-milestone-count { project-id: project-id }))))
  )
    (asserts! (is-eq tx-sender (get creator project)) err-unauthorized)
    (asserts! (is-eq (get status project) "voting") err-project-not-active)
    
    (map-set project-milestones
      { milestone-id: milestone-id }
      {
        project-id: project-id,
        title: title,
        description: description,
        funding-amount: funding-amount,
        order-index: order-index,
        completed: false,
        completion-votes-for: u0,
        completion-votes-against: u0,
        submission-hash: none,
        submitted-at: none,
        verified-at: none
      }
    )
    
    (map-set project-milestone-count
      { project-id: project-id }
      { count: (+ current-count u1) }
    )
    
    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)
  )
)

(define-public (submit-milestone-completion (milestone-id uint) (completion-hash (buff 32)))
  (let (
    (milestone (unwrap! (map-get? project-milestones { milestone-id: milestone-id }) err-milestone-not-found))
    (project (unwrap! (map-get? projects { project-id: (get project-id milestone) }) err-not-found))
  )
    (asserts! (is-eq tx-sender (get creator project)) err-unauthorized)
    (asserts! (not (get completed milestone)) err-milestone-already-completed)
    (asserts! (is-none (get submitted-at milestone)) err-milestone-already-completed)
    
    (map-set project-milestones
      { milestone-id: milestone-id }
      (merge milestone {
        submission-hash: (some completion-hash),
        submitted-at: (some stacks-block-height)
      })
    )
    (ok true)
  )
)

(define-public (vote-milestone-completion (milestone-id uint) (approve bool))
  (let (
    (milestone (unwrap! (map-get? project-milestones { milestone-id: milestone-id }) err-milestone-not-found))
    (member-data (unwrap! (map-get? member-stakes { member: tx-sender }) err-unauthorized))
    (voting-power (get voting-power member-data))
    (existing-vote (map-get? milestone-completion-votes { milestone-id: milestone-id, voter: tx-sender }))
    (submitted-at (unwrap! (get submitted-at milestone) err-milestone-not-ready))
  )
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (not (get completed milestone)) err-milestone-already-completed)
    (asserts! (< stacks-block-height (+ submitted-at (var-get milestone-voting-period))) err-voting-ended)
    
    (map-set milestone-completion-votes
      { milestone-id: milestone-id, voter: tx-sender }
      { vote: approve, voting-power: voting-power }
    )
    
    (map-set project-milestones
      { milestone-id: milestone-id }
      (merge milestone {
        completion-votes-for: (if approve (+ (get completion-votes-for milestone) voting-power) (get completion-votes-for milestone)),
        completion-votes-against: (if approve (get completion-votes-against milestone) (+ (get completion-votes-against milestone) voting-power))
      })
    )
    (ok true)
  )
)

(define-public (finalize-milestone-completion (milestone-id uint))
  (let (
    (milestone (unwrap! (map-get? project-milestones { milestone-id: milestone-id }) err-milestone-not-found))
    (project (unwrap! (map-get? projects { project-id: (get project-id milestone) }) err-not-found))
    (total-votes (+ (get completion-votes-for milestone) (get completion-votes-against milestone)))
    (votes-for (get completion-votes-for milestone))
    (votes-against (get completion-votes-against milestone))
    (submitted-at (unwrap! (get submitted-at milestone) err-milestone-not-ready))
  )
    (asserts! (not (get completed milestone)) err-milestone-already-completed)
    (asserts! (>= stacks-block-height (+ submitted-at (var-get milestone-voting-period))) err-voting-ended)
    (asserts! (>= total-votes (var-get minimum-votes)) err-milestone-insufficient-votes)
    
    (let (
      (approved (> votes-for votes-against))
    )
      (if approved
        (begin
          (map-set project-milestones
            { milestone-id: milestone-id }
            (merge milestone {
              completed: true,
              verified-at: (some stacks-block-height)
            })
          )
          (try! (as-contract (stx-transfer? (get funding-amount milestone) tx-sender (get creator project))))
          (ok true)
        )
        (begin
          (map-set project-milestones
            { milestone-id: milestone-id }
            (merge milestone {
              submission-hash: none,
              submitted-at: none,
              completion-votes-for: u0,
              completion-votes-against: u0
            })
          )
          (ok false)
        )
      )
    )
  )
)

(define-public (get-milestone-funding (milestone-id uint))
  (let (
    (milestone (unwrap! (map-get? project-milestones { milestone-id: milestone-id }) err-milestone-not-found))
    (project (unwrap! (map-get? projects { project-id: (get project-id milestone) }) err-not-found))
  )
    (asserts! (is-eq tx-sender (get creator project)) err-unauthorized)
    (asserts! (get completed milestone) err-milestone-not-ready)
    
    (ok (get funding-amount milestone))
  )
)

(define-public (fund-milestone-pool (project-id uint) (amount uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
    (current-backing (default-to u0 (get amount (map-get? project-backers { project-id: project-id, backer: tx-sender }))))
  )
    (asserts! (is-eq (get status project) "approved") err-project-not-active)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set project-backers
      { project-id: project-id, backer: tx-sender }
      { amount: (+ current-backing amount) }
    )
    
    (let (
      (new-funding (+ (get current-funding project) amount))
    )
      (map-set projects
        { project-id: project-id }
        (merge project { current-funding: new-funding })
      )
      (ok true)
    )
  )
)

;; Submit environmental impact data for completed projects
(define-public (submit-impact-data (project-id uint) (co2-reduced uint) (trees-planted uint) (energy-saved uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
  )
    (asserts! (is-eq tx-sender (get creator project)) err-unauthorized)
    (asserts! (is-eq (get status project) "completed") err-project-not-active)
    (asserts! (> (+ co2-reduced trees-planted energy-saved) u0) err-invalid-impact-data)
    
    (map-set project-impact-data
      { project-id: project-id }
      {
        co2-reduced: co2-reduced,
        trees-planted: trees-planted,
        energy-saved: energy-saved,
        verified: false,
        verification-votes-for: u0,
        verification-votes-against: u0,
        submitted-at: (some stacks-block-height),
        verified-at: none
      }
    )
    (ok true)
  )
)

;; DAO members vote on impact data verification
(define-public (vote-impact-verification (project-id uint) (approve bool))
  (let (
    (impact-data (unwrap! (map-get? project-impact-data { project-id: project-id }) err-not-found))
    (member-data (unwrap! (map-get? member-stakes { member: tx-sender }) err-unauthorized))
    (voting-power (get voting-power member-data))
    (existing-vote (map-get? impact-verification-votes { project-id: project-id, voter: tx-sender }))
    (submitted-at (unwrap! (get submitted-at impact-data) err-invalid-impact-data))
  )
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (not (get verified impact-data)) err-invalid-impact-data)
    (asserts! (< stacks-block-height (+ submitted-at (var-get credit-verification-period))) err-voting-ended)
    
    (map-set impact-verification-votes
      { project-id: project-id, voter: tx-sender }
      { vote: approve, voting-power: voting-power }
    )
    
    (map-set project-impact-data
      { project-id: project-id }
      (merge impact-data {
        verification-votes-for: (if approve (+ (get verification-votes-for impact-data) voting-power) (get verification-votes-for impact-data)),
        verification-votes-against: (if approve (get verification-votes-against impact-data) (+ (get verification-votes-against impact-data) voting-power))
      })
    )
    (ok true)
  )
)

;; Finalize impact verification and mint carbon credits
(define-public (finalize-impact-verification (project-id uint))
  (let (
    (impact-data (unwrap! (map-get? project-impact-data { project-id: project-id }) err-not-found))
    (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
    (total-votes (+ (get verification-votes-for impact-data) (get verification-votes-against impact-data)))
    (votes-for (get verification-votes-for impact-data))
    (submitted-at (unwrap! (get submitted-at impact-data) err-invalid-impact-data))
  )
    (asserts! (not (get verified impact-data)) err-invalid-impact-data)
    (asserts! (>= stacks-block-height (+ submitted-at (var-get credit-verification-period))) err-voting-ended)
    (asserts! (>= total-votes (var-get minimum-votes)) err-insufficient-credits)
    
    (let (
      (approved (> votes-for (get verification-votes-against impact-data)))
    )
      (if approved
        (begin
          ;; Calculate carbon credits based on impact (simplified formula)
          (let (
            (credits-from-co2 (/ (get co2-reduced impact-data) u100))  ;; 1 credit per 100kg CO2
            (credits-from-trees (/ (get trees-planted impact-data) u10)) ;; 1 credit per 10 trees
            (credits-from-energy (/ (get energy-saved impact-data) u1000)) ;; 1 credit per 1000 kWh
            (total-credits (+ (+ credits-from-co2 credits-from-trees) credits-from-energy))
          )
            (map-set project-impact-data
              { project-id: project-id }
              (merge impact-data {
                verified: true,
                verified-at: (some stacks-block-height)
              })
            )
            
            (map-set project-carbon-credits
              { project-id: project-id }
              {
                total-credits: total-credits,
                distributed-credits: u0,
                available-credits: total-credits
              }
            )
            
            (var-set total-carbon-credits (+ (var-get total-carbon-credits) total-credits))
            (ok total-credits)
          )
        )
        (ok u0)
      )
    )
  )
)

;; Distribute carbon credits to project backers proportionally
(define-public (distribute-credits-to-backers (project-id uint) (backer principal))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
    (credits-data (unwrap! (map-get? project-carbon-credits { project-id: project-id }) err-credits-not-available))
    (backer-data (unwrap! (map-get? project-backers { project-id: project-id, backer: backer }) err-not-found))
    (current-credits (default-to u0 (get credits (map-get? carbon-credit-balances { holder: backer }))))
  )
    (asserts! (> (get available-credits credits-data) u0) err-credits-not-available)
    
    ;; Calculate proportional credits based on backing amount
    (let (
      (backing-ratio (/ (* (get amount backer-data) u100) (get current-funding project)))
      (credits-to-distribute (/ (* (get total-credits credits-data) backing-ratio) u100))
    )
      (asserts! (> credits-to-distribute u0) err-invalid-trade-amount)
      (asserts! (<= credits-to-distribute (get available-credits credits-data)) err-insufficient-credits)
      
      (map-set carbon-credit-balances
        { holder: backer }
        { credits: (+ current-credits credits-to-distribute) }
      )
      
      (map-set project-carbon-credits
        { project-id: project-id }
        (merge credits-data {
          distributed-credits: (+ (get distributed-credits credits-data) credits-to-distribute),
          available-credits: (- (get available-credits credits-data) credits-to-distribute)
        })
      )
      (ok credits-to-distribute)
    )
  )
)

;; Create trade order to sell carbon credits
(define-public (create-credit-trade-order (credits-to-sell uint) (price-per-credit uint))
  (let (
    (seller-credits (default-to u0 (get credits (map-get? carbon-credit-balances { holder: tx-sender }))))
    (order-id (+ stacks-block-height (var-get next-project-id))) ;; Simple order ID generation
  )
    (asserts! (> credits-to-sell u0) err-invalid-trade-amount)
    (asserts! (> price-per-credit u0) err-invalid-trade-amount)
    (asserts! (>= seller-credits credits-to-sell) err-insufficient-credits)
    
    (map-set credit-trade-orders
      { order-id: order-id, seller: tx-sender }
      {
        credits-offered: credits-to-sell,
        price-per-credit: price-per-credit,
        active: true,
        created-at: stacks-block-height
      }
    )
    
    ;; Lock credits during trade
    (map-set carbon-credit-balances
      { holder: tx-sender }
      { credits: (- seller-credits credits-to-sell) }
    )
    (ok order-id)
  )
)

;; Buy carbon credits from trade order
(define-public (buy-carbon-credits (order-id uint) (seller principal) (credits-to-buy uint))
  (let (
    (trade-order (unwrap! (map-get? credit-trade-orders { order-id: order-id, seller: seller }) err-not-found))
    (buyer-credits (default-to u0 (get credits (map-get? carbon-credit-balances { holder: tx-sender }))))
    (total-cost (* credits-to-buy (get price-per-credit trade-order)))
  )
    (asserts! (not (is-eq tx-sender seller)) err-cannot-trade-with-self)
    (asserts! (get active trade-order) err-not-found)
    (asserts! (> credits-to-buy u0) err-invalid-trade-amount)
    (asserts! (<= credits-to-buy (get credits-offered trade-order)) err-insufficient-credits)
    
    ;; Transfer STX payment to seller
    (try! (stx-transfer? total-cost tx-sender seller))
    
    ;; Transfer credits to buyer
    (map-set carbon-credit-balances
      { holder: tx-sender }
      { credits: (+ buyer-credits credits-to-buy) }
    )
    
    ;; Update trade order
    (let (
      (remaining-credits (- (get credits-offered trade-order) credits-to-buy))
    )
      (if (is-eq remaining-credits u0)
        (map-set credit-trade-orders
          { order-id: order-id, seller: seller }
          (merge trade-order { active: false })
        )
        (map-set credit-trade-orders
          { order-id: order-id, seller: seller }
          (merge trade-order { credits-offered: remaining-credits })
        )
      )
    )
    (ok true)
  )
)

;; Retire carbon credits (remove from circulation for offsetting)
(define-public (retire-carbon-credits (credits-to-retire uint))
  (let (
    (holder-credits (default-to u0 (get credits (map-get? carbon-credit-balances { holder: tx-sender }))))
  )
    (asserts! (> credits-to-retire u0) err-invalid-trade-amount)
    (asserts! (>= holder-credits credits-to-retire) err-insufficient-credits)
    
    (map-set carbon-credit-balances
      { holder: tx-sender }
      { credits: (- holder-credits credits-to-retire) }
    )
    
    (var-set total-carbon-credits (- (var-get total-carbon-credits) credits-to-retire))
    (ok true)
  )
)

(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-member-info (member principal))
  (map-get? member-stakes { member: member })
)

(define-read-only (get-project-vote (project-id uint) (voter principal))
  (map-get? project-votes { project-id: project-id, voter: voter })
)

(define-read-only (get-backing-amount (project-id uint) (backer principal))
  (map-get? project-backers { project-id: project-id, backer: backer })
)

(define-read-only (get-dao-treasury)
  (var-get dao-treasury)
)

(define-read-only (get-next-project-id)
  (var-get next-project-id)
)

(define-read-only (get-voting-period)
  (var-get voting-period)
)

(define-read-only (get-minimum-votes)
  (var-get minimum-votes)
)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? project-milestones { milestone-id: milestone-id })
)

(define-read-only (get-milestone-completion-vote (milestone-id uint) (voter principal))
  (map-get? milestone-completion-votes { milestone-id: milestone-id, voter: voter })
)

(define-read-only (get-project-milestone-count (project-id uint))
  (map-get? project-milestone-count { project-id: project-id })
)

(define-read-only (get-next-milestone-id)
  (var-get next-milestone-id)
)

(define-read-only (get-milestone-voting-period)
  (var-get milestone-voting-period)
)

;; Carbon credit system read-only functions
(define-read-only (get-project-impact-data (project-id uint))
  (map-get? project-impact-data { project-id: project-id })
)

(define-read-only (get-project-carbon-credits (project-id uint))
  (map-get? project-carbon-credits { project-id: project-id })
)

(define-read-only (get-carbon-credit-balance (holder principal))
  (map-get? carbon-credit-balances { holder: holder })
)

(define-read-only (get-credit-trade-order (order-id uint) (seller principal))
  (map-get? credit-trade-orders { order-id: order-id, seller: seller })
)

(define-read-only (get-impact-verification-vote (project-id uint) (voter principal))
  (map-get? impact-verification-votes { project-id: project-id, voter: voter })
)

(define-read-only (get-total-carbon-credits)
  (var-get total-carbon-credits)
)

(define-read-only (get-credit-verification-period)
  (var-get credit-verification-period)
)



