(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-already-voted (err u103))
(define-constant err-project-not-active (err u104))
(define-constant err-voting-ended (err u105))
(define-constant err-minimum-not-met (err u106))
(define-constant err-unauthorized (err u107))

(define-data-var next-project-id uint u1)
(define-data-var dao-treasury uint u0)
(define-data-var voting-period uint u1008)
(define-data-var minimum-votes uint u10)

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