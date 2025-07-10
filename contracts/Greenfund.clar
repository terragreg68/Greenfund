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

(define-data-var next-project-id uint u1)
(define-data-var next-milestone-id uint u1)
(define-data-var milestone-voting-period uint u144)
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