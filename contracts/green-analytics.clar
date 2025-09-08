;; Green Project Analytics Dashboard - Comprehensive Reporting System

;; Constants
(define-constant err-not-authorized (err u600))
(define-constant err-invalid-period (err u601))
(define-constant err-insufficient-data (err u602))
(define-constant err-invalid-metric (err u603))

;; Data variables
(define-data-var analytics-enabled bool true)
(define-data-var total-projects-analyzed uint u0)
(define-data-var last-analytics-update uint u0)

;; Project performance metrics
(define-map project-performance-metrics uint {
    success-rate: uint,           ;; Percentage of milestones completed on time
    funding-efficiency: uint,     ;; Percentage of funding goal achieved
    impact-score: uint,           ;; Normalized environmental impact score
    completion-time: uint,        ;; Blocks taken to complete project
    backer-satisfaction: uint,    ;; Community satisfaction rating
    last-updated: uint
})

;; Temporal analytics for trending
(define-map monthly-dao-stats {year: uint, month: uint} {
    projects-submitted: uint,
    projects-approved: uint,
    projects-funded: uint,
    projects-completed: uint,
    total-funding-raised: uint,
    total-carbon-credits: uint,
    active-members: uint,
    total-votes-cast: uint
})

;; Member contribution analytics
(define-map member-analytics principal {
    projects-backed: uint,
    total-backing-amount: uint,
    votes-cast: uint,
    projects-created: uint,
    carbon-credits-earned: uint,
    contribution-score: uint,
    last-activity: uint
})

;; Environmental impact aggregations
(define-map impact-categories (string-ascii 20) {
    total-co2-reduced: uint,
    total-trees-planted: uint,
    total-energy-saved: uint,
    projects-count: uint,
    average-impact-per-project: uint,
    trend-direction: uint          ;; 0=declining, 1=stable, 2=growing
})

;; Project category performance
(define-map category-performance (string-ascii 30) {
    projects-count: uint,
    success-rate: uint,
    average-funding: uint,
    total-impact-score: uint,
    completion-rate: uint
})

;; Funding flow analytics
(define-map funding-flow-metrics uint {
    period-start: uint,
    period-end: uint,
    inflow: uint,                 ;; New funding received
    outflow: uint,                ;; Funds withdrawn by projects
    net-flow: uint,               ;; Net change in treasury
    projects-funded: uint,
    average-project-size: uint
})

;; Read-only functions for analytics queries
(define-read-only (get-project-performance (project-id uint))
    (map-get? project-performance-metrics project-id)
)

(define-read-only (get-monthly-dao-stats (year uint) (month uint))
    (map-get? monthly-dao-stats {year: year, month: month})
)

(define-read-only (get-member-analytics (member principal))
    (map-get? member-analytics member)
)

(define-read-only (get-impact-category-stats (category (string-ascii 20)))
    (map-get? impact-categories category)
)

(define-read-only (get-category-performance (category (string-ascii 30)))
    (map-get? category-performance category)
)

(define-read-only (get-funding-flow-metrics (period-id uint))
    (map-get? funding-flow-metrics period-id)
)

;; Calculate project success score
(define-private (calculate-project-success-score (funding-efficiency uint) (completion-time uint) (impact-score uint))
    (let (
        (efficiency-weight u40)
        (time-weight u30)
        (impact-weight u30)
        (max-completion-time u10000)  ;; Max expected completion time in blocks
    )
        (+ 
            (/ (* funding-efficiency efficiency-weight) u100)
            (+ 
                (/ (* (- max-completion-time (min completion-time max-completion-time)) time-weight) max-completion-time)
                (/ (* impact-score impact-weight) u100)
            )
        )
    )
)

;; Update project performance metrics
(define-public (update-project-metrics (project-id uint) (milestones-completed uint) (total-milestones uint) (funding-received uint) (funding-goal uint) (environmental-impact uint) (completion-blocks uint))
    (let (
        (success-rate (if (> total-milestones u0) (/ (* milestones-completed u100) total-milestones) u0))
        (funding-efficiency (if (> funding-goal u0) (min (/ (* funding-received u100) funding-goal) u100) u0))
        (normalized-impact (min environmental-impact u100))
        (performance-score (calculate-project-success-score funding-efficiency completion-blocks normalized-impact))
    )
        (map-set project-performance-metrics project-id {
            success-rate: success-rate,
            funding-efficiency: funding-efficiency,
            impact-score: normalized-impact,
            completion-time: completion-blocks,
            backer-satisfaction: performance-score,  ;; Simplified satisfaction based on performance
            last-updated: stacks-block-height
        })
        (var-set total-projects-analyzed (+ (var-get total-projects-analyzed) u1))
        (var-set last-analytics-update stacks-block-height)
        (ok true)
    )
)

;; Update monthly DAO statistics
(define-public (record-monthly-stats (year uint) (month uint) (projects-submitted uint) (projects-approved uint) (projects-funded uint) (projects-completed uint) (total-funding uint) (carbon-credits uint) (active-members uint) (votes-cast uint))
    (begin
        (asserts! (and (<= month u12) (> month u0)) err-invalid-period)
        (asserts! (>= year u2023) err-invalid-period)
        
        (map-set monthly-dao-stats {year: year, month: month} {
            projects-submitted: projects-submitted,
            projects-approved: projects-approved,
            projects-funded: projects-funded,
            projects-completed: projects-completed,
            total-funding-raised: total-funding,
            total-carbon-credits: carbon-credits,
            active-members: active-members,
            total-votes-cast: votes-cast
        })
        (ok true)
    )
)

;; Track member contribution analytics
(define-public (update-member-analytics (member principal) (new-backing-amount uint) (votes-increment uint) (projects-created-increment uint) (credits-earned uint))
    (let (
        (current-analytics (default-to {
            projects-backed: u0,
            total-backing-amount: u0,
            votes-cast: u0,
            projects-created: u0,
            carbon-credits-earned: u0,
            contribution-score: u0,
            last-activity: u0
        } (map-get? member-analytics member)))
        (new-total-backing (+ (get total-backing-amount current-analytics) new-backing-amount))
        (new-votes-cast (+ (get votes-cast current-analytics) votes-increment))
        (new-projects-created (+ (get projects-created current-analytics) projects-created-increment))
        (new-credits-earned (+ (get carbon-credits-earned current-analytics) credits-earned))
    )
        (let (
            (contribution-score (+ 
                (/ new-total-backing u1000000)  ;; 1 point per STX backed
                (* new-votes-cast u2)           ;; 2 points per vote
                (* new-projects-created u10)    ;; 10 points per project created
                (/ new-credits-earned u10)      ;; 1 point per 10 credits earned
            ))
            (projects-backed (if (> new-backing-amount u0) (+ (get projects-backed current-analytics) u1) (get projects-backed current-analytics)))
        )
            (map-set member-analytics member {
                projects-backed: projects-backed,
                total-backing-amount: new-total-backing,
                votes-cast: new-votes-cast,
                projects-created: new-projects-created,
                carbon-credits-earned: new-credits-earned,
                contribution-score: contribution-score,
                last-activity: stacks-block-height
            })
            (ok contribution-score)
        )
    )
)

;; Update environmental impact category statistics
(define-public (update-impact-category (category (string-ascii 20)) (co2-reduced uint) (trees-planted uint) (energy-saved uint))
    (let (
        (current-stats (default-to {
            total-co2-reduced: u0,
            total-trees-planted: u0,
            total-energy-saved: u0,
            projects-count: u0,
            average-impact-per-project: u0,
            trend-direction: u1
        } (map-get? impact-categories category)))
        (new-co2 (+ (get total-co2-reduced current-stats) co2-reduced))
        (new-trees (+ (get total-trees-planted current-stats) trees-planted))
        (new-energy (+ (get total-energy-saved current-stats) energy-saved))
        (new-projects-count (+ (get projects-count current-stats) u1))
    )
        (let (
            (total-impact (+ (+ new-co2 new-trees) new-energy))
            (average-impact (if (> new-projects-count u0) (/ total-impact new-projects-count) u0))
            (old-average (get average-impact-per-project current-stats))
            (trend (if (> average-impact old-average) u2 (if (< average-impact old-average) u0 u1)))
        )
            (map-set impact-categories category {
                total-co2-reduced: new-co2,
                total-trees-planted: new-trees,
                total-energy-saved: new-energy,
                projects-count: new-projects-count,
                average-impact-per-project: average-impact,
                trend-direction: trend
            })
            (ok true)
        )
    )
)

;; Record funding flow metrics for a period
(define-public (record-funding-flow (period-id uint) (period-start uint) (period-end uint) (inflow uint) (outflow uint) (projects-funded-count uint))
    (let (
        (net-flow (if (>= inflow outflow) (- inflow outflow) u0))
        (average-size (if (> projects-funded-count u0) (/ inflow projects-funded-count) u0))
    )
        (map-set funding-flow-metrics period-id {
            period-start: period-start,
            period-end: period-end,
            inflow: inflow,
            outflow: outflow,
            net-flow: net-flow,
            projects-funded: projects-funded-count,
            average-project-size: average-size
        })
        (ok true)
    )
)

;; Generate comprehensive DAO health report
(define-read-only (get-dao-health-report)
    (let (
        (current-block stacks-block-height)
        (total-analyzed (var-get total-projects-analyzed))
        (last-update (var-get last-analytics-update))
    )
        {
            total-projects-analyzed: total-analyzed,
            last-analytics-update: last-update,
            analytics-enabled: (var-get analytics-enabled),
            blocks-since-update: (- current-block last-update),
            dao-maturity-score: (min (/ total-analyzed u10) u100)  ;; Basic maturity indicator
        }
    )
)

;; Get top performing projects summary
(define-read-only (calculate-project-rank (project-id uint))
    (let (
        (metrics (map-get? project-performance-metrics project-id))
    )
        (match metrics
            some-metrics 
                (let (
                    (success-score (get success-rate some-metrics))
                    (impact-score (get impact-score some-metrics))
                    (efficiency-score (get funding-efficiency some-metrics))
                    (overall-score (/ (+ (+ success-score impact-score) efficiency-score) u3))
                )
                    (some overall-score)
                )
            none
        )
    )
)

;; Calculate member leaderboard score
(define-read-only (get-member-leaderboard-score (member principal))
    (let (
        (analytics (map-get? member-analytics member))
    )
        (match analytics
            some-analytics
                {
                    member: member,
                    contribution-score: (get contribution-score some-analytics),
                    projects-backed: (get projects-backed some-analytics),
                    votes-cast: (get votes-cast some-analytics),
                    projects-created: (get projects-created some-analytics),
                    last-activity: (get last-activity some-analytics)
                }
            {
                member: member,
                contribution-score: u0,
                projects-backed: u0,
                votes-cast: u0,
                projects-created: u0,
                last-activity: u0
            }
        )
    )
)

;; Administrative function to toggle analytics
(define-public (toggle-analytics)
    (begin
        (var-set analytics-enabled (not (var-get analytics-enabled)))
        (ok (var-get analytics-enabled))
    )
)

;; Get analytics system status
(define-read-only (get-analytics-status)
    {
        enabled: (var-get analytics-enabled),
        total-projects-analyzed: (var-get total-projects-analyzed),
        last-update: (var-get last-analytics-update),
        current-block: stacks-block-height
    }
)
