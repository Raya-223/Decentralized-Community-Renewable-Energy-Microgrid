(define-public (transfer-governance-tokens (amount uint) (recipient principal) (memo (optional (buff 34))))
  (begin
    (try! (ft-transfer? community-energy-token amount tx-sender recipient))
    (match memo to-print (print to-print) 0x)
    (ok true)
  )
);; ===============================================
;; Community Energy Microgrid Platform
;; ===============================================
;; A decentralized platform for community renewable energy management
;; with peer-to-peer trading, storage coordination, and grid resilience

;; ===============================================
;; CONTRACT 1: Core Energy Management & Trading
;; ===============================================

;; File: contracts/energy-trading.clar

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-NODE (err u103))
(define-constant ERR-ORDER-NOT-FOUND (err u104))
(define-constant ERR-INVALID-PRICE (err u105))
(define-constant ERR-EMERGENCY-ACTIVE (err u106))

;; Data Variables
(define-data-var emergency-mode bool false)
(define-data-var grid-resilience-score uint u100)
(define-data-var total-carbon-credits uint u0)
(define-data-var energy-price-per-kwh uint u50) ;; in micro-STX

;; Data Maps
(define-map energy-nodes
  { node-id: uint }
  {
    owner: principal,
    capacity-kwh: uint,
    current-generation: uint,
    current-storage: uint,
    storage-capacity: uint,
    node-type: (string-ascii 20), ;; "solar", "wind", "battery", "hybrid"
    location: (string-ascii 50),
    active: bool,
    carbon-offset: uint
  }
)

(define-map energy-balances
  { owner: principal }
  {
    available-energy: uint, ;; in watt-hours
    reserved-energy: uint,
    carbon-credits: uint,
    reputation-score: uint
  }
)

(define-map trading-orders
  { order-id: uint }
  {
    seller: principal,
    buyer: (optional principal),
    energy-amount: uint, ;; in watt-hours
    price-per-kwh: uint, ;; in micro-STX
    order-type: (string-ascii 10), ;; "sell", "buy"
    expires-at: uint,
    filled: bool,
    emergency-priority: bool
  }
)

(define-map emergency-allocations
  { recipient: principal }
  {
    allocated-energy: uint,
    priority-level: uint, ;; 1=critical, 2=high, 3=normal
    expires-at: uint
  }
)

;; Auto-incrementing counters
(define-data-var next-node-id uint u1)
(define-data-var next-order-id uint u1)

;; ===============================================
;; Node Management Functions
;; ===============================================

(define-public (register-energy-node
  (capacity-kwh uint)
  (storage-capacity uint)
  (node-type (string-ascii 20))
  (location (string-ascii 50)))
  (let ((node-id (var-get next-node-id)))
    (asserts! (> capacity-kwh u0) ERR-INVALID-AMOUNT)

    (map-set energy-nodes
      { node-id: node-id }
      {
        owner: tx-sender,
        capacity-kwh: capacity-kwh,
        current-generation: u0,
        current-storage: u0,
        storage-capacity: storage-capacity,
        node-type: node-type,
        location: location,
        active: true,
        carbon-offset: u0
      }
    )

    ;; Initialize energy balance for new node owner
    (map-set energy-balances
      { owner: tx-sender }
      (merge
        (default-to
          { available-energy: u0, reserved-energy: u0, carbon-credits: u0, reputation-score: u100 }
          (map-get? energy-balances { owner: tx-sender })
        )
        { reputation-score: u100 }
      )
    )

    (var-set next-node-id (+ node-id u1))
    (ok node-id)
  )
)

(define-public (update-energy-generation (node-id uint) (generation-wh uint))
  (let ((node-data (unwrap! (map-get? energy-nodes { node-id: node-id }) ERR-INVALID-NODE)))
    (asserts! (is-eq (get owner node-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (get active node-data) ERR-INVALID-NODE)

    (map-set energy-nodes
      { node-id: node-id }
      (merge node-data { current-generation: generation-wh })
    )

    ;; Update owner's energy balance
    (update-energy-balance tx-sender generation-wh u0)

    ;; Calculate carbon credits (1 credit per kWh generated from renewables)
    (let ((carbon-credits (/ generation-wh u1000)))
      (var-set total-carbon-credits (+ (var-get total-carbon-credits) carbon-credits))
      (update-carbon-credits tx-sender carbon-credits)
    )

    (ok true)
  )
)

(define-public (update-energy-storage (node-id uint) (storage-wh uint))
  (let ((node-data (unwrap! (map-get? energy-nodes { node-id: node-id }) ERR-INVALID-NODE)))
    (asserts! (is-eq (get owner node-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (<= storage-wh (get storage-capacity node-data)) ERR-INVALID-AMOUNT)

    (map-set energy-nodes
      { node-id: node-id }
      (merge node-data { current-storage: storage-wh })
    )
    (ok true)
  )
)

;; ===============================================
;; Energy Trading Functions
;; ===============================================

(define-public (create-sell-order
  (energy-amount uint)
  (price-per-kwh uint)
  (expires-at uint))
  (let (
    (order-id (var-get next-order-id))
    (balance (default-to
      { available-energy: u0, reserved-energy: u0, carbon-credits: u0, reputation-score: u100 }
      (map-get? energy-balances { owner: tx-sender })
    ))
  )
    (asserts! (> energy-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> price-per-kwh u0) ERR-INVALID-PRICE)
    (asserts! (>= (get available-energy balance) energy-amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (> expires-at stacks-block-height) ERR-INVALID-AMOUNT)

    ;; Reserve energy for the order
    (map-set energy-balances
      { owner: tx-sender }
      (merge balance {
        available-energy: (- (get available-energy balance) energy-amount),
        reserved-energy: (+ (get reserved-energy balance) energy-amount)
      })
    )

    (map-set trading-orders
      { order-id: order-id }
      {
        seller: tx-sender,
        buyer: none,
        energy-amount: energy-amount,
        price-per-kwh: price-per-kwh,
        order-type: "sell",
        expires-at: expires-at,
        filled: false,
        emergency-priority: false
      }
    )

    (var-set next-order-id (+ order-id u1))
    (ok order-id)
  )
)

(define-public (fulfill-order (order-id uint))
  (let (
    (order (unwrap! (map-get? trading-orders { order-id: order-id }) ERR-ORDER-NOT-FOUND))
    (buyer-balance (default-to
      { available-energy: u0, reserved-energy: u0, carbon-credits: u0, reputation-score: u100 }
      (map-get? energy-balances { owner: tx-sender })
    ))
    (seller-balance (default-to
      { available-energy: u0, reserved-energy: u0, carbon-credits: u0, reputation-score: u100 }
      (map-get? energy-balances { owner: (get seller order) })
    ))
    (total-price (* (get energy-amount order) (/ (get price-per-kwh order) u1000)))
  )
    (asserts! (not (get filled order)) ERR-ORDER-NOT-FOUND)
    (asserts! (< stacks-block-height (get expires-at order)) ERR-ORDER-NOT-FOUND)
    (asserts! (not (is-eq tx-sender (get seller order))) ERR-NOT-AUTHORIZED)

    ;; Transfer STX payment to seller
    (try! (stx-transfer? total-price tx-sender (get seller order)))

    ;; Update energy balances
    (map-set energy-balances
      { owner: tx-sender }
      (merge buyer-balance {
        available-energy: (+ (get available-energy buyer-balance) (get energy-amount order))
      })
    )

    (map-set energy-balances
      { owner: (get seller order) }
      (merge seller-balance {
        reserved-energy: (- (get reserved-energy seller-balance) (get energy-amount order))
      })
    )

    ;; Mark order as filled
    (map-set trading-orders
      { order-id: order-id }
      (merge order { buyer: (some tx-sender), filled: true })
    )

    ;; Update reputation scores
    (update-reputation (get seller order) u5)
    (update-reputation tx-sender u2)

    (ok true)
  )
)

;; ===============================================
;; Emergency Management Functions
;; ===============================================

(define-public (activate-emergency-mode)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set emergency-mode true)
    (ok true)
  )
)

(define-public (deactivate-emergency-mode)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set emergency-mode false)
    (ok true)
  )
)

(define-public (allocate-emergency-energy
  (recipient principal)
  (energy-amount uint)
  (priority-level uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (var-get emergency-mode) ERR-EMERGENCY-ACTIVE)
    (asserts! (and (>= priority-level u1) (<= priority-level u3)) ERR-INVALID-AMOUNT)

    (map-set emergency-allocations
      { recipient: recipient }
      {
        allocated-energy: energy-amount,
        priority-level: priority-level,
        expires-at: (+ stacks-block-height u144) ;; ~24 hours
      }
    )

    ;; Update recipient's energy balance
    (update-energy-balance recipient energy-amount u0)
    (ok true)
  )
)

;; ===============================================
;; Grid Resilience & Monitoring Functions
;; ===============================================

(define-public (update-grid-resilience-score (new-score uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-score u100) ERR-INVALID-AMOUNT)
    (var-set grid-resilience-score new-score)
    (ok true)
  )
)

(define-public (report-grid-event
  (event-type (string-ascii 50))
  (severity uint)
  (affected-nodes (list 10 uint)))
  (begin
    (asserts! (and (>= severity u1) (<= severity u5)) ERR-INVALID-AMOUNT)

    ;; Adjust grid resilience based on severity
    (let ((current-score (var-get grid-resilience-score)))
      (if (> severity u3)
        (var-set grid-resilience-score (max (- current-score (* severity u5)) u0))
        (var-set grid-resilience-score (min (+ current-score u2) u100))
      )
    )

    ;; If high severity, consider emergency mode
    (if (> severity u4)
      (var-set emergency-mode true)
      true
    )

    (ok true)
  )
)

;; ===============================================
;; Helper Functions
;; ===============================================

(define-private (update-energy-balance (owner principal) (energy-delta uint) (carbon-delta uint))
  (let ((balance (default-to
        { available-energy: u0, reserved-energy: u0, carbon-credits: u0, reputation-score: u100 }
        (map-get? energy-balances { owner: owner })
      )))
    (map-set energy-balances
      { owner: owner }
      (merge balance {
        available-energy: (+ (get available-energy balance) energy-delta),
        carbon-credits: (+ (get carbon-credits balance) carbon-delta)
      })
    )
  )
)

(define-private (update-carbon-credits (owner principal) (credits uint))
  (let ((balance (default-to
        { available-energy: u0, reserved-energy: u0, carbon-credits: u0, reputation-score: u100 }
        (map-get? energy-balances { owner: owner })
      )))
    (map-set energy-balances
      { owner: owner }
      (merge balance { carbon-credits: (+ (get carbon-credits balance) credits) })
    )
  )
)

(define-private (update-reputation (user principal) (points uint))
  (let ((balance (default-to
        { available-energy: u0, reserved-energy: u0, carbon-credits: u0, reputation-score: u100 }
        (map-get? energy-balances { owner: user })
      )))
    (map-set energy-balances
      { owner: user }
      (merge balance {
        reputation-score: (min (+ (get reputation-score balance) points) u100)
      })
    )
  )
)

(define-private (max (a uint) (b uint))
  (if (> a b) a b)
)

(define-private (min (a uint) (b uint))
  (if (< a b) a b)
)

;; ===============================================
;; Read-Only Functions
;; ===============================================

(define-read-only (get-node-info (node-id uint))
  (map-get? energy-nodes { node-id: node-id })
)

(define-read-only (get-energy-balance (owner principal))
  (map-get? energy-balances { owner: owner })
)

(define-read-only (get-trading-order (order-id uint))
  (map-get? trading-orders { order-id: order-id })
)

(define-read-only (get-emergency-allocation (recipient principal))
  (map-get? emergency-allocations { recipient: recipient })
)

(define-read-only (get-grid-status)
  {
    emergency-mode: (var-get emergency-mode),
    resilience-score: (var-get grid-resilience-score),
    total-carbon-credits: (var-get total-carbon-credits),
    current-energy-price: (var-get energy-price-per-kwh)
  }
)

(define-read-only (calculate-carbon-neutrality (period-days uint))
  (let (
    (total-credits (var-get total-carbon-credits))
    (estimated-consumption (* period-days u24000)) ;; Rough estimate: 24kWh/day average
  )
    {
      total-credits: total-credits,
      estimated-consumption: estimated-consumption,
      carbon-neutral: (>= total-credits estimated-consumption),
      surplus-credits: (if (>= total-credits estimated-consumption)
                        (- total-credits estimated-consumption)
                        u0)
    }
  )
)

;; ===============================================
;; CONTRACT 2: Governance & Community Management
;; ===============================================

;; File: contracts/community-governance.clar

;; Constants
(define-constant GOVERNANCE-TOKEN-NAME "Community Energy Token")
(define-constant GOVERNANCE-TOKEN-SYMBOL "CET")
(define-constant GOVERNANCE-TOKEN-DECIMALS u6)
(define-constant PROPOSAL-VOTING-PERIOD u1008) ;; ~1 week in blocks
(define-constant MIN-PROPOSAL-THRESHOLD u1000000) ;; 1 CET minimum to propose

(define-constant ERR-GOV-NOT-AUTHORIZED (err u200))
(define-constant ERR-GOV-INSUFFICIENT-TOKENS (err u201))
(define-constant ERR-GOV-PROPOSAL-NOT-FOUND (err u202))
(define-constant ERR-GOV-VOTING-ENDED (err u203))
(define-constant ERR-GOV-ALREADY-VOTED (err u204))
(define-constant ERR-GOV-PROPOSAL-ACTIVE (err u205))

;; Data Variables
(define-data-var total-supply uint u0)
(define-data-var next-proposal-id uint u1)

;; Fungible Token Definition
(define-fungible-token community-energy-token)

;; Data Maps
(define-map token-balances principal uint)

(define-map governance-proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposal-type: (string-ascii 20), ;; "parameter", "upgrade", "allocation"
    target-contract: (optional principal),
    target-function: (optional (string-ascii 50)),
    parameters: (optional (string-ascii 200)),
    votes-for: uint,
    votes-against: uint,
    created-at: uint,
    ends-at: uint,
    executed: bool,
    quorum-required: uint
  }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  { vote: bool, voting-power: uint }
)

(define-map community-members
  { member: principal }
  {
    joined-at: uint,
    energy-contributed: uint,
    governance-weight: uint,
    active: bool
  }
)

;; ===============================================
;; Token Management
;; ===============================================

(define-public (mint-tokens (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-GOV-NOT-AUTHORIZED)
    (try! (ft-mint? community-energy-token amount recipient))
    (var-set total-supply (+ (var-get total-supply) amount))
    (ok true)
  )
)

(define-public (transfer-tokens (amount uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-GOV-NOT-AUTHORIZED)
    (try! (ft-transfer? community-energy-token amount sender recipient))
    (ok true)
  )
)

;; ===============================================
;; Governance Functions
;; ===============================================

(define-public (submit-proposal
  (title (string-ascii 100))
  (description (string-ascii 500))
  (proposal-type (string-ascii 20))
  (target-contract (optional principal))
  (target-function (optional (string-ascii 50)))
  (parameters (optional (string-ascii 200))))
  (let (
    (proposal-id (var-get next-proposal-id))
    (proposer-balance (ft-get-balance community-energy-token tx-sender))
  )
    (asserts! (>= proposer-balance MIN-PROPOSAL-THRESHOLD) ERR-GOV-INSUFFICIENT-TOKENS)

    (map-set governance-proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        title: title,
        description: description,
        proposal-type: proposal-type,
        target-contract: target-contract,
        target-function: target-function,
        parameters: parameters,
        votes-for: u0,
        votes-against: u0,
        created-at: stacks-block-height,
        ends-at: (+ stacks-block-height PROPOSAL-VOTING-PERIOD),
        executed: false,
        quorum-required: (/ (var-get total-supply) u3) ;; 33% quorum
      }
    )

    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let (
    (proposal (unwrap! (map-get? governance-proposals { proposal-id: proposal-id }) ERR-GOV-PROPOSAL-NOT-FOUND))
    (voter-balance (ft-get-balance community-energy-token tx-sender))
    (existing-vote (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender }))
  )
    (asserts! (> voter-balance u0) ERR-GOV-INSUFFICIENT-TOKENS)
    (asserts! (< stacks-block-height (get ends-at proposal)) ERR-GOV-VOTING-ENDED)
    (asserts! (is-none existing-vote) ERR-GOV-ALREADY-VOTED)

    ;; Record vote
    (map-set proposal-votes
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote-for, voting-power: voter-balance }
    )

    ;; Update proposal vote counts
    (map-set governance-proposals
      { proposal-id: proposal-id }
      (if vote-for
        (merge proposal { votes-for: (+ (get votes-for proposal) voter-balance) })
        (merge proposal { votes-against: (+ (get votes-against proposal) voter-balance) })
      )
    )

    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? governance-proposals { proposal-id: proposal-id }) ERR-GOV-PROPOSAL-NOT-FOUND)))
    (asserts! (>= stacks-block-height (get ends-at proposal)) ERR-GOV-PROPOSAL-ACTIVE)
    (asserts! (not (get executed proposal)) ERR-GOV-PROPOSAL-ACTIVE)

    ;; Check if proposal passed
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR-GOV-NOT-AUTHORIZED)
    (asserts! (>= (+ (get votes-for proposal) (get votes-against proposal))
                  (get quorum-required proposal)) ERR-GOV-INSUFFICIENT-TOKENS)

    ;; Mark as executed
    (map-set governance-proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true })
    )

    ;; Execute proposal logic based on type
    (begin
      (if (is-eq (get proposal-type proposal) "allocation")
        (print {
          event: "proposal-executed",
          proposal-id: proposal-id,
          proposal-type: "allocation",
          proposer: (get proposer proposal)
        })
        (print {
          event: "proposal-executed",
          proposal-id: proposal-id,
          proposal-type: (get proposal-type proposal),
          proposer: (get proposer proposal)
        })
      )
      (ok true)
    )
  )
)

;; ===============================================
;; Community Management
;; ===============================================

(define-public (join-community)
  (begin
    (map-set community-members
      { member: tx-sender }
      {
        joined-at: stacks-block-height,
        energy-contributed: u0,
        governance-weight: u1,
        active: true
      }
    )

    ;; Mint initial governance tokens
    (try! (ft-mint? community-energy-token u100000000 tx-sender)) ;; 100 CET
    (var-set total-supply (+ (var-get total-supply) u100000000))

    (ok true)
  )
)

(define-public (update-energy-contribution (member principal) (contribution uint))
  (let ((member-data (unwrap! (map-get? community-members { member: member }) ERR-GOV-NOT-AUTHORIZED)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-GOV-NOT-AUTHORIZED)

    (map-set community-members
      { member: member }
      (merge member-data { energy-contributed: (+ (get energy-contributed member-data) contribution) })
    )

    ;; Mint additional tokens based on contribution
    (let ((bonus-tokens (* contribution u10))) ;; 10 tokens per kWh contributed
      (try! (ft-mint? community-energy-token bonus-tokens member))
      (var-set total-supply (+ (var-get total-supply) bonus-tokens))
    )

    (ok true)
  )
)

;; ===============================================
;; Helper Functions
;; ===============================================

(define-private (execute-community-allocation (allocation-amount uint) (recipient principal))
  (begin
    ;; This would integrate with the energy trading contract
    ;; to perform actual energy allocations in a real implementation
    (print {
      event: "community-allocation",
      recipient: recipient,
      amount: allocation-amount
    })
    (ok true)
  )
)

;; ===============================================
;; Read-Only Functions
;; ===============================================

(define-read-only (get-proposal (proposal-id uint))
  (map-get? governance-proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-community-member (member principal))
  (map-get? community-members { member: member })
)

(define-read-only (get-token-balance (owner principal))
  (ft-get-balance community-energy-token owner)
)

(define-read-only (get-total-supply)
  (var-get total-supply)
)

(define-read-only (get-governance-stats)
  {
    total-proposals: (- (var-get next-proposal-id) u1),
    total-supply: (var-get total-supply),
    min-proposal-threshold: MIN-PROPOSAL-THRESHOLD,
    voting-period-blocks: PROPOSAL-VOTING-PERIOD
  }
)

;; Token Information Functions
(define-read-only (get-token-name)
  (ok GOVERNANCE-TOKEN-NAME)
)

(define-read-only (get-token-symbol)
  (ok GOVERNANCE-TOKEN-SYMBOL)
)

(define-read-only (get-token-decimals)
  (ok GOVERNANCE-TOKEN-DECIMALS)
)

(define-read-only (get-balance (owner principal))
  (ok (ft-get-balance community-energy-token owner))
)

(define-read-only (get-total-token-supply)
  (ok (ft-get-supply community-energy-token))
)

(define-read-only (get-token-uri)
  (ok none)
)
