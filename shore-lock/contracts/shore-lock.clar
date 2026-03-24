;; Shore-Lock Maritime Strategy Blockchain Game
;; A revolutionary maritime strategy game with NFT vessels, territory control,
;; and play-to-earn TIDE token economy

;; ===================================
;; CONSTANTS AND ERROR CODES
;; ===================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-VESSEL-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-TERRITORY (err u103))
(define-constant ERR-VESSEL-IN-TRANSIT (err u104))
(define-constant ERR-TOURNAMENT-NOT-ACTIVE (err u105))
(define-constant ERR-ALREADY-REGISTERED (err u106))
(define-constant ERR-INVALID-COORDINATES (err u107))
(define-constant ERR-COOLDOWN-ACTIVE (err u108))
(define-constant ERR-MINT-FAILED (err u109))

;; Game parameters
(define-constant TIDE-DECIMALS u6)
(define-constant DELIVERY-BASE-REWARD u1000000) ;; 1 TIDE
(define-constant TERRITORY-REWARD-PER-BLOCK u100)
(define-constant VESSEL-REGISTRATION-FEE u5000000) ;; 5 STX

;; ===================================
;; DATA VARIABLES
;; ===================================

(define-data-var total-tide-supply uint u0)
(define-data-var total-vessels uint u0)
(define-data-var tournament-season uint u0)
(define-data-var tournament-active bool false)
(define-data-var treasury-balance uint u0)

;; ===================================
;; DATA MAPS
;; ===================================

;; Vessel registry - NFT vessels with attributes
(define-map vessels
    { vessel-id: uint }
    {
        owner: principal,
        vessel-type: (string-ascii 20),
        experience-points: uint,
        speed: uint,
        cargo-capacity: uint,
        current-x: int,
        current-y: int,
        in-transit: bool,
        last-action-block: uint
    }
)

;; TIDE token balances
(define-map tide-balances
    { address: principal }
    { balance: uint }
)

;; Territory control - grid-based ocean territories
(define-map territories
    { x: int, y: int }
    {
        controller: (optional principal),
        control-strength: uint,
        last-contested-block: uint,
        resource-type: (string-ascii 20)
    }
)

;; Active deliveries
(define-map deliveries
    { delivery-id: uint }
    {
        vessel-id: uint,
        from-x: int,
        from-y: int,
        to-x: int,
        to-y: int,
        cargo-value: uint,
        started-block: uint,
        completed: bool
    }
)

;; Tournament participants
(define-map tournament-entries
    { season: uint, participant: principal }
    {
        vessel-id: uint,
        score: uint,
        registered-block: uint
    }
)

;; Harbor Council voting power
(define-map voting-power
    { address: principal }
    { power: uint }
)

;; Delivery counter
(define-data-var next-delivery-id uint u1)

;; ===================================
;; PRIVATE FUNCTIONS
;; ===================================

;; Calculate distance between two coordinates (Manhattan distance)
(define-private (calculate-distance (x1 int) (y1 int) (x2 int) (y2 int))
    (+ 
        (if (>= x2 x1) (- x2 x1) (- x1 x2))
        (if (>= y2 y1) (- y2 y1) (- y1 y2))
    )
)

;; Calculate delivery reward based on distance and cargo
(define-private (calculate-delivery-reward (distance uint) (cargo-value uint))
    (+ DELIVERY-BASE-REWARD (* distance cargo-value))
)

;; Validate coordinates are within game bounds
(define-private (valid-coordinates (x int) (y int))
    (and 
        (>= x -1000)
        (<= x 1000)
        (>= y -1000)
        (<= y 1000)
    )
)

;; ===================================
;; PUBLIC FUNCTIONS - VESSEL MANAGEMENT
;; ===================================

;; Register a new vessel NFT
(define-public (register-vessel (vessel-type (string-ascii 20)) (speed uint) (cargo-capacity uint))
    (let
        (
            (vessel-id (+ (var-get total-vessels) u1))
            (sender tx-sender)
        )
        ;; Check registration fee payment
        (try! (stx-transfer? VESSEL-REGISTRATION-FEE sender CONTRACT-OWNER))
        
        ;; Create vessel
        (map-set vessels
            { vessel-id: vessel-id }
            {
                owner: sender,
                vessel-type: vessel-type,
                experience-points: u0,
                speed: speed,
                cargo-capacity: cargo-capacity,
                current-x: 0,
                current-y: 0,
                in-transit: false,
                last-action-block: block-height
            }
        )
        
        ;; Update total vessels
        (var-set total-vessels vessel-id)
        
        ;; Initialize voting power
        (map-set voting-power
            { address: sender }
            { power: u1 }
        )
        
        (ok vessel-id)
    )
)

;; Move vessel to new coordinates
(define-public (move-vessel (vessel-id uint) (new-x int) (new-y int))
    (let
        (
            (vessel (unwrap! (map-get? vessels { vessel-id: vessel-id }) ERR-VESSEL-NOT-FOUND))
            (sender tx-sender)
        )
        ;; Verify ownership
        (asserts! (is-eq (get owner vessel) sender) ERR-NOT-AUTHORIZED)
        
        ;; Verify not in transit
        (asserts! (not (get in-transit vessel)) ERR-VESSEL-IN-TRANSIT)
        
        ;; Verify coordinates
        (asserts! (valid-coordinates new-x new-y) ERR-INVALID-COORDINATES)
        
        ;; Calculate cooldown based on distance and speed
        (let
            (
                (distance (to-uint (calculate-distance (get current-x vessel) (get current-y vessel) new-x new-y)))
                (cooldown-blocks (/ distance (get speed vessel)))
            )
            (asserts! (>= block-height (+ (get last-action-block vessel) cooldown-blocks)) ERR-COOLDOWN-ACTIVE)
        )
        
        ;; Update vessel position
        (map-set vessels
            { vessel-id: vessel-id }
            (merge vessel {
                current-x: new-x,
                current-y: new-y,
                last-action-block: block-height
            })
        )
        
        (ok true)
    )
)

;; ===================================
;; PUBLIC FUNCTIONS - TIDE TOKEN ECONOMY
;; ===================================

;; Start a delivery mission
(define-public (start-delivery (vessel-id uint) (to-x int) (to-y int) (cargo-value uint))
    (let
        (
            (vessel (unwrap! (map-get? vessels { vessel-id: vessel-id }) ERR-VESSEL-NOT-FOUND))
            (sender tx-sender)
            (delivery-id (var-get next-delivery-id))
        )
        ;; Verify ownership
        (asserts! (is-eq (get owner vessel) sender) ERR-NOT-AUTHORIZED)
        
        ;; Verify not already in transit
        (asserts! (not (get in-transit vessel)) ERR-VESSEL-IN-TRANSIT)
        
        ;; Verify destination coordinates
        (asserts! (valid-coordinates to-x to-y) ERR-INVALID-COORDINATES)
        
        ;; Create delivery record
        (map-set deliveries
            { delivery-id: delivery-id }
            {
                vessel-id: vessel-id,
                from-x: (get current-x vessel),
                from-y: (get current-y vessel),
                to-x: to-x,
                to-y: to-y,
                cargo-value: cargo-value,
                started-block: block-height,
                completed: false
            }
        )
        
        ;; Mark vessel in transit
        (map-set vessels
            { vessel-id: vessel-id }
            (merge vessel { in-transit: true })
        )
        
        ;; Increment delivery counter
        (var-set next-delivery-id (+ delivery-id u1))
        
        (ok delivery-id)
    )
)

;; Complete delivery and earn TIDE tokens
(define-public (complete-delivery (delivery-id uint))
    (let
        (
            (delivery (unwrap! (map-get? deliveries { delivery-id: delivery-id }) ERR-VESSEL-NOT-FOUND))
            (vessel-id (get vessel-id delivery))
            (vessel (unwrap! (map-get? vessels { vessel-id: vessel-id }) ERR-VESSEL-NOT-FOUND))
            (sender tx-sender)
        )
        ;; Verify ownership
        (asserts! (is-eq (get owner vessel) sender) ERR-NOT-AUTHORIZED)
        
        ;; Verify delivery not already completed
        (asserts! (not (get completed delivery)) ERR-NOT-AUTHORIZED)
        
        ;; Verify vessel reached destination
        (asserts! (and 
            (is-eq (get current-x vessel) (get to-x delivery))
            (is-eq (get current-y vessel) (get to-y delivery))
        ) ERR-INVALID-COORDINATES)
        
        ;; Calculate reward
        (let
            (
                (distance (to-uint (calculate-distance 
                    (get from-x delivery) (get from-y delivery)
                    (get to-x delivery) (get to-y delivery)
                )))
                (reward (calculate-delivery-reward distance (get cargo-value delivery)))
                (new-exp (+ (get experience-points vessel) distance))
            )
            ;; Award TIDE tokens
            (try! (mint-tide sender reward))
            
            ;; Update vessel state
            (map-set vessels
                { vessel-id: vessel-id }
                (merge vessel {
                    in-transit: false,
                    experience-points: new-exp,
                    last-action-block: block-height
                })
            )
            
            ;; Update voting power based on experience
            (map-set voting-power
                { address: sender }
                { power: (/ new-exp u100) }
            )
            
            ;; Mark delivery completed
            (map-set deliveries
                { delivery-id: delivery-id }
                (merge delivery { completed: true })
            )
            
            (ok reward)
        )
    )
)

;; Mint TIDE tokens (internal use only, requires contract authorization)
(define-public (mint-tide (recipient principal) (amount uint))
    (begin
        ;; Only allow minting from contract itself (called internally)
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq contract-caller tx-sender)) ERR-NOT-AUTHORIZED)
        
        (let
            (
                (current-balance (default-to { balance: u0 } (map-get? tide-balances { address: recipient })))
                (new-balance (+ (get balance current-balance) amount))
            )
            (map-set tide-balances
                { address: recipient }
                { balance: new-balance }
            )
            (var-set total-tide-supply (+ (var-get total-tide-supply) amount))
            (ok true)
        )
    )
)

;; Transfer TIDE tokens
(define-public (transfer-tide (amount uint) (recipient principal))
    (let
        (
            (sender tx-sender)
            (sender-balance (default-to { balance: u0 } (map-get? tide-balances { address: sender })))
        )
        ;; Check sufficient balance
        (asserts! (>= (get balance sender-balance) amount) ERR-INSUFFICIENT-BALANCE)
        
        ;; Update balances
        (map-set tide-balances
            { address: sender }
            { balance: (- (get balance sender-balance) amount) }
        )
        
        (let
            (
                (recipient-balance (default-to { balance: u0 } (map-get? tide-balances { address: recipient })))
            )
            (map-set tide-balances
                { address: recipient }
                { balance: (+ (get balance recipient-balance) amount) }
            )
        )
        
        (ok true)
    )
)

;; ===================================
;; PUBLIC FUNCTIONS - TERRITORY CONTROL
;; ===================================

;; Claim or contest territory
(define-public (claim-territory (x int) (y int) (vessel-id uint))
    (let
        (
            (vessel (unwrap! (map-get? vessels { vessel-id: vessel-id }) ERR-VESSEL-NOT-FOUND))
            (sender tx-sender)
            (territory (default-to 
                { controller: none, control-strength: u0, last-contested-block: u0, resource-type: "empty" }
                (map-get? territories { x: x, y: y })
            ))
        )
        ;; Verify ownership
        (asserts! (is-eq (get owner vessel) sender) ERR-NOT-AUTHORIZED)
        
        ;; Verify vessel is at territory location
        (asserts! (and (is-eq (get current-x vessel) x) (is-eq (get current-y vessel) y)) ERR-INVALID-COORDINATES)
        
        ;; Update territory control
        (map-set territories
            { x: x, y: y }
            {
                controller: (some sender),
                control-strength: (+ (get control-strength territory) (get experience-points vessel)),
                last-contested-block: block-height,
                resource-type: (get resource-type territory)
            }
        )
        
        (ok true)
    )
)

;; Collect territory rewards
(define-public (collect-territory-rewards (x int) (y int))
    (let
        (
            (territory (unwrap! (map-get? territories { x: x, y: y }) ERR-INVALID-TERRITORY))
            (sender tx-sender)
            (controller (unwrap! (get controller territory) ERR-NOT-AUTHORIZED))
        )
        ;; Verify controller
        (asserts! (is-eq controller sender) ERR-NOT-AUTHORIZED)
        
        ;; Calculate rewards based on blocks held
        (let
            (
                (blocks-held (- block-height (get last-contested-block territory)))
                (reward (* blocks-held TERRITORY-REWARD-PER-BLOCK))
            )
            ;; Award TIDE tokens
            (try! (mint-tide sender reward))
            
            ;; Update last contested block
            (map-set territories
                { x: x, y: y }
                (merge territory { last-contested-block: block-height })
            )
            
            (ok reward)
        )
    )
)

;; ===================================
;; PUBLIC FUNCTIONS - TOURNAMENTS
;; ===================================

;; Start a new tournament season (admin only)
(define-public (start-tournament-season)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set tournament-season (+ (var-get tournament-season) u1))
        (var-set tournament-active true)
        (ok (var-get tournament-season))
    )
)

;; Register for tournament
(define-public (register-tournament (vessel-id uint))
    (let
        (
            (vessel (unwrap! (map-get? vessels { vessel-id: vessel-id }) ERR-VESSEL-NOT-FOUND))
            (sender tx-sender)
            (season (var-get tournament-season))
        )
        ;; Verify tournament is active
        (asserts! (var-get tournament-active) ERR-TOURNAMENT-NOT-ACTIVE)
        
        ;; Verify ownership
        (asserts! (is-eq (get owner vessel) sender) ERR-NOT-AUTHORIZED)
        
        ;; Check not already registered
        (asserts! (is-none (map-get? tournament-entries { season: season, participant: sender })) ERR-ALREADY-REGISTERED)
        
        ;; Register entry
        (map-set tournament-entries
            { season: season, participant: sender }
            {
                vessel-id: vessel-id,
                score: u0,
                registered-block: block-height
            }
        )
        
        (ok true)
    )
)

;; Update tournament score
(define-public (update-tournament-score (participant principal) (additional-score uint))
    (let
        (
            (season (var-get tournament-season))
            (entry (unwrap! (map-get? tournament-entries { season: season, participant: participant }) ERR-VESSEL-NOT-FOUND))
        )
        ;; Only contract can update scores (through automated game logic)
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        ;; Update score
        (map-set tournament-entries
            { season: season, participant: participant }
            (merge entry { score: (+ (get score entry) additional-score) })
        )
        
        (ok true)
    )
)

;; End tournament season (admin only)
(define-public (end-tournament-season)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set tournament-active false)
        (ok true)
    )
)

;; ===================================
;; READ-ONLY FUNCTIONS
;; ===================================

;; Get vessel details
(define-read-only (get-vessel-info (vessel-id uint))
    (map-get? vessels { vessel-id: vessel-id })
)

;; Get TIDE balance
(define-read-only (get-tide-balance (address principal))
    (default-to { balance: u0 } (map-get? tide-balances { address: address }))
)

;; Get territory info
(define-read-only (get-territory-info (x int) (y int))
    (map-get? territories { x: x, y: y })
)

;; Get delivery info
(define-read-only (get-delivery-info (delivery-id uint))
    (map-get? deliveries { delivery-id: delivery-id })
)

;; Get voting power
(define-read-only (get-voting-power (address principal))
    (default-to { power: u0 } (map-get? voting-power { address: address }))
)

;; Get tournament entry
(define-read-only (get-tournament-entry (season uint) (participant principal))
    (map-get? tournament-entries { season: season, participant: participant })
)

;; Get total TIDE supply
(define-read-only (get-total-tide-supply)
    (ok (var-get total-tide-supply))
)

;; Get total vessels
(define-read-only (get-total-vessels)
    (ok (var-get total-vessels))
)

;; Get current tournament season
(define-read-only (get-tournament-season)
    (ok (var-get tournament-season))
)

;; Check if tournament is active
(define-read-only (is-tournament-active)
    (ok (var-get tournament-active))
)
