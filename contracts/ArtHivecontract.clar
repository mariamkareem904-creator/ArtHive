;; title: ArtHive Collaborative Art Generation DAO
;; version: 1.0.0
;; summary: A decentralized platform for collaborative art creation with layer-based contributions and proportional ownership
;; description: This contract enables artists to collaboratively create digital art through sequential layer modifications,
;;              with community voting on layer inclusion and automatic revenue distribution based on contributions.

;; traits
(define-trait nft-trait
  (
    (get-last-token-id () (response uint uint))
    (get-token-uri (uint) (response (optional (string-ascii 256)) uint))
    (get-owner (uint) (response (optional principal) uint))
    (transfer (uint principal principal) (response bool uint))
  )
)

;; token definitions
(define-non-fungible-token arthive-art uint)

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-input (err u103))
(define-constant err-voting-closed (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-insufficient-votes (err u106))
(define-constant err-layer-exists (err u107))
(define-constant err-artwork-finalized (err u108))
(define-constant err-invalid-percentage (err u109))

(define-constant voting-period u144) ;; ~24 hours in blocks
(define-constant min-votes-required u10)
(define-constant approval-threshold u60) ;; 60% approval needed

;; data vars
(define-data-var next-artwork-id uint u1)
(define-data-var next-layer-id uint u1)
(define-data-var platform-fee-percentage uint u250) ;; 2.5%

;; data maps
(define-map artworks
  { artwork-id: uint }
  {
    creator: principal,
    title: (string-ascii 256),
    description: (string-ascii 1024),
    base-uri: (string-ascii 512),
    total-layers: uint,
    is-finalized: bool,
    created-at: uint,
    total-revenue: uint
  }
)

(define-map layers
  { artwork-id: uint, layer-id: uint }
  {
    artist: principal,
    layer-uri: (string-ascii 512),
    description: (string-ascii 256),
    is-approved: bool,
    votes-for: uint,
    votes-against: uint,
    voting-deadline: uint,
    created-at: uint,
    contribution-weight: uint
  }
)

(define-map layer-votes
  { artwork-id: uint, layer-id: uint, voter: principal }
  { vote: bool, voted-at: uint }
)

(define-map artwork-contributors
  { artwork-id: uint, contributor: principal }
  { total-contribution-weight: uint, layers-count: uint }
)

(define-map forks
  { parent-artwork-id: uint, fork-id: uint }
  {
    creator: principal,
    fork-artwork-id: uint,
    forked-at-layer: uint,
    created-at: uint
  }
)

(define-map revenue-claims
  { artwork-id: uint, contributor: principal }
  { claimed-amount: uint, last-claim-block: uint }
)


;; public functions

;; Create a new artwork with base canvas
(define-public (create-artwork (title (string-ascii 256)) (description (string-ascii 1024)) (base-uri (string-ascii 512)))
  (let 
    (
      (artwork-id (var-get next-artwork-id))
    )
    (try! (nft-mint? arthive-art artwork-id tx-sender))
    (map-set artworks
      { artwork-id: artwork-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        base-uri: base-uri,
        total-layers: u0,
        is-finalized: false,
        created-at: stacks-block-height,
        total-revenue: u0
      }
    )
    (map-set artwork-contributors
      { artwork-id: artwork-id, contributor: tx-sender }
      { total-contribution-weight: u100, layers-count: u1 }
    )
    (var-set next-artwork-id (+ artwork-id u1))
    (ok artwork-id)
  )
)

;; Submit a new layer for an artwork
(define-public (submit-layer (artwork-id uint) (layer-uri (string-ascii 512)) (description (string-ascii 256)))
  (let
    (
      (artwork (unwrap! (map-get? artworks { artwork-id: artwork-id }) err-not-found))
      (layer-id (var-get next-layer-id))
      (voting-deadline (+ stacks-block-height voting-period))
    )
    (asserts! (not (get is-finalized artwork)) err-artwork-finalized)
    (asserts! (is-none (map-get? layers { artwork-id: artwork-id, layer-id: layer-id })) err-layer-exists)
    
    (map-set layers
      { artwork-id: artwork-id, layer-id: layer-id }
      {
        artist: tx-sender,
        layer-uri: layer-uri,
        description: description,
        is-approved: false,
        votes-for: u0,
        votes-against: u0,
        voting-deadline: voting-deadline,
        created-at: stacks-block-height,
        contribution-weight: u0
      }
    )
    (var-set next-layer-id (+ layer-id u1))
    (ok layer-id)
  )
)

;; Vote on a layer
(define-public (vote-on-layer (artwork-id uint) (layer-id uint) (vote bool))
  (let
    (
      (layer (unwrap! (map-get? layers { artwork-id: artwork-id, layer-id: layer-id }) err-not-found))
      (existing-vote (map-get? layer-votes { artwork-id: artwork-id, layer-id: layer-id, voter: tx-sender }))
    )
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (< stacks-block-height (get voting-deadline layer)) err-voting-closed)
    
    (map-set layer-votes
      { artwork-id: artwork-id, layer-id: layer-id, voter: tx-sender }
      { vote: vote, voted-at: stacks-block-height }
    )
    
    (if vote
      (map-set layers
        { artwork-id: artwork-id, layer-id: layer-id }
        (merge layer { votes-for: (+ (get votes-for layer) u1) })
      )
      (map-set layers
        { artwork-id: artwork-id, layer-id: layer-id }
        (merge layer { votes-against: (+ (get votes-against layer) u1) })
      )
    )
    (ok true)
  )
)

;; Finalize layer voting and approve/reject
(define-public (finalize-layer-vote (artwork-id uint) (layer-id uint))
  (let
    (
      (layer (unwrap! (map-get? layers { artwork-id: artwork-id, layer-id: layer-id }) err-not-found))
      (artwork (unwrap! (map-get? artworks { artwork-id: artwork-id }) err-not-found))
      (total-votes (+ (get votes-for layer) (get votes-against layer)))
      (approval-percentage (if (> total-votes u0) (/ (* (get votes-for layer) u100) total-votes) u0))
    )
    (asserts! (>= stacks-block-height (get voting-deadline layer)) err-voting-closed)
    (asserts! (>= total-votes min-votes-required) err-insufficient-votes)
    
    (if (>= approval-percentage approval-threshold)
      (begin
        ;; Approve layer
        (let
          (
            (contribution-weight (calculate-contribution-weight artwork-id layer-id))
            (existing-contributor (map-get? artwork-contributors { artwork-id: artwork-id, contributor: (get artist layer) }))
          )
          (map-set layers
            { artwork-id: artwork-id, layer-id: layer-id }
            (merge layer { is-approved: true, contribution-weight: contribution-weight })
          )
          (map-set artworks
            { artwork-id: artwork-id }
            (merge artwork { total-layers: (+ (get total-layers artwork) u1) })
          )
          ;; Update contributor data
          (match existing-contributor
            contributor-data
            (map-set artwork-contributors
              { artwork-id: artwork-id, contributor: (get artist layer) }
              {
                total-contribution-weight: (+ (get total-contribution-weight contributor-data) contribution-weight),
                layers-count: (+ (get layers-count contributor-data) u1)
              }
            )
            (map-set artwork-contributors
              { artwork-id: artwork-id, contributor: (get artist layer) }
              { total-contribution-weight: contribution-weight, layers-count: u1 }
            )
          )
          (ok true)
        )
      )
      ;; Reject layer - no changes needed as layer remains unapproved
      (ok false)
    )
  )
)

;; Fork an artwork at a specific layer
(define-public (fork-artwork (parent-artwork-id uint) (fork-at-layer uint) (new-title (string-ascii 256)) (new-description (string-ascii 1024)))
  (let
    (
      (parent-artwork (unwrap! (map-get? artworks { artwork-id: parent-artwork-id }) err-not-found))
      (new-artwork-id (var-get next-artwork-id))
      (fork-id (+ (get total-layers parent-artwork) u1))
    )
    (asserts! (<= fork-at-layer (get total-layers parent-artwork)) err-invalid-input)
    
    ;; Create new artwork
    (try! (nft-mint? arthive-art new-artwork-id tx-sender))
    (map-set artworks
      { artwork-id: new-artwork-id }
      {
        creator: tx-sender,
        title: new-title,
        description: new-description,
        base-uri: (get base-uri parent-artwork),
        total-layers: fork-at-layer,
        is-finalized: false,
        created-at: stacks-block-height,
        total-revenue: u0
      }
    )
    
    ;; Record fork relationship
    (map-set forks
      { parent-artwork-id: parent-artwork-id, fork-id: fork-id }
      {
        creator: tx-sender,
        fork-artwork-id: new-artwork-id,
        forked-at-layer: fork-at-layer,
        created-at: stacks-block-height
      }
    )
    
    (var-set next-artwork-id (+ new-artwork-id u1))
    (ok new-artwork-id)
  )
)

;; Finalize artwork (prevents further layers)
(define-public (finalize-artwork (artwork-id uint))
  (let
    (
      (artwork (unwrap! (map-get? artworks { artwork-id: artwork-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator artwork)) err-unauthorized)
    (map-set artworks
      { artwork-id: artwork-id }
      (merge artwork { is-finalized: true })
    )
    (ok true)
  )
)

;; Add revenue to artwork (called by external revenue sources)
(define-public (add-revenue (artwork-id uint) (amount uint))
  (let
    (
      (artwork (unwrap! (map-get? artworks { artwork-id: artwork-id }) err-not-found))
    )
    (map-set artworks
      { artwork-id: artwork-id }
      (merge artwork { total-revenue: (+ (get total-revenue artwork) amount) })
    )
    (ok true)
  )
)

;; Claim revenue share
(define-public (claim-revenue (artwork-id uint))
  (let
    (
      (artwork (unwrap! (map-get? artworks { artwork-id: artwork-id }) err-not-found))
      (contributor-data (unwrap! (map-get? artwork-contributors { artwork-id: artwork-id, contributor: tx-sender }) err-unauthorized))
      (claim-data (default-to { claimed-amount: u0, last-claim-block: u0 } 
                    (map-get? revenue-claims { artwork-id: artwork-id, contributor: tx-sender })))
      (total-contribution-weight (calculate-total-contribution-weight artwork-id))
      (contributor-share (if (> total-contribution-weight u0)
                          (/ (* (get total-revenue artwork) (get total-contribution-weight contributor-data)) total-contribution-weight)
                          u0))
      (claimable-amount (- contributor-share (get claimed-amount claim-data)))
      (platform-fee (/ (* claimable-amount (var-get platform-fee-percentage)) u10000))
      (net-amount (- claimable-amount platform-fee))
    )
    (asserts! (> claimable-amount u0) err-invalid-input)
    
    ;; Update claim record
    (map-set revenue-claims
      { artwork-id: artwork-id, contributor: tx-sender }
      { claimed-amount: contributor-share, last-claim-block: stacks-block-height }
    )
    
    ;; Transfer tokens (this would integrate with STX or other token transfer)
    ;; For now, we just record the claim
    (ok net-amount)
  )
)


;; read only functions

;; Get artwork details
(define-read-only (get-artwork (artwork-id uint))
  (map-get? artworks { artwork-id: artwork-id })
)

;; Get layer details
(define-read-only (get-layer (artwork-id uint) (layer-id uint))
  (map-get? layers { artwork-id: artwork-id, layer-id: layer-id })
)

;; Get contributor data
(define-read-only (get-contributor (artwork-id uint) (contributor principal))
  (map-get? artwork-contributors { artwork-id: artwork-id, contributor: contributor })
)

;; Get vote details
(define-read-only (get-vote (artwork-id uint) (layer-id uint) (voter principal))
  (map-get? layer-votes { artwork-id: artwork-id, layer-id: layer-id, voter: voter })
)

;; Get fork details
(define-read-only (get-fork (parent-artwork-id uint) (fork-id uint))
  (map-get? forks { parent-artwork-id: parent-artwork-id, fork-id: fork-id })
)

;; Get current artwork ID
(define-read-only (get-next-artwork-id)
  (var-get next-artwork-id)
)

;; Get current layer ID
(define-read-only (get-next-layer-id)
  (var-get next-layer-id)
)

;; Calculate claimable revenue for a contributor
(define-read-only (get-claimable-revenue (artwork-id uint) (contributor principal))
  (match (map-get? artwork-contributors { artwork-id: artwork-id, contributor: contributor })
    contributor-data
    (let
      (
        (artwork (unwrap-panic (map-get? artworks { artwork-id: artwork-id })))
        (claim-data (default-to { claimed-amount: u0, last-claim-block: u0 } 
                      (map-get? revenue-claims { artwork-id: artwork-id, contributor: contributor })))
        (total-contribution-weight (calculate-total-contribution-weight artwork-id))
        (contributor-share (if (> total-contribution-weight u0)
                            (/ (* (get total-revenue artwork) (get total-contribution-weight contributor-data)) total-contribution-weight)
                            u0))
        (claimable-amount (- contributor-share (get claimed-amount claim-data)))
      )
      (ok claimable-amount)
    )
    (ok u0)
  )
)

;; Get NFT URI (implementing trait requirement)
(define-read-only (get-token-uri (token-id uint))
  (match (map-get? artworks { artwork-id: token-id })
    artwork-data (ok (some (get base-uri artwork-data)))
    (ok none)
  )
)

;; Get NFT owner (implementing trait requirement)
(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? arthive-art token-id))
)

;; Get last token ID (implementing trait requirement)
(define-read-only (get-last-token-id)
  (ok (- (var-get next-artwork-id) u1))
)

;; private functions

;; Calculate contribution weight for a layer based on various factors
(define-private (calculate-contribution-weight (artwork-id uint) (layer-id uint))
  (let
    (
      (layer (unwrap-panic (map-get? layers { artwork-id: artwork-id, layer-id: layer-id })))
      (artwork (unwrap-panic (map-get? artworks { artwork-id: artwork-id })))
      (total-votes (+ (get votes-for layer) (get votes-against layer)))
      (approval-percentage (if (> total-votes u0) (/ (* (get votes-for layer) u100) total-votes) u0))
      (base-weight u50)
      (vote-bonus (if (> approval-percentage u80) u25 
                   (if (> approval-percentage u60) u15 u0)))
      (layer-position-bonus (if (< (get total-layers artwork) u5) u15 u5))
    )
    (+ base-weight vote-bonus layer-position-bonus)
  )
)

;; Calculate total contribution weight for an artwork
(define-private (calculate-total-contribution-weight (artwork-id uint))
  ;; This is a simplified calculation - in practice, you'd iterate through all contributors
  ;; For now, we'll use a base calculation
  (let
    (
      (artwork (unwrap-panic (map-get? artworks { artwork-id: artwork-id })))
    )
    (* (+ (get total-layers artwork) u1) u50) ;; Simplified calculation
  )
)

;; Transfer NFT (implementing trait requirement)
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) err-unauthorized)
    (nft-transfer? arthive-art token-id sender recipient)
  )
)
