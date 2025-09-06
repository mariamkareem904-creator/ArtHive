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
