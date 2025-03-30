

;; ArtAuthentication Smart Contract
;; A platform for digital art verification and authenticity tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant min-stake-amount u1000)
(define-constant verification-fee u100)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-STAKE (err u101))
(define-constant ERR-ALREADY-VERIFIED (err u102))
(define-constant ERR-NOT-FOUND (err u103))
(define-constant ERR-INSUFFICIENT-STAKE (err u104))

;; Data Variables
(define-data-var total-artworks uint u0)
(define-data-var total-verifiers uint u0)

;; Data Maps
(define-map Artworks
    { artwork-id: uint }
    {
        artist: principal,
        title: (string-ascii 64),
        creation-date: uint,
        verified: bool,
        verifier: (optional principal),
        royalty-percentage: uint,
        price: uint
    }
)

(define-map Verifiers
    { address: principal }
    {
        stake-amount: uint,
        verification-count: uint,
        active: bool
    }
)

(define-map ArtworkCertificates
    { artwork-id: uint }
    {
        certificate-hash: (string-ascii 64),
        timestamp: uint,
        verifier: principal
    }
)

;; Public Functions

;; Register new artwork
(define-public (register-artwork (title (string-ascii 64)) (royalty uint) (price uint))
    (let ((new-id (+ (var-get total-artworks) u1)))
        (map-set Artworks
            { artwork-id: new-id }
            {
                artist: tx-sender,
                title: title,
                creation-date: stacks-block-height,
                verified: false,
                verifier: none,
                royalty-percentage: royalty,
                price: price
            }
        )
        (var-set total-artworks new-id)
        (ok new-id)
    )
)

;; Stake tokens to become a verifier
(define-public (stake-for-verification (stake-amount uint))
    (if (>= stake-amount min-stake-amount)
        (begin
            (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
            (map-set Verifiers
                { address: tx-sender }
                {
                    stake-amount: stake-amount,
                    verification-count: u0,
                    active: true
                }
            )
            (var-set total-verifiers (+ (var-get total-verifiers) u1))
            (ok true)
        )
        ERR-INSUFFICIENT-STAKE
    )
)

;; Verify artwork
(define-public (verify-artwork (artwork-id uint) (certificate-hash (string-ascii 64)))
    (let (
        (verifier-info (unwrap! (map-get? Verifiers {address: tx-sender}) ERR-NOT-AUTHORIZED))
        (artwork-info (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND))
    )
        (asserts! (get active verifier-info) ERR-NOT-AUTHORIZED)
        (asserts! (not (get verified artwork-info)) ERR-ALREADY-VERIFIED)
        
        (map-set Artworks
            { artwork-id: artwork-id }
            (merge-artwork-info artwork-info {
                verified: true,
                verifier: (some tx-sender)
            })
        )
        
        (map-set ArtworkCertificates
            { artwork-id: artwork-id }
            {
                certificate-hash: certificate-hash,
                timestamp: stacks-block-height,
                verifier: tx-sender
            }
        )
        (ok true)
    )
)

;; Read-only functions

(define-read-only (get-artwork-details (artwork-id uint))
    (map-get? Artworks {artwork-id: artwork-id})
)

(define-read-only (get-verifier-details (address principal))
    (map-get? Verifiers {address: address})
)

(define-read-only (get-artwork-certificate (artwork-id uint))
    (map-get? ArtworkCertificates {artwork-id: artwork-id})
)

(define-read-only (is-verifier (address principal))
    (match (map-get? Verifiers {address: address})
        verifier-info (get active verifier-info)
        false
    )
)

;; Private functions

(define-private (merge-artwork-info (a {
        artist: principal,
        title: (string-ascii 64),
        creation-date: uint,
        verified: bool,
        verifier: (optional principal),
        royalty-percentage: uint,
        price: uint
    }) (b {
        verified: bool,
        verifier: (optional principal)
    }))
    {
        artist: (get artist a),
        title: (get title a),
        creation-date: (get creation-date a),
        verified: (get verified b),
        verifier: (get verifier b),
        royalty-percentage: (get royalty-percentage a),
        price: (get price a)
    }
)
