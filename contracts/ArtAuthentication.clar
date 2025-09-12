

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
(define-constant ERR-DISPUTE-EXISTS (err u109))
(define-constant ERR-DISPUTE-NOT-FOUND (err u110))
(define-constant ERR-DISPUTE-RESOLVED (err u111))
(define-constant ERR-NOT-ARBITRATOR (err u112))
(define-constant ERR-ALREADY-VOTED (err u113))
(define-constant ERR-INVALID-VOTE (err u114))
(define-constant ERR-DISPUTE-PERIOD-ENDED (err u115))
(define-constant ERR-INSUFFICIENT-ARBITRATOR-STAKE (err u116))

(define-constant dispute-period-blocks u144)
(define-constant arbitrator-min-stake u5000)
(define-constant required-arbitrator-votes u3)

(define-data-var total-disputes uint u0)
(define-data-var total-arbitrators uint u0)

(define-map Disputes
    { dispute-id: uint }
    {
        artwork-id: uint,
        disputed-verifier: principal,
        disputer: principal,
        reason: (string-ascii 256),
        status: (string-ascii 20),
        created-at: uint,
        resolved-at: (optional uint),
        resolution: (optional (string-ascii 20))
    }
)

(define-map DisputeVotes
    { dispute-id: uint, arbitrator: principal }
    {
        vote: (string-ascii 20),
        timestamp: uint
    }
)

(define-map DisputeVoteCounts
    { dispute-id: uint }
    {
        uphold-count: uint,
        overturn-count: uint,
        total-votes: uint
    }
)

(define-map Arbitrators
    { address: principal }
    {
        stake-amount: uint,
        disputes-resolved: uint,
        active: bool,
        reputation-score: uint
    }
)

(define-map ArbitratorReputation
    { arbitrator: principal }
    {
        correct-votes: uint,
        total-votes: uint,
        reputation-percentage: uint
    }
)

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


(define-public (register-arbitrator (stake-amount uint))
    (if (>= stake-amount arbitrator-min-stake)
        (begin
            (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
            (map-set Arbitrators
                { address: tx-sender }
                {
                    stake-amount: stake-amount,
                    disputes-resolved: u0,
                    active: true,
                    reputation-score: u100
                }
            )
            (var-set total-arbitrators (+ (var-get total-arbitrators) u1))
            (ok true)
        )
        ERR-INSUFFICIENT-ARBITRATOR-STAKE
    )
)

(define-public (file-dispute (artwork-id uint) (disputed-verifier principal) (reason (string-ascii 256)))
    (let (
        (artwork (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND))
        (certificate (unwrap! (map-get? ArtworkCertificates {artwork-id: artwork-id}) ERR-NOT-FOUND))
        (new-dispute-id (+ (var-get total-disputes) u1))
    )
        (asserts! (get verified artwork) ERR-NOT-FOUND)
        (asserts! (is-eq (get verifier certificate) disputed-verifier) ERR-NOT-AUTHORIZED)
        ;; (asserts! (is-none (get-active-dispute artwork-id)) ERR-DISPUTE-EXISTS)
        (asserts! (<= (- stacks-block-height (get timestamp certificate)) dispute-period-blocks) ERR-DISPUTE-PERIOD-ENDED)
        
        (map-set Disputes
            { dispute-id: new-dispute-id }
            {
                artwork-id: artwork-id,
                disputed-verifier: disputed-verifier,
                disputer: tx-sender,
                reason: reason,
                status: "pending",
                created-at: stacks-block-height,
                resolved-at: none,
                resolution: none
            }
        )
        
        (map-set DisputeVoteCounts
            { dispute-id: new-dispute-id }
            {
                uphold-count: u0,
                overturn-count: u0,
                total-votes: u0
            }
        )
        
        (var-set total-disputes new-dispute-id)
        (ok new-dispute-id)
    )
)

(define-public (vote-on-dispute (dispute-id uint) (vote (string-ascii 20)))
    (let (
        (dispute (unwrap! (map-get? Disputes {dispute-id: dispute-id}) ERR-DISPUTE-NOT-FOUND))
        (arbitrator (unwrap! (map-get? Arbitrators {address: tx-sender}) ERR-NOT-ARBITRATOR))
        (vote-counts (unwrap! (map-get? DisputeVoteCounts {dispute-id: dispute-id}) ERR-DISPUTE-NOT-FOUND))
    )
        (asserts! (get active arbitrator) ERR-NOT-ARBITRATOR)
        (asserts! (is-eq (get status dispute) "pending") ERR-DISPUTE-RESOLVED)
        (asserts! (is-none (map-get? DisputeVotes {dispute-id: dispute-id, arbitrator: tx-sender})) ERR-ALREADY-VOTED)
        (asserts! (or (is-eq vote "uphold") (is-eq vote "overturn")) ERR-INVALID-VOTE)
        
        (map-set DisputeVotes
            { dispute-id: dispute-id, arbitrator: tx-sender }
            {
                vote: vote,
                timestamp: stacks-block-height
            }
        )
        
        (let (
            (new-uphold-count (if (is-eq vote "uphold") (+ (get uphold-count vote-counts) u1) (get uphold-count vote-counts)))
            (new-overturn-count (if (is-eq vote "overturn") (+ (get overturn-count vote-counts) u1) (get overturn-count vote-counts)))
            (new-total-votes (+ (get total-votes vote-counts) u1))
        )
            (map-set DisputeVoteCounts
                { dispute-id: dispute-id }
                {
                    uphold-count: new-uphold-count,
                    overturn-count: new-overturn-count,
                    total-votes: new-total-votes
                }
            )
            ;; (if (>= new-total-votes required-arbitrator-votes)
            ;;     (try! (resolve-dispute-internal dispute-id new-uphold-count new-overturn-count))
            ;;     (ok true)
            ;; )
            (ok true)
        )
    )
)

(define-private (resolve-dispute-internal (dispute-id uint) (uphold-count uint) (overturn-count uint))
    (let (
        (dispute (unwrap! (map-get? Disputes {dispute-id: dispute-id}) ERR-DISPUTE-NOT-FOUND))
        (resolution (if (> uphold-count overturn-count) "uphold" "overturn"))
        (artwork-id (get artwork-id dispute))
        (disputed-verifier (get disputed-verifier dispute))
    )
        (map-set Disputes
            { dispute-id: dispute-id }
            {
                artwork-id: artwork-id,
                disputed-verifier: disputed-verifier,
                disputer: (get disputer dispute),
                reason: (get reason dispute),
                status: "resolved",
                created-at: (get created-at dispute),
                resolved-at: (some stacks-block-height),
                resolution: (some resolution)
            }
        )
        
        (if (is-eq resolution "overturn")
            (begin
                (try! (revert-verification-internal artwork-id))
                (try! (slash-verifier-stake disputed-verifier))
                (ok true)
            )
            (ok true)
        )
    )
)

(define-private (revert-verification-internal (artwork-id uint))
    (let ((artwork (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND)))
        (map-set Artworks
            { artwork-id: artwork-id }
            {
                artist: (get artist artwork),
                title: (get title artwork),
                creation-date: (get creation-date artwork),
                verified: false,
                verifier: none,
                royalty-percentage: (get royalty-percentage artwork),
                price: (get price artwork)
            }
        )
        (map-delete ArtworkCertificates { artwork-id: artwork-id })
        (ok true)
    )
)

(define-private (slash-verifier-stake (verifier principal))
    (let ((verifier-info (unwrap! (map-get? Verifiers {address: verifier}) ERR-NOT-FOUND)))
        (map-set Verifiers
            { address: verifier }
            {
                stake-amount: (/ (get stake-amount verifier-info) u2),
                verification-count: (get verification-count verifier-info),
                active: false
            }
        )
        (ok true)
    )
)

;; (define-private (get-active-dispute (artwork-id uint))
;;     (let ((disputes-list (filter-disputes-by-artwork artwork-id)))
;;         (option-fold find-pending-dispute none disputes-list)
;;     )
;; )

(define-private (filter-disputes-by-artwork (artwork-id uint))
    (list)
)

(define-private (find-pending-dispute (dispute-id uint) (acc (optional uint)))
    (if (is-some acc)
        acc
        (let ((dispute (map-get? Disputes {dispute-id: dispute-id})))
            (if (and (is-some dispute) (is-eq (get status (unwrap-panic dispute)) "pending"))
                (some dispute-id)
                none
            )
        )
    )
)

(define-read-only (get-dispute-details (dispute-id uint))
    (map-get? Disputes {dispute-id: dispute-id})
)

(define-read-only (get-dispute-votes (dispute-id uint))
    (map-get? DisputeVoteCounts {dispute-id: dispute-id})
)

(define-read-only (get-arbitrator-details (address principal))
    (map-get? Arbitrators {address: address})
)

(define-read-only (is-arbitrator (address principal))
    (match (map-get? Arbitrators {address: address})
        arbitrator-info (get active arbitrator-info)
        false
    )
)

(define-read-only (get-arbitrator-vote (dispute-id uint) (arbitrator principal))
    (map-get? DisputeVotes {dispute-id: dispute-id, arbitrator: arbitrator})
)

(define-read-only (can-file-dispute (artwork-id uint))
    (let ((certificate (map-get? ArtworkCertificates {artwork-id: artwork-id})))
        (if (is-some certificate)
            (let ((cert-data (unwrap-panic certificate)))
                (and
                    (<= (- stacks-block-height (get timestamp cert-data)) dispute-period-blocks)
                    ;; (is-none (get-active-dispute artwork-id))
                )
            )
            false
        )
    )
)

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



(define-map Categories 
    { category-id: uint }
    { name: (string-ascii 32) }
)

(define-map ArtworkCategories
    { artwork-id: uint }
    { category-id: uint }
)

(define-data-var total-categories uint u0)

(define-public (create-category (name (string-ascii 32)))
    (let ((new-id (+ (var-get total-categories) u1)))
        (map-set Categories
            { category-id: new-id }
            { name: name }
        )
        (var-set total-categories new-id)
        (ok new-id)
    )
)

(define-public (set-artwork-category (artwork-id uint) (category-id uint))
    (let ((artwork (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND)))
        (asserts! (is-eq (get artist artwork) tx-sender) ERR-NOT-AUTHORIZED)
        (map-set ArtworkCategories
            { artwork-id: artwork-id }
            { category-id: category-id }
        )
        (ok true)
    )
)


(define-map ArtworkHistory
    { artwork-id: uint, action-id: uint }
    {
        action-type: (string-ascii 20),
        actor: principal,
        timestamp: uint,
        details: (string-ascii 256)
    }
)

(define-map ArtworkActionCounter 
    { artwork-id: uint }
    { count: uint }
)

(define-public (record-artwork-action (artwork-id uint) (action-type (string-ascii 20)) (details (string-ascii 256)))
    (let (
        (counter (default-to {count: u0} (map-get? ArtworkActionCounter {artwork-id: artwork-id})))
        (new-action-id (+ (get count counter) u1))
    )
        (map-set ArtworkHistory
            { artwork-id: artwork-id, action-id: new-action-id }
            {
                action-type: action-type,
                actor: tx-sender,
                timestamp: stacks-block-height,
                details: details
            }
        )
        (map-set ArtworkActionCounter {artwork-id: artwork-id} {count: new-action-id})
        (ok true)
    )
)


(define-map VerifierRatings
    { verifier: principal }
    {
        total-ratings: uint,
        rating-sum: uint,
        average-rating: uint
    }
)

(define-public (rate-verifier (verifier principal) (rating uint))
    (let (
        (current-ratings (default-to {total-ratings: u0, rating-sum: u0, average-rating: u0} 
            (map-get? VerifierRatings {verifier: verifier})))
        (new-total (+ (get total-ratings current-ratings) u1))
        (new-sum (+ (get rating-sum current-ratings) rating))
    )
        (asserts! (<= rating u5) (err u106))
        (map-set VerifierRatings
            {verifier: verifier}
            {
                total-ratings: new-total,
                rating-sum: new-sum,
                average-rating: (/ new-sum new-total)
            }
        )
        (ok true)
    )
)


(define-map Collections
    { collection-id: uint }
    {
        name: (string-ascii 64),
        creator: principal,
        description: (string-ascii 256)
    }
)

(define-map CollectionArtworks
    { collection-id: uint, artwork-id: uint }
    { included: bool }
)

(define-data-var total-collections uint u0)

(define-public (create-collection (name (string-ascii 64)) (description (string-ascii 256)))
    (let ((new-id (+ (var-get total-collections) u1)))
        (map-set Collections
            { collection-id: new-id }
            {
                name: name,
                creator: tx-sender,
                description: description
            }
        )
        (var-set total-collections new-id)
        (ok new-id)
    )
)

(define-public (add-to-collection (collection-id uint) (artwork-id uint))
    (let ((collection (unwrap! (map-get? Collections {collection-id: collection-id}) ERR-NOT-FOUND)))
        (asserts! (is-eq (get creator collection) tx-sender) ERR-NOT-AUTHORIZED)
        (map-set CollectionArtworks
            { collection-id: collection-id, artwork-id: artwork-id }
            { included: true }
        )
        (ok true)
    )
)


(define-map Comments
    { artwork-id: uint, comment-id: uint }
    {
        author: principal,
        content: (string-ascii 256),
        timestamp: uint
    }
)

(define-map ArtworkCommentCounter
    { artwork-id: uint }
    { count: uint }
)

(define-public (add-comment (artwork-id uint) (content (string-ascii 256)))
    (let (
        (counter (default-to {count: u0} (map-get? ArtworkCommentCounter {artwork-id: artwork-id})))
        (new-comment-id (+ (get count counter) u1))
    )
        (map-set Comments
            { artwork-id: artwork-id, comment-id: new-comment-id }
            {
                author: tx-sender,
                content: content,
                timestamp: stacks-block-height
            }
        )
        (map-set ArtworkCommentCounter {artwork-id: artwork-id} {count: new-comment-id})
        (ok true)
    )
)


(define-map ArtworkTags
    { artwork-id: uint, tag: (string-ascii 32) }
    { active: bool }
)

(define-public (add-artwork-tag (artwork-id uint) (tag (string-ascii 32)))
    (let ((artwork (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND)))
        (asserts! (is-eq (get artist artwork) tx-sender) ERR-NOT-AUTHORIZED)
        (map-set ArtworkTags
            { artwork-id: artwork-id, tag: tag }
            { active: true }
        )
        (ok true)
    )
)



(define-map ArtworkBids
    { artwork-id: uint, bidder: principal }
    {
        bid-amount: uint,
        timestamp: uint
    }
)

(define-map HighestBids
    { artwork-id: uint }
    {
        bidder: principal,
        amount: uint
    }
)

(define-public (place-bid (artwork-id uint) (bid-amount uint))
    (let (
        (artwork (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND))
        (current-highest (default-to {bidder: tx-sender, amount: u0} 
            (map-get? HighestBids {artwork-id: artwork-id})))
    )
        (asserts! (> bid-amount (get amount current-highest)) (err u107))
        (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
        
        (map-set ArtworkBids
            {artwork-id: artwork-id, bidder: tx-sender}
            {bid-amount: bid-amount, timestamp: stacks-block-height}
        )
        
        (map-set HighestBids
            {artwork-id: artwork-id}
            {bidder: tx-sender, amount: bid-amount}
        )
        (ok true)
    )
)

(define-public (accept-bid (artwork-id uint))
    (let (
        (artwork (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND))
        (highest-bid (unwrap! (map-get? HighestBids {artwork-id: artwork-id}) ERR-NOT-FOUND))
    )
        (asserts! (is-eq (get artist artwork) tx-sender) ERR-NOT-AUTHORIZED)
        (try! (as-contract (stx-transfer? (get amount highest-bid) tx-sender (get artist artwork))))
        (ok true)
    )
)


(define-map Exhibitions
    { exhibition-id: uint }
    {
        name: (string-ascii 64),
        curator: principal,
        start-block: uint,
        end-block: uint,
        active: bool
    }
)

(define-map ExhibitionArtworks
    { exhibition-id: uint, artwork-id: uint }
    {
        special-price: uint,
        featured: bool
    }
)

(define-data-var total-exhibitions uint u0)

(define-public (create-exhibition (name (string-ascii 64)) (duration uint))
    (let ((new-id (+ (var-get total-exhibitions) u1)))
        (map-set Exhibitions
            { exhibition-id: new-id }
            {
                name: name,
                curator: tx-sender,
                start-block: stacks-block-height,
                end-block: (+ stacks-block-height duration),
                active: true
            }
        )
        (var-set total-exhibitions new-id)
        (ok new-id)
    )
)

(define-public (add-artwork-to-exhibition (exhibition-id uint) (artwork-id uint) (special-price uint))
    (let (
        (exhibition (unwrap! (map-get? Exhibitions {exhibition-id: exhibition-id}) ERR-NOT-FOUND))
        (artwork (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND))
    )
        (asserts! (is-eq (get curator exhibition) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get active exhibition) (err u108))
        
        (map-set ExhibitionArtworks
            { exhibition-id: exhibition-id, artwork-id: artwork-id }
            { special-price: special-price, featured: true }
        )
        (ok true)
    )
)

(define-public (remove-artwork-from-exhibition (exhibition-id uint) (artwork-id uint))
    (let (
        (exhibition (unwrap! (map-get? Exhibitions {exhibition-id: exhibition-id}) ERR-NOT-FOUND))
        (artwork (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND))
    )
        (asserts! (is-eq (get curator exhibition) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get active exhibition) (err u108))
        
        (map-delete ExhibitionArtworks
            { exhibition-id: exhibition-id, artwork-id: artwork-id }
        )
        (ok true)
    )
)
(define-public (end-exhibition (exhibition-id uint))
    (let (
        (exhibition (unwrap! (map-get? Exhibitions {exhibition-id: exhibition-id}) ERR-NOT-FOUND))
    )
        (asserts! (is-eq (get curator exhibition) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get active exhibition) (err u108))
        
        (map-set Exhibitions
            { exhibition-id: exhibition-id }
            {
                name: (get name exhibition),
                curator: tx-sender,
                start-block: (get start-block exhibition),
                end-block: stacks-block-height,
                active: false
            }
        )
        (ok true)
    )
)

(define-map ArtworkOwnership
    { artwork-id: uint }
    { 
        current-owner: principal,
        previous-owner: (optional principal),
        transfer-count: uint
    }
)

(define-map MarketplaceListings
    { artwork-id: uint }
    {
        seller: principal,
        price: uint,
        listed: bool,
        listing-timestamp: uint
    }
)

(define-map OwnershipHistory
    { artwork-id: uint, transfer-id: uint }
    {
        from-owner: principal,
        to-owner: principal,
        price: uint,
        timestamp: uint
    }
)

(define-map OwnershipTransferCounter
    { artwork-id: uint }
    { count: uint }
)

(define-constant ERR-NOT-OWNER (err u105))
(define-constant ERR-NOT-LISTED (err u106))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u107))
(define-constant ERR-ALREADY-LISTED (err u108))

(define-public (initialize-ownership (artwork-id uint))
    (let ((artwork (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND)))
        (asserts! (is-eq (get artist artwork) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? ArtworkOwnership {artwork-id: artwork-id})) ERR-ALREADY-VERIFIED)
        
        (map-set ArtworkOwnership
            { artwork-id: artwork-id }
            {
                current-owner: tx-sender,
                previous-owner: none,
                transfer-count: u0
            }
        )
        (ok true)
    )
)

(define-public (list-artwork-for-sale (artwork-id uint) (price uint))
    (let ((ownership (unwrap! (map-get? ArtworkOwnership {artwork-id: artwork-id}) ERR-NOT-FOUND)))
        (asserts! (is-eq (get current-owner ownership) tx-sender) ERR-NOT-OWNER)
        (asserts! (is-none (map-get? MarketplaceListings {artwork-id: artwork-id})) ERR-ALREADY-LISTED)
        
        (map-set MarketplaceListings
            { artwork-id: artwork-id }
            {
                seller: tx-sender,
                price: price,
                listed: true,
                listing-timestamp: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (remove-listing (artwork-id uint))
    (let ((listing (unwrap! (map-get? MarketplaceListings {artwork-id: artwork-id}) ERR-NOT-LISTED)))
        (asserts! (is-eq (get seller listing) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get listed listing) ERR-NOT-LISTED)
        
        (map-delete MarketplaceListings { artwork-id: artwork-id })
        (ok true)
    )
)

(define-public (purchase-artwork (artwork-id uint))
    (let (
        (listing (unwrap! (map-get? MarketplaceListings {artwork-id: artwork-id}) ERR-NOT-LISTED))
        (artwork (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND))
        (ownership (unwrap! (map-get? ArtworkOwnership {artwork-id: artwork-id}) ERR-NOT-FOUND))
        (sale-price (get price listing))
        (royalty-amount (/ (* sale-price (get royalty-percentage artwork)) u100))
        (seller-amount (- sale-price royalty-amount))
    )
        (asserts! (get listed listing) ERR-NOT-LISTED)
        (asserts! (not (is-eq tx-sender (get seller listing))) ERR-NOT-AUTHORIZED)
        
        (try! (stx-transfer? royalty-amount tx-sender (get artist artwork)))
        (try! (stx-transfer? seller-amount tx-sender (get seller listing)))
        
        (try! (transfer-ownership-internal artwork-id (get current-owner ownership) tx-sender sale-price))
        
        (map-delete MarketplaceListings { artwork-id: artwork-id })
        (ok true)
    )
)

(define-public (transfer-artwork (artwork-id uint) (new-owner principal))
    (let ((ownership (unwrap! (map-get? ArtworkOwnership {artwork-id: artwork-id}) ERR-NOT-FOUND)))
        (asserts! (is-eq (get current-owner ownership) tx-sender) ERR-NOT-OWNER)
        
        (try! (transfer-ownership-internal artwork-id tx-sender new-owner u0))
        (ok true)
    )
)

(define-private (transfer-ownership-internal (artwork-id uint) (from-owner principal) (to-owner principal) (price uint))
    (let (
        (ownership (unwrap! (map-get? ArtworkOwnership {artwork-id: artwork-id}) ERR-NOT-FOUND))
        (counter (default-to {count: u0} (map-get? OwnershipTransferCounter {artwork-id: artwork-id})))
        (new-transfer-id (+ (get count counter) u1))
    )
        (map-set ArtworkOwnership
            { artwork-id: artwork-id }
            {
                current-owner: to-owner,
                previous-owner: (some from-owner),
                transfer-count: (+ (get transfer-count ownership) u1)
            }
        )
        
        (map-set OwnershipHistory
            { artwork-id: artwork-id, transfer-id: new-transfer-id }
            {
                from-owner: from-owner,
                to-owner: to-owner,
                price: price,
                timestamp: stacks-block-height
            }
        )
        
        (map-set OwnershipTransferCounter {artwork-id: artwork-id} {count: new-transfer-id})
        (ok true)
    )
)

(define-read-only (get-artwork-owner (artwork-id uint))
    (map-get? ArtworkOwnership {artwork-id: artwork-id})
)

(define-read-only (get-marketplace-listing (artwork-id uint))
    (map-get? MarketplaceListings {artwork-id: artwork-id})
)

(define-read-only (get-ownership-history (artwork-id uint) (transfer-id uint))
    (map-get? OwnershipHistory {artwork-id: artwork-id, transfer-id: transfer-id})
)

(define-read-only (is-artwork-owner (artwork-id uint) (address principal))
    (match (map-get? ArtworkOwnership {artwork-id: artwork-id})
        ownership (is-eq (get current-owner ownership) address)
        false
    )
)

(define-read-only (calculate-purchase-breakdown (artwork-id uint))
    (let (
        (listing (unwrap! (map-get? MarketplaceListings {artwork-id: artwork-id}) ERR-NOT-LISTED))
        (artwork (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND))
        (sale-price (get price listing))
        (royalty-amount (/ (* sale-price (get royalty-percentage artwork)) u100))
        (seller-amount (- sale-price royalty-amount))
    )
        (ok {
            total-price: sale-price,
            royalty-to-artist: royalty-amount,
            payment-to-seller: seller-amount,
            artist: (get artist artwork),
            seller: (get seller listing)
        })
    )
)

;; ============================================================================
;; COMPREHENSIVE ART PROVENANCE CHAIN SYSTEM
;; ============================================================================

;; Provenance event types and data structures
(define-constant PROVENANCE-CREATION "creation")
(define-constant PROVENANCE-AUTHENTICATION "authentication") 
(define-constant PROVENANCE-CONSERVATION "conservation")
(define-constant PROVENANCE-EXHIBITION "exhibition")
(define-constant PROVENANCE-CONDITION-ASSESSMENT "condition")
(define-constant PROVENANCE-OWNERSHIP-CHANGE "ownership")
(define-constant PROVENANCE-LOCATION-CHANGE "location")
(define-constant PROVENANCE-INSURANCE-UPDATE "insurance")

;; Error constants for provenance system
(define-constant ERR-INVALID-PROVENANCE-TYPE (err u117))
(define-constant ERR-PROVENANCE-NOT-FOUND (err u118))
(define-constant ERR-INVALID-CONDITION-SCORE (err u119))
(define-constant ERR-CONSERVATION-UNAUTHORIZED (err u120))

;; Data variables for provenance tracking
(define-data-var total-provenance-events uint u0)
(define-data-var total-conservators uint u0)

;; Main provenance chain - comprehensive event tracking
(define-map ProvenanceChain
    { artwork-id: uint, event-id: uint }
    {
        event-type: (string-ascii 32),
        event-performer: principal,
        timestamp: uint,
        block-height: uint,
        description: (string-ascii 512),
        location: (optional (string-ascii 256)),
        documentation-hash: (optional (string-ascii 64)),
        verified: bool,
        verifier: (optional principal)
    }
)

;; Event counter per artwork for proper sequencing
(define-map ArtworkProvenanceCounter
    { artwork-id: uint }
    { event-count: uint }
)

;; Conservation records with detailed tracking
(define-map ConservationRecords
    { artwork-id: uint, conservation-id: uint }
    {
        conservator: principal,
        conservation-type: (string-ascii 64),
        condition-before: uint,
        condition-after: uint,
        materials-used: (string-ascii 256),
        techniques-applied: (string-ascii 256),
        cost: uint,
        duration-days: uint,
        timestamp: uint,
        documentation-hash: (string-ascii 64)
    }
)

;; Conservation counter per artwork
(define-map ArtworkConservationCounter
    { artwork-id: uint }
    { conservation-count: uint }
)

;; Certified conservators registry
(define-map CertifiedConservators
    { conservator: principal }
    {
        certification-level: uint,
        specializations: (string-ascii 256),
        active: bool,
        conservation-count: uint,
        average-rating: uint,
        certification-body: (string-ascii 128)
    }
)

;; Exhibition provenance linking
(define-map ExhibitionProvenance
    { artwork-id: uint, exhibition-id: uint }
    {
        participation-type: (string-ascii 32),
        display-duration: uint,
        special-handling: bool,
        insurance-value: uint,
        condition-report: (string-ascii 256),
        timestamp: uint
    }
)

;; Physical condition assessments over time
(define-map ConditionAssessments
    { artwork-id: uint, assessment-id: uint }
    {
        assessor: principal,
        overall-condition: uint,
        structural-integrity: uint,
        surface-condition: uint,
        color-stability: uint,
        environmental-damage: uint,
        assessment-method: (string-ascii 128),
        recommendations: (string-ascii 512),
        next-assessment-due: uint,
        timestamp: uint
    }
)

;; Assessment counter per artwork
(define-map ArtworkAssessmentCounter
    { artwork-id: uint }
    { assessment-count: uint }
)

;; Location and custody tracking
(define-map LocationHistory
    { artwork-id: uint, location-id: uint }
    {
        location-type: (string-ascii 32),
        institution-name: (string-ascii 128),
        physical-address: (string-ascii 256),
        custody-type: (string-ascii 32),
        environmental-conditions: (string-ascii 128),
        security-level: uint,
        insurance-coverage: uint,
        moved-from: (optional (string-ascii 128)),
        timestamp: uint
    }
)

;; Location counter per artwork
(define-map ArtworkLocationCounter
    { artwork-id: uint }
    { location-count: uint }
)

;; Insurance and valuation history
(define-map InsuranceHistory
    { artwork-id: uint, insurance-id: uint }
    {
        insurer: (string-ascii 128),
        policy-number: (string-ascii 64),
        insured-value: uint,
        premium-amount: uint,
        coverage-type: (string-ascii 64),
        policy-start: uint,
        policy-end: uint,
        appraisal-date: uint,
        appraiser: (string-ascii 128),
        timestamp: uint
    }
)

;; Insurance counter per artwork
(define-map ArtworkInsuranceCounter
    { artwork-id: uint }
    { insurance-count: uint }
)

;; Register certified conservator
(define-public (register-conservator 
    (certification-level uint) 
    (specializations (string-ascii 256))
    (certification-body (string-ascii 128)))
    (begin
        (asserts! (<= certification-level u5) ERR-INVALID-CONDITION-SCORE)
        (map-set CertifiedConservators
            { conservator: tx-sender }
            {
                certification-level: certification-level,
                specializations: specializations,
                active: true,
                conservation-count: u0,
                average-rating: u0,
                certification-body: certification-body
            }
        )
        (var-set total-conservators (+ (var-get total-conservators) u1))
        (ok true)
    )
)

;; Record conservation work with comprehensive details
(define-public (record-conservation
    (artwork-id uint)
    (conservation-type (string-ascii 64))
    (condition-before uint)
    (condition-after uint)
    (materials-used (string-ascii 256))
    (techniques-applied (string-ascii 256))
    (cost uint)
    (duration-days uint)
    (documentation-hash (string-ascii 64)))
    (let (
        (artwork (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND))
        (conservator (unwrap! (map-get? CertifiedConservators {conservator: tx-sender}) ERR-CONSERVATION-UNAUTHORIZED))
        (counter (default-to {conservation-count: u0} (map-get? ArtworkConservationCounter {artwork-id: artwork-id})))
        (new-conservation-id (+ (get conservation-count counter) u1))
    )
        (asserts! (get active conservator) ERR-CONSERVATION-UNAUTHORIZED)
        (asserts! (and (<= condition-before u10) (<= condition-after u10)) ERR-INVALID-CONDITION-SCORE)
        
        (map-set ConservationRecords
            { artwork-id: artwork-id, conservation-id: new-conservation-id }
            {
                conservator: tx-sender,
                conservation-type: conservation-type,
                condition-before: condition-before,
                condition-after: condition-after,
                materials-used: materials-used,
                techniques-applied: techniques-applied,
                cost: cost,
                duration-days: duration-days,
                timestamp: stacks-block-height,
                documentation-hash: documentation-hash
            }
        )
        
        (map-set ArtworkConservationCounter {artwork-id: artwork-id} {conservation-count: new-conservation-id})
        
        ;; Record in main provenance chain
        (unwrap! (record-provenance-event 
            artwork-id 
            PROVENANCE-CONSERVATION 
            (concat "Conservation: " conservation-type)
            (some documentation-hash)
            none) ERR-INVALID-PROVENANCE-TYPE)
        
        (ok new-conservation-id)
    )
)

;; Record comprehensive condition assessment
(define-public (record-condition-assessment
    (artwork-id uint)
    (overall-condition uint)
    (structural-integrity uint)
    (surface-condition uint)
    (color-stability uint)
    (environmental-damage uint)
    (assessment-method (string-ascii 128))
    (recommendations (string-ascii 512))
    (next-assessment-due uint))
    (let (
        (artwork (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND))
        (counter (default-to {assessment-count: u0} (map-get? ArtworkAssessmentCounter {artwork-id: artwork-id})))
        (new-assessment-id (+ (get assessment-count counter) u1))
    )
        (asserts! (and 
            (<= overall-condition u10)
            (<= structural-integrity u10)
            (<= surface-condition u10)
            (<= color-stability u10)
            (<= environmental-damage u10)) ERR-INVALID-CONDITION-SCORE)
        
        (map-set ConditionAssessments
            { artwork-id: artwork-id, assessment-id: new-assessment-id }
            {
                assessor: tx-sender,
                overall-condition: overall-condition,
                structural-integrity: structural-integrity,
                surface-condition: surface-condition,
                color-stability: color-stability,
                environmental-damage: environmental-damage,
                assessment-method: assessment-method,
                recommendations: recommendations,
                next-assessment-due: next-assessment-due,
                timestamp: stacks-block-height
            }
        )
        
        (map-set ArtworkAssessmentCounter {artwork-id: artwork-id} {assessment-count: new-assessment-id})
        
        ;; Record in main provenance chain
        (unwrap! (record-provenance-event 
            artwork-id 
            PROVENANCE-CONDITION-ASSESSMENT 
            (concat "Condition assessment - Overall: " (uint-to-ascii overall-condition))
            none
            none) ERR-INVALID-PROVENANCE-TYPE)
        
        (ok new-assessment-id)
    )
)

;; Record location change with detailed custody information
(define-public (record-location-change
    (artwork-id uint)
    (location-type (string-ascii 32))
    (institution-name (string-ascii 128))
    (physical-address (string-ascii 256))
    (custody-type (string-ascii 32))
    (environmental-conditions (string-ascii 128))
    (security-level uint)
    (insurance-coverage uint)
    (moved-from (optional (string-ascii 128))))
    (let (
        (artwork (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND))
        (counter (default-to {location-count: u0} (map-get? ArtworkLocationCounter {artwork-id: artwork-id})))
        (new-location-id (+ (get location-count counter) u1))
    )
        (asserts! (<= security-level u10) ERR-INVALID-CONDITION-SCORE)
        
        (map-set LocationHistory
            { artwork-id: artwork-id, location-id: new-location-id }
            {
                location-type: location-type,
                institution-name: institution-name,
                physical-address: physical-address,
                custody-type: custody-type,
                environmental-conditions: environmental-conditions,
                security-level: security-level,
                insurance-coverage: insurance-coverage,
                moved-from: moved-from,
                timestamp: stacks-block-height
            }
        )
        
        (map-set ArtworkLocationCounter {artwork-id: artwork-id} {location-count: new-location-id})
        
        ;; Record in main provenance chain
        (unwrap! (record-provenance-event 
            artwork-id 
            PROVENANCE-LOCATION-CHANGE 
            (concat "Moved to: " institution-name)
            none
            (some physical-address)) ERR-INVALID-PROVENANCE-TYPE)
        
        (ok new-location-id)
    )
)

;; Record insurance and valuation updates
(define-public (record-insurance-update
    (artwork-id uint)
    (insurer (string-ascii 128))
    (policy-number (string-ascii 64))
    (insured-value uint)
    (premium-amount uint)
    (coverage-type (string-ascii 64))
    (policy-start uint)
    (policy-end uint)
    (appraisal-date uint)
    (appraiser (string-ascii 128)))
    (let (
        (artwork (unwrap! (map-get? Artworks {artwork-id: artwork-id}) ERR-NOT-FOUND))
        (counter (default-to {insurance-count: u0} (map-get? ArtworkInsuranceCounter {artwork-id: artwork-id})))
        (new-insurance-id (+ (get insurance-count counter) u1))
    )
        (map-set InsuranceHistory
            { artwork-id: artwork-id, insurance-id: new-insurance-id }
            {
                insurer: insurer,
                policy-number: policy-number,
                insured-value: insured-value,
                premium-amount: premium-amount,
                coverage-type: coverage-type,
                policy-start: policy-start,
                policy-end: policy-end,
                appraisal-date: appraisal-date,
                appraiser: appraiser,
                timestamp: stacks-block-height
            }
        )
        
        (map-set ArtworkInsuranceCounter {artwork-id: artwork-id} {insurance-count: new-insurance-id})
        
        ;; Record in main provenance chain
        (unwrap! (record-provenance-event 
            artwork-id 
            PROVENANCE-INSURANCE-UPDATE 
            (concat "Insurance updated - Value: " (uint-to-ascii insured-value))
            none
            none) ERR-INVALID-PROVENANCE-TYPE)
        
        (ok new-insurance-id)
    )
)

;; Core provenance event recording function
(define-private (record-provenance-event
    (artwork-id uint)
    (event-type (string-ascii 32))
    (description (string-ascii 512))
    (documentation-hash (optional (string-ascii 64)))
    (location (optional (string-ascii 256))))
    (let (
        (counter (default-to {event-count: u0} (map-get? ArtworkProvenanceCounter {artwork-id: artwork-id})))
        (new-event-id (+ (get event-count counter) u1))
    )
        (map-set ProvenanceChain
            { artwork-id: artwork-id, event-id: new-event-id }
            {
                event-type: event-type,
                event-performer: tx-sender,
                timestamp: stacks-block-height,
                block-height: stacks-block-height,
                description: description,
                location: location,
                documentation-hash: documentation-hash,
                verified: false,
                verifier: none
            }
        )
        
        (map-set ArtworkProvenanceCounter {artwork-id: artwork-id} {event-count: new-event-id})
        (var-set total-provenance-events (+ (var-get total-provenance-events) u1))
        (ok new-event-id)
    )
)

;; Read-only functions for provenance queries

(define-read-only (get-provenance-event (artwork-id uint) (event-id uint))
    (map-get? ProvenanceChain {artwork-id: artwork-id, event-id: event-id})
)

(define-read-only (get-conservation-record (artwork-id uint) (conservation-id uint))
    (map-get? ConservationRecords {artwork-id: artwork-id, conservation-id: conservation-id})
)

(define-read-only (get-condition-assessment (artwork-id uint) (assessment-id uint))
    (map-get? ConditionAssessments {artwork-id: artwork-id, assessment-id: assessment-id})
)

(define-read-only (get-location-history (artwork-id uint) (location-id uint))
    (map-get? LocationHistory {artwork-id: artwork-id, location-id: location-id})
)

(define-read-only (get-insurance-record (artwork-id uint) (insurance-id uint))
    (map-get? InsuranceHistory {artwork-id: artwork-id, insurance-id: insurance-id})
)

(define-read-only (get-conservator-profile (conservator principal))
    (map-get? CertifiedConservators {conservator: conservator})
)

(define-read-only (get-artwork-provenance-count (artwork-id uint))
    (default-to {event-count: u0} (map-get? ArtworkProvenanceCounter {artwork-id: artwork-id}))
)

(define-read-only (get-artwork-conservation-count (artwork-id uint))
    (default-to {conservation-count: u0} (map-get? ArtworkConservationCounter {artwork-id: artwork-id}))
)

(define-read-only (get-latest-condition-score (artwork-id uint))
    (let ((assessment-count (get assessment-count (get-artwork-assessment-count artwork-id))))
        (if (> assessment-count u0)
            (map-get? ConditionAssessments {artwork-id: artwork-id, assessment-id: assessment-count})
            none
        )
    )
)

(define-read-only (get-current-location (artwork-id uint))
    (let ((location-count (get location-count (get-artwork-location-count artwork-id))))
        (if (> location-count u0)
            (map-get? LocationHistory {artwork-id: artwork-id, location-id: location-count})
            none
        )
    )
)

(define-read-only (get-artwork-assessment-count (artwork-id uint))
    (default-to {assessment-count: u0} (map-get? ArtworkAssessmentCounter {artwork-id: artwork-id}))
)

(define-read-only (get-artwork-location-count (artwork-id uint))
    (default-to {location-count: u0} (map-get? ArtworkLocationCounter {artwork-id: artwork-id}))
)

;; Helper function to convert uint to string (simplified)
(define-private (uint-to-ascii (value uint))
    (if (is-eq value u0) "0"
    (if (is-eq value u1) "1"
    (if (is-eq value u2) "2"
    (if (is-eq value u3) "3"
    (if (is-eq value u4) "4"
    (if (is-eq value u5) "5"
    (if (is-eq value u6) "6"
    (if (is-eq value u7) "7"
    (if (is-eq value u8) "8"
    (if (is-eq value u9) "9"
    "10"))))))))))
)


