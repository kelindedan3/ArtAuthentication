

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