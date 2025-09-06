;; Commercial Property Valuation Smart Contract
;; Handles commercial property assessment with income analysis and market comparisons

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_INVALID_PARAMS (err u400))
(define-constant ERR_ALREADY_EXISTS (err u409))

;; Data structures for property information
(define-map Properties
    { property-id: uint }
    {
        owner: principal,
        address: (string-ascii 256),
        property-type: (string-ascii 50),
        square-footage: uint,
        year-built: uint,
        annual-income: uint,
        operating-expenses: uint,
        market-value: uint,
        cap-rate: uint, ;; In basis points (e.g., 750 = 7.5%)
        last-updated: uint
    }
)

;; Track comparable sales
(define-map ComparableSales
    { comp-id: uint }
    {
        property-id: uint,
        sale-price: uint,
        price-per-sqft: uint,
        sale-date: uint,
        property-type: (string-ascii 50)
    }
)

;; Data variables
(define-data-var next-property-id uint u1)
(define-data-var next-comp-id uint u1)

;; Public functions

;; Register a new commercial property
(define-public (register-property 
    (owner principal)
    (address (string-ascii 256))
    (property-type (string-ascii 50))
    (square-footage uint)
    (year-built uint)
    (annual-income uint)
    (operating-expenses uint))
    (let
        (
            (property-id (var-get next-property-id))
            (net-income (- annual-income operating-expenses))
            (market-value (if (> net-income u0)
                            (/ (* net-income u10000) u750) ;; Using 7.5% cap rate as default
                            u0))
            (cap-rate (if (and (> market-value u0) (> net-income u0))
                        (/ (* net-income u10000) market-value)
                        u0))
        )
        (begin
            (asserts! (> square-footage u0) ERR_INVALID_PARAMS)
            (asserts! (> year-built u1800) ERR_INVALID_PARAMS)
            (asserts! (>= annual-income operating-expenses) ERR_INVALID_PARAMS)
            
            (map-set Properties
                { property-id: property-id }
                {
                    owner: owner,
                    address: address,
                    property-type: property-type,
                    square-footage: square-footage,
                    year-built: year-built,
                    annual-income: annual-income,
                    operating-expenses: operating-expenses,
                    market-value: market-value,
                    cap-rate: cap-rate,
                    last-updated: stacks-block-height
                }
            )
            
            (var-set next-property-id (+ property-id u1))
            (ok property-id)
        )
    )
)

;; Update property valuation
(define-public (update-valuation
    (property-id uint)
    (annual-income uint)
    (operating-expenses uint)
    (market-value uint))
    (let
        (
            (property-data (unwrap! (map-get? Properties { property-id: property-id }) ERR_NOT_FOUND))
            (net-income (- annual-income operating-expenses))
            (cap-rate (if (and (> market-value u0) (> net-income u0))
                        (/ (* net-income u10000) market-value)
                        u0))
        )
        (begin
            (asserts! (is-eq tx-sender (get owner property-data)) ERR_UNAUTHORIZED)
            (asserts! (>= annual-income operating-expenses) ERR_INVALID_PARAMS)
            
            (map-set Properties
                { property-id: property-id }
                (merge property-data {
                    annual-income: annual-income,
                    operating-expenses: operating-expenses,
                    market-value: market-value,
                    cap-rate: cap-rate,
                    last-updated: stacks-block-height
                })
            )
            (ok true)
        )
    )
)

;; Add comparable sale data
(define-public (add-comparable-sale
    (property-id uint)
    (sale-price uint)
    (sale-date uint)
    (sold-property-type (string-ascii 50)))
    (let
        (
            (comp-id (var-get next-comp-id))
            (property-data (unwrap! (map-get? Properties { property-id: property-id }) ERR_NOT_FOUND))
            (price-per-sqft (/ sale-price (get square-footage property-data)))
        )
        (begin
            (asserts! (is-eq tx-sender (get owner property-data)) ERR_UNAUTHORIZED)
            (asserts! (> sale-price u0) ERR_INVALID_PARAMS)
            
            (map-set ComparableSales
                { comp-id: comp-id }
                {
                    property-id: property-id,
                    sale-price: sale-price,
                    price-per-sqft: price-per-sqft,
                    sale-date: sale-date,
                    property-type: sold-property-type
                }
            )
            
            (var-set next-comp-id (+ comp-id u1))
            (ok comp-id)
        )
    )
)

;; Read-only functions

;; Get property details
(define-read-only (get-property (property-id uint))
    (map-get? Properties { property-id: property-id })
)

;; Get comparable sale details
(define-read-only (get-comparable-sale (comp-id uint))
    (map-get? ComparableSales { comp-id: comp-id })
)

;; Calculate NOI (Net Operating Income)
(define-read-only (calculate-noi (property-id uint))
    (match (map-get? Properties { property-id: property-id })
        property-data (ok (- (get annual-income property-data) (get operating-expenses property-data)))
        ERR_NOT_FOUND
    )
)

;; Calculate investment return (ROI)
(define-read-only (calculate-roi (property-id uint) (investment-amount uint))
    (match (map-get? Properties { property-id: property-id })
        property-data 
        (let
            (
                (noi (- (get annual-income property-data) (get operating-expenses property-data)))
                (roi (if (> investment-amount u0)
                       (/ (* noi u10000) investment-amount)
                       u0))
            )
            (ok roi)
        )
        ERR_NOT_FOUND
    )
)

;; Get property count
(define-read-only (get-property-count)
    (- (var-get next-property-id) u1)
)

;; Get comparable sales count
(define-read-only (get-comparable-count)
    (- (var-get next-comp-id) u1)
)
