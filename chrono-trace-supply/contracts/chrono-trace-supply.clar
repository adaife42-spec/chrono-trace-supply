;; ChronoTrace Supply Chain Tracking Contract
;; A simplified temporal provenance tracking system

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-input (err u103))

;; Define data variables
(define-data-var next-product-id uint u1)
(define-data-var next-event-id uint u1)

;; Define maps for product tracking
(define-map products
    { product-id: uint }
    {
        manufacturer: principal,
        product-type: (string-ascii 50),
        batch-number: (string-ascii 30),
        manufacturing-date: uint,
        quality-score: uint,
        current-status: (string-ascii 20),
        current-location: (string-ascii 100),
        is-active: bool
    }
)

;; Define map for temporal events
(define-map temporal-events
    { event-id: uint }
    {
        product-id: uint,
        event-type: (string-ascii 30),
        timestamp: uint,
        location: (string-ascii 100),
        sensor-data: (string-ascii 200),
        quality-metrics: uint,
        recorded-by: principal,
        block-height: uint
    }
)

;; Define map for authorized sensors/participants
(define-map authorized-sensors
    { sensor-address: principal }
    { 
        sensor-type: (string-ascii 30),
        location: (string-ascii 100),
        is-active: bool
    }
)

;; Define map for product ownership/custody chain
(define-map custody-chain
    { product-id: uint, custody-index: uint }
    {
        custodian: principal,
        transfer-date: uint,
        transfer-reason: (string-ascii 50)
    }
)

;; Read-only functions

;; Get product details
(define-read-only (get-product (product-id uint))
    (map-get? products { product-id: product-id })
)

;; Get temporal event details
(define-read-only (get-temporal-event (event-id uint))
    (map-get? temporal-events { event-id: event-id })
)

;; Get sensor authorization status
(define-read-only (is-sensor-authorized (sensor-address principal))
    (match (map-get? authorized-sensors { sensor-address: sensor-address })
        sensor-info (get is-active sensor-info)
        false
    )
)

;; Get current product count
(define-read-only (get-total-products)
    (- (var-get next-product-id) u1)
)

;; Get current event count
(define-read-only (get-total-events)
    (- (var-get next-event-id) u1)
)

;; Public functions

;; Register a new product in the system
(define-public (register-product 
    (product-type (string-ascii 50))
    (batch-number (string-ascii 30))
    (manufacturing-date uint)
    (initial-location (string-ascii 100)))
    (let 
        (
            (product-id (var-get next-product-id))
        )
        ;; Store product information
        (map-set products
            { product-id: product-id }
            {
                manufacturer: tx-sender,
                product-type: product-type,
                batch-number: batch-number,
                manufacturing-date: manufacturing-date,
                quality-score: u100,
                current-status: "manufactured",
                current-location: initial-location,
                is-active: true
            }
        )
        
        ;; Record initial custody
        (map-set custody-chain
            { product-id: product-id, custody-index: u0 }
            {
                custodian: tx-sender,
                transfer-date: manufacturing-date,
                transfer-reason: "manufacturing"
            }
        )
        
        ;; Increment product ID counter
        (var-set next-product-id (+ product-id u1))
        
        ;; Return success with product ID
        (ok product-id)
    )
)

;; Record a temporal event for a product
(define-public (record-temporal-event
    (product-id uint)
    (event-type (string-ascii 30))
    (location (string-ascii 100))
    (sensor-data (string-ascii 200))
    (quality-metrics uint))
    (let
        (
            (event-id (var-get next-event-id))
            (current-block block-height)
        )
        ;; Verify sensor is authorized
        (asserts! (is-sensor-authorized tx-sender) err-unauthorized)
        
        ;; Verify product exists
        (asserts! (is-some (map-get? products { product-id: product-id })) err-not-found)
        
        ;; Record the temporal event
        (map-set temporal-events
            { event-id: event-id }
            {
                product-id: product-id,
                event-type: event-type,
                timestamp: (unwrap-panic (get-block-info? time current-block)),
                location: location,
                sensor-data: sensor-data,
                quality-metrics: quality-metrics,
                recorded-by: tx-sender,
                block-height: current-block
            }
        )
        
        ;; Update product location and status if it's a movement event
        (if (is-eq event-type "location-update")
            (match (map-get? products { product-id: product-id })
                product-info 
                (map-set products 
                    { product-id: product-id }
                    (merge product-info { current-location: location })
                )
                false
            )
            true
        )
        
        ;; Increment event ID counter
        (var-set next-event-id (+ event-id u1))
        
        ;; Return success with event ID
        (ok event-id)
    )
)

;; Transfer product custody
(define-public (transfer-custody
    (product-id uint)
    (new-custodian principal)
    (transfer-reason (string-ascii 50)))
    (let
        (
            (custody-index (get-custody-count product-id))
        )
        ;; Verify product exists and sender is current custodian
        (match (map-get? products { product-id: product-id })
            product-info
            (begin
                (asserts! (is-eq (get manufacturer product-info) tx-sender) err-unauthorized)
                
                ;; Record custody transfer
                (map-set custody-chain
                    { product-id: product-id, custody-index: custody-index }
                    {
                        custodian: new-custodian,
                        transfer-date: (unwrap-panic (get-block-info? time block-height)),
                        transfer-reason: transfer-reason
                    }
                )
                
                ;; Update product manufacturer field to new custodian
                (map-set products
                    { product-id: product-id }
                    (merge product-info { manufacturer: new-custodian })
                )
                
                (ok true)
            )
            err-not-found
        )
    )
)

;; Update product quality score
(define-public (update-quality-score
    (product-id uint)
    (new-quality-score uint))
    (begin
        ;; Verify sensor is authorized
        (asserts! (is-sensor-authorized tx-sender) err-unauthorized)
        
        ;; Verify quality score is valid (0-100)
        (asserts! (<= new-quality-score u100) err-invalid-input)
        
        ;; Update product quality score
        (match (map-get? products { product-id: product-id })
            product-info
            (begin
                (map-set products
                    { product-id: product-id }
                    (merge product-info { quality-score: new-quality-score })
                )
                (ok true)
            )
            err-not-found
        )
    )
)

;; Authorize a new sensor
(define-public (authorize-sensor
    (sensor-address principal)
    (sensor-type (string-ascii 30))
    (sensor-location (string-ascii 100)))
    (begin
        ;; Only contract owner can authorize sensors
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        ;; Add sensor to authorized list
        (map-set authorized-sensors
            { sensor-address: sensor-address }
            {
                sensor-type: sensor-type,
                location: sensor-location,
                is-active: true
            }
        )
        
        (ok true)
    )
)

;; Deactivate a sensor
(define-public (deactivate-sensor (sensor-address principal))
    (begin
        ;; Only contract owner can deactivate sensors
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        ;; Update sensor status
        (match (map-get? authorized-sensors { sensor-address: sensor-address })
            sensor-info
            (begin
                (map-set authorized-sensors
                    { sensor-address: sensor-address }
                    (merge sensor-info { is-active: false })
                )
                (ok true)
            )
            err-not-found
        )
    )
)

;; Helper function to get custody count for a product
(define-private (get-custody-count (product-id uint))
    ;; This is a simplified version - in a real implementation,
    ;; you might want to track this more efficiently
    u1
)