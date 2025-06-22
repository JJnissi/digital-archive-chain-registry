;; Digital Archive Blockchain Registry
;; Decentralized document and asset management infrastructure
;; 
;; Comprehensive blockchain solution for digital asset cataloging and governance
;; Implements robust authentication protocols with enterprise-grade access controls
;; Provides tamper-proof document storage with cryptographic integrity verification
;; Supports multi-tenant architecture with granular permission management

;; System administration and authority configuration
(define-constant system-administrator-principal tx-sender)

;; Detailed error response framework for robust exception handling
(define-constant asset-not-found-exception (err u401))
(define-constant duplicate-entry-violation (err u402))
(define-constant invalid-parameters-exception (err u403))
(define-constant file-capacity-exceeded-exception (err u404))
(define-constant access-denied-exception (err u405))
(define-constant ownership-verification-failed (err u406))
(define-constant administrator-rights-required (err u400))
(define-constant content-access-forbidden (err u407))
(define-constant category-validation-error (err u408))

;; Global registry counter for unique asset identification
(define-data-var digital-archive-counter uint u0)

;; Primary digital asset storage repository
(define-map blockchain-digital-archive
  { asset-unique-identifier: uint }
  {
    document-display-name: (string-ascii 64),
    asset-owner-address: principal,
    file-size-bytes: uint,
    creation-block-height: uint,
    asset-description-text: (string-ascii 128),
    category-tag-list: (list 10 (string-ascii 32))
  }
)

;; Sophisticated permission control system for access management
(define-map access-control-registry
  { asset-unique-identifier: uint, user-principal-address: principal }
  { read-access-enabled: bool }
)

;; ===== Private helper functions for internal operations =====

;; Category tag format validation with comprehensive string checks
(define-private (validate-category-tag-format (single-category-tag (string-ascii 32)))
  (and
    (> (len single-category-tag) u0)
    (< (len single-category-tag) u33)
  )
)

(define-private (validate-complete-tag-list (category-tag-collection (list 10 (string-ascii 32))))
  (and
    (> (len category-tag-collection) u0)
    (<= (len category-tag-collection) u10)
    (is-eq (len (filter validate-category-tag-format category-tag-collection)) (len category-tag-collection))
  )
)

;; Asset existence check within the blockchain archive
(define-private (check-asset-exists-in-archive (asset-unique-identifier uint))
  (is-some (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier }))
)

;; File size extraction utility for asset management
(define-private (get-asset-file-size (asset-unique-identifier uint))
  (default-to u0
    (get file-size-bytes
      (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
    )
  )
)

;; Ownership verification mechanism with principal comparison
(define-private (verify-asset-ownership-rights (asset-unique-identifier uint) (user-principal-address principal))
  (match (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
    asset-data (is-eq (get asset-owner-address asset-data) user-principal-address)
    false
  )
)

;; ===== Core public functions for blockchain operations =====

;; Complete digital asset registration with comprehensive validation
(define-public (register-new-digital-asset
  (document-display-name (string-ascii 64))
  (file-size-bytes uint)
  (asset-description-text (string-ascii 128))
  (category-tag-list (list 10 (string-ascii 32)))
)
  (let
    (
      (new-asset-identifier (+ (var-get digital-archive-counter) u1))
    )
    ;; Comprehensive input validation with detailed error reporting
    (asserts! (> (len document-display-name) u0) invalid-parameters-exception)
    (asserts! (< (len document-display-name) u65) invalid-parameters-exception)
    (asserts! (> file-size-bytes u0) file-capacity-exceeded-exception)
    (asserts! (< file-size-bytes u1000000000) file-capacity-exceeded-exception)
    (asserts! (> (len asset-description-text) u0) invalid-parameters-exception)
    (asserts! (< (len asset-description-text) u129) invalid-parameters-exception)
    (asserts! (validate-complete-tag-list category-tag-list) category-validation-error)
    
    ;; Execute secure asset registration in blockchain archive
    (map-insert blockchain-digital-archive
      { asset-unique-identifier: new-asset-identifier }
      {
        document-display-name: document-display-name,
        asset-owner-address: tx-sender,
        file-size-bytes: file-size-bytes,
        creation-block-height: block-height,
        asset-description-text: asset-description-text,
        category-tag-list: category-tag-list
      }
    )
 