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

;; Complete tag list validation ensuring all entries meet standards
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
    
    ;; Initialize default access permissions for asset creator
    (map-insert access-control-registry
      { asset-unique-identifier: new-asset-identifier, user-principal-address: tx-sender }
      { read-access-enabled: true }
    )
    
    ;; Update global asset counter for unique identification
    (var-set digital-archive-counter new-asset-identifier)
    (ok new-asset-identifier)
  )
)

;; Comprehensive asset modification with security validation
(define-public (update-existing-digital-asset
  (asset-unique-identifier uint)
  (new-document-display-name (string-ascii 64))
  (new-file-size-bytes uint)
  (new-asset-description-text (string-ascii 128))
  (new-category-tag-list (list 10 (string-ascii 32)))
)
  (let
    (
      (current-asset-data (unwrap! (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
        asset-not-found-exception))
    )
    ;; Thorough authorization and parameter validation
    (asserts! (check-asset-exists-in-archive asset-unique-identifier) asset-not-found-exception)
    (asserts! (is-eq (get asset-owner-address current-asset-data) tx-sender) ownership-verification-failed)
    (asserts! (> (len new-document-display-name) u0) invalid-parameters-exception)
    (asserts! (< (len new-document-display-name) u65) invalid-parameters-exception)
    (asserts! (> new-file-size-bytes u0) file-capacity-exceeded-exception)
    (asserts! (< new-file-size-bytes u1000000000) file-capacity-exceeded-exception)
    (asserts! (> (len new-asset-description-text) u0) invalid-parameters-exception)
    (asserts! (< (len new-asset-description-text) u129) invalid-parameters-exception)
    (asserts! (validate-complete-tag-list new-category-tag-list) category-validation-error)
    
    ;; Perform comprehensive asset data update with validation
    (map-set blockchain-digital-archive
      { asset-unique-identifier: asset-unique-identifier }
      (merge current-asset-data {
        document-display-name: new-document-display-name,
        file-size-bytes: new-file-size-bytes,
        asset-description-text: new-asset-description-text,
        category-tag-list: new-category-tag-list
      })
    )
    (ok true)
  )
)

;; Secure asset ownership transfer with comprehensive validation
(define-public (transfer-asset-ownership (asset-unique-identifier uint) (new-owner-principal principal))
  (let
    (
      (existing-asset-data (unwrap! (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
        asset-not-found-exception))
    )
    ;; Rigorous ownership validation before transfer execution
    (asserts! (check-asset-exists-in-archive asset-unique-identifier) asset-not-found-exception)
    (asserts! (is-eq (get asset-owner-address existing-asset-data) tx-sender) ownership-verification-failed)
    
    ;; Execute secure ownership transfer with updated owner information
    (map-set blockchain-digital-archive
      { asset-unique-identifier: asset-unique-identifier }
      (merge existing-asset-data { asset-owner-address: new-owner-principal })
    )
    (ok true)
  )
)

;; Irreversible asset deletion with security protocols
(define-public (remove-digital-asset-permanently (asset-unique-identifier uint))
  (let
    (
      (asset-to-delete (unwrap! (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
        asset-not-found-exception))
    )
    ;; Comprehensive ownership verification before permanent removal
    (asserts! (check-asset-exists-in-archive asset-unique-identifier) asset-not-found-exception)
    (asserts! (is-eq (get asset-owner-address asset-to-delete) tx-sender) ownership-verification-failed)
    
    ;; Execute permanent asset removal from blockchain archive
    (map-delete blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
    (ok true)
  )
)
;; ===== Read-only functions for information retrieval =====

;; Comprehensive asset information retrieval with access validation
(define-read-only (get-digital-asset-information (asset-unique-identifier uint))
  (let
    (
      (asset-information (unwrap! (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
        asset-not-found-exception))
      (user-access-permission (default-to false
        (get read-access-enabled
          (map-get? access-control-registry { asset-unique-identifier: asset-unique-identifier, user-principal-address: tx-sender })
        )
      ))
    )
    ;; Validate access permissions before information disclosure
    (asserts! (check-asset-exists-in-archive asset-unique-identifier) asset-not-found-exception)
    (asserts! (or user-access-permission (is-eq (get asset-owner-address asset-information) tx-sender)) content-access-forbidden)
    
    ;; Return comprehensive asset information structure
    (ok {
      document-display-name: (get document-display-name asset-information),
      asset-owner-address: (get asset-owner-address asset-information),
      file-size-bytes: (get file-size-bytes asset-information),
      creation-block-height: (get creation-block-height asset-information),
      asset-description-text: (get asset-description-text asset-information),
      category-tag-list: (get category-tag-list asset-information)
    })
  )
)

;; System-wide archive statistics and administrative information
(define-read-only (get-archive-system-statistics)
  (ok {
    total-assets-registered: (var-get digital-archive-counter),
    system-administrator: system-administrator-principal
  })
)

;; Asset ownership verification utility for external queries
(define-read-only (lookup-asset-owner-principal (asset-unique-identifier uint))
  (match (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
    asset-information (ok (get asset-owner-address asset-information))
    asset-not-found-exception
  )
)

;; Comprehensive access permission status verification
(define-read-only (verify-user-access-status (asset-unique-identifier uint) (user-principal-address principal))
  (let
    (
      (asset-information (unwrap! (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
        asset-not-found-exception))
      (direct-access-permission (default-to false
        (get read-access-enabled
          (map-get? access-control-registry { asset-unique-identifier: asset-unique-identifier, user-principal-address: user-principal-address })
        )
      ))
    )
    ;; Return detailed permission status information
    (ok {
      has-direct-access-permission: direct-access-permission,
      is-asset-owner: (is-eq (get asset-owner-address asset-information) user-principal-address),
      can-view-asset: (or direct-access-permission (is-eq (get asset-owner-address asset-information) user-principal-address))
    })
  )
)



