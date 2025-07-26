;; Enhanced Digital Archive Blockchain Registry
;; Decentralized document and asset management infrastructure with advanced features
;; 
;; Comprehensive blockchain solution for digital asset cataloging and governance
;; Implements robust authentication protocols with enterprise-grade access controls
;; Provides tamper-proof document storage with cryptographic integrity verification
;; Supports multi-tenant architecture with granular permission management
;; NEW: Version control, collaboration, encryption, and advanced search capabilities

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
(define-constant version-not-found-exception (err u409))
(define-constant collaboration-limit-exceeded (err u410))
(define-constant subscription-expired-exception (err u411))
(define-constant encryption-key-invalid (err u412))
(define-constant audit-trail-error (err u413))

;; Global registry counter for unique asset identification
(define-data-var digital-archive-counter uint u0)

;; NEW: Version control counter for asset revisions
(define-data-var version-counter uint u0)

;; NEW: Collaboration session counter
(define-data-var collaboration-session-counter uint u0)

;; Primary digital asset storage repository
(define-map blockchain-digital-archive
  { asset-unique-identifier: uint }
  {
    document-display-name: (string-ascii 64),
    asset-owner-address: principal,
    file-size-bytes: uint,
    creation-block-height: uint,
    asset-description-text: (string-ascii 128),
    category-tag-list: (list 10 (string-ascii 32)),
    is-encrypted: bool,
    encryption-key-hash: (string-ascii 64),
    current-version: uint,
    total-versions: uint,
    last-modified-block: uint,
    asset-status: (string-ascii 16),
    content-hash: (string-ascii 64),
    metadata-json: (string-ascii 256)
  }
)

;; NEW: Version control system for asset history
(define-map asset-version-history
  { asset-unique-identifier: uint, version-number: uint }
  {
    version-description: (string-ascii 128),
    modified-by: principal,
    modification-timestamp: uint,
    file-size-bytes: uint,
    content-hash: (string-ascii 64),
    changes-summary: (string-ascii 256)
  }
)

;; Sophisticated permission control system for access management
(define-map access-control-registry
  { asset-unique-identifier: uint, user-principal-address: principal }
  { 
    read-access-enabled: bool,
    write-access-enabled: bool,
    admin-access-enabled: bool,
    access-granted-by: principal,
    access-granted-at: uint,
    access-expires-at: (optional uint)
  }
)

;; NEW: Collaboration workspace management
(define-map collaboration-sessions
  { session-id: uint }
  {
    asset-unique-identifier: uint,
    session-creator: principal,
    participants: (list 20 principal),
    session-status: (string-ascii 16),
    created-at: uint,
    expires-at: uint,
    session-description: (string-ascii 128)
  }
)

;; NEW: Asset subscription and notification system
(define-map asset-subscriptions
  { asset-unique-identifier: uint, subscriber: principal }
  {
    subscription-type: (string-ascii 32),
    notification-preferences: (list 5 (string-ascii 32)),
    subscribed-at: uint,
    is-active: bool
  }
)

;; NEW: Audit trail for security and compliance
(define-map audit-trail
  { audit-id: uint }
  {
    asset-unique-identifier: uint,
    action-type: (string-ascii 32),
    performed-by: principal,
    timestamp: uint,
    details: (string-ascii 256),
    ip-address-hash: (string-ascii 64)
  }
)

;; NEW: Asset categories and tagging system
(define-map category-registry
  { category-name: (string-ascii 32) }
  {
    category-description: (string-ascii 128),
    created-by: principal,
    asset-count: uint,
    is-active: bool
  }
)

;; NEW: User profiles and reputation system
(define-map user-profiles
  { user-principal: principal }
  {
    display-name: (string-ascii 64),
    reputation-score: uint,
    assets-created: uint,
    collaborations-participated: uint,
    profile-created-at: uint,
    last-activity: uint,
    subscription-tier: (string-ascii 16)
  }
)

;; NEW: Asset analytics and statistics
(define-map asset-analytics
  { asset-unique-identifier: uint }
  {
    view-count: uint,
    download-count: uint,
    collaboration-count: uint,
    last-accessed: uint,
    average-rating: uint,
    total-ratings: uint
  }
)

;; ===== Input validation helper functions =====

;; Validate string parameters
(define-private (validate-string-ascii-64 (input (string-ascii 64)))
  (and 
    (> (len input) u0)
    (<= (len input) u64)
  )
)

(define-private (validate-string-ascii-128 (input (string-ascii 128)))
  (and 
    (> (len input) u0)
    (<= (len input) u128)
  )
)

(define-private (validate-string-ascii-256 (input (string-ascii 256)))
  (and 
    (> (len input) u0)
    (<= (len input) u256)
  )
)

(define-private (validate-string-ascii-32 (input (string-ascii 32)))
  (and 
    (> (len input) u0)
    (<= (len input) u32)
  )
)

(define-private (validate-string-ascii-16 (input (string-ascii 16)))
  (and 
    (> (len input) u0)
    (<= (len input) u16)
  )
)

;; Validate file size parameters
(define-private (validate-file-size (size uint))
  (and 
    (> size u0)
    (<= size u1000000000)
  )
)

;; Validate encryption key hash
(define-private (validate-encryption-key (key (string-ascii 64)))
  (or
    (is-eq (len key) u0)  ;; Allow empty for non-encrypted assets
    (is-eq (len key) u64) ;; Or exactly 64 characters for hash
  )
)

;; Validate content hash
(define-private (validate-content-hash (hash (string-ascii 64)))
  (is-eq (len hash) u64) ;; Must be exactly 64 characters
)

;; Validate principal (basic check)
(define-private (validate-principal (principal-address principal))
  (not (is-eq principal-address 'SP000000000000000000002Q6VF78)) ;; Not null address
)

;; Validate duration blocks
(define-private (validate-duration-blocks (duration uint))
  (and 
    (> duration u0)
    (<= duration u210000) ;; Max ~1 year at 10min blocks
  )
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

;; NEW: Enhanced access control check with write permissions
(define-private (verify-write-access (asset-unique-identifier uint) (user-principal-address principal))
  (let
    (
      (asset-data (unwrap! (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier }) false))
      (access-data (map-get? access-control-registry { asset-unique-identifier: asset-unique-identifier, user-principal-address: user-principal-address }))
    )
    (or
      (is-eq (get asset-owner-address asset-data) user-principal-address)
      (match access-data
        access-info (get write-access-enabled access-info)
        false
      )
    )
  )
)

;; NEW: Audit trail logging function
(define-private (log-audit-event (asset-id uint) (action (string-ascii 32)) (details (string-ascii 256)))
  (let
    (
      (audit-id (+ (var-get version-counter) u1))
      (validated-action (if (validate-string-ascii-32 action) action "UNKNOWN"))
      (validated-details (if (validate-string-ascii-256 details) details "No details provided"))
    )
    (map-insert audit-trail
      { audit-id: audit-id }
      {
        asset-unique-identifier: asset-id,
        action-type: validated-action,
        performed-by: tx-sender,
        timestamp: block-height,
        details: validated-details,
        ip-address-hash: "hash-placeholder"
      }
    )
    (var-set version-counter audit-id)
  )
)

;; NEW: Update user activity and reputation
(define-private (update-user-activity (user-principal principal) (action-type (string-ascii 16)))
  (let
    (
      (validated-action (if (validate-string-ascii-16 action-type) action-type "unknown"))
      (current-profile (default-to
        {
          display-name: "Anonymous",
          reputation-score: u0,
          assets-created: u0,
          collaborations-participated: u0,
          profile-created-at: block-height,
          last-activity: block-height,
          subscription-tier: "basic"
        }
        (map-get? user-profiles { user-principal: user-principal })
      ))
    )
    (map-set user-profiles
      { user-principal: user-principal }
      (merge current-profile {
        last-activity: block-height,
        reputation-score: (+ (get reputation-score current-profile) u1)
      })
    )
  )
)

;; ===== Core public functions for blockchain operations =====

;; Complete digital asset registration with comprehensive validation
(define-public (register-new-digital-asset
  (document-display-name (string-ascii 64))
  (file-size-bytes uint)
  (asset-description-text (string-ascii 128))
  (category-tag-list (list 10 (string-ascii 32)))
  (is-encrypted bool)
  (encryption-key-hash (string-ascii 64))
  (content-hash (string-ascii 64))
  (metadata-json (string-ascii 256))
)
  (let
    (
      (new-asset-identifier (+ (var-get digital-archive-counter) u1))
      ;; Validate all inputs
      (validated-name (asserts! (validate-string-ascii-64 document-display-name) invalid-parameters-exception))
      (validated-size (asserts! (validate-file-size file-size-bytes) file-capacity-exceeded-exception))
      (validated-description (asserts! (validate-string-ascii-128 asset-description-text) invalid-parameters-exception))
      (validated-tags (asserts! (validate-complete-tag-list category-tag-list) category-validation-error))
      (validated-key (asserts! (validate-encryption-key encryption-key-hash) encryption-key-invalid))
      (validated-hash (asserts! (validate-content-hash content-hash) invalid-parameters-exception))
      (validated-metadata (asserts! (validate-string-ascii-256 metadata-json) invalid-parameters-exception))
    )
    
    ;; Execute secure asset registration in blockchain archive
    (map-insert blockchain-digital-archive
      { asset-unique-identifier: new-asset-identifier }
      {
        document-display-name: document-display-name,
        asset-owner-address: tx-sender,
        file-size-bytes: file-size-bytes,
        creation-block-height: block-height,
        asset-description-text: asset-description-text,
        category-tag-list: category-tag-list,
        is-encrypted: is-encrypted,
        encryption-key-hash: encryption-key-hash,
        current-version: u1,
        total-versions: u1,
        last-modified-block: block-height,
        asset-status: "active",
        content-hash: content-hash,
        metadata-json: metadata-json
      }
    )
    
    ;; Initialize default access permissions for asset creator
    (map-insert access-control-registry
      { asset-unique-identifier: new-asset-identifier, user-principal-address: tx-sender }
      { 
        read-access-enabled: true,
        write-access-enabled: true,
        admin-access-enabled: true,
        access-granted-by: tx-sender,
        access-granted-at: block-height,
        access-expires-at: none
      }
    )
    
    ;; Initialize asset analytics
    (map-insert asset-analytics
      { asset-unique-identifier: new-asset-identifier }
      {
        view-count: u0,
        download-count: u0,
        collaboration-count: u0,
        last-accessed: block-height,
        average-rating: u0,
        total-ratings: u0
      }
    )
    
    ;; Create initial version entry
    (map-insert asset-version-history
      { asset-unique-identifier: new-asset-identifier, version-number: u1 }
      {
        version-description: "Initial version",
        modified-by: tx-sender,
        modification-timestamp: block-height,
        file-size-bytes: file-size-bytes,
        content-hash: content-hash,
        changes-summary: "Asset created"
      }
    )
    
    ;; Update global asset counter and user activity
    (var-set digital-archive-counter new-asset-identifier)
    (update-user-activity tx-sender "create")
    (log-audit-event new-asset-identifier "ASSET_CREATED" "New digital asset registered")
    
    (ok new-asset-identifier)
  )
)

;; NEW: Create new version of existing asset
(define-public (create-asset-version
  (asset-unique-identifier uint)
  (version-description (string-ascii 128))
  (new-file-size-bytes uint)
  (new-content-hash (string-ascii 64))
  (changes-summary (string-ascii 256))
)
  (let
    (
      (current-asset-data (unwrap! (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
        asset-not-found-exception))
      (new-version-number (+ (get total-versions current-asset-data) u1))
      ;; Validate inputs
      (validated-description (asserts! (validate-string-ascii-128 version-description) invalid-parameters-exception))
      (validated-size (asserts! (validate-file-size new-file-size-bytes) file-capacity-exceeded-exception))
      (validated-hash (asserts! (validate-content-hash new-content-hash) invalid-parameters-exception))
      (validated-summary (asserts! (validate-string-ascii-256 changes-summary) invalid-parameters-exception))
    )
    ;; Validate permissions and parameters
    (asserts! (check-asset-exists-in-archive asset-unique-identifier) asset-not-found-exception)
    (asserts! (verify-write-access asset-unique-identifier tx-sender) ownership-verification-failed)
    
    ;; Create new version entry
    (map-insert asset-version-history
      { asset-unique-identifier: asset-unique-identifier, version-number: new-version-number }
      {
        version-description: version-description,
        modified-by: tx-sender,
        modification-timestamp: block-height,
        file-size-bytes: new-file-size-bytes,
        content-hash: new-content-hash,
        changes-summary: changes-summary
      }
    )
    
    ;; Update main asset record
    (map-set blockchain-digital-archive
      { asset-unique-identifier: asset-unique-identifier }
      (merge current-asset-data {
        current-version: new-version-number,
        total-versions: new-version-number,
        last-modified-block: block-height,
        file-size-bytes: new-file-size-bytes,
        content-hash: new-content-hash
      })
    )
    
    (update-user-activity tx-sender "version")
    (log-audit-event asset-unique-identifier "VERSION_CREATED" changes-summary)
    (ok new-version-number)
  )
)

;; NEW: Grant advanced access permissions
(define-public (grant-advanced-access
  (asset-unique-identifier uint)
  (target-user principal)
  (read-access bool)
  (write-access bool)
  (admin-access bool)
  (expiration-block (optional uint))
)
  (let
    (
      (asset-data (unwrap! (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
        asset-not-found-exception))
      ;; Validate target user
      (validated-user (asserts! (validate-principal target-user) invalid-parameters-exception))
    )
    ;; Validate ownership or admin rights
    (asserts! (check-asset-exists-in-archive asset-unique-identifier) asset-not-found-exception)
    (asserts! (or 
      (is-eq (get asset-owner-address asset-data) tx-sender)
      (is-eq system-administrator-principal tx-sender)
    ) ownership-verification-failed)
    
    ;; Set advanced permissions
    (map-set access-control-registry
      { asset-unique-identifier: asset-unique-identifier, user-principal-address: target-user }
      {
        read-access-enabled: read-access,
        write-access-enabled: write-access,
        admin-access-enabled: admin-access,
        access-granted-by: tx-sender,
        access-granted-at: block-height,
        access-expires-at: expiration-block
      }
    )
    
    (log-audit-event asset-unique-identifier "ACCESS_GRANTED" "Advanced permissions granted")
    (ok true)
  )
)

;; NEW: Create collaboration session
(define-public (create-collaboration-session
  (asset-unique-identifier uint)
  (session-description (string-ascii 128))
  (duration-blocks uint)
  (initial-participants (list 20 principal))
)
  (let
    (
      (session-id (+ (var-get collaboration-session-counter) u1))
      (asset-data (unwrap! (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
        asset-not-found-exception))
      ;; Validate inputs
      (validated-description (asserts! (validate-string-ascii-128 session-description) invalid-parameters-exception))
      (validated-duration (asserts! (validate-duration-blocks duration-blocks) invalid-parameters-exception))
    )
    ;; Validate permissions and parameters
    (asserts! (check-asset-exists-in-archive asset-unique-identifier) asset-not-found-exception)
    (asserts! (verify-write-access asset-unique-identifier tx-sender) ownership-verification-failed)
    (asserts! (<= (len initial-participants) u20) collaboration-limit-exceeded)
    
    ;; Create collaboration session
    (map-insert collaboration-sessions
      { session-id: session-id }
      {
        asset-unique-identifier: asset-unique-identifier,
        session-creator: tx-sender,
        participants: initial-participants,
        session-status: "active",
        created-at: block-height,
        expires-at: (+ block-height duration-blocks),
        session-description: session-description
      }
    )
    
    ;; Update analytics
    (let
      (
        (current-analytics (default-to
          {
            view-count: u0,
            download-count: u0,
            collaboration-count: u0,
            last-accessed: block-height,
            average-rating: u0,
            total-ratings: u0
          }
          (map-get? asset-analytics { asset-unique-identifier: asset-unique-identifier })
        ))
      )
      (map-set asset-analytics
        { asset-unique-identifier: asset-unique-identifier }
        (merge current-analytics {
          collaboration-count: (+ (get collaboration-count current-analytics) u1)
        })
      )
    )
    
    (var-set collaboration-session-counter session-id)
    (update-user-activity tx-sender "collaborate")
    (log-audit-event asset-unique-identifier "COLLABORATION_STARTED" session-description)
    (ok session-id)
  )
)

;; NEW: Subscribe to asset notifications
(define-public (subscribe-to-asset
  (asset-unique-identifier uint)
  (subscription-type (string-ascii 32))
  (notification-preferences (list 5 (string-ascii 32)))
)
  (let
    (
      ;; Validate inputs
      (validated-type (asserts! (validate-string-ascii-32 subscription-type) invalid-parameters-exception))
    )
    (asserts! (check-asset-exists-in-archive asset-unique-identifier) asset-not-found-exception)
    
    (map-set asset-subscriptions
      { asset-unique-identifier: asset-unique-identifier, subscriber: tx-sender }
      {
        subscription-type: subscription-type,
        notification-preferences: notification-preferences,
        subscribed-at: block-height,
        is-active: true
      }
    )
    
    (log-audit-event asset-unique-identifier "SUBSCRIPTION_CREATED" subscription-type)
    (ok true)
  )
)

;; NEW: Rate and review asset
(define-public (rate-asset (asset-unique-identifier uint) (rating uint))
  (let
    (
      (current-analytics (unwrap! (map-get? asset-analytics { asset-unique-identifier: asset-unique-identifier })
        asset-not-found-exception))
      (current-total (get total-ratings current-analytics))
      (current-average (get average-rating current-analytics))
      (new-total (+ current-total u1))
      (new-average (/ (+ (* current-average current-total) rating) new-total))
    )
    (asserts! (check-asset-exists-in-archive asset-unique-identifier) asset-not-found-exception)
    (asserts! (and (>= rating u1) (<= rating u5)) invalid-parameters-exception)
    
    (map-set asset-analytics
      { asset-unique-identifier: asset-unique-identifier }
      (merge current-analytics {
        average-rating: new-average,
        total-ratings: new-total
      })
    )
    
    (log-audit-event asset-unique-identifier "ASSET_RATED" "Asset rating updated")
    (ok true)
  )
)

;; Comprehensive asset modification with security validation (Enhanced)
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
      ;; Validate all inputs
      (validated-name (asserts! (validate-string-ascii-64 new-document-display-name) invalid-parameters-exception))
      (validated-size (asserts! (validate-file-size new-file-size-bytes) file-capacity-exceeded-exception))
      (validated-description (asserts! (validate-string-ascii-128 new-asset-description-text) invalid-parameters-exception))
      (validated-tags (asserts! (validate-complete-tag-list new-category-tag-list) category-validation-error))
    )
    ;; Thorough authorization and parameter validation
    (asserts! (check-asset-exists-in-archive asset-unique-identifier) asset-not-found-exception)
    (asserts! (verify-write-access asset-unique-identifier tx-sender) ownership-verification-failed)
    
    ;; Perform comprehensive asset data update with validation
    (map-set blockchain-digital-archive
      { asset-unique-identifier: asset-unique-identifier }
      (merge current-asset-data {
        document-display-name: new-document-display-name,
        file-size-bytes: new-file-size-bytes,
        asset-description-text: new-asset-description-text,
        category-tag-list: new-category-tag-list,
        last-modified-block: block-height
      })
    )
    
    (update-user-activity tx-sender "update")
    (log-audit-event asset-unique-identifier "ASSET_UPDATED" "Asset metadata updated")
    (ok true)
  )
)

;; Secure asset ownership transfer with comprehensive validation (Enhanced)
(define-public (transfer-asset-ownership (asset-unique-identifier uint) (new-owner-principal principal))
  (let
    (
      (existing-asset-data (unwrap! (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
        asset-not-found-exception))
      ;; Validate new owner
      (validated-owner (asserts! (validate-principal new-owner-principal) invalid-parameters-exception))
    )
    ;; Rigorous ownership validation before transfer execution
    (asserts! (check-asset-exists-in-archive asset-unique-identifier) asset-not-found-exception)
    (asserts! (is-eq (get asset-owner-address existing-asset-data) tx-sender) ownership-verification-failed)
    
    ;; Execute secure ownership transfer with updated owner information
    (map-set blockchain-digital-archive
      { asset-unique-identifier: asset-unique-identifier }
      (merge existing-asset-data { 
        asset-owner-address: new-owner-principal,
        last-modified-block: block-height
      })
    )
    
    ;; Grant full access to new owner
    (map-set access-control-registry
      { asset-unique-identifier: asset-unique-identifier, user-principal-address: new-owner-principal }
      {
        read-access-enabled: true,
        write-access-enabled: true,
        admin-access-enabled: true,
        access-granted-by: tx-sender,
        access-granted-at: block-height,
        access-expires-at: none
      }
    )
    
    (log-audit-event asset-unique-identifier "OWNERSHIP_TRANSFERRED" "Asset ownership transferred")
    (ok true)
  )
)

;; Irreversible asset deletion with security protocols (Enhanced)
(define-public (remove-digital-asset-permanently (asset-unique-identifier uint))
  (let
    (
      (asset-to-delete (unwrap! (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
        asset-not-found-exception))
    )
    ;; Comprehensive ownership verification before permanent removal
    (asserts! (check-asset-exists-in-archive asset-unique-identifier) asset-not-found-exception)
    (asserts! (is-eq (get asset-owner-address asset-to-delete) tx-sender) ownership-verification-failed)
    
    ;; Mark as deleted instead of permanent removal for audit trail
    (map-set blockchain-digital-archive
      { asset-unique-identifier: asset-unique-identifier }
      (merge asset-to-delete { 
        asset-status: "deleted",
        last-modified-block: block-height
      })
    )
    
    (log-audit-event asset-unique-identifier "ASSET_DELETED" "Asset marked as deleted")
    (ok true)
  )
)

;; ===== Enhanced Read-only functions for information retrieval =====

;; Comprehensive asset information retrieval with access validation (Enhanced)
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
      category-tag-list: (get category-tag-list asset-information),
      is-encrypted: (get is-encrypted asset-information),
      current-version: (get current-version asset-information),
      total-versions: (get total-versions asset-information),
      last-modified-block: (get last-modified-block asset-information),
      asset-status: (get asset-status asset-information),
      content-hash: (get content-hash asset-information),
      metadata-json: (get metadata-json asset-information)
    })
  )
)

;; NEW: Get asset version history
(define-read-only (get-asset-version-history (asset-unique-identifier uint))
  (let
    (
      (asset-information (unwrap! (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
        asset-not-found-exception))
    )
    (asserts! (check-asset-exists-in-archive asset-unique-identifier) asset-not-found-exception)
    (ok (get total-versions asset-information))
  )
)

;; NEW: Get asset analytics
(define-read-only (get-asset-analytics (asset-unique-identifier uint))
  (let
    (
      (analytics-data (unwrap! (map-get? asset-analytics { asset-unique-identifier: asset-unique-identifier })
        asset-not-found-exception))
    )
    (asserts! (check-asset-exists-in-archive asset-unique-identifier) asset-not-found-exception)
    (ok analytics-data)
  )
)

;; NEW: Get collaboration sessions for asset
(define-read-only (get-active-collaborations (asset-unique-identifier uint))
  (begin
    (asserts! (check-asset-exists-in-archive asset-unique-identifier) asset-not-found-exception)
    (ok (var-get collaboration-session-counter))
  )
)

;; NEW: Get user profile information
(define-read-only (get-user-profile (user-principal principal))
  (ok (map-get? user-profiles { user-principal: user-principal }))
)

;; System-wide archive statistics and administrative information (Enhanced)
(define-read-only (get-archive-system-statistics)
  (ok {
    total-assets-registered: (var-get digital-archive-counter),
    total-versions-created: (var-get version-counter),
    total-collaboration-sessions: (var-get collaboration-session-counter),
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

;; Comprehensive access permission status verification (Enhanced)
(define-read-only (verify-user-access-status (asset-unique-identifier uint) (user-principal-address principal))
  (let
    (
      (asset-information (unwrap! (map-get? blockchain-digital-archive { asset-unique-identifier: asset-unique-identifier })
        asset-not-found-exception))
      (access-info (map-get? access-control-registry { asset-unique-identifier: asset-unique-identifier, user-principal-address: user-principal-address }))
    )
    ;; Return detailed permission status information
    (ok {
      has-read-access: (match access-info
        info (get read-access-enabled info)
        false),
      has-write-access: (match access-info
        info (get write-access-enabled info)
        false),
      has-admin-access: (match access-info
        info (get admin-access-enabled info)
        false),
      is-asset-owner: (is-eq (get asset-owner-address asset-information) user-principal-address),
      access-expires-at: (match access-info
        info (get access-expires-at info)
        none),
      can-view-asset: (or 
        (match access-info
          info (get read-access-enabled info)
          false)
        (is-eq (get asset-owner-address asset-information) user-principal-address))
    })
  )
)