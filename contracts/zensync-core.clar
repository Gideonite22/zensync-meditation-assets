;; zensync-core.clar
;; ZenSync Meditation Assets Core Contract

;; This contract manages the core functionality for ZenSync, a meditation-focused digital asset platform.
;; It allows users to track their meditation journey through collectible digital tokens and achievements,
;; record meditation sessions, and build a digital representation of their mindfulness practice.

;; =============================
;; Constants and Error Codes
;; =============================

(define-constant contract-owner tx-sender)

;; Error codes
(define-constant err-not-authorized (err u100))
(define-constant err-invalid-duration (err u101))
(define-constant err-invalid-meditation-type (err u102))
(define-constant err-session-already-recorded (err u103))
(define-constant err-group-already-exists (err u104))
(define-constant err-group-does-not-exist (err u105))
(define-constant err-already-member (err u106))
(define-constant err-not-member (err u107))
(define-constant err-invalid-milestone (err u108))

;; Meditation types
(define-constant meditation-type-mindfulness u1)
(define-constant meditation-type-focused u2)
(define-constant meditation-type-loving-kindness u3)
(define-constant meditation-type-body-scan u4)
(define-constant meditation-type-transcendental u5)

;; Achievement types
(define-constant achievement-session-count u1)  ;; Based on number of sessions
(define-constant achievement-streak u2)         ;; Based on consecutive days
(define-constant achievement-duration u3)       ;; Based on cumulative time
(define-constant achievement-variety u4)        ;; Based on different meditation types

;; Milestone thresholds
(define-constant session-milestones (list u10 u50 u100 u500 u1000))
(define-constant streak-milestones (list u7 u30 u100 u365))
(define-constant duration-milestones (list u600 u3600 u18000 u36000 u108000)) ;; In minutes (10h, 60h, 300h, 600h, 1800h)

;; =============================
;; Data Maps and Variables
;; =============================

;; Track meditation sessions
(define-map meditation-sessions
  { user: principal, timestamp: uint }
  {
    duration: uint,        ;; Duration in minutes
    meditation-type: uint, ;; Type of meditation
    notes: (optional (string-utf8 256))
  }
)

;; Track user stats
(define-map user-stats
  { user: principal }
  {
    total-sessions: uint,
    total-duration: uint,
    current-streak: uint,
    last-meditation-date: uint,
    meditation-types-used: (list 10 uint),
    achievements-earned: (list 100 uint)
  }
)

;; Track all achievements
(define-map achievements
  { achievement-id: uint }
  {
    owner: principal,
    achievement-type: uint,
    milestone: uint,
    earned-at: uint,
    description: (string-utf8 256)
  }
)

;; Track meditation groups
(define-map meditation-groups
  { group-id: uint }
  {
    name: (string-utf8 64),
    description: (string-utf8 256),
    creator: principal,
    created-at: uint,
    members: (list 100 principal)
  }
)

;; Group membership for lookup
(define-map group-membership
  { user: principal, group-id: uint }
  { is-member: bool }
)

;; Counters
(define-data-var achievement-id-counter uint u0)
(define-data-var group-id-counter uint u0)

;; =============================
;; Private Functions
;; =============================

;; Check if meditation type is valid
(define-private (is-valid-meditation-type (meditation-type uint))
  (or
    (is-eq meditation-type meditation-type-mindfulness)
    (is-eq meditation-type meditation-type-focused)
    (is-eq meditation-type meditation-type-loving-kindness)
    (is-eq meditation-type meditation-type-body-scan)
    (is-eq meditation-type meditation-type-transcendental)
  )
)

;; Get current date as YYYYMMDD
(define-private (get-current-date)
  (let (
    (current-time (unwrap-panic (get-block-info? time u0)))
    (seconds-per-day u86400)
  )
    ;; Convert to days since epoch and then to a simplified YYYYMMDD format
    ;; This is a simplification - production code would need proper date logic
    (/ current-time seconds-per-day)
  )
)

;; Check if user meditated yesterday to maintain streak
(define-private (is-streak-active (user principal) (current-date uint))
  (let (
    (user-data (default-to
      { total-sessions: u0, total-duration: u0, current-streak: u0, 
        last-meditation-date: u0, meditation-types-used: (list), achievements-earned: (list) }
      (map-get? user-stats { user: user })))
    (last-date (get last-meditation-date user-data))
  )
    ;; If last meditation was yesterday, streak continues
    (is-eq (+ last-date u1) current-date)
  )
)

;; Generate achievement ID
(define-private (generate-achievement-id)
  (let ((current-id (var-get achievement-id-counter)))
    (var-set achievement-id-counter (+ current-id u1))
    current-id
  )
)

;; Create a new achievement
(define-private (create-achievement (user principal) (achievement-type uint) (milestone uint) (description (string-utf8 256)))
  (let (
    (achievement-id (generate-achievement-id))
    (current-time (unwrap-panic (get-block-info? time u0)))
  )
    ;; Record the achievement
    (map-set achievements
      { achievement-id: achievement-id }
      {
        owner: user,
        achievement-type: achievement-type,
        milestone: milestone,
        earned-at: current-time,
        description: description
      }
    )
    
    ;; Update user stats to include the new achievement
    (match (map-get? user-stats { user: user })
      user-data
      (map-set user-stats
        { user: user }
        (merge user-data {
          achievements-earned: (unwrap-panic (as-max-len? 
            (append (get achievements-earned user-data) achievement-id) 
            u100))
        })
      )
      ;; This shouldn't happen as user stats should already exist
      false
    )
    
    achievement-id
  )
)

;; Check and award session count achievements
(define-private (check-session-achievements (user principal) (session-count uint))
  (let (
    (eligible-milestone (find (lambda (milestone) (is-eq session-count milestone)) session-milestones))
  )
    (match eligible-milestone
      milestone (create-achievement 
                  user 
                  achievement-session-count 
                  milestone 
                  (concat (concat "Completed " (to-utf8 (unwrap-panic (as-max-len? milestone u10)))) " meditation sessions"))
      ;; No milestone reached
      u0
    )
  )
)

;; Check and award streak achievements
(define-private (check-streak-achievements (user principal) (streak-count uint))
  (let (
    (eligible-milestone (find (lambda (milestone) (is-eq streak-count milestone)) streak-milestones))
  )
    (match eligible-milestone
      milestone (create-achievement 
                  user 
                  achievement-streak 
                  milestone 
                  (concat (concat "Maintained a " (to-utf8 (unwrap-panic (as-max-len? milestone u10)))) "-day meditation streak"))
      ;; No milestone reached
      u0
    )
  )
)

;; Check and award duration achievements
(define-private (check-duration-achievements (user principal) (total-duration uint))
  (let (
    (eligible-milestones (filter (lambda (milestone) (>= total-duration milestone)) duration-milestones))
  )
    (match (element-at eligible-milestones (- (len eligible-milestones) u1))
      milestone
      (let (
        (user-data (default-to
          { total-sessions: u0, total-duration: u0, current-streak: u0, 
            last-meditation-date: u0, meditation-types-used: (list), achievements-earned: (list) }
          (map-get? user-stats { user: user })))
        (earned-achievements (get achievements-earned user-data))
        (has-achievement (find (lambda (id) 
          (let ((achievement (unwrap-panic (map-get? achievements { achievement-id: id }))))
            (and 
              (is-eq (get achievement-type achievement) achievement-duration)
              (is-eq (get milestone achievement) milestone)
            )
          )) earned-achievements))
      )
        (if (is-some has-achievement) 
          u0 ;; Already has this achievement
          (create-achievement 
            user 
            achievement-duration 
            milestone 
            (concat (concat "Accumulated " (to-utf8 (unwrap-panic (as-max-len? (/ milestone u60) u10)))) " hours of meditation"))
        )
      )
      ;; No milestone reached
      u0
    )
  )
)

;; Check for variety achievements (using different meditation types)
(define-private (check-variety-achievements (user principal) (meditation-types (list 10 uint)))
  (let (
    (unique-types (len meditation-types))
  )
    (if (>= unique-types u5)
      (let (
        (user-data (default-to
          { total-sessions: u0, total-duration: u0, current-streak: u0, 
            last-meditation-date: u0, meditation-types-used: (list), achievements-earned: (list) }
          (map-get? user-stats { user: user })))
        (earned-achievements (get achievements-earned user-data))
        (has-achievement (find (lambda (id) 
          (let ((achievement (unwrap-panic (map-get? achievements { achievement-id: id }))))
            (is-eq (get achievement-type achievement) achievement-variety)
          )) earned-achievements))
      )
        (if (is-some has-achievement) 
          u0 ;; Already has this achievement
          (create-achievement 
            user 
            achievement-variety 
            unique-types 
            "Explored all meditation types - true mindfulness versatility!")
        )
      )
      u0 ;; Not enough variety yet
    )
  )
)

;; Update a user's meditation types list, ensuring no duplicates
(define-private (update-meditation-types (current-types (list 10 uint)) (new-type uint))
  (if (index-of? current-types new-type)
    current-types ;; Already in the list
    (unwrap-panic (as-max-len? (append current-types new-type) u10))
  )
)

;; =============================
;; Read-Only Functions
;; =============================

;; Get user stats
(define-read-only (get-user-stats (user principal))
  (default-to
    { total-sessions: u0, total-duration: u0, current-streak: u0, 
      last-meditation-date: u0, meditation-types-used: (list), achievements-earned: (list) }
    (map-get? user-stats { user: user })
  )
)

;; Get achievement details
(define-read-only (get-achievement (achievement-id uint))
  (map-get? achievements { achievement-id: achievement-id })
)

;; Get user's meditation session
(define-read-only (get-meditation-session (user principal) (timestamp uint))
  (map-get? meditation-sessions { user: user, timestamp: timestamp })
)

;; Get meditation group details
(define-read-only (get-meditation-group (group-id uint))
  (map-get? meditation-groups { group-id: group-id })
)

;; Check if user is member of a group
(define-read-only (is-group-member (user principal) (group-id uint))
  (default-to false (get is-member (map-get? group-membership { user: user, group-id: group-id })))
)

;; =============================
;; Public Functions
;; =============================

;; Record a new meditation session
(define-public (record-meditation-session (duration uint) (meditation-type uint) (notes (optional (string-utf8 256))))
  (let (
    (user tx-sender)
    (current-time (unwrap-panic (get-block-info? time u0)))
    (current-date (get-current-date))
  )
    ;; Input validations
    (asserts! (> duration u0) err-invalid-duration)
    (asserts! (is-valid-meditation-type meditation-type) err-invalid-meditation-type)
    (asserts! (is-none (map-get? meditation-sessions { user: user, timestamp: current-time })) err-session-already-recorded)
    
    ;; Record the session
    (map-set meditation-sessions
      { user: user, timestamp: current-time }
      { duration: duration, meditation-type: meditation-type, notes: notes }
    )
    
    ;; Get or initialize user stats
    (let (
      (user-data (default-to
        { total-sessions: u0, total-duration: u0, current-streak: u0, 
          last-meditation-date: u0, meditation-types-used: (list), achievements-earned: (list) }
        (map-get? user-stats { user: user })))
      (last-date (get last-meditation-date user-data))
      (new-streak (if (or (is-eq last-date u0) (is-streak-active user current-date))
                      (+ (get current-streak user-data) u1)
                      u1)) ;; Start new streak
      (new-total-sessions (+ (get total-sessions user-data) u1))
      (new-total-duration (+ (get total-duration user-data) duration))
      (new-meditation-types (update-meditation-types (get meditation-types-used user-data) meditation-type))
    )
      ;; Update user stats
      (map-set user-stats
        { user: user }
        {
          total-sessions: new-total-sessions,
          total-duration: new-total-duration,
          current-streak: new-streak,
          last-meditation-date: current-date,
          meditation-types-used: new-meditation-types,
          achievements-earned: (get achievements-earned user-data)
        }
      )
      
      ;; Check and award achievements
      (check-session-achievements user new-total-sessions)
      (check-streak-achievements user new-streak)
      (check-duration-achievements user new-total-duration)
      (check-variety-achievements user new-meditation-types)
      
      ;; Return session data
      (ok { 
        timestamp: current-time, 
        duration: duration, 
        meditation-type: meditation-type, 
        streak: new-streak 
      })
    )
  )
)

;; Create a new meditation group
(define-public (create-meditation-group (name (string-utf8 64)) (description (string-utf8 256)))
  (let (
    (user tx-sender)
    (current-time (unwrap-panic (get-block-info? time u0)))
    (group-id (var-get group-id-counter))
  )
    ;; Create the group
    (map-set meditation-groups
      { group-id: group-id }
      {
        name: name,
        description: description,
        creator: user,
        created-at: current-time,
        members: (list user)
      }
    )
    
    ;; Add creator as a member
    (map-set group-membership
      { user: user, group-id: group-id }
      { is-member: true }
    )
    
    ;; Increment group ID counter
    (var-set group-id-counter (+ group-id u1))
    
    (ok group-id)
  )
)

;; Join a meditation group
(define-public (join-meditation-group (group-id uint))
  (let (
    (user tx-sender)
  )
    ;; Check if group exists
    (asserts! (is-some (map-get? meditation-groups { group-id: group-id })) err-group-does-not-exist)
    
    ;; Check if user is already a member
    (asserts! (not (is-group-member user group-id)) err-already-member)
    
    ;; Add user to group
    (match (map-get? meditation-groups { group-id: group-id })
      group-data
      (begin
        ;; Update group members list
        (map-set meditation-groups
          { group-id: group-id }
          (merge group-data {
            members: (unwrap-panic (as-max-len? 
              (append (get members group-data) user) 
              u100))
          })
        )
        
        ;; Update membership map
        (map-set group-membership
          { user: user, group-id: group-id }
          { is-member: true }
        )
        
        (ok true)
      )
      (err err-group-does-not-exist)
    )
  )
)

;; Leave a meditation group
(define-public (leave-meditation-group (group-id uint))
  (let (
    (user tx-sender)
  )
    ;; Check if group exists
    (asserts! (is-some (map-get? meditation-groups { group-id: group-id })) err-group-does-not-exist)
    
    ;; Check if user is a member
    (asserts! (is-group-member user group-id) err-not-member)
    
    ;; Remove user from group
    (match (map-get? meditation-groups { group-id: group-id })
      group-data
      (begin
        ;; Cannot leave if you're the creator
        (asserts! (not (is-eq user (get creator group-data))) err-not-authorized)
        
        ;; Filter out the user from members list
        (let (
          (updated-members (filter (lambda (member) (not (is-eq member user))) (get members group-data)))
        )
          ;; Update group members list
          (map-set meditation-groups
            { group-id: group-id }
            (merge group-data {
              members: updated-members
            })
          )
          
          ;; Update membership map
          (map-set group-membership
            { user: user, group-id: group-id }
            { is-member: false }
          )
          
          (ok true)
        )
      )
      (err err-group-does-not-exist)
    )
  )
)

;; Verify an achievement publicly
(define-public (verify-achievement (achievement-id uint))
  (match (map-get? achievements { achievement-id: achievement-id })
    achievement 
    (ok {
      owner: (get owner achievement),
      achievement-type: (get achievement-type achievement),
      milestone: (get milestone achievement),
      earned-at: (get earned-at achievement),
      description: (get description achievement)
    })
    (err err-invalid-milestone)
  )
)

;; Share an achievement with group - registers the sharing event on-chain
(define-public (share-achievement-with-group (achievement-id uint) (group-id uint))
  (let (
    (user tx-sender)
  )
    ;; Check if achievement exists and belongs to user
    (match (map-get? achievements { achievement-id: achievement-id })
      achievement
      (begin
        (asserts! (is-eq user (get owner achievement)) err-not-authorized)
        
        ;; Check if group exists
        (asserts! (is-some (map-get? meditation-groups { group-id: group-id })) err-group-does-not-exist)
        
        ;; Check if user is a member
        (asserts! (is-group-member user group-id) err-not-member)
        
        ;; Simply return success - in a real contract we might record this share
        ;; or trigger an event/notification
        (ok {
          achievement-id: achievement-id,
          group-id: group-id,
          shared-at: (unwrap-panic (get-block-info? time u0))
        })
      )
      (err err-invalid-milestone)
    )
  )
)