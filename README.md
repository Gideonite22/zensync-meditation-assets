# ZenSync Meditation Assets

A blockchain-based platform for tracking and rewarding meditation practice through digital achievements and community engagement.

## Overview

ZenSync is a digital asset platform designed for meditation practitioners to:
- Record and track meditation sessions
- Earn achievements based on practice milestones
- Join meditation groups and share progress
- Build a verifiable record of their mindfulness journey

The platform creates a permanent, transparent record of meditation practice while fostering community engagement through shared achievements and group participation.

## Architecture

The system is built around a core smart contract that manages:
- Meditation session recording
- Achievement tracking and verification
- Group membership and interactions
- User statistics and progress tracking

```mermaid
graph TD
    A[User] -->|Records Session| B[Meditation Sessions]
    B -->|Updates| C[User Stats]
    C -->|Triggers| D[Achievement System]
    A -->|Joins/Creates| E[Meditation Groups]
    D -->|Shares With| E
    
    subgraph Core Contract
    B
    C
    D
    E
    end
```

## Contract Documentation

### Core Contract (zensync-core.clar)

The main contract managing all ZenSync functionality.

#### Key Features
- Meditation session recording and tracking
- Achievement system with multiple categories
- Group management and social features
- Comprehensive user statistics

#### Achievement Types
1. Session Count (milestone-based)
2. Meditation Streaks
3. Total Duration
4. Practice Variety

#### Meditation Types
- Mindfulness
- Focused
- Loving-kindness
- Body Scan
- Transcendental

## Getting Started

### Prerequisites
- Clarinet
- Stacks wallet for contract interaction

### Basic Usage

1. Record a meditation session:
```clarity
(contract-call? .zensync-core record-meditation-session 
    u30  ;; duration in minutes
    u1   ;; meditation type (1 = mindfulness)
    none ;; optional notes
)
```

2. Create a meditation group:
```clarity
(contract-call? .zensync-core create-meditation-group 
    "Morning Meditators" 
    "A group for early risers"
)
```

## Function Reference

### Public Functions

#### record-meditation-session
```clarity
(define-public (record-meditation-session (duration uint) (meditation-type uint) (notes (optional (string-utf8 256))))
```
Records a new meditation session and triggers achievement checks.

#### create-meditation-group
```clarity
(define-public (create-meditation-group (name (string-utf8 64)) (description (string-utf8 256)))
```
Creates a new meditation group with the caller as creator.

#### join-meditation-group
```clarity
(define-public (join-meditation-group (group-id uint))
```
Allows a user to join an existing meditation group.

#### verify-achievement
```clarity
(define-public (verify-achievement (achievement-id uint))
```
Publicly verifies an achievement's authenticity.

### Read-Only Functions

#### get-user-stats
```clarity
(define-read-only (get-user-stats (user principal))
```
Retrieves complete statistics for a user.

#### get-meditation-session
```clarity
(define-read-only (get-meditation-session (user principal) (timestamp uint))
```
Retrieves details of a specific meditation session.

## Development

### Testing
1. Install Clarinet
2. Run the test suite:
```bash
clarinet test
```

### Local Development
1. Start a local Clarinet console:
```bash
clarinet console
```
2. Deploy contracts:
```clarity
(contract-call? .zensync-core ...)
```

## Security Considerations

### Limitations
- Group membership limited to 100 members per group
- Achievement list capped at 100 achievements per user
- Meditation types list limited to 10 types

### Best Practices
- Verify achievement ownership before sharing
- Check group membership before attempting group operations
- Validate all duration and meditation type inputs
- Be aware of streak calculation limitations around time zones