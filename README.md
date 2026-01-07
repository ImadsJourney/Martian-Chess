# Martian Chess (Haskell / Stack)

Martian Chess is a university Haskell project built with **Stack** and developed over **four weeks** as part of a semester portfolio. It implements the full game logic for a modified variant of **Martian Chess**, including board parsing/serialization via a FEN-like notation, legal move generation, move execution with scoring, and win detection.

## Features
- FEN-like notation for board states (parse + serialize)
- Legal move generation for **Pawn**, **Drone**, and **Queen**
- Move execution with **scoring** (captures across the canal) and merging rules
- Win detection when a zone becomes empty (tie-breaker: last player)
- Unit tests with **HSpec** and coverage reporting
- Playable via a provided **GUI** that communicates with the engine through a small CLI interface

## Tech Stack
- Language: **Haskell**
- Build tool: **Stack**
- Testing: **HSpec**
- Coverage: `stack test --coverage`

## How to Build
```bash
stack build

