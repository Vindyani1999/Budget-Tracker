-- File: README.md

# Budget Tracker (Haskell) - Mini Project

## Group members & project title

- Member 1: EG/2020/4100 Nipun N M A C
- Member 2: EG/2020/4116 Prabuddhika R P H
- Member 3: EG/2020/4191 Sandarenu D T
- Member 4: EG/2020/4253 Vindyani K A C H

**Project title:** PureBudget — A Functional Budget Tracker

## Problem description (real-world scenario)

Personal budgeting requires immutable, auditable calculations across transaction histories. This project demonstrates a functional approach to computing balances, monthly summaries, category breakdowns, and projections from transaction CSV files.

## Structure

- Main.hs
- DataTypes.hs
- Processing.hs
- IOHandler.hs
- Utils.hs

## How to run

1. Ensure GHC is installed (GHC 8.10+ recommended).
2. Put `sample.csv` in the project folder or provide your own CSV (date,amount,category,note).
3. In terminal:

```bash
ghci Main.hs
:main
```

4. Follow interactive prompts.

## Sample input (sample.csv)

```
2025-01-05,1200.00,Salary,January salary
2025-01-10,-45.50,Groceries,Market
2025-01-12,-15.00,Transport,Bus
2025-02-01,1200.00,Salary,February salary
2025-02-14,-200.00,Rent,Feb rent
```

## FP concepts used (short examples)

- Pure functions: `balance :: [Transaction] -> Double` — deterministic and testable.
- ADT: `Transaction` and `Summary` in `DataTypes.hs`.
- Higher-order functions: `map`, `foldl'`, used throughout `Processing.hs`.
- Immutability & composition: pipeline `read -> parse -> process -> write` with no hidden state.
- Parallelism (suggested): using `Control.Parallel.Strategies` to compute independent summaries in parallel (extension idea).

## Tests

Write GHCi tests or HUnit tests for small functions: `balance`, `monthlySummary`, `categorySummary`.
