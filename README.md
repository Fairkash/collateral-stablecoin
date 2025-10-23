Collateral Stablecoin
Collateral Stablecoin is a decentralized, collateral-backed stablecoin system built on the Stacks blockchain using Clarity smart contracts.
Users can deposit STX or other tokens as collateral to mint a stablecoin pegged to a reference asset (e.g., USD).

Features
Mint stablecoin using collateral
Enforced collateralization ratio (e.g., 150%)
Repay and withdraw collateral
Liquidate risky positions
Transparent on-chain accounting

Technical Overview
Language: Clarity
Core Mechanism: Collateralized Debt Position (CDP)
Key Functions:
deposit-collateral(principal, amount) → lock collateral
mint-stablecoin(amount) → mint pegged tokens
repay-debt(amount) → burn stablecoins to repay
withdraw-collateral(amount) → unlock collateral
liquidate(user) → seize collateral if undercollateralized
Constants:
min-collateral-ratio – e.g., 150%
liquidation-penalty – e.g., 10%
