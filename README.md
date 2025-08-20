# The-Man-in-the-Middle

This project develops a scalable framework to evaluate baseball cutoff play decision-making using anonymized MiLB player and ball-tracking data.

- Built a two-model framework:
  - Logistic regression to estimate baserunner advancement probabilities
  - Random Forest to recommend optimal cutoff actions
- Integrated predictions with RE24 run expectancy to compute expected run values for every possible decision
- Findings: cutoff men often favor aggressive throws when holding the ball would minimize run expectancy
- Delivered insights through an interactive dashboard with team- and player-level breakdowns
