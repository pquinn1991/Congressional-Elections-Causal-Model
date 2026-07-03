# Congressional Elections — Causal Counterfactuals

The causal half of my House election work: counterfactual estimates of what
campaign spending could actually have changed in U.S. House races, from my
[Northwestern M.S. thesis](https://github.com/pquinn1991/Thesis)
([SSRN](https://ssrn.com/abstract=3204653)). Companion to the
[2018 predictive forecast](https://github.com/pquinn1991/Congressional-Elections-Predictive-Model)
built for CNN's midterm coverage — the thesis compared what predictive and
causal models of the same elections can and can't tell you.

Two causal models generate the counterfactuals:

- a **structural agent-based (BLP-style) discrete-choice model** of voter
  behavior, which estimates the causal effect of candidate spending,
  incumbency, and voter registration, and
- a **causal forest**, which estimates heterogeneous treatment effects of
  spending across districts.

From each model: how much additional spending the losing candidate would have
needed to flip a district — which identifies races where money plausibly
mattered versus races where no realistic budget changes the outcome.

## Contents

| Path | What it is |
|---|---|
| `blpChart.R` | Interactive plotly chart of the spending counterfactuals (the "causal" interactive served at parkermquinn.com/midterms). |
| `cf2.csv` | Counterfactual spending estimates from the agent-based model. |
| `cfCounterFactualsNew.csv` | Counterfactual estimates from the causal forest. |

The models themselves live in the
[Thesis repo](https://github.com/pquinn1991/Thesis) (`Scripts/blpModel.R` and
the causal forest scripts); this repo holds the outputs and the
visualization layer, published as it ran in 2018.
