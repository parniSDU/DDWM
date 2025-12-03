# DDWM: Data-Driven Warranty Modelling


## Description

**DDWM** employs a data-driven approach to extracting warranty models
for multi-state deteriorating repairable products, with a focus on scenarios
involving stochastic repair times. While analytical solutions exist for cases
with negligible or fixed repair/replacement durations, no explicit solutions
are available when these times are stochastic. To address the lack of explicit
solutions for stochastic repair times, we first extract models from failure and
repair data, then apply a modified Proxel-based simulation to determine optimal
repair-replacement policies that minimize expected warranty servicing
costs per item sold. Our results reveal that when minimal repairs are performed
instantaneously, replacement is generally favored over early repairs.
Conversely, when repair times are non-zero, the system tends to prefer repair
over replacement. Additionally, we find that data-driven warranty models
evolve with continuous data integration but often underestimate reliability
due to biased failure data, highlighting the need for bias-aware modeling
techniques.
## Installation
```
install.packages("devtools")
# Install DDWM
devtools::install_github("parniSDU/DDWM")
```
