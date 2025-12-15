
# DDWM: Data-Driven Warranty Modelling

<img src="GAEM.png" style="width:40%;" align=right>


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

#### Required Arguments (ordered)

1. Raw Simulated Dataset---> SimData_K4_alpha0.5_size500,<br>
2. Warranty Period---> W,<br>
3. Degeradation Model---> model,<br>
4. Time Steps for Proxel-based Simulation ---> delta,<br> tol=0.0000001, alpha = 0.6, K=3
5. Tolerance for Proxel-based Simulation---> tol,<br>
6. Remaining Warranty Time as Decision Variable---> alpha,<br>
7. Degradation State threshhold---> K,<br>

#### Outputs: A list containing:

1. A vector of Instantaneous Unavailability Values 
2. Expected Warranty Cost

## Example
An example of DDWM's can be found in the examples/ directory.
```
#Running Example

library(DDWM)

# reading and preprocessing the data
rawlogs<-read.table(file="Example/SimData_K4_alpha0.5_size500.txt", header=TRUE)
prelogs<-PreData(rawlogs, W=2)

# State estimation for the data simulated from policy 1
result<-StateEstimation(prelogs, policy = 1)

# Learning the model parameters from the augmented data
cutpoint<-result$CutPoints[4]
result_param<-parameters(result$EstimatedData, use_estimated = TRUE, cutpoint = cutpoint, policy = 2)


# Building the model from the learned parameters
out<-result_param

model<-list(
  transition_states <- list(
    'WorkingState 1' = list(state= c('WorkingState 2', 'FailureState 1'), cost=c(0,0)),
    'WorkingState 2' = list(state=c('WorkingState 3', 'FailureState 2'), cost=c(0,0)),
    'WorkingState 3' = list(state=c('WorkingState 4', 'FailureState 3'), cost= c(0,0)),
    'WorkingState 4' = list(state=c('FailureState 4'), cost=c(0)),
    'FailureState 1' = list(state=c('WorkingState 1'), cost=c(out$cm1)), #minimal repair
    'FailureState 2' = list(state=c('WorkingState 2', 'WorkingState 1'), cost=c(out$cm2, 450)),
    'FailureState 3' = list(state=c('WorkingState 3', 'WorkingState 1'), cost=c(out$cm3, 600)),
    'FailureState 4' = list(state=c('WorkingState 4', 'WorkingState 1'), cost=c(out$cm4, out$cr4)) #replacement or repair
  ),
  
  time_distributions <- list(
    'WorkingState 1' = c(shape = 1, rate = out$mu1),  # New condition
    'WorkingState 2' = c(shape = 1, rate = out$mu2),  # Minor Deterioration
    'WorkingState 3' = c(shape = 1, rate = out$mu3),  # Major Deterioration
    'WorkingState 4' = c(shape = 1, rate = out$mu4),   # Replacement (Failure)
    'FailureState 1' = c(shape = 1, rate = out$Theta1),  # New condition
    'FailureState 2' = c(shape = 1, rate = out$Theta2),  # Minor Deterioration
    'FailureState 3' = c(shape = 1, rate = out$Theta3),  # Major Deterioration
    'FailureState 4' = c(shape = 1, rate = out$Theta4)   # Replacement (Failure)
  ),
  
  
  prob_transition <- list(
    'WorkingState 1' = out$P1,  # New condition
    'WorkingState 2' = out$P2,  # Minor Deterioration
    'WorkingState 3' = out$P3,  # Major Deterioration
    'WorkingState 4' = NULL   # Replacement (Failure)
  )
  
)

names(model)<-c("transition_states", "time_distributions", "prob_transition")


# Calculating the isntantaneous unavailablity values along with the expected warranty cost

AverageWarrantyCost<- Proxel_s("workingState 1", W=2, model, delta=1, tol=0.0000001, alpha = 0.6, K=3)

```

### Contributions, Questions, Issues, and Feedback

Users interested in expanding functionalities in DDWM are welcome to do so. Issues reports are encouraged through GitHub's [issue tracker](https://github.com/parniSDU/DDWM/issues).

### Citation

If you use DDWM in your work, we kindly ask that you cite our paper.

```

