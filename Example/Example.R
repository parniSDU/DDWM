#Running Example

library(DDWM)

#K=3; a=1; SimSize<-500
#path<-paste("C:/Users/parni/OneDrive - Syddansk Universitet/ftaproximExtension/RESS/Data/SimData_K",K,"_alpha",a,"_size",SimSize,".txt", sep = "")

# reading and preprocessing the data
d<-read.table(path, header = TRUE)
d2<-PreData(d, 2)

# State estimation
result<-StateEstimation(d2, policy = 2)

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


# Calculating the isntantaneous unavailablity values along with the average warranty cost

AverageWarrantyCost<- Proxel_s("workingState 1", W=2, model, delta=1, tol=0.0000001, alpha = 0.6, K=3)
