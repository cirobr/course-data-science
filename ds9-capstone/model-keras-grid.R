library(tfruns)

# tuning parameters
runs <- tuning_run("model-keras-tuning.R", 
                   flags = list(activationType = c("relu", 
                                                   "tanh", 
                                                   "sigmoid", 
                                                   "elu", 
                                                   "selu", 
                                                   "hard_sigmoid", 
                                                   "exponential", 
                                                   "gelu", 
                                                   "swish"),
                                optimizerType  = c("adam")
                   ))
runs
