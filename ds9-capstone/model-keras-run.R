library(tfruns)

# tuning parameters
runs <- tuning_run("model-keras-tuning.R", 
                   flags = list(activationType = c("tanh", "relu", "leaky-relu", "selu"),
                                optimizerType  = c("adam")
                                #epochSize      = 30 #seq(1, 25, by=2)
                   ))
runs
