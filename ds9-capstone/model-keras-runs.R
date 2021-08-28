library(tfruns)

# tuning parameters
runs <- tuning_run("model-keras-tuning.R", 
                   flags = list(activationType = c("relu", "tanh","sigmoid" ,"leaky-relu", "selu"),
                                optimizerType  = c("adam")
                   ))
runs
