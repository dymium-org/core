# Constant values to be used within the module can be declared here.
#
# To use these constants inside your event script you may import the constants script
# as the following 'constants <- modules::import('{{{module_path}}}/constants.R')'.
#
# The following codes are examples of how you may declare your constants.
modules::export('MYFIRST_CONSTANT') # must export your constants, individually.
MYFIRST_CONSTANT <- list()
MYFIRST_CONSTANT$SOMEVALUE1 <- 'VALUE1'
MYFIRST_CONSTANT$SOMEVALUE2 <- 'VALUE2'

modules::export('MYSECOND_CONSTANT') # must export your constants, individually.
MYSECOND_CONSTANT <- list()
MYSECOND_CONSTANT$SOMEVALUE1 <- 'VALUE1'
MYSECOND_CONSTANT$SOMEVALUE2 <- 'VALUE2'
