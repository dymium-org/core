# The script is where helper functions of the module should reside.
# Sometimes you may have a function that are used across the module, in many events,
# this is the central place for you to store this type of function. Hence, in every
# event scrips that you create using 'dymiumCore::use_event' this script, helpers.R,
# will be imported. If not needed, you may remove the import line.
#
# To use these helper functions inside your event script I suggest you import the helper script
# as the following 'helpers <- modules::import('{{{module_path}}}/helpers.R')'.

# If the package dymimCore is not needed you may remove the line below which imports it
modules::import('dymiumCore')

# If you need your constants here uncomment the line below
# constants <- modules::use('{{{module_path}}}/constants.R')
