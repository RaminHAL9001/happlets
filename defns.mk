# Name your *.cabal file.
CABAL_FILE := happlets.cabal

# list of top-level directory paths in the "./src/" directory. this should be a
# "sed" regular expression, so if you have more than one top-level director,
# list it like so:
#  module_top := data\|language\|control/concurrent\|text/parsercombinator
MODULE_TOP := Happlets

