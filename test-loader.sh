#!/bin/bash
#
# test_loader.sh
# 
# Usage: ./test_loader.sh <input_file>
#

input_file=$1

$FACTORIE_ROOT/run_class.sh cc.factorie.app.nlp.load.LoadTigerConll09 $input_file

