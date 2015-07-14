#!/bin/bash
#
# test_loader.sh
# 
# Usage: ./test_loader.sh <input_file>
#

#input_file=$1

$FACTORIE_ROOT/run_class.sh cc.factorie.app.nlp.pos.GermanChainPosTrainer \
--train-file=/Users/Esther/Documents/Summer2015/German_training/tiger_release_aug07.corrected.16012013.conll09_training \
--test-file=/Users/Esther/Documents/Summer2015/German_test/tiger_release_aug07.corrected.16012013.conll09_test \
--model=GermanPosTagger.factorie 


