#!/bin/bash
#
# test_loader.sh
# 
# Usage: ./test_loader.sh <input_file>
#

#input_file=$1

$FACTORIE_ROOT/run_class.sh cc.factorie.app.nlp.pos.HindiChainPosTrainer \
--train-file=/Users/Esther/Documents/Summer2015/hindmonocorp05_training_modified \
--test-file=/Users/Esther/Documents/Summer2015/hindmonocorp05_test_modified \
--model=HindiPosTagger.factorie 


