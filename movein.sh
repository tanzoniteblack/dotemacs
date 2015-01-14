#!/bin/bash
TARGET=$1

scp ~/.environment $TARGET:.environment
cat setup-machine.sh | ssh $TARGET bash
