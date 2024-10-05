#!/bin/bash

release="/home/octa/osn/upgrade.tar.gz"

if [[ -e $release ]]; then
    tar jxf $release -C /home/octa
    rm $release
fi
