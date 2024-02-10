#!/bin/bash

release="/home/octa/osn/upgrade.tar.bz2"

if [[ -e $release ]]; then
    tar jxf $release -C /home/octa
    rm $release
fi
