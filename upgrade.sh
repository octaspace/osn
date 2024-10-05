#!/bin/bash

release="/home/octa/osn/upgrade.tar.gz"

if [[ -e $release ]]; then
    tar zxf $release -C /home/octa
    rm $release
fi
