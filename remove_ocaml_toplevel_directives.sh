#!/bin/bash
sed 's/^#.*$/(* AUTOCOMMENTED BY SCRIPT: \0 *)/' $@
