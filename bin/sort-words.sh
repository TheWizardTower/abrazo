#!/usr/bin/env bash

tr ' ' '\n' | sort | tr '\n' ' ' | sed 's/^[ \t]*//;s/[ \t]*$//'
