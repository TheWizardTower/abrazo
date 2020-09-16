#!/usr/bin/sh

iostat -c -o JSON | jq '.sysstat.hosts[0].statistics[0]."avg-cpu".system'
