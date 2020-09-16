#!/usr/bin/env fish

set -lx target_time $argv[1]

if test -z "$target_time"
   set -x target_time "now"
end

set -lx target_epoch (date +%s -d "$target_time")

echo (echo "Pacific:"; date +%R -d @"$target_epoch"; echo "/ Zulu:"; date +%R --utc -d @"$target_epoch"; echo "/ AEST:"; bash -c "TZ=Australia/Sydney date +%R -d @\"$target_epoch\"") | tr '\n' ' ' ; echo ""
