#!/usr/bin/env fish

echo (echo "Pacific:"; date +%R; echo "/ Zulu:"; date +%R --utc; echo "/ AEST:"; bash -c "TZ=Australia/Sydney date +%R") | tr '\n' ' ' ; echo ""
