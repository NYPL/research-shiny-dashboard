#!/bin/bash

set -e
set -x

# the scan and deliver derivates (ready for dashboard app consumption)
# are already handled by https://github.com/NYPL/scan-and-deliver-stats

# so we'll just copy them to the target folder here
# maybe once day we'll use sym-links

LOCATION="/home/tony/Dropbox/NYPL/scan-and-deliver/target"
echo ""

rsync -Phav $LOCATION/*.dat ./target/;
