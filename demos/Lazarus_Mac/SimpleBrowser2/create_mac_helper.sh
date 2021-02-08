#!/bin/sh

CDIR=$(pwd)
cd "$(dirname "$0")"

../AppHelper/create_mac_helper_apps.sh ../../../bin/SimpleBrowser2.app

cd "$CDIR"
