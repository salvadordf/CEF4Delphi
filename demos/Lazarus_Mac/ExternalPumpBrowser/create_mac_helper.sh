#!/bin/sh

CDIR=$(pwd)
cd "$(dirname "$0")"

../../Lazarus_any_OS/AppHelper/create_mac_helper_apps.sh ../../../bin/ExternalPumpBrowser.app

cd "$CDIR"
