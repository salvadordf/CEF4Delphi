#!/bin/sh

CDIR=$(pwd)
cd "$(dirname "$0")"

../AppHelper/create_mac_helper_apps.sh ../../../bin/ExternalPumpBrowser.app

if [ "$(grep -i TCrCocoaApplication ../../../bin/ExternalPumpBrowser.app/Contents/Info.plist)" = "" ];
then
sed -i '' "1,4s/<dict>/<dict>\n  <key>NSPrincipalClass<\/key>\n  <string>TCrCocoaApplication<\/string>/" ../../../bin/ExternalPumpBrowser.app/Contents/Info.plist
fi

cd "$CDIR"
