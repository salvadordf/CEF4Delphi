#!/bin/sh

CDIR=$(pwd)
cd "$(dirname "$0")"

./AppHelper/create_mac_helper_apps.sh ../../../bin/BrowserWindowDom.app

if [ "$(grep -i TCrCocoaApplication ../../../bin/BrowserWindowDom.app/Contents/Info.plist)" = "" ];
then
sed -i '' "1,4s/<dict>/<dict>\n  <key>NSPrincipalClass<\/key>\n  <string>TCrCocoaApplication<\/string>/" ../../../bin/BrowserWindowDom.app/Contents/Info.plist
fi

cd "$CDIR"
