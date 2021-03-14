#!/bin/sh

CDIR=$(pwd)
cd "$(dirname "$0")"

./AppHelper/create_mac_helper_apps.sh ../../../bin/BrowserWindowOsrDom.app

if [ "$(grep -i TCrCocoaApplication ../../../bin/BrowserWindowOsrDom.app/Contents/Info.plist)" = "" ];
then
sed -i '' "1,4s/<dict>/<dict>\n  <key>NSPrincipalClass<\/key>\n  <string>TCrCocoaApplication<\/string>/" ../../../bin/BrowserWindowOsrDom.app/Contents/Info.plist
fi

cd "$CDIR"
