#!/bin/sh

BASEDIR=$(dirname "$0")
DEST=$1
SRC=$2

if [ "$SRC" = "" ];
then
  SRC=$BASEDIR/../../../../bin/AppHelper.app
fi

if [ "$1" = "" ] || [ ! -e "$DEST" ] || [ ! -e "$SRC" ];
then
  echo "Usage"
  echo " $0 destpath/project.app"
  echo " $0 destpath/project.app  sourcedir/AppHelper.app"
  echo
  if [ ! -e "$DEST" ];
  then
    echo "Error: Target app bundle not found. (Did you compile AND create the bundle?)"
  fi
  if [ ! -e "$SRC" ];
  then
    echo "Error: Source (AppHelper) app bundle not found. (Did you compile AND create the bundle?)"
  fi
  exit;
fi

SRCAPP=$(basename "$SRC")
SRCAPP="${SRCAPP%\.app}"
DESTAPP=$(basename "$DEST")
DESTAPP="${DESTAPP%\.app}"

SUB=""
rm -rf "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app"
cp -r "$SRC" "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app"
mv "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app/Contents/MacOS/$SRCAPP" "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app/Contents/MacOS/$DESTAPP Helper$SUB"
sed -i '' "s/$SRCAPP/$DESTAPP Helper$SUB/g" "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app/Contents/Info.plist"

SUB=" (GPU)"
rm -rf "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app"
cp -r "$SRC" "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app"
mv "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app/Contents/MacOS/$SRCAPP" "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app/Contents/MacOS/$DESTAPP Helper$SUB"
sed -i '' "s/$SRCAPP/$DESTAPP Helper$SUB/g" "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app/Contents/Info.plist"

SUB=" (Renderer)"
rm -rf "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app"
cp -r "$SRC" "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app"
mv "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app/Contents/MacOS/$SRCAPP" "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app/Contents/MacOS/$DESTAPP Helper$SUB"
sed -i '' "s/$SRCAPP/$DESTAPP Helper$SUB/g" "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app/Contents/Info.plist"

SUB=" (Plugin)"
rm -rf "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app"
cp -r "$SRC" "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app"
mv "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app/Contents/MacOS/$SRCAPP" "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app/Contents/MacOS/$DESTAPP Helper$SUB"
sed -i '' "s/$SRCAPP/$DESTAPP Helper$SUB/g" "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app/Contents/Info.plist"

SUB=" (Alerts)"
rm -rf "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app"
cp -r "$SRC" "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app"
mv "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app/Contents/MacOS/$SRCAPP" "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app/Contents/MacOS/$DESTAPP Helper$SUB"
sed -i '' "s/$SRCAPP/$DESTAPP Helper$SUB/g" "$DEST/Contents/Frameworks/$DESTAPP Helper$SUB.app/Contents/Info.plist"

