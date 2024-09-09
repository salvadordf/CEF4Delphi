ExternalPumpBrowser

# ABOUT

This example uses 
  TCEFLinkedWindowParent
  TCEFWorkScheduler 

TCEFWorkScheduler feeds the CEF messageloop by calling DoMessageLoopWork(). On Mac this is currently the only way to run the CEF messageloop.


# SETUP

1) Go to "project options" and create the "App Bundle"
2) Download the CEF framework and place the content of the "Release" folder into ExternalPumpBrowser.app/Contents/Frameworks/Chromium Embedded Framework.framework
You should have:
  Chromium Embedded Framework
  Libraries/*
  Resources/*
3) Open project "AppHelper" (from demos/Lazarus_any_OS ), create App Bundle and compile the AppHelper.
   Run demos/Lazarus_Mac/ExternalPumpBrowser/create_mac_helper.sh
4) Open project ExternalPumpBrowser, compile and run

CEF4Delphi may report an invalid macOS version. In that case, use the -WMxx.y custom option to set a higher minimum macOS version for that project.

You might also need to sign the application with an "Outgoing network socket" entitlement in the helpers.

Set LSUIElement=1 in the info.plist to hide the helpers in the dock.

