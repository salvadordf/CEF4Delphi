BrowserWindowOSR

# ABOUT

This example uses 
  TLazarusBrowserWindowOSR
  Examining DOM

TCEFWorkScheduler feeds the CEF messageloop by calling DoMessageLoopWork(). On Mac this is currently the only way to run the CEF messageloop.


# SETUP

** Windows
1) Download the CEF framework and place the content of the "Release" folder into the same folder as your exe.
  Alternatively you can point "GlobalCEFApp.FrameworkDirPath" to the location with the libraries.
2) Run the project

** Linux
1) Download the CEF framework and place the content of the "Release" folder into the same folder as your exe.
  Alternatively you can point "GlobalCEFApp.FrameworkDirPath" to the location with the libraries.
2) Run the project

Note:
- For your own Linux project you must modify the project source (lpr) and add "InitSubProcess" to the "uses" clause, so that it is in the list *before* the unit "Interfaces".
- The call to "DestroyGlobalCEFApp" must be in a unit *not* used by "unit InitSubProcess" (including not used in any nested way).


** Mac
1) Go to "project options" and create the "App Bundle"
2) Download the CEF framework and place the content of the "Release" folder into BrowserWindowOsrDom.app/Contents/Frameworks/Chromium Embedded Framework.framework
You should have:
  Chromium Embedded Framework
  Libraries/*
  Resources/*
3) Open project "AppHelper" (from the subfolder in this project's folder), create App Bundle and compile the AppHelper.
   Run create_mac_helper.sh
4) Open project BrowserWindowOsrDom, compile and run


