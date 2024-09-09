CEF4Delphi may report an invalid macOS version. In that case, use the -WMxx.y custom option to set a higher minimum macOS version for that project.

You might also need to sign the application with an "Outgoing network socket" entitlement in the helpers.

Set LSUIElement=1 in the info.plist to hide the helpers in the dock.