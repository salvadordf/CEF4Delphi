unit uJSExtension_sp;

interface

procedure CreateGlobalCEFApp;

implementation

uses
  uCEFApplicationCore, uCEFConstants, uCEFv8Value, uCEFInterfaces, uCEFMiscFunctions,
  uTestExtensionHandler;

// BASIC CONCEPTS
// ==============

// Chromium uses several processes to carry out all the tasks needed to handle a web page :
// 1. The main application process is called "BROWSER PROCESS" and it runs the UI.
// 2. The layout and interpretation of HTML is done in the "RENDERER PROCESS".

// Read this for more details about Chromium's architecture :
// http://www.chromium.org/developers/design-documents/multi-process-architecture

// Each process is isolated from the rest and you need to use some kind of inter-process
// communication (IPC) to send information between them. This isolation and protection is
// guaranteed by the operating system and it's the main reason Chromium uses several processes.

// In many cases, you need to use JavaScript or visit the DOM to return some results to Delphi.
// The DOM and JavaScript live in the RENDERER PROCESS, while the Delphi code of your application
// lives in the BROWSER PROCESS.

// As commented before, the operating system isolates each process and this means that you
// can't access anything declared in one process like variables, fields, classes, controls, etc.
// from a different process.

// However, CEF has several ways to send information between processes and you can also use your
// own inter-process communication methods.

// If you need to execute some JavaScript code all you need is to call TChromium.ExecuteJavaScript
// from your application's code in the BROWSER PROCESS and CEF will take care of executing that
// code in the RENDERER PROCESS.

// If you need to send a message to the RENDERER PROCESS from the BROWSER PROCESS you can use
// TChromium.SendProcessMessage.

// To send messages to the BROWSER PROCESS from the RENDERER PROCESS you can use
// ICefFrame.SendProcessMessage


//     --------------   TChromium.SendProcessMessage  --------------
//     |            | ------------------------------> |            |
//     |  BROWSER   |                                 |  RENDERER  |
//     |            |                                 |            |
//     |  PROCESS   |   ICefFrame.SendProcessMessage  |  PROCESS   |
//     |            | <------------------------------ |            |
//     --------------                                 --------------


// To receive the messages sent from the RENDERER PROCESS you need to use the
// TChromium.OnProcessMessageReceived event. This event is executed in a CEF thread that belongs
// to the BROWSER PROCESS.

// To receive the messages sent from the BROWSER PROCESS you need to use the
// TCefApplication.OnProcessMessageReceived event (GlobalCEFApp.OnProcessMessageReceived).
// This event is executed in a CEF thread that belongs to the RENDERER PROCESS.


// JAVASCRIPT EXTENSIONS
// =====================

// CEF exposes a large number of JS features for integration in client applications.
// You can use JS types, arrays, functions, extensions, objects, etc.

// All of those features are described in detail here :
// https://bitbucket.org/chromiumembedded/cef/wiki/JavaScriptIntegration.md

// One of the most interesting JS features available in CEF are the JavaScript extensions because they
// can be used to execute custom Delphi code from JavaScript.

// If you put all you know so far together you can get any result or information in Delphi from
// JavaScript following these steps :
// 1. Use TChromium.ExecuteJavaScript to execute your custom JavaScript code.
// 2. That custom JavaScript code is executed in the RENDERER PROCESS and it can call functions in your
//    custom JavaScript extension, which executes Delphi code. This Delphi code is also executed in
//    the RENDERER PROCESS.
// 3. The Delphi code in the JavaScript extension can use ICefFrame.SendProcessMessage to send
//    information back to the BROWSER PROCESS.
// 4. The BROWSER PROCESS receives the information in the TChromium.OnProcessMessageReceived event.

// To create a JavaScript extension in CEF you have to create a new class that inherits from
// TCefv8HandlerOwn and it has to override the "execute" function. Open uTestExtensionHandler.pas
// to see an example and read this for more details about the "execute" parameters :
// https://magpcss.org/ceforum/apidocs3/projects/(default)/CefV8Handler.html

// In order to use that extension, you must register it in the GlobalCEFApp.OnWebKitInitialized event
// as you can see in the GlobalCEFApp_OnWebKitInitialized procedure on this PAS unit.

// You have to call the CefRegisterExtension function with 3 parameters :
// 1. name : The extension name.
// 2. code : Any valid JS code but in this case it includes 2 "native function" forward declarations.
// 3. Handler : An instance of your TCefv8HandlerOwn subclass.

// Notice that the code used with the CefRegisterExtension function in this demo is declaring
// "myextension.mouseover" as a function that calls the "mouseover" native function, and the
// "myextension.sendresulttobrowser" function that calls the "sendresulttobrowser" native function.

// The "execute" function in the custom TCefv8HandlerOwn subclass will compare the "name" parameter
// with the name of the of the native function used in the code that registered this extension.
// As you can see in this demo, TTestExtensionHandler.Execute compares the "name" parameter with
// "mouseover" and "sendresulttobrowser" to execute the code you want for each of those custom functions.

// TTestExtensionHandler.Execute is executed in the RENDERER PROCESS and it uses a process message
// to send some results to he BROWSER PROCESS.
// It uses TCefv8ContextRef.Current.Browser.MainFrame to call the SendProcessMessage procedure for
// the main frame.

// The message is a TCefProcessMessageRef instance and you can set the information you want to send using
// its ArgumentList property.

// You can add several items to ArgumentList using different indexes in the SetString, SetInt, SetBool,
// SetBinary, etc. functions.
// There is a size limit in the binary parameters of only a few kilobytes. Compress the binary data, use
// alternative IPC methods or use a database protected by a mutex if necessary.

// For more information about this, read the following pages :
// https://bitbucket.org/chromiumembedded/cef/wiki/JavaScriptIntegration.md#markdown-header-functions-and-extensions
// https://bitbucket.org/chromiumembedded/cef/src/b6b033a52bb3f7059d169a4c8389966c8fe2531b/include/capi/cef_v8_capi.h#lines-924


// TESTING THIS DEMO :
// ===================
// 1. Run the demo and wait until google.com is loaded
// 2. Right-click and select the "Set the mouseover event" menu option.
// 3. Move the mouse pointer over the web page and see the HTML elements in the status bar.

// When you select the "Set the mouseover event" menu option, the
// TChromium.OnContextMenuCommand event is triggered and it adds an event listener to the
// document's body. That listener calls one of the functions available in the registered
// extension called "myextension.mouseover".

// The TTestExtensionHandler.Execute function in the extension is executed and it
// uses TCefv8ContextRef.Current.Browser.MainFrame.SendProcessMessage(PID_BROWSER, msg)
// to send a process message with the results to the browser process.

// That message is received in the TChromium.OnProcessMessageReceived event and it shows
// the information in the status bar.

// If you have to debug the code executed by the extension you will need to use the
// debugging methods described in
// https://www.briskbard.com/index.php?lang=en&pageid=cef

procedure GlobalCEFApp_OnWebKitInitialized;
var
  TempExtensionCode : string;
  TempHandler       : ICefv8Handler;
begin
  try
    // This is a JS extension example with 2 functions and several parameters.
    // Please, read the "JavaScript Integration" wiki page at
    // https://bitbucket.org/chromiumembedded/cef/wiki/JavaScriptIntegration.md

    TempExtensionCode := 'var myextension;' +
                         'if (!myextension)' +
                         '  myextension = {};' +
                         '(function() {' +
                         '  myextension.mouseover = function(a) {' +
                         '    native function mouseover();' +
                         '    mouseover(a);' +
                         '  };' +
                         '  myextension.sendresulttobrowser = function(b,c) {' +
                         '    native function sendresulttobrowser();' +
                         '    sendresulttobrowser(b,c);' +
                         '  };' +
                         '})();';

    TempHandler := TTestExtensionHandler.Create;

    if CefRegisterExtension('myextension', TempExtensionCode, TempHandler) then
      {$IFDEF DEBUG}CefDebugLog('JavaScript extension registered successfully!'){$ENDIF}
     else
      {$IFDEF DEBUG}CefDebugLog('There was an error registering the JavaScript extension!'){$ENDIF};
  finally
    TempHandler := nil;
  end;
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                     := TCefApplicationCore.Create;
  GlobalCEFApp.OnWebKitInitialized := GlobalCEFApp_OnWebKitInitialized;
  {$IFDEF DEBUG}
  GlobalCEFApp.LogFile             := 'debug.log';
  GlobalCEFApp.LogSeverity         := LOGSEVERITY_INFO;
  {$ENDIF}
  GlobalCEFApp.StartSubProcess;
end;

end.
