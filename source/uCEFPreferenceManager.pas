unit uCEFPreferenceManager;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  /// <summary>
  /// Manage access to preferences. Many built-in preferences are registered by
  /// Chromium. Custom preferences can be registered in
  /// ICefBrowserProcessHandler.OnRegisterCustomPreferences.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_preference_capi.h">CEF source file: /include/capi/cef_preference_capi.h (cef_preference_manager_t)</see></para>
  /// </remarks>
  TCefPreferenceManagerRef = class(TCefBaseRefCountedRef, ICefPreferenceManager)
    protected
      /// <summary>
      /// Returns true (1) if a preference with the specified |name| exists. This
      /// function must be called on the browser process UI thread.
      /// </summary>
      function  HasPreference(const name: ustring): Boolean;
      /// <summary>
      /// Returns the value for the preference with the specified |name|. Returns
      /// NULL if the preference does not exist. The returned object contains a copy
      /// of the underlying preference value and modifications to the returned
      /// object will not modify the underlying preference value. This function must
      /// be called on the browser process UI thread.
      /// </summary>
      function  GetPreference(const name: ustring): ICefValue;
      /// <summary>
      /// Returns the value for the preference with the specified |name|. Returns
      /// NULL if the preference does not exist. The returned object contains a copy
      /// of the underlying preference value and modifications to the returned
      /// object will not modify the underlying preference value. This function must
      /// be called on the browser process UI thread.
      /// </summary>
      function  GetAllPreferences(includeDefaults: Boolean): ICefDictionaryValue;
      /// <summary>
      /// Returns true (1) if the preference with the specified |name| can be
      /// modified using SetPreference. As one example preferences set via the
      /// command-line usually cannot be modified. This function must be called on
      /// the browser process UI thread.
      /// </summary>
      function  CanSetPreference(const name: ustring): Boolean;
      /// <summary>
      /// Returns true (1) if the preference with the specified |name| can be
      /// modified using SetPreference. As one example preferences set via the
      /// command-line usually cannot be modified. This function must be called on
      /// the browser process UI thread.
      /// </summary>
      function  SetPreference(const name: ustring; const value: ICefValue; out error: ustring): Boolean;
      /// <summary>
      /// Add an observer for preference changes. |name| is the name of the
      /// preference to observe. If |name| is NULL then all preferences will be
      /// observed. Observing all preferences has performance consequences and is
      /// not recommended outside of testing scenarios. The observer will remain
      /// registered until the returned Registration object is destroyed. This
      /// function must be called on the browser process UI thread.
      /// </summary>
      function  AddPreferenceObserver(const name: ustring; const observer: ICefPreferenceObserver): ICefRegistration;
    public
      class function  UnWrap(data: Pointer): ICefPreferenceManager;
      /// <summary>
      /// Returns the global preference manager object.
      /// </summary>
      class function  Global: ICefPreferenceManager;
      /// <summary>
      /// Returns the current Chrome Variations configuration (combination of field
      /// trials and chrome://flags) as equivalent command-line switches
      /// (`--[enable|disable]-features=XXXX`, etc). These switches can be used to
      /// apply the same configuration when launching a CEF-based application. See
      /// https://developer.chrome.com/docs/web-platform/chrome-variations for
      /// background and details. Note that field trial tests are disabled by default
      /// in Official CEF builds (via the `disable_fieldtrial_testing_config=true (1)`
      /// GN flag). This function must be called on the browser process UI thread.
      /// </summary>
      class procedure GetChromeVariationsAsSwitches(const switches: TStrings);
      /// <summary>
      /// Returns the current Chrome Variations configuration (combination of field
      /// trials and chrome://flags) as human-readable strings. This is the human-
      /// readable equivalent of the "Active Variations" section of chrome://version.
      /// See https://developer.chrome.com/docs/web-platform/chrome-variations for
      /// background and details. Note that field trial tests are disabled by default
      /// in Official CEF builds (via the `disable_fieldtrial_testing_config=true (1)`
      /// GN flag). This function must be called on the browser process UI thread.
      /// </summary>
      class procedure GetChromeVariationsAsStrings(const strings: TStrings);
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFValue, uCEFDictionaryValue,
  uCEFRegistration, uCEFStringList;

class function TCefPreferenceManagerRef.UnWrap(data: Pointer): ICefPreferenceManager;
begin
  if (data <> nil) then
    Result := Create(data) as ICefPreferenceManager
   else
    Result := nil;
end;

class function TCefPreferenceManagerRef.Global: ICefPreferenceManager;
begin
  Result := UnWrap(cef_preference_manager_get_global());
end;

class procedure TCefPreferenceManagerRef.GetChromeVariationsAsSwitches(const switches: TStrings);
var
  TempSL : ICefStringList;
begin
  if (switches <> nil) then
    begin
      TempSL := TCefStringListOwn.Create;
      cef_preference_manager_get_chrome_variations_as_switches(TempSL.Handle);
      TempSL.CopyToStrings(switches);
    end;
end;

class procedure TCefPreferenceManagerRef.GetChromeVariationsAsStrings(const strings: TStrings);
var
  TempSL : ICefStringList;
begin
  if (strings <> nil) then
    begin
      TempSL := TCefStringListOwn.Create;
      cef_preference_manager_get_chrome_variations_as_strings(TempSL.Handle);
      TempSL.CopyToStrings(strings);
    end;
end;

function TCefPreferenceManagerRef.HasPreference(const name: ustring): Boolean;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   := PCefPreferenceManager(FData)^.has_preference(PCefPreferenceManager(FData), @TempName) <> 0;
end;

function TCefPreferenceManagerRef.GetPreference(const name: ustring): ICefValue;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   := TCefValueRef.UnWrap(PCefPreferenceManager(FData)^.get_preference(PCefPreferenceManager(FData), @TempName));
end;

function TCefPreferenceManagerRef.GetAllPreferences(includeDefaults: Boolean): ICefDictionaryValue;
begin
  Result := TCefDictionaryValueRef.UnWrap(PCefPreferenceManager(FData)^.get_all_preferences(PCefPreferenceManager(FData), Ord(includeDefaults)));
end;

function TCefPreferenceManagerRef.CanSetPreference(const name: ustring): Boolean;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   := PCefPreferenceManager(FData)^.can_set_preference(PCefPreferenceManager(FData), @TempName) <> 0;
end;

function TCefPreferenceManagerRef.SetPreference(const name  : ustring;
                                                const value : ICefValue;
                                                out   error : ustring): Boolean;
var
  TempName, TempError : TCefString;
begin
  CefStringInitialize(@TempError);

  TempName := CefString(name);
  Result   := PCefPreferenceManager(FData)^.set_preference(PCefPreferenceManager(FData), @TempName, CefGetData(value), @TempError) <> 0;
  error    := CefStringClearAndGet(@TempError);
end;

function TCefPreferenceManagerRef.AddPreferenceObserver(const name: ustring; const observer: ICefPreferenceObserver): ICefRegistration;
var
  TempName    : TCefString;
  TempNamePtr : Pointer;
begin
  if (length(name) > 0) then
    begin
      TempName    := CefString(name);
      TempNamePtr := @TempName;
    end
   else
    TempNamePtr := nil;

  Result := TCefRegistrationRef.UnWrap(PCefPreferenceManager(FData)^.add_preference_observer(PCefPreferenceManager(FData), TempNamePtr, CefGetData(observer)));
end;

end.
