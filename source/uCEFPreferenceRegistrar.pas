unit uCEFPreferenceRegistrar;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseScopedWrapper, uCEFTypes, uCEFInterfaces;

type
  /// <summary>
  /// Class that manages custom preference registrations.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_preference_capi.h">CEF source file: /include/capi/cef_preference_capi.h (cef_preference_registrar_t)</see></para>
  /// </remarks>
  TCefPreferenceRegistrarRef = class(TCEFBaseScopedWrapperRef)
    public
      /// <summary>
      /// Register a preference with the specified |name| and |default_value|. To
      /// avoid conflicts with built-in preferences the |name| value should contain
      /// an application-specific prefix followed by a period (e.g. "myapp.value").
      /// The contents of |default_value| will be copied. The data type for the
      /// preference will be inferred from |default_value|'s type and cannot be
      /// changed after registration. Returns true (1) on success. Returns false (0)
      /// if |name| is already registered or if |default_value| has an invalid type.
      /// This function must be called from within the scope of the
      /// ICefBrowserProcessHandler.OnRegisterCustomPreferences callback.
      /// </summary>
      function AddPreference(const name: ustring; const default_value: ICefValue): boolean;
      /// <summary>
      /// Override the default value of a previously registered preference. |value|
      /// is required and must have the same type as the registered default. Its
      /// contents will be copied. User and managed values take precedence over this
      /// default. Returns false (0) and sets |error| if the preference is not
      /// registered or |value| is invalid or has the wrong type. This function must
      /// be called from within OnRegisterCustomPreferences.
      /// </summary>
      function SetDefaultPreference(const name: ustring; const value: ICefValue; var error: ustring): boolean;
      /// <summary>
      /// Returns a copy of the current default value for the preference with the
      /// specified |name|, or nil if the preference is not registered. This
      /// function must be called from within OnRegisterCustomPreferences.
      /// </summary>
      function GetDefaultPreference(const name: ustring): ICefValue;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFValue;

function TCefPreferenceRegistrarRef.AddPreference(const name          : ustring;
                                                  const default_value : ICefValue): Boolean;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   := PCefPreferenceRegistrar(FData)^.add_preference(PCefPreferenceRegistrar(FData),
                                                             @TempName,
                                                             CefGetData(default_value)) <> 0;
end;

function TCefPreferenceRegistrarRef.SetDefaultPreference(const name  : ustring;
                                                         const value : ICefValue;
                                                         var   error : ustring): boolean;
var
  TempName, TempError : TCefString;
begin
  TempName := CefString(name);
  Result   := PCefPreferenceRegistrar(FData)^.set_default_preference(PCefPreferenceRegistrar(FData),
                                                                     @TempName,
                                                                     CefGetData(value),
                                                                     @TempError) <> 0;
  error    := CefString(@TempError);
end;

function TCefPreferenceRegistrarRef.GetDefaultPreference(const name: ustring): ICefValue;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   := TCefValueRef.UnWrap(PCefPreferenceRegistrar(FData)^.get_default_preference(PCefPreferenceRegistrar(FData),
                                                                                         @TempName));
end;

end.
