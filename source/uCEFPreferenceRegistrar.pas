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
      function AddPreference(const name: ustring; const default_value: ICefValue): Boolean;
  end;

implementation

uses
  uCEFMiscFunctions;

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

end.
