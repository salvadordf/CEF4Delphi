unit uCEFSchemeRegistrar;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseScopedWrapper, uCEFTypes;

type
  /// <summary>
  /// Class that manages custom scheme registrations.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_scheme_capi.h">CEF source file: /include/capi/cef_scheme_capi.h (cef_scheme_registrar_t)</see></para>
  /// </remarks>
  TCefSchemeRegistrarRef = class(TCEFBaseScopedWrapperRef)
    public
      /// <summary>
      /// <para>Register a custom scheme. This function should not be called for the
      /// built-in HTTP, HTTPS, FILE, FTP, ABOUT and DATA schemes.</para>
      /// <para>This function may be called on any thread. It should only be called once
      /// per unique |scheme_name| value. If |scheme_name| is already registered or
      /// if an error occurs this function will return false (0).</para>
      /// </summary>
      /// <remarks>
      /// <para><see>See the CEF_SCHEME_OPTION_* constants in the uCEFConstants unit for possible values for |options|.</see></para>
      /// </remarks>
      function AddCustomScheme(const schemeName: ustring; options : TCefSchemeOptions): Boolean;
  end;

implementation

uses
  uCEFMiscFunctions;

function TCefSchemeRegistrarRef.AddCustomScheme(const schemeName : ustring;
                                                      options    : TCefSchemeOptions): Boolean;
var
  TempName : TCefString;
begin
  TempName := CefString(schemeName);
  Result   := PCefSchemeRegistrar(FData)^.add_custom_scheme(PCefSchemeRegistrar(FData),
                                                            @TempName,
                                                            options) <> 0;
end;

end.
