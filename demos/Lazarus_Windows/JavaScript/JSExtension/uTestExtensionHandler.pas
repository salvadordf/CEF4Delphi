// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2021 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uTestExtensionHandler;

{$MODE Delphi}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows,
  {$ELSE}
  LCLIntf, LCLType, LMessages,
  {$ENDIF}
  uCEFRenderProcessHandler, uCEFBrowserProcessHandler, uCEFInterfaces, uCEFProcessMessage,
  uCEFv8Context, uCEFTypes, uCEFv8Handler;

type
  TTestExtensionHandler = class(TCefv8HandlerOwn)
    protected
      function Execute(const name: ustring; const obj: ICefv8Value; const arguments: TCefv8ValueArray; var retval: ICefv8Value; var exception: ustring): Boolean; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFConstants, uJSExtension;

function TTestExtensionHandler.Execute(const name      : ustring;
                                       const obj       : ICefv8Value;
                                       const arguments : TCefv8ValueArray;
                                       var   retval    : ICefv8Value;
                                       var   exception : ustring): Boolean;
var
  TempMessage : ICefProcessMessage;
  TempFrame   : ICefFrame;
begin
  Result := False;

  try
    if (name = 'mouseover') then
      begin
        if (length(arguments) > 0) and arguments[0].IsString then
          begin
            TempMessage := TCefProcessMessageRef.New(MOUSEOVER_MESSAGE_NAME);
            TempMessage.ArgumentList.SetString(0, arguments[0].GetStringValue);

            TempFrame := TCefv8ContextRef.Current.Browser.MainFrame;

            if (TempFrame <> nil) and TempFrame.IsValid then
              TempFrame.SendProcessMessage(PID_BROWSER, TempMessage);
          end;

        Result := True;
      end
     else
      if (name = 'sendresulttobrowser') then
        begin
          if (length(arguments) > 1) and arguments[0].IsString and arguments[1].IsString then
            begin
              TempMessage := TCefProcessMessageRef.New(arguments[1].GetStringValue);
              TempMessage.ArgumentList.SetString(0, arguments[0].GetStringValue);

              TempFrame := TCefv8ContextRef.Current.Browser.MainFrame;

              if (TempFrame <> nil) and TempFrame.IsValid then
                TempFrame.SendProcessMessage(PID_BROWSER, TempMessage);
            end;

          Result := True;
        end;
  finally
    TempMessage := nil;
  end;
end;

end.
