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
//        Copyright © 2022 Salvador Diaz Fau. All rights reserved.
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

unit uCEFCommandHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefCommandHandlerOwn = class(TCefBaseRefCountedOwn, ICefCommandHandler)
    protected
      function  OnChromeCommand(const browser: ICefBrowser; command_id: integer; disposition: TCefWindowOpenDisposition): boolean; virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomCommandHandler = class(TCefCommandHandlerOwn)
    protected
      FEvents : Pointer;

      function  OnChromeCommand(const browser: ICefBrowser; command_id: integer; disposition: TCefWindowOpenDisposition): boolean; override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser;

function cef_command_handler_on_chrome_command(self        : PCefCommandHandler;
                                               browser     : PCefBrowser;
                                               command_id  : integer;
                                               disposition : TCefWindowOpenDisposition): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefCommandHandlerOwn) then
    Result := Ord(TCefCommandHandlerOwn(TempObject).OnChromeCommand(TCefBrowserRef.UnWrap(browser),
                                                                    command_id,
                                                                    disposition));
end;

constructor TCefCommandHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefCommandHandler));

  PCefCommandHandler(FData)^.on_chrome_command := {$IFDEF FPC}@{$ENDIF}cef_command_handler_on_chrome_command;
end;

function TCefCommandHandlerOwn.OnChromeCommand(const browser: ICefBrowser; command_id: integer; disposition: TCefWindowOpenDisposition): boolean;
begin
  Result := False;
end;

procedure TCefCommandHandlerOwn.RemoveReferences;
begin
  //
end;


// TCustomCommandHandler

constructor TCustomCommandHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomCommandHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomCommandHandler.RemoveReferences;
begin
  FEvents := nil;
end;

function TCustomCommandHandler.OnChromeCommand(const browser: ICefBrowser; command_id: integer; disposition: TCefWindowOpenDisposition): boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnChromeCommand(browser, command_id, disposition)
   else
    Result := inherited OnChromeCommand(browser, command_id, disposition);
end;

end.
