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

unit uCEFUrlRequestClientComponent;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.Messages, WinApi.ActiveX,{$ENDIF}
    System.Classes, System.Math,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows, ActiveX,{$ENDIF} Classes, Math,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, LMessages, InterfaceBase,
    {$ELSE}
    Messages,
    {$ENDIF}
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFUrlRequestClientEvents, uCEFUrlrequestClient, uCEFUrlRequest;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
  TCEFUrlRequestClientComponent = class(TComponent, ICEFUrlRequestClientEvents)
    protected
      FClient               : ICefUrlrequestClient;
      FThreadID             : TCefThreadId;

      FOnRequestComplete    : TOnRequestComplete;
      FOnUploadProgress     : TOnUploadProgress;
      FOnDownloadProgress   : TOnDownloadProgress;
      FOnDownloadData       : TOnDownloadData;
      FOnGetAuthCredentials : TOnGetAuthCredentials;
      FOnCreateURLRequest   : TNotifyEvent;

      // ICefUrlrequestClient
      procedure doOnRequestComplete(const request: ICefUrlRequest);
      procedure doOnUploadProgress(const request: ICefUrlRequest; current, total: Int64);
      procedure doOnDownloadProgress(const request: ICefUrlRequest; current, total: Int64);
      procedure doOnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
      function  doOnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;

      // Custom
      procedure doOnCreateURLRequest;

      procedure DestroyRequestClient;

    public
      constructor Create(AOwner: TComponent); override;
      procedure   AfterConstruction; override;
      procedure   BeforeDestruction; override;

      procedure   AddURLRequest;

      property Client               : ICefUrlrequestClient   read FClient;
      property ThreadID             : TCefThreadId           read FThreadID              write FThreadID;

    published
      property OnRequestComplete    : TOnRequestComplete     read FOnRequestComplete     write FOnRequestComplete;
      property OnUploadProgress     : TOnUploadProgress      read FOnUploadProgress      write FOnUploadProgress;
      property OnDownloadProgress   : TOnDownloadProgress    read FOnDownloadProgress    write FOnDownloadProgress;
      property OnDownloadData       : TOnDownloadData        read FOnDownloadData        write FOnDownloadData;
      property OnGetAuthCredentials : TOnGetAuthCredentials  read FOnGetAuthCredentials  write FOnGetAuthCredentials;
      property OnCreateURLRequest   : TNotifyEvent           read FOnCreateURLRequest    write FOnCreateURLRequest;
  end;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

// *********************************************************
// ********************** ATTENTION ! **********************
// *********************************************************
// **                                                     **
// **  MANY OF THE EVENTS IN CEF4DELPHI COMPONENTS LIKE   **
// **  TCHROMIUM, TFMXCHROMIUM OR TCEFAPPLICATION ARE     **
// **  EXECUTED IN A CEF THREAD BY DEFAULT.               **
// **                                                     **
// **  WINDOWS CONTROLS MUST BE CREATED AND DESTROYED IN  **
// **  THE SAME THREAD TO AVOID ERRORS.                   **
// **  SOME OF THEM RECREATE THE HANDLERS IF THEY ARE     **
// **  MODIFIED AND CAN CAUSE THE SAME ERRORS.            **
// **                                                     **
// **  DON'T CREATE, MODIFY OR DESTROY WINDOWS CONTROLS   **
// **  INSIDE THE CEF4DELPHI EVENTS AND USE               **
// **  SYNCHRONIZATION OBJECTS TO PROTECT VARIABLES AND   **
// **  FIELDS IF THEY ARE ALSO USED IN THE MAIN THREAD.   **
// **                                                     **
// **  READ THIS FOR MORE INFORMATION :                   **
// **  https://www.briskbard.com/index.php?pageid=cef     **
// **                                                     **
// **  USE OUR FORUMS FOR MORE QUESTIONS :                **
// **  https://www.briskbard.com/forum/                   **
// **                                                     **
// *********************************************************
// *********************************************************

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFRequest, uCEFTask, uCEFMiscFunctions;


constructor TCEFUrlRequestClientComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FClient               := nil;
  FThreadID             := TID_UI;
  FOnRequestComplete    := nil;
  FOnUploadProgress     := nil;
  FOnDownloadProgress   := nil;
  FOnDownloadData       := nil;
  FOnGetAuthCredentials := nil;
  FOnCreateURLRequest   := nil;
end;

procedure TCEFUrlRequestClientComponent.AfterConstruction;
begin
  inherited AfterConstruction;

  if not(csDesigning in ComponentState) then
    FClient := TCustomCefUrlrequestClient.Create(self);
end;

procedure TCEFUrlRequestClientComponent.BeforeDestruction;
begin
  DestroyRequestClient;

  inherited BeforeDestruction;
end;

procedure TCEFUrlRequestClientComponent.DestroyRequestClient;
begin
  try
    if (FClient <> nil) then
      begin
        FClient.RemoveReferences;
        FClient := nil;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TCEFUrlRequestClientComponent.DestroyRequestClient', e) then raise;
  end;
end;

procedure TCEFUrlRequestClientComponent.doOnRequestComplete(const request: ICefUrlRequest);
begin
  if assigned(FOnRequestComplete) then FOnRequestComplete(self, request);
end;

procedure TCEFUrlRequestClientComponent.doOnUploadProgress(const request: ICefUrlRequest; current, total: Int64);
begin
  if assigned(FOnUploadProgress) then FOnUploadProgress(self, request, current, total);
end;

procedure TCEFUrlRequestClientComponent.doOnDownloadProgress(const request: ICefUrlRequest; current, total: Int64);
begin
  if assigned(FOnDownloadProgress) then FOnDownloadProgress(self, request, current, total);
end;

procedure TCEFUrlRequestClientComponent.doOnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
begin
  if assigned(FOnDownloadData) then FOnDownloadData(self, request, data, datalength);
end;

function TCEFUrlRequestClientComponent.doOnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
begin
  Result := False;

  if assigned(FOnGetAuthCredentials) then FOnGetAuthCredentials(self, isProxy, host, port, realm, scheme, callback, Result);
end;

procedure TCEFUrlRequestClientComponent.doOnCreateURLRequest;
begin
  if assigned(FOnCreateURLRequest) then FOnCreateURLRequest(self);
end;

procedure TCEFUrlRequestClientComponent.AddURLRequest;
var
  TempTask : ICefTask;
begin
  TempTask := TCefURLRequestTask.Create(self);
  CefPostTask(FThreadID, TempTask);
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tcefurlrequestclientcomponent.lrs}
  RegisterComponents('Chromium', [TCEFUrlRequestClientComponent]);
end;
{$ENDIF}

end.
