// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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

unit uCEFTask;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefTaskOwn = class(TCefBaseRefCountedOwn, ICefTask)
    protected
      procedure Execute; virtual;

    public
      constructor Create; virtual;
  end;

  TCefTaskRef = class(TCefBaseRefCountedRef, ICefTask)
    protected
      procedure Execute; virtual;

    public
      class function UnWrap(data: Pointer): ICefTask;
  end;

  TCefFastTaskProc = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure;

  TCefFastTask = class(TCefTaskOwn)
    protected
      FMethod: TCefFastTaskProc;

      procedure Execute; override;

    public
      class procedure New(threadId: TCefThreadId; const method: TCefFastTaskProc);
      class procedure NewDelayed(threadId: TCefThreadId; Delay: Int64; const method: TCefFastTaskProc);
      constructor Create(const method: TCefFastTaskProc); reintroduce;
  end;

  TCefGetTextTask = class(TCefTaskOwn)
    protected
      FChromiumBrowser : TObject;
      FFrameName       : ustring;
      FFrame           : ICefFrame;
      FFrameIdentifier : int64;

      procedure Execute; override;

    public
      constructor Create(const aChromiumBrowser : TObject; const aFrameName : ustring); reintroduce; overload;
      constructor Create(const aChromiumBrowser : TObject; const aFrame : ICefFrame); reintroduce; overload;
      constructor Create(const aChromiumBrowser : TObject; const aFrameIdentifier : int64); reintroduce; overload;
      destructor  Destroy; override;
  end;

  TCefGetHTMLTask = class(TCefGetTextTask)
    protected
      procedure Execute; override;
  end;

  TCefDeleteCookiesTask = class(TCefTaskOwn)
    protected
      FCallBack : ICefDeleteCookiesCallback;

      procedure Execute; override;

    public
      constructor Create(const aCallBack : ICefDeleteCookiesCallback); reintroduce;
  end;

  TCefUpdatePrefsTask = class(TCefTaskOwn)
    protected
      FChromiumBrowser : TObject;

      procedure Execute; override;

    public
      constructor Create(const aChromiumBrowser : TObject); reintroduce;
  end;

  TCefSavePrefsTask = class(TCefTaskOwn)
    protected
      FChromiumBrowser : TObject;

      procedure Execute; override;

    public
      constructor Create(const aChromiumBrowser : TObject); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFChromium, uCEFCookieManager;

procedure cef_task_execute(self: PCefTask); stdcall;
begin
  TCefTaskOwn(CefGetObject(self)).Execute();
end;

constructor TCefTaskOwn.Create;
begin
  inherited CreateData(SizeOf(TCefTask));

  with PCefTask(FData)^ do execute := cef_task_execute;
end;

procedure TCefTaskOwn.Execute;
begin
  //
end;


// TCefTaskRef


procedure TCefTaskRef.Execute;
begin
  PCefTask(FData).execute(FData);
end;

class function TCefTaskRef.UnWrap(data: Pointer): ICefTask;
begin
  if data <> nil then
    Result := Create(data) as ICefTask
   else
    Result := nil;
end;


// TCefFastTask


constructor TCefFastTask.Create(const method: TCefFastTaskProc);
begin
  inherited Create;

  FMethod := method;
end;

procedure TCefFastTask.Execute;
begin
  FMethod();
end;

class procedure TCefFastTask.New(threadId: TCefThreadId; const method: TCefFastTaskProc);
begin
  CefPostTask(threadId, Create(method));
end;

class procedure TCefFastTask.NewDelayed(threadId: TCefThreadId; Delay: Int64; const method: TCefFastTaskProc);
begin
  CefPostDelayedTask(threadId, Create(method), Delay);
end;

// TCefGetTextTask


constructor TCefGetTextTask.Create(const aChromiumBrowser : TObject; const aFrameName : ustring);
begin
  inherited Create;

  FChromiumBrowser := aChromiumBrowser;
  FFrameName       := aFrameName;
  FFrame           := nil;
  FFrameIdentifier := 0;
end;

constructor TCefGetTextTask.Create(const aChromiumBrowser : TObject; const aFrame : ICefFrame);
begin
  inherited Create;

  FChromiumBrowser := aChromiumBrowser;
  FFrameName       := '';
  FFrame           := aFrame;
  FFrameIdentifier := 0;
end;

constructor TCefGetTextTask.Create(const aChromiumBrowser : TObject; const aFrameIdentifier : int64);
begin
  inherited Create;

  FChromiumBrowser := aChromiumBrowser;
  FFrameName       := '';
  FFrame           := nil;
  FFrameIdentifier := aFrameIdentifier;
end;

destructor TCefGetTextTask.Destroy;
begin
  FFrame := nil;

  inherited Destroy;
end;

procedure TCefGetTextTask.Execute;
begin
  if (FChromiumBrowser <> nil) and (FChromiumBrowser is TChromium) then
    begin
      if (FFrame <> nil) then
        TChromium(FChromiumBrowser).Internal_GetText(FFrame)
       else
        if (FFrameIdentifier <> 0) then
          TChromium(FChromiumBrowser).Internal_GetText(FFrameIdentifier)
         else
          TChromium(FChromiumBrowser).Internal_GetText(FFrameName);
    end;
end;


// TCefGetHTMLTask

procedure TCefGetHTMLTask.Execute;
begin
  if (FChromiumBrowser <> nil) and (FChromiumBrowser is TChromium) then
    begin
      if (FFrame <> nil) then
        TChromium(FChromiumBrowser).Internal_GetHTML(FFrame)
       else
        if (FFrameIdentifier <> 0) then
          TChromium(FChromiumBrowser).Internal_GetHTML(FFrameIdentifier)
         else
          TChromium(FChromiumBrowser).Internal_GetHTML(FFrameName);
    end;
end;


// TCefDeleteCookiesTask


constructor TCefDeleteCookiesTask.Create(const aCallBack : ICefDeleteCookiesCallback);
begin
  inherited Create;

  FCallBack := aCallBack;
end;

procedure TCefDeleteCookiesTask.Execute;
var
  CookieManager : ICefCookieManager;
begin
  CookieManager := TCefCookieManagerRef.Global(nil);
  CookieManager.DeleteCookies('', '', FCallBack);
end;


// TCefUpdatePrefsTask


constructor TCefUpdatePrefsTask.Create(const aChromiumBrowser : TObject);
begin
  inherited Create;

  FChromiumBrowser := aChromiumBrowser;
end;

procedure TCefUpdatePrefsTask.Execute;
begin
  if (FChromiumBrowser <> nil) and (FChromiumBrowser is TChromium) then
    TChromium(FChromiumBrowser).Internal_UpdatePreferences;
end;


// TCefSavePrefsTask


constructor TCefSavePrefsTask.Create(const aChromiumBrowser : TObject);
begin
  inherited Create;

  FChromiumBrowser := aChromiumBrowser;
end;

procedure TCefSavePrefsTask.Execute;
begin
  if (FChromiumBrowser <> nil) and (FChromiumBrowser is TChromium) then
    TChromium(FChromiumBrowser).Internal_SavePreferences;
end;

end.
