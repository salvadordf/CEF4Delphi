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

interface

uses
  uCEFBase, uCEFInterfaces, uCEFTypes;

type
  TCefTaskOwn = class(TCefBaseOwn, ICefTask)
    protected
      procedure Execute; virtual;

    public
      constructor Create; virtual;
  end;

  TCefTaskRef = class(TCefBaseRef, ICefTask)
    protected
      procedure Execute; virtual;

    public
      class function UnWrap(data: Pointer): ICefTask;
  end;

  TCefFastTaskProc = reference to procedure;

  TCefFastTask = class(TCefTaskOwn)
    protected
      FMethod: TCefFastTaskProc;

      procedure Execute; override;

    public
      class procedure New(threadId: TCefThreadId; const method: TCefFastTaskProc);
      class procedure NewDelayed(threadId: TCefThreadId; Delay: Int64; const method: TCefFastTaskProc);
      constructor Create(const method: TCefFastTaskProc); reintroduce;
  end;

  TCefGetHTMLTask = class(TCefTaskOwn)
    protected
      FChromiumBrowser : TObject;

      procedure Execute; override;

    public
      constructor Create(const aChromiumBrowser : TObject); reintroduce;
  end;

  TCefGetDocumentTask = class(TCefTaskOwn)
    protected
      FChromiumBrowser : TObject;

      procedure Execute; override;

    public
      constructor Create(const aChromiumBrowser : TObject); reintroduce;
  end;

  TCefDeleteCookiesTask = class(TCefTaskOwn)
    protected
      FCallBack : ICefDeleteCookiesCallback;

      procedure Execute; override;

    public
      constructor Create(const aCallBack : ICefDeleteCookiesCallback); reintroduce;
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

// TCefGetHTMLTask
constructor TCefGetHTMLTask.Create(const aChromiumBrowser : TObject);
begin
  inherited Create;

  FChromiumBrowser := aChromiumBrowser;
end;

procedure TCefGetHTMLTask.Execute;
begin
  if (FChromiumBrowser <> nil) and (FChromiumBrowser is TChromium) then
    TChromium(FChromiumBrowser).GetHTML;
end;

// TCefGetDocumentTask
constructor TCefGetDocumentTask.Create(const aChromiumBrowser : TObject);
begin
  inherited Create;

  FChromiumBrowser := aChromiumBrowser;
end;

procedure TCefGetDocumentTask.Execute;
begin
  if (FChromiumBrowser <> nil) and (FChromiumBrowser is TChromium) then
    TChromium(FChromiumBrowser).VisitDOM;
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

end.
