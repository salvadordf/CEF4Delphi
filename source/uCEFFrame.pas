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

unit uCEFFrame;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefFrameRef = class(TCefBaseRefCountedRef, ICefFrame)
    public
      function  IsValid: Boolean;
      procedure Undo;
      procedure Redo;
      procedure Cut;
      procedure Copy;
      procedure Paste;
      procedure Del;
      procedure SelectAll;
      procedure ViewSource;
      procedure GetSource(const visitor: ICefStringVisitor);
      procedure GetSourceProc(const proc: TCefStringVisitorProc);
      procedure GetText(const visitor: ICefStringVisitor);
      procedure GetTextProc(const proc: TCefStringVisitorProc);
      procedure LoadRequest(const request: ICefRequest);
      procedure LoadUrl(const url: ustring);
      procedure ExecuteJavaScript(const code, scriptUrl: ustring; startLine: Integer);
      function  IsMain: Boolean;
      function  IsFocused: Boolean;
      function  GetName: ustring;
      function  GetIdentifier: Int64;
      function  GetParent: ICefFrame;
      function  GetUrl: ustring;
      function  GetBrowser: ICefBrowser;
      function  GetV8Context: ICefv8Context;
      procedure VisitDom(const visitor: ICefDomVisitor);
      procedure VisitDomProc(const proc: TCefDomVisitorProc);
      function  CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient): ICefUrlRequest;
      procedure SendProcessMessage(targetProcess: TCefProcessId; const message_: ICefProcessMessage);

      class function UnWrap(data: Pointer): ICefFrame;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFStringVisitor, uCEFv8Context, uCEFDomVisitor, uCEFUrlRequest;

function TCefFrameRef.IsValid: Boolean;
begin
  Result := PCefFrame(FData)^.is_valid(PCefFrame(FData)) <> 0;
end;

procedure TCefFrameRef.Copy;
begin
  PCefFrame(FData)^.copy(PCefFrame(FData));
end;

procedure TCefFrameRef.Cut;
begin
  PCefFrame(FData)^.cut(PCefFrame(FData));
end;

procedure TCefFrameRef.Del;
begin
  PCefFrame(FData)^.del(PCefFrame(FData));
end;

procedure TCefFrameRef.ExecuteJavaScript(const code, scriptUrl: ustring; startLine: Integer);
var
  TempCode, TempURL : TCefString;
begin
  TempCode := CefString(code);
  TempURL  := CefString(scriptUrl);
  PCefFrame(FData)^.execute_java_script(PCefFrame(FData), @TempCode, @TempURL, startline);
end;

function TCefFrameRef.GetBrowser: ICefBrowser;
begin
  Result := TCefBrowserRef.UnWrap(PCefFrame(FData)^.get_browser(PCefFrame(FData)));
end;

function TCefFrameRef.GetIdentifier: Int64;
begin
  Result := PCefFrame(FData)^.get_identifier(PCefFrame(FData));
end;

function TCefFrameRef.GetName: ustring;
begin
  Result := CefStringFreeAndGet(PCefFrame(FData)^.get_name(PCefFrame(FData)));
end;

function TCefFrameRef.GetParent: ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefFrame(FData)^.get_parent(PCefFrame(FData)));
end;

procedure TCefFrameRef.GetSource(const visitor: ICefStringVisitor);
begin
  PCefFrame(FData)^.get_source(PCefFrame(FData), CefGetData(visitor));
end;

procedure TCefFrameRef.GetSourceProc(const proc: TCefStringVisitorProc);
begin
  GetSource(TCefFastStringVisitor.Create(proc));
end;

procedure TCefFrameRef.getText(const visitor: ICefStringVisitor);
begin
  PCefFrame(FData)^.get_text(PCefFrame(FData), CefGetData(visitor));
end;

procedure TCefFrameRef.GetTextProc(const proc: TCefStringVisitorProc);
begin
  GetText(TCefFastStringVisitor.Create(proc));
end;

function TCefFrameRef.GetUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefFrame(FData)^.get_url(PCefFrame(FData)));
end;

function TCefFrameRef.GetV8Context: ICefv8Context;
begin
  Result := TCefv8ContextRef.UnWrap(PCefFrame(FData)^.get_v8context(PCefFrame(FData)));
end;

function TCefFrameRef.IsFocused: Boolean;
begin
  Result := PCefFrame(FData)^.is_focused(PCefFrame(FData)) <> 0;
end;

function TCefFrameRef.IsMain: Boolean;
begin
  Result := PCefFrame(FData)^.is_main(PCefFrame(FData)) <> 0;
end;

procedure TCefFrameRef.LoadRequest(const request: ICefRequest);
begin
  PCefFrame(FData)^.load_request(PCefFrame(FData), CefGetData(request));
end;

procedure TCefFrameRef.LoadUrl(const url: ustring);
var
  TempURL : TCefString;
begin
  TempURL := CefString(url);
  PCefFrame(FData)^.load_url(PCefFrame(FData), @TempURL);
end;

procedure TCefFrameRef.Paste;
begin
  PCefFrame(FData)^.paste(PCefFrame(FData));
end;

procedure TCefFrameRef.Redo;
begin
  PCefFrame(FData)^.redo(PCefFrame(FData));
end;

procedure TCefFrameRef.SelectAll;
begin
  PCefFrame(FData)^.select_all(PCefFrame(FData));
end;

procedure TCefFrameRef.Undo;
begin
  PCefFrame(FData)^.undo(PCefFrame(FData));
end;

procedure TCefFrameRef.ViewSource;
begin
  PCefFrame(FData)^.view_source(PCefFrame(FData));
end;

procedure TCefFrameRef.VisitDom(const visitor: ICefDomVisitor);
begin
  PCefFrame(FData)^.visit_dom(PCefFrame(FData), CefGetData(visitor));
end;

procedure TCefFrameRef.VisitDomProc(const proc: TCefDomVisitorProc);
begin
  VisitDom(TCefFastDomVisitor.Create(proc) as ICefDomVisitor);
end;

function TCefFrameRef.CreateUrlRequest(const request : ICefRequest;
                                       const client  : ICefUrlrequestClient): ICefUrlRequest;
begin
  Result := TCefUrlRequestRef.UnWrap(PCefFrame(FData)^.create_urlrequest(PCefFrame(FData),
                                                                         CefGetData(request),
                                                                         CefGetData(client)));
end;

procedure TCefFrameRef.SendProcessMessage(      targetProcess : TCefProcessId;
                                          const message_      : ICefProcessMessage);
begin
  PCefFrame(FData)^.send_process_message(PCefFrame(FData),
                                         targetProcess,
                                         CefGetData(message_));
end;

class function TCefFrameRef.UnWrap(data: Pointer): ICefFrame;
begin
  if (data <> nil) then
    Result := Create(data) as ICefFrame
   else
    Result := nil;
end;

end.
