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
//        Copyright © 2018 Salvador Díaz Fau. All rights reserved.
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

unit uCEFStringVisitor;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefStringVisitorOwn = class(TCefBaseRefCountedOwn, ICefStringVisitor)
    protected
      procedure Visit(const str: ustring); virtual;
      procedure InitializeVars; virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastStringVisitor = class(TCefStringVisitorOwn)
    protected
      FVisit: TCefStringVisitorProc;

      procedure Visit(const str: ustring); override;

    public
      constructor Create(const callback: TCefStringVisitorProc); reintroduce;
  end;

  TCustomCefStringVisitor = class(TCefStringVisitorOwn)
    protected
      FChromiumBrowser : IChromiumEvents;

      procedure Visit(const str: ustring); override;

    public
      constructor Create(const aChromiumBrowser : IChromiumEvents); reintroduce;
      destructor  Destroy; override;
      procedure   InitializeVars; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure cef_string_visitor_visit(self: PCefStringVisitor; const str: PCefString); stdcall;
begin
  TCefStringVisitorOwn(CefGetObject(self)).Visit(CefString(str));
end;

// TCefStringVisitorOwn

constructor TCefStringVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefStringVisitor));

  with PCefStringVisitor(FData)^ do visit := cef_string_visitor_visit;
end;

procedure TCefStringVisitorOwn.Visit(const str: ustring);
begin
  //
end;

procedure TCefStringVisitorOwn.InitializeVars;
begin
  //
end;

// TCefFastStringVisitor

constructor TCefFastStringVisitor.Create(const callback: TCefStringVisitorProc);
begin
  inherited Create;

  FVisit := callback;
end;

procedure TCefFastStringVisitor.Visit(const str: ustring);
begin
  FVisit(str);
end;

// TCustomCefStringVisitor

constructor TCustomCefStringVisitor.Create(const aChromiumBrowser : IChromiumEvents);
begin
  inherited Create;

  FChromiumBrowser := aChromiumBrowser;
end;

destructor TCustomCefStringVisitor.Destroy;
begin
  InitializeVars;

  inherited Destroy;
end;

procedure TCustomCefStringVisitor.InitializeVars;
begin
  FChromiumBrowser := nil;
end;

procedure TCustomCefStringVisitor.Visit(const str: ustring);
begin
  if (FChromiumBrowser <> nil) then FChromiumBrowser.doTextResultAvailable(str);
end;

end.
