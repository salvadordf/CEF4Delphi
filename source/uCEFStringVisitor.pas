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

    public
      constructor Create; virtual;
  end;

  TCefFastStringVisitor = class(TCefStringVisitorOwn, ICefStringVisitor)
    protected
      FVisit: TCefStringVisitorProc;

      procedure Visit(const str: ustring); override;

    public
      constructor Create(const callback: TCefStringVisitorProc); reintroduce;
  end;

  TCustomCefStringVisitor = class(TCefStringVisitorOwn)
    protected
      FChromiumBrowser : TObject;

      procedure Visit(const str: ustring); override;

    public
      constructor Create(const aChromiumBrowser : TObject); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFChromium;

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

constructor TCustomCefStringVisitor.Create(const aChromiumBrowser : TObject);
begin
  inherited Create;

  FChromiumBrowser := aChromiumBrowser;
end;

procedure TCustomCefStringVisitor.Visit(const str: ustring);
begin
  if (FChromiumBrowser <> nil) and (FChromiumBrowser is TChromium) then
    TChromium(FChromiumBrowser).Internal_TextResultAvailable(str);
end;

end.
