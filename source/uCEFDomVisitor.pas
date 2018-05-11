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
//        Copyright � 2018 Salvador D�az Fau. All rights reserved.
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

unit uCEFDomVisitor;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces;

type
  TCefDomVisitorOwn = class(TCefBaseRefCountedOwn, ICefDomVisitor)
    protected
      procedure visit(const document: ICefDomDocument); virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastDomVisitor = class(TCefDomVisitorOwn)
    protected
      FProc    : TCefDomVisitorProc;

      procedure visit(const document: ICefDomDocument); override;

    public
      constructor Create(const proc: TCefDomVisitorProc); reintroduce; virtual;
  end;

  TCefFastDomVisitor2 = class(TCefDomVisitorOwn)
    protected
      FProc    : TCefDomVisitorProc2;
      FBrowser : ICefBrowser;

      procedure visit(const document: ICefDomDocument); override;

    public
      constructor Create(const browser: ICefBrowser; const proc: TCefDomVisitorProc2); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFTypes, uCEFDomDocument;

procedure cef_dom_visitor_visite(self: PCefDomVisitor; document: PCefDomDocument); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDomVisitorOwn) then
    TCefDomVisitorOwn(TempObject).visit(TCefDomDocumentRef.UnWrap(document));
end;

// TCefDomVisitorOwn

constructor TCefDomVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDomVisitor));

  PCefDomVisitor(FData).visit := cef_dom_visitor_visite;
end;

procedure TCefDomVisitorOwn.visit(const document: ICefDomDocument);
begin

end;

// TCefFastDomVisitor

constructor TCefFastDomVisitor.Create(const proc: TCefDomVisitorProc);
begin
  inherited Create;

  FProc := proc;
end;

procedure TCefFastDomVisitor.visit(const document: ICefDomDocument);
begin
  FProc(document);
end;


// TCefFastDomVisitor2

constructor TCefFastDomVisitor2.Create(const browser: ICefBrowser; const proc: TCefDomVisitorProc2);
begin
  inherited Create;

  FBrowser := browser;
  FProc    := proc;
end;

destructor TCefFastDomVisitor2.Destroy;
begin
  FBrowser := nil;

  inherited Destroy;
end;

procedure TCefFastDomVisitor2.visit(const document: ICefDomDocument);
begin
  FProc(FBrowser, document);
end;

end.
