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

unit uCEFDomVisitor;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBase, uCEFInterfaces;

type
  TCefDomVisitorOwn = class(TCefBaseOwn, ICefDomVisitor)
    protected
      procedure visit(const document: ICefDomDocument); virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastDomVisitor = class(TCefDomVisitorOwn)
    protected
      FProc: TCefDomVisitorProc;

      procedure visit(const document: ICefDomDocument); override;

    public
      constructor Create(const proc: TCefDomVisitorProc); reintroduce; virtual;
  end;

  TCustomDomVisitor = class(TCefDomVisitorOwn)
    protected
      FChromiumBrowser : TObject;

      procedure visit(const document: ICefDomDocument); override;

    public
      constructor Create(const aChromiumBrowser : TObject); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFTypes, uCEFDomDocument, uCEFChromium;

procedure cef_dom_visitor_visite(self: PCefDomVisitor; document: PCefDomDocument); stdcall;
begin
  TCefDomVisitorOwn(CefGetObject(self)).visit(TCefDomDocumentRef.UnWrap(document));
end;

// TCefDomVisitorOwn

constructor TCefDomVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDomVisitor));

  with PCefDomVisitor(FData)^ do visit := cef_dom_visitor_visite;
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

// TCustomDomVisitor

constructor TCustomDomVisitor.Create(const aChromiumBrowser : TObject);
begin
  inherited Create;

  FChromiumBrowser := aChromiumBrowser;
end;

procedure TCustomDomVisitor.visit(const document: ICefDomDocument);
begin
  if (FChromiumBrowser <> nil) and (FChromiumBrowser is TChromium) then
    TChromium(FChromiumBrowser).DOMVisitorVisit(document);
end;

end.
