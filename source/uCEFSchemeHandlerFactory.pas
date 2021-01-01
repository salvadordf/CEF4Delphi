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

unit uCEFSchemeHandlerFactory;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFResourceHandler;

type
  TCefSchemeHandlerFactoryOwn = class(TCefBaseRefCountedOwn, ICefSchemeHandlerFactory)
    protected
      FClass : TCefResourceHandlerClass;

      function New(const browser: ICefBrowser; const frame: ICefFrame; const schemeName: ustring; const request: ICefRequest): ICefResourceHandler; virtual;

    public
      constructor Create(const AClass: TCefResourceHandlerClass); virtual;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame, uCEFRequest;

function cef_scheme_handler_factory_create(      self        : PCefSchemeHandlerFactory;
                                                 browser     : PCefBrowser;
                                                 frame       : PCefFrame;
                                           const scheme_name : PCefString;
                                                 request     : PCefRequest): PCefResourceHandler; stdcall;
var
  TempObject : TObject;
begin
  Result     := nil;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefSchemeHandlerFactoryOwn) then
    Result := CefGetData(TCefSchemeHandlerFactoryOwn(TempObject).New(TCefBrowserRef.UnWrap(browser),
                                                                     TCefFrameRef.UnWrap(frame),
                                                                     CefString(scheme_name),
                                                                     TCefRequestRef.UnWrap(request)));
end;

constructor TCefSchemeHandlerFactoryOwn.Create(const AClass: TCefResourceHandlerClass);
begin
  inherited CreateData(SizeOf(TCefSchemeHandlerFactory));

  FClass := AClass;

  PCefSchemeHandlerFactory(FData)^.create := {$IFDEF FPC}@{$ENDIF}cef_scheme_handler_factory_create;
end;

function TCefSchemeHandlerFactoryOwn.New(const browser    : ICefBrowser;
                                         const frame      : ICefFrame;
                                         const schemeName : ustring;
                                         const request    : ICefRequest): ICefResourceHandler;
begin
  if (FClass <> nil) then
    Result := FClass.Create(browser, frame, schemeName, request)
   else
    Result := nil;
end;


end.
