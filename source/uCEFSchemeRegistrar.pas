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

unit uCEFSchemeRegistrar;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseScopedWrapper, uCEFTypes;

type
  TCefSchemeRegistrarRef = class(TCEFBaseScopedWrapperRef)
    public
      function AddCustomScheme(const schemeName: ustring; options : TCefSchemeOptions): Boolean;
  end;

implementation

uses
  uCEFMiscFunctions;

function TCefSchemeRegistrarRef.AddCustomScheme(const schemeName : ustring;
                                                      options    : TCefSchemeOptions): Boolean;
var
  TempName : TCefString;
begin
  TempName := CefString(schemeName);
  Result   := PCefSchemeRegistrar(FData)^.add_custom_scheme(PCefSchemeRegistrar(FData),
                                                            @TempName,
                                                            options) <> 0;
end;

end.
