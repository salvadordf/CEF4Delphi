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

unit uCEFv8Types;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFInterfaces, uCEFTypes;

type
  TCefV8AccessorGetterProc = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(const name: ustring; const obj: ICefv8Value; out value: ICefv8Value; const exception: ustring): Boolean;
  TCefV8AccessorSetterProc = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(const name: ustring; const obj, value: ICefv8Value; const exception: ustring): Boolean;

  TCefV8InterceptorGetterByNameProc  = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(const name: ustring; const obj: ICefv8Value; out value: ICefv8Value; const exception: ustring): Boolean;
  TCefV8InterceptorSetterByNameProc  = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(const name: ustring; const obj, value: ICefv8Value; const exception: ustring): Boolean;
  TCefV8InterceptorGetterByIndexProc = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(index: integer; const obj: ICefv8Value; out value: ICefv8Value; const exception: ustring): Boolean;
  TCefV8InterceptorSetterByIndexProc = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(index: integer; const obj, value: ICefv8Value; const exception: ustring): Boolean;

implementation

end.
