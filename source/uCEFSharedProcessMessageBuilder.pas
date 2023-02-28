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
//        Copyright © 2023 Salvador Diaz Fau. All rights reserved.
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

unit uCEFSharedProcessMessageBuilder;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefSharedProcessMessageBuilderRef = class(TCefBaseRefCountedRef, ICefSharedProcessMessageBuilder)
    protected
      function IsValid: boolean;
      function Size: NativeUInt;
      function Memory: pointer;
      function Build: ICefProcessMessage;
    public
      class function UnWrap(data: Pointer): ICefSharedProcessMessageBuilder;
      class function CreateBuilder(const name: ustring; byte_size: NativeUInt): ICefSharedProcessMessageBuilder;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFProcessMessage;

class function TCefSharedProcessMessageBuilderRef.CreateBuilder(const name: ustring; byte_size: NativeUInt): ICefSharedProcessMessageBuilder;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   := UnWrap(cef_shared_process_message_builder_create(@TempName, byte_size));
end;

class function TCefSharedProcessMessageBuilderRef.UnWrap(data: Pointer): ICefSharedProcessMessageBuilder;
begin
  if (data <> nil) then
    Result := Create(data) as ICefSharedProcessMessageBuilder
   else
    Result := nil;
end;

function TCefSharedProcessMessageBuilderRef.IsValid: boolean;
begin
  Result := PCefSharedProcessMessageBuilder(FData)^.is_valid(PCefSharedProcessMessageBuilder(FData)) <> 0;
end;

function TCefSharedProcessMessageBuilderRef.Size: NativeUInt;
begin
  Result := PCefSharedProcessMessageBuilder(FData)^.size(PCefSharedProcessMessageBuilder(FData));
end;

function TCefSharedProcessMessageBuilderRef.Memory: pointer;
begin
  Result := PCefSharedProcessMessageBuilder(FData)^.memory(PCefSharedProcessMessageBuilder(FData));
end;

function TCefSharedProcessMessageBuilderRef.Build: ICefProcessMessage;
begin
  Result := TCefProcessMessageRef.UnWrap(PCefSharedProcessMessageBuilder(FData)^.build(PCefSharedProcessMessageBuilder(FData)));
end;

end.
