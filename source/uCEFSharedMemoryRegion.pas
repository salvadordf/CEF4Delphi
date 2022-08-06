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
//        Copyright © 2022 Salvador Diaz Fau. All rights reserved.
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

unit uCEFSharedMemoryRegion;

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
  TCefSharedMemoryRegionRef = class(TCefBaseRefCountedRef, ICefSharedMemoryRegion)
    protected
      function IsValid: boolean;
      function Size: NativeUInt;
      function Memory: pointer;

    public
      class function UnWrap(data: Pointer): ICefSharedMemoryRegion;
  end;

implementation

function TCefSharedMemoryRegionRef.IsValid: Boolean;
begin
  Result := PCefSharedMemoryRegion(FData)^.is_valid(PCefSharedMemoryRegion(FData)) <> 0;
end;

function TCefSharedMemoryRegionRef.Size: NativeUInt;
begin
  Result := PCefSharedMemoryRegion(FData)^.Size(PCefSharedMemoryRegion(FData));
end;

function TCefSharedMemoryRegionRef.Memory: pointer;
begin
  Result := PCefSharedMemoryRegion(FData)^.Memory(PCefSharedMemoryRegion(FData));
end;

class function TCefSharedMemoryRegionRef.UnWrap(data: Pointer): ICefSharedMemoryRegion;
begin
  if (data <> nil) then
    Result := Create(data) as ICefSharedMemoryRegion
   else
    Result := nil;
end;

end.
