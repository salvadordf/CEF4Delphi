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

unit uCEFThread;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefThreadRef = class(TCefBaseRefCountedRef, ICefThread)
    protected
      function  GetTaskRunner : ICefTaskRunner;
      function  GetPlatformThreadID : TCefPlatformThreadId;
      procedure Stop;
      function  IsRunning : boolean;
    public
      class function UnWrap(data: Pointer): ICefThread;
      class function New(const display_name: ustring; priority: TCefThreadPriority; message_loop_type: TCefMessageLoopType; stoppable: integer; com_init_mode: TCefCOMInitMode): ICefThread;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFTaskRunner;

function TCefThreadRef.GetTaskRunner : ICefTaskRunner;
begin
  Result := TCefTaskRunnerRef.UnWrap(PCefThread(FData).get_task_runner(FData));
end;

function TCefThreadRef.GetPlatformThreadID : TCefPlatformThreadId;
begin
  Result := PCefThread(FData).get_platform_thread_id(FData);
end;

procedure TCefThreadRef.Stop;
begin
  PCefThread(FData).stop(FData);
end;

function TCefThreadRef.IsRunning: Boolean;
begin
  Result := (PCefThread(FData).is_running(FData) <> 0);
end;

class function TCefThreadRef.UnWrap(data: Pointer): ICefThread;
begin
  if (data <> nil) then
    Result := Create(data) as ICefThread
   else
    Result := nil;
end;

class function TCefThreadRef.New(const display_name: ustring; priority: TCefThreadPriority; message_loop_type: TCefMessageLoopType; stoppable: integer; com_init_mode: TCefCOMInitMode): ICefThread;
var
  TempString : TCefString;
begin
  TempString := CefString(display_name);
  Result     := UnWrap(cef_thread_create(@TempString, priority, message_loop_type, stoppable, com_init_mode));
end;

end.
