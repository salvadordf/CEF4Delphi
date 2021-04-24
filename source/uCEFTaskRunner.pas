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

unit uCEFTaskRunner;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefTaskRunnerRef = class(TCefBaseRefCountedRef, ICefTaskRunner)
  protected
    function IsSame(const that: ICefTaskRunner): Boolean;
    function BelongsToCurrentThread: Boolean;
    function BelongsToThread(threadId: TCefThreadId): Boolean;
    function PostTask(const task: ICefTask): Boolean;
    function PostDelayedTask(const task: ICefTask; delayMs: Int64): Boolean;
  public
    class function UnWrap(data: Pointer): ICefTaskRunner;
    class function GetForCurrentThread: ICefTaskRunner;
    class function GetForThread(threadId: TCefThreadId): ICefTaskRunner;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


function TCefTaskRunnerRef.BelongsToCurrentThread: Boolean;
begin
  Result := PCefTaskRunner(FData)^.belongs_to_current_thread(PCefTaskRunner(FData)) <> 0;
end;

function TCefTaskRunnerRef.BelongsToThread(threadId: TCefThreadId): Boolean;
begin
  Result := PCefTaskRunner(FData)^.belongs_to_thread(PCefTaskRunner(FData), threadId) <> 0;
end;

class function TCefTaskRunnerRef.GetForCurrentThread: ICefTaskRunner;
begin
  Result := UnWrap(cef_task_runner_get_for_current_thread());
end;

class function TCefTaskRunnerRef.GetForThread(threadId: TCefThreadId): ICefTaskRunner;
begin
  Result := UnWrap(cef_task_runner_get_for_thread(threadId));
end;

function TCefTaskRunnerRef.IsSame(const that: ICefTaskRunner): Boolean;
begin
  Result := PCefTaskRunner(FData)^.is_same(PCefTaskRunner(FData), CefGetData(that)) <> 0;
end;

function TCefTaskRunnerRef.PostDelayedTask(const task: ICefTask; delayMs: Int64): Boolean;
begin
  Result := PCefTaskRunner(FData)^.post_delayed_task(PCefTaskRunner(FData), CefGetData(task), delayMs) <> 0;
end;

function TCefTaskRunnerRef.PostTask(const task: ICefTask): Boolean;
begin
  Result := PCefTaskRunner(FData)^.post_task(PCefTaskRunner(FData), CefGetData(task)) <> 0;
end;

class function TCefTaskRunnerRef.UnWrap(data: Pointer): ICefTaskRunner;
begin
  if (data <> nil) then
    Result := Create(data) as ICefTaskRunner
   else
    Result := nil;
end;

end.
