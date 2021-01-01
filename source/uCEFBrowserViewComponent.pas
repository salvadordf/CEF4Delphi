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

unit uCEFBrowserViewComponent;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, InterfaceBase,
    {$ENDIF}
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFViewsFrameworkEvents, uCEFViewComponent;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
  TCEFBrowserViewComponent = class(TCEFViewComponent, ICefBrowserViewDelegateEvents)
    protected
      FBrowserView                      : ICefBrowserView;
      FBrowserViewDlg                   : ICefBrowserViewDelegate;

      // ICefBrowserViewDelegateEvents
      FOnBrowserCreated                 : TOnBrowserCreatedEvent;
      FOnBrowserDestroyed               : TOnBrowserDestroyedEvent;
      FOnGetDelegateForPopupBrowserView : TOnGetDelegateForPopupBrowserViewEvent;
      FOnPopupBrowserViewCreated        : TOnPopupBrowserViewCreatedEvent;

      procedure DestroyView; override;
      procedure Initialize; override;

      function  GetInitialized : boolean; override;
      function  GetAsView : ICefView; override;
      function  GetAsBrowserView : ICefBrowserView; override;
      function  GetBrowser : ICefBrowser;

      // ICefBrowserViewDelegateEvents
      procedure doOnBrowserCreated(const browser_view: ICefBrowserView; const browser: ICefBrowser);
      procedure doOnBrowserDestroyed(const browser_view: ICefBrowserView; const browser: ICefBrowser);
      procedure doOnGetDelegateForPopupBrowserView(const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean; var aResult : ICefBrowserViewDelegate);
      procedure doOnPopupBrowserViewCreated(const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean; var aResult : boolean);

    public
      function  CreateBrowserView(const client: ICefClient; const url: ustring; const settings: TCefBrowserSettings; const extra_info: ICefDictionaryValue; const request_context: ICefRequestContext): boolean;
      function  GetForBrowser(const browser: ICefBrowser): boolean;
      procedure SetPreferAccelerators(prefer_accelerators: boolean);

      property Browser                           : ICefBrowser                             read GetBrowser;
      property BrowserView                       : ICefBrowserView                         read FBrowserView;

    published
      property OnBrowserCreated                  : TOnBrowserCreatedEvent                  read FOnBrowserCreated                  write FOnBrowserCreated;
      property OnBrowserDestroyed                : TOnBrowserDestroyedEvent                read FOnBrowserDestroyed                write FOnBrowserDestroyed;
      property OnGetDelegateForPopupBrowserView  : TOnGetDelegateForPopupBrowserViewEvent  read FOnGetDelegateForPopupBrowserView  write FOnGetDelegateForPopupBrowserView;
      property OnPopupBrowserViewCreated         : TOnPopupBrowserViewCreatedEvent         read FOnPopupBrowserViewCreated         write FOnPopupBrowserViewCreated;
  end;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

// *********************************************************
// ********************** ATTENTION ! **********************
// *********************************************************
// **                                                     **
// **  MANY OF THE EVENTS IN CEF4DELPHI COMPONENTS LIKE   **
// **  TCHROMIUM, TFMXCHROMIUM OR TCEFAPPLICATION ARE     **
// **  EXECUTED IN A CEF THREAD BY DEFAULT.               **
// **                                                     **
// **  WINDOWS CONTROLS MUST BE CREATED AND DESTROYED IN  **
// **  THE SAME THREAD TO AVOID ERRORS.                   **
// **  SOME OF THEM RECREATE THE HANDLERS IF THEY ARE     **
// **  MODIFIED AND CAN CAUSE THE SAME ERRORS.            **
// **                                                     **
// **  DON'T CREATE, MODIFY OR DESTROY WINDOWS CONTROLS   **
// **  INSIDE THE CEF4DELPHI EVENTS AND USE               **
// **  SYNCHRONIZATION OBJECTS TO PROTECT VARIABLES AND   **
// **  FIELDS IF THEY ARE ALSO USED IN THE MAIN THREAD.   **
// **                                                     **
// **  READ THIS FOR MORE INFORMATION :                   **
// **  https://www.briskbard.com/index.php?pageid=cef     **
// **                                                     **
// **  USE OUR FORUMS FOR MORE QUESTIONS :                **
// **  https://www.briskbard.com/forum/                   **
// **                                                     **
// *********************************************************
// *********************************************************

implementation

uses
  uCEFBrowserView, uCEFBrowserViewDelegate, uCEFMiscFunctions;

procedure TCEFBrowserViewComponent.Initialize;
begin
  inherited Initialize;

  FBrowserView                      := nil;
  FBrowserViewDlg                   := nil;
  FOnBrowserCreated                 := nil;
  FOnBrowserDestroyed               := nil;
  FOnGetDelegateForPopupBrowserView := nil;
  FOnPopupBrowserViewCreated        := nil;
end;

procedure TCEFBrowserViewComponent.DestroyView;
begin
  if (FBrowserViewDlg <> nil) then
    begin
      FBrowserViewDlg.DestroyOtherRefs;
      FBrowserViewDlg := nil;
    end;

  FBrowserView := nil;
end;

function TCEFBrowserViewComponent.CreateBrowserView(const client          : ICefClient;
                                                    const url             : ustring;
                                                    const settings        : TCefBrowserSettings;
                                                    const extra_info      : ICefDictionaryValue;
                                                    const request_context : ICefRequestContext): boolean;
begin
  Result := False;

  if CefCurrentlyOn(TID_UI) and (client <> nil) then
    begin
      if (FBrowserViewDlg = nil) then
        FBrowserViewDlg := TCustomBrowserViewDelegate.Create(self);

      FBrowserView := TCefBrowserViewRef.CreateBrowserView(client, url, settings, extra_info, request_context, FBrowserViewDlg);
      Result       := (FBrowserView <> nil);
    end;
end;

function TCEFBrowserViewComponent.GetForBrowser(const browser: ICefBrowser): boolean;
begin
  Result := False;

  if CefCurrentlyOn(TID_UI) and (browser <> nil) then
    begin
      FBrowserView := TCefBrowserViewRef.GetForBrowser(browser);
      Result       := (FBrowserView <> nil);
    end;
end;

function TCEFBrowserViewComponent.GetInitialized : boolean;
begin
  Result := (FBrowserView <> nil);
end;

function TCEFBrowserViewComponent.GetAsView : ICefView;
begin
  Result := FBrowserView as ICefView;
end;

function TCEFBrowserViewComponent.GetAsBrowserView : ICefBrowserView;
begin
  Result := FBrowserView;
end;

function TCEFBrowserViewComponent.GetBrowser : ICefBrowser;
begin
  if Initialized then
    Result := FBrowserView.GetBrowser
   else
    Result := nil;
end;

procedure TCEFBrowserViewComponent.SetPreferAccelerators(prefer_accelerators: boolean);
begin
  if Initialized then FBrowserView.SetPreferAccelerators(prefer_accelerators);
end;

procedure TCEFBrowserViewComponent.doOnBrowserCreated(const browser_view : ICefBrowserView;
                                                      const browser      : ICefBrowser);
begin
  if assigned(FOnBrowserCreated) then
    FOnBrowserCreated(self, browser_view, browser);
end;

procedure TCEFBrowserViewComponent.doOnBrowserDestroyed(const browser_view : ICefBrowserView;
                                                        const browser      : ICefBrowser);
begin
  if assigned(FOnBrowserDestroyed) then
    FOnBrowserDestroyed(self, browser_view, browser);
end;

procedure TCEFBrowserViewComponent.doOnGetDelegateForPopupBrowserView(const browser_view : ICefBrowserView;
                                                                      const settings     : TCefBrowserSettings;
                                                                      const client       : ICefClient;
                                                                            is_devtools  : boolean;
                                                                      var   aResult      : ICefBrowserViewDelegate);
begin
  if assigned(FOnGetDelegateForPopupBrowserView) then
    FOnGetDelegateForPopupBrowserView(self, browser_view, settings, client, is_devtools, aResult);
end;

procedure TCEFBrowserViewComponent.doOnPopupBrowserViewCreated(const browser_view       : ICefBrowserView;
                                                               const popup_browser_view : ICefBrowserView;
                                                                     is_devtools        : boolean;
                                                               var   aResult            : boolean);
begin
  if assigned(FOnPopupBrowserViewCreated) then
    FOnPopupBrowserViewCreated(self, browser_view, popup_browser_view, is_devtools, aResult);
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tcefbrowserviewcomponent.lrs}
  RegisterComponents('Chromium Views Framework', [TCEFBrowserViewComponent]);
end;
{$ENDIF}

end.
