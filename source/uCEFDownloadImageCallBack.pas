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

unit uCEFDownloadImageCallBack;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefDownloadImageCallbackOwn = class(TCefBaseRefCountedOwn, ICefDownloadImageCallback)
    protected
      procedure OnDownloadImageFinished(const imageUrl: ustring; httpStatusCode: Integer; const image: ICefImage); virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TCefFastDownloadImageCallback = class(TCefDownloadImageCallbackOwn)
    protected
      FProc: TOnDownloadImageFinishedProc;

      procedure OnDownloadImageFinished(const imageUrl: ustring; httpStatusCode: Integer; const image: ICefImage); override;

    public
      constructor Create(const proc: TOnDownloadImageFinishedProc); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFImage;

procedure cef_download_image_callback_on_download_image_finished(self: PCefDownloadImageCallback; const image_url: PCefString; http_status_code: Integer; image: PCefImage); stdcall;
begin
  with TCefDownloadImageCallbackOwn(CefGetObject(self)) do
    OnDownloadImageFinished(CefString(image_url), http_status_code, TCefImageRef.UnWrap(image));
end;

constructor TCefDownloadImageCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDownloadImageCallback));

  with PCefDownloadImageCallback(FData)^ do
    on_download_image_finished := cef_download_image_callback_on_download_image_finished;
end;

constructor TCefFastDownloadImageCallback.Create(const proc: TOnDownloadImageFinishedProc);
begin
  inherited Create;
  FProc := proc;
end;

procedure TCefFastDownloadImageCallback.OnDownloadImageFinished(const imageUrl: ustring; httpStatusCode: Integer; const image: ICefImage);
begin
  FProc(imageUrl, httpStatusCode, image);
end;


end.

