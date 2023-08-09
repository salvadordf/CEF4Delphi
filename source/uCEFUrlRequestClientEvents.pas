unit uCEFUrlRequestClientEvents;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces;

type
  TOnRequestComplete    = procedure(Sender: TObject; const request: ICefUrlRequest) of object;
  TOnUploadProgress     = procedure(Sender: TObject; const request: ICefUrlRequest; current, total: Int64) of object;
  TOnDownloadProgress   = procedure(Sender: TObject; const request: ICefUrlRequest; current, total: Int64) of object;
  TOnDownloadData       = procedure(Sender: TObject; const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt) of object;
  TOnGetAuthCredentials = procedure(Sender: TObject; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback; var aResult : Boolean) of object;

implementation

end.
