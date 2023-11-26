program ConsoleLoader;

{$APPTYPE CONSOLE}

{$I ..\..\..\source\cef.inc}

{$R *.res}

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF }

  procedure InitializeCEF4Delphi; stdcall; external 'OSRDLLBrowser.dll';
  procedure FinalizeCEF4Delphi; stdcall; external 'OSRDLLBrowser.dll';
  procedure ShowBrowser; stdcall; external 'OSRDLLBrowser.dll';

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}{$ENDIF}

procedure ExecuteProgram;
var
  TempKey : char;
begin
  Write('Press ENTER to show a web browser created in a Delphi DLL :');
  Read(TempKey);

  InitializeCEF4Delphi;
  ShowBrowser;
  FinalizeCEF4Delphi;
end;

begin
  try
    ExecuteProgram;
  except
    on E: Exception do
      Writeln('Error : ', E.Message);
  end;
end.
