{*******************************************************}
{                                                       }
{              Linux FireMonkey Platform                }
{                                                       }
{          Copyright(c) 2017-2019 Eugene Kryukov. 	}
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMUX.Config;

interface

// Chromium requires that the process has only one thread when it's initialized.
// FmuxInit must be called after the Chromium initialization.
// Setting DoNotCallFmuxInit to True allows us to call FmuxInit after that.
var
  DoNotCallFmuxInit: Boolean = True;

implementation
end.

