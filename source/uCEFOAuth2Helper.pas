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

unit uCEFOAuth2Helper;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF}System.Classes, System.UITypes, System.SysUtils,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, {$IFDEF FPC}dynlibs,{$ENDIF}SysUtils,
  {$ENDIF}
  uCEFInterfaces, uCEFTypes;

// REFERENCES :
// ============
// https://tools.ietf.org/html/rfc6749
// https://tools.ietf.org/html/rfc6750
// https://tools.ietf.org/html/rfc8252
// https://tools.ietf.org/html/rfc6819
// https://tools.ietf.org/html/rfc7636
// https://tools.ietf.org/html/draft-ietf-oauth-native-apps-12
// https://tools.ietf.org/html/draft-ietf-oauth-security-topics-13
// https://developers.google.com/identity/protocols/OAuth2
// https://developers.google.com/identity/protocols/OAuth2InstalledApp
// https://developers.google.com/identity/protocols/googlescopes
// https://developers.google.com/identity/protocols/OpenIDConnect
// https://aaronparecki.com/oauth-2-simplified/
// https://example-app.com/pkce

const
  DEFAULT_REDIRECT_IPV4_HOST    = '127.0.0.1';
  DEFAULT_REDIRECT_IPV6_HOST    = '[::1]';
  DEFAULT_REDIRECT_PORT         = 50000;

  GOOGLE_DISCOVERY_DOCUMENT     = 'https://accounts.google.com/.well-known/openid-configuration';

type
  TOAuthChallengeMethod = (cmPlain, cmSHA256);

  TCEFOAuth2Helper = class
    protected
      FRedirectHost     : ustring;
      FRedirectPort     : integer;
      FAuthEndpoint     : ustring;
      FTokenEndpoint    : ustring;
      FClientID         : ustring;
      FClientSecret     : ustring;
      FAuthCode         : ustring;
      FError            : ustring;
      FErrorDescription : ustring;
      FCodeVerifier     : ustring;
      FCodeChallenge    : ustring;
      FScope            : ustring;
      FAccessToken      : ustring;
      FIDToken          : ustring;
      FState            : ustring;
      FIncState         : ustring;
      FRefreshToken     : ustring;
      FTokenExpiry      : integer;
      FTokenType        : ustring;
      FLoginHint        : ustring;
      FChallengeMethod  : TOAuthChallengeMethod;

      function  GetRedirectURI : ustring; virtual;
      function  GetAuthCodeURI : ustring; virtual;
      function  GetRefreshParams : ustring; virtual;
      function  GetTokeExchangeParams : ustring; virtual;
      function  GetValidState : boolean; virtual;

      procedure GenerateRandomCodeChallenge; virtual;
      procedure GenerateRandomState; virtual;
      function  GenerateRandomString(aLength : cardinal) : ustring;
      procedure ParseQueryPair(const aPair : ustring);
      function  ReadJSONString(const aDictionary : ICefDictionaryValue; const aKey : ustring) : ustring;
      function  ReadJSONInteger(const aDictionary : ICefDictionaryValue; const aKey : ustring) : integer;
      function  CalculateSHA256Hash(const aString : ustring) : TCefCustomByteArray;

    public
      constructor Create;
      procedure   Initialize; virtual;
      function    ParseTokenExchangeResponse(const aResponse : ustring) : boolean; virtual;
      function    ParseRefreshTokenResponse(const aResponse : ustring) : boolean; virtual;
      function    ParseCodeRequestResponse(const aURL : ustring) : boolean; virtual;

      property  AuthEndpoint          : ustring                  read FAuthEndpoint            write FAuthEndpoint;
      property  TokenEndpoint         : ustring                  read FTokenEndpoint           write FTokenEndpoint;

      property  ClientID              : ustring                  read FClientID                write FClientID;
      property  ClientSecret          : ustring                  read FClientSecret            write FClientSecret;

      property  RedirectHost          : ustring                  read FRedirectHost            write FRedirectHost;
      property  RedirectPort          : integer                  read FRedirectPort            write FRedirectPort;

      property  ChallengeMethod       : TOAuthChallengeMethod    read FChallengeMethod         write FChallengeMethod;
      property  Scope                 : ustring                  read FScope                   write FScope;

      property  Error                 : ustring                  read FError;
      property  ErrorDescription      : ustring                  read FErrorDescription;

      property  AccessToken           : ustring                  read FAccessToken;
      property  IDToken               : ustring                  read FIDToken;
      property  RefreshToken          : ustring                  read FRefreshToken;
      property  TokenExpiry           : integer                  read FTokenExpiry;
      property  TokenType             : ustring                  read FTokenType;

      property  CodeVerifier          : ustring                  read FCodeVerifier;
      property  CodeChallenge         : ustring                  read FCodeChallenge;

      property  RedirectURI           : ustring                  read GetRedirectURI;
      property  AuthCodeURI           : ustring                  read GetAuthCodeURI;
      property  TokeExchangeParams    : ustring                  read GetTokeExchangeParams;
      property  RefreshParams         : ustring                  read GetRefreshParams;
      property  ValidState            : boolean                  read GetValidState;
  end;

implementation

uses
  {$IFDEF DELPHI22_UP}System.Hash,{$ENDIF}
  {$IFDEF FPC}DCPSha256,{$ENDIF}
  uCEFMiscFunctions, uCEFJson;

constructor TCEFOAuth2Helper.Create;
begin
  inherited Create;

  Initialize;
end;

procedure TCEFOAuth2Helper.Initialize;
begin
  FRedirectHost     := DEFAULT_REDIRECT_IPV4_HOST;
  FRedirectPort     := DEFAULT_REDIRECT_PORT;                           // This should be a random unused port
  FAuthEndpoint     := 'https://accounts.google.com/o/oauth2/v2/auth';  // obtained from GOOGLE_DISCOVERY_DOCUMENT
  FTokenEndpoint    := 'https://oauth2.googleapis.com/token';           // obtained from GOOGLE_DISCOVERY_DOCUMENT
  FClientID         := '';
  FClientSecret     := '';
  FAuthCode         := '';
  FError            := '';
  FErrorDescription := '';
  FCodeVerifier     := '';
  FCodeChallenge    := '';
  FScope            := 'openid';
  FAccessToken      := '';
  FIDToken          := '';
  FState            := '';
  FIncState         := '';
  FRefreshToken     := '';
  FTokenExpiry      := 0;
  FTokenType        := '';
  FLoginHint        := '';
  FChallengeMethod  := cmPlain;
end;

function TCEFOAuth2Helper.GetRedirectURI : ustring;
begin
  Result := 'http://' + FRedirectHost + ':' + inttostr(FRedirectPort);
end;

function TCEFOAuth2Helper.GetAuthCodeURI : ustring;
begin
  GenerateRandomCodeChallenge;
  GenerateRandomState;

  Result := FAuthEndpoint +
            '?response_type=code' +
            '&client_id='         + CefUriEncode(FClientID, True) +
            '&redirect_uri='      + CefUriEncode(RedirectURI, True) +
            '&scope='             + CefUriEncode(FScope, True) +
            '&state='             + FState +
            '&code_challenge='    + CefUriEncode(FCodeChallenge, True);

  if (ChallengeMethod = cmPlain) then
    Result := Result + '&code_challenge_method=plain'
   else
    Result := Result + '&code_challenge_method=S256';

  if (length(FLoginHint) > 0) then
    Result := Result + '&login_hint=' + CefUriEncode(FLoginHint, True);
end;

function TCEFOAuth2Helper.GetTokeExchangeParams : ustring;
begin
  Result := 'grant_type=authorization_code' +
            '&code='          + FAuthCode +
            '&client_id='     + CefUriEncode(FClientID, True) +
            '&client_secret=' + CefUriEncode(FClientSecret, True) +
            '&redirect_uri='  + CefUriEncode(RedirectURI, True) +
            '&code_verifier=' + CefUriEncode(FCodeVerifier, True);
end;

function TCEFOAuth2Helper.GetRefreshParams : ustring;
begin
  Result := 'grant_type=refresh_token' +
            '&client_id='     + CefUriEncode(FClientID, True) +
            '&client_secret=' + CefUriEncode(FClientSecret, True) +
            '&refresh_token=' + CefUriEncode(FRefreshToken, True);
end;

function TCEFOAuth2Helper.GetValidState : boolean;
begin
  Result := (CompareStr(FState, FIncState) = 0);
end;

function TCEFOAuth2Helper.GenerateRandomString(aLength : cardinal) : ustring;
const
  UnreservedCharValuesLen = 66;
  UnreservedCharValues : array [0..pred(UnreservedCharValuesLen)] of
                           char = ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
                                   'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
                                   'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
                                   'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
                                   '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                                   '-', '.', '_', '~');
var
  i : cardinal;
begin
  Result := '';
  i      := 0;

  Randomize;

  while (i < aLength) do
    begin
      Result := Result + UnreservedCharValues[Random(UnreservedCharValuesLen)];
      inc(i);
    end;
end;

procedure TCEFOAuth2Helper.GenerateRandomCodeChallenge;
var
  TempHash : TCefCustomByteArray;
begin
  // The "code_verifier" can have a minimum length of 43 characters
  // and a maximum length of 128 characters.
  FCodeVerifier := GenerateRandomString(64);

  case ChallengeMethod of
    cmPlain  : FCodeChallenge := FCodeVerifier;

    cmSHA256 :
      try
        TempHash := CalculateSHA256Hash(FCodeVerifier);

        if (length(TempHash) > 0) then
          begin
            FCodeChallenge := CefBase64Encode(@TempHash[0], length(TempHash));

            // Converts base64 to base64url.
            FCodeChallenge := StringReplace(FCodeChallenge, '+', '-', [rfReplaceAll]);
            FCodeChallenge := StringReplace(FCodeChallenge, '/', '_', [rfReplaceAll]);

            // Strips padding.
            while (length(FCodeChallenge) > 0) and (FCodeChallenge[length(FCodeChallenge)] = '=') do
              FCodeChallenge := copy(FCodeChallenge, 1, pred(length(FCodeChallenge)));
          end
         else
          FCodeChallenge := '';
      finally
        if (TempHash <> nil) then
          begin
            Finalize(TempHash);
            TempHash := nil;
          end;
      end;
  end;
end;

{$IFDEF FPC}
function TCEFOAuth2Helper.CalculateSHA256Hash(const aString : ustring) : TCefCustomByteArray;
var
  TempSHA256 : TDCP_sha256;
  TempString : AnsiString;
begin
  Result := nil;
  try
    TempSHA256 := TDCP_sha256.Create(nil);
    TempSHA256.Burn;
    TempSHA256.Init;
    TempString := AnsiString(aString);
    TempSHA256.Update(TempString[1], length(TempString));
    SetLength(Result, 32);
    TempSHA256.Final(Result[0]);
  finally
    FreeAndNil(TempSHA256);
  end;
end;
{$ELSE}
{$IFDEF DELPHI22_UP}
function TCEFOAuth2Helper.CalculateSHA256Hash(const aString : ustring) : TCefCustomByteArray;
var
  TempBytes : TBytes;
begin
  TempBytes := THashSHA2.GetHashBytes(FCodeVerifier);
  SetLength(Result, length(TempBytes));
  Move(TempBytes[0], Result[0], length(TempBytes));
end;
{$ELSE}
// TO-DO: Calculate SHA256 hash in older Delphi versions
function TCEFOAuth2Helper.CalculateSHA256Hash(const aString : ustring) : TCefCustomByteArray;
begin
  Result := nil;
end;
{$ENDIF}
{$ENDIF}

procedure TCEFOAuth2Helper.GenerateRandomState;
begin
  // The "state" should be a string of 30 or so characters constructed
  // using a high-quality random-number generator.

  Randomize;

  FState := IntToHex(Random(high(integer)), 8) +
            IntToHex(Random(high(integer)), 8) +
            IntToHex(Random(high(integer)), 8) +
            IntToHex(Random(high(integer)), 8);
end;

function TCEFOAuth2Helper.ReadJSONString(const aDictionary : ICefDictionaryValue; const aKey : ustring) : ustring;
var
  TempValue : ICefValue;
begin
  Result := '';

  if (aDictionary <> nil) then
    begin
      TempValue := aDictionary.GetValue(aKey);
      if (TempValue <> nil) and (TempValue.GetType = VTYPE_STRING) then Result := TempValue.GetString;
    end;
end;

function TCEFOAuth2Helper.ReadJSONInteger(const aDictionary : ICefDictionaryValue; const aKey : ustring) : integer;
var
  TempValue : ICefValue;
begin
  Result := 0;

  if (aDictionary <> nil) then
    begin
      TempValue := aDictionary.GetValue(aKey);
      if (TempValue <> nil) and (TempValue.GetType = VTYPE_INT) then Result := TempValue.GetInt;
    end;
end;

function TCEFOAuth2Helper.ParseTokenExchangeResponse(const aResponse : ustring) : boolean;
var
  TempRoot       : ICefValue;
  TempDictionary : ICefDictionaryValue;
begin
  Result := False;

  try
    if (length(aResponse) > 0) then
      begin
        TempRoot := TCEFJson.Parse(aResponse);

        if (TempRoot <> nil) and (TempRoot.GetType = VTYPE_DICTIONARY) then
          begin
            TempDictionary := TempRoot.GetDictionary;
            FError         := ReadJSONString(TempDictionary, 'error');

            if (length(FError) > 0) then
              FErrorDescription := ReadJSONString(TempDictionary, 'error_description')
             else
              begin
                FAccessToken := ReadJSONString(TempDictionary, 'access_token');

                if (length(FAccessToken) > 0) then
                  begin
                    FIDToken      := ReadJSONString(TempDictionary,  'id_token');
                    FScope        := ReadJSONString(TempDictionary,  'scope');
                    FTokenType    := ReadJSONString(TempDictionary,  'token_type');
                    FRefreshToken := ReadJSONString(TempDictionary,  'refresh_token');
                    FTokenExpiry  := ReadJSONInteger(TempDictionary, 'expires_in');
                    Result        := True;
                  end;
              end;
          end;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TCEFOAuth2Helper.ParseTokenExchangeResponse', e) then raise;
  end;
end;

function TCEFOAuth2Helper.ParseRefreshTokenResponse(const aResponse : ustring) : boolean;
var
  TempRoot       : ICefValue;
  TempDictionary : ICefDictionaryValue;
begin
  Result := False;

  try
    if (length(aResponse) > 0) then
      begin
        TempRoot := TCEFJson.Parse(aResponse);

        if (TempRoot <> nil) and (TempRoot.GetType = VTYPE_DICTIONARY) then
          begin
            TempDictionary := TempRoot.GetDictionary;
            FError         := ReadJSONString(TempDictionary, 'error');

            if (length(FError) > 0) then
              FErrorDescription := ReadJSONString(TempDictionary, 'error_description')
             else
              begin
                FAccessToken := ReadJSONString(TempDictionary, 'access_token');

                if (length(FAccessToken) > 0) then
                  begin
                    FTokenType   := ReadJSONString(TempDictionary,  'token_type');
                    FTokenExpiry := ReadJSONInteger(TempDictionary, 'expires_in');
                    Result       := True;
                  end;
              end;
          end;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TCEFOAuth2Helper.ParseRefreshTokenResponse', e) then raise;
  end;
end;

procedure TCEFOAuth2Helper.ParseQueryPair(const aPair : ustring);
var
  TempKey   : ustring;
  TempValue : ustring;
  i         : integer;
begin
  i := pos('=', aPair);

  if (i > 0) then
    begin
      TempKey   := copy(aPair, 1, pred(i));
      TempValue := copy(aPair, succ(i), length(aPair));

      if      (CompareStr(TempKey, 'code')              = 0) then FAuthCode         := TempValue
      else if (CompareStr(TempKey, 'state')             = 0) then FIncState         := TempValue
      else if (CompareStr(TempKey, 'error')             = 0) then FError            := TempValue
      else if (CompareStr(TempKey, 'error_description') = 0) then FErrorDescription := TempValue;
    end;
end;

function TCEFOAuth2Helper.ParseCodeRequestResponse(const aURL : ustring) : boolean;
var
  TempParts : TUrlParts;
  TempQuery : ustring;
  i         : integer;
begin
  Result := False;

  if (length(aURL) > 0) then
    begin
      CefParseUrl(aURL, TempParts);

      TempQuery := TempParts.query;

      if (length(TempQuery) > 0) then
        begin
          i := pos('&', TempQuery);

          while (i > 0) do
            begin
              ParseQueryPair(copy(TempQuery, 1, pred(i)));
              TempQuery := copy(TempQuery, succ(i), length(TempQuery));
              i         := pos('&', TempQuery);
            end;

          ParseQueryPair(TempQuery);

          Result := ValidState and (length(FAuthCode) > 0);
        end;
    end;
end;

end.
