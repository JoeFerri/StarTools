{
  This file is part of StarTools.
  StarTools is a fan-made suite of tools for Star Citizen.

  Copyright (c) 2025 Giuseppe Ferri <jfinfoit@gmail.com>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  See the file LICENSE, included in this distribution,
  for details about the copyright.

 **********************************************************************}



{* Web Unit }
unit WebUnit;

{$mode objfpc}{$H+}{$J-}{$R+}

interface

uses
  Classes, SysUtils, Windows, WinHttp, RegExpr, ExtCtrls, Dialogs,
  Graphics, FPReadPNG, FPReadJPEG, LazFileUtils, URIParser,
  //StrUtils,
  IOUnit,
  VersionUnit;




{*
  Scrapes the public dossier of a user to retrieve Organization details.
  @param(Nickname The Star Citizen Nickname to look up.)
  @param(OrgName Output parameter for the Organization SID/Name.)
  @param(LogoURL Output parameter for the absolute URL of the organization's logo.)
  @returns(@True if the organization data was successfully parsed.)
}
function GetOrgDataAndLogoURL(const Nickname: string; out OrgName: string; out LogoURL: UnicodeString): Boolean;

{*
  Retrieves an organization logo, prioritising the local cache over the network.
  If the file exists in the cache, it is loaded into the stream. Otherwise, it is downloaded and saved locally.
  @param(OrgName The name of the organization (used for cache filename).)
  @param(LogoURL The remote URL of the image.)
  @param(TargetStream The destination TStream where the image data will be written.)
  @returns(@True if the data was successfully loaded from cache or downloaded.)
}
function GetOrLoadOrgLogoToStream(const OrgName: string; const LogoURL: UnicodeString; TargetStream: TStream): Boolean;

{*
  Scrapes the public dossier of a user to retrieve the Avatar logo URL.
  @param(Nickname The Star Citizen Nickname to look up.)
  @param(LogoURL Output parameter for the absolute URL of the avatar image.)
  @returns(@True if the avatar URL was successfully parsed.)
}
function GetAvatarDataAndLogoURL(const Nickname: string; out LogoURL: UnicodeString): Boolean;

{*
  Retrieves a citizen's avatar logo, prioritising the local cache over the network.
  If the file exists in the cache, it is loaded into the stream. Otherwise, it is downloaded and saved locally.
  @param(Nickname The Star Citizen Nickname (used for cache filename).)
  @param(LogoURL The remote URL of the image.)
  @param(TargetStream The destination TStream where the image data will be written.)
  @returns(@True if the data was successfully loaded from cache or downloaded.)
}
function GetOrLoadAvatarLogoToStream(const Nickname: string; const LogoURL: UnicodeString; TargetStream: TStream): Boolean;

{*
  Constructs a complete regular expression to find an organization's logo URL based on its name.
  @param(OrgName The Organization SID/Name to include in the regex pattern.)
  @returns(A string containing the formatted Regular Expression.)
}
function RegExOrgLogoURL(OrgName: String): String;





const
  {* Standard HTTPS protocol prefix. }
  HTTPSPrefix = UnicodeString('https://');

  {* User-Agent string used for WinHttp requests. }
  StarToolsAgent = 'StarTools Agent/' + UnicodeString(StarToolsVersionMM);

  {* Main RSI website domain. }
  RobertsSpaceIndustriesHost = UnicodeString('robertsspaceindustries.com');

  {* RSI Content Delivery Network domain. }
  RobertsSpaceIndustriesCDNHost = UnicodeString('cdn.robertsspaceindustries.com');       

  {* RSI Service Status domain. }
  RobertsSpaceIndustriesStatusHost = UnicodeString('status.robertsspaceindustries.com');

  {* URL path prefix for public citizen profiles. }
  ENCitizenPrefix = UnicodeString('/en/citizens/');

  {* URL path prefix for public organization profiles. }
  ENOrgsPrefix = UnicodeString('/en/orgs/');


  { REG EX }

  {* Regular expression to extract the Avatar image URL from a citizen's dossier page. }
  RegExAvatarLogoURL = '(?:"\/account\/profile"|id\s*=\s*"public-profile".*?UEE Citizen Record).*?<img\s+src\s*=\s*"([^>\s"]+)';

  {* Prefix for the regular expression used to find the organization logo link. }
  RegExOrgLogoURLPrefix = '<a\s+href\s*=\s*"\/orgs\/';

  {* Suffix for the regular expression used to find the organization logo link. }
  RegExOrgLogoURLSuffix = '"\s*>\s*<img\s+src\s*=\s*"([^>\s"]+)"';

  {* Regular expression to extract the Spectrum Identification (SID) from an organization block. }
  RegExOrgSID = '<span[^>]*>\s*Spectrum\s+Identification\s+\(SID\)\s*<\/span>\s*<strong[^>]*>\s*([^<\s]+)';

  {*}
  RegExStatusPlatform = 'Platform.*?<span\s+class\s*=\s*"component-status".*?>\s*(.*?)\s*<\/span>'                        {/gs}

  {*}
  RegExStatusPersistentUniverse = 'Persistent Universe.*?<span\s+class\s*=\s*"component-status".*?>\s*(.*?)\s*<\/span>';  {/gs}

  {*}
  RegExStatusArenaCommander = 'Arena Commander.*?<span\s+class\s*=\s*"component-status".*?>\s*(.*?)\s*<\/span>';          {/gs}





implementation



function RegExOrgLogoURL(OrgName: String): String;
begin
  Result := RegExOrgLogoURLPrefix + OrgName + RegExOrgLogoURLSuffix;
end;


function DownloadToStream(hSession: HINTERNET; const FullURL: UnicodeString; Stream: TStream): Boolean;
var
  hConnect, hRequest: HINTERNET;
  wPath: UnicodeString;
  Uri: TURI;
  dwStatusCode, dwSize, dwDownloaded: DWORD;
  Buffer: array[0..8191] of Byte;
begin
  Result := False;

  Uri := ParseURI(String(FullURL));

  // Uri.Path is the path, Uri.Document is the file, Uri.Params are any query params
  // WinHttpOpenRequest requires the full path after the host
  wPath := UnicodeString(Uri.Path) + UnicodeString(Uri.Document) + UnicodeString(Uri.Params);
  if (wPath = '') then wPath := '/';

  hConnect := WinHttpConnect(hSession, PWideChar(UnicodeString(Uri.Host)), Uri.Port, 0);
  //hConnect := WinHttpConnect(hSession, PWideChar(RobertsSpaceIndustriesHost), INTERNET_DEFAULT_HTTPS_PORT, 0);
  if not Assigned(hConnect) then Exit;

  try
    hRequest := WinHttpOpenRequest(hConnect, 'GET', PWideChar(wPath), nil, nil, nil, WINHTTP_FLAG_SECURE);
    //hRequest := WinHttpOpenRequest(hConnect, 'GET', PWideChar(wPath), nil, nil, nil,
    //              ifThen(Uri.Protocol = 'https', WINHTTP_FLAG_SECURE, 0)); // webunit.pas(122,74) Error: Incompatible type for arg no. 3: Got "ShortInt", expected "AnsiString"
    if not Assigned(hRequest) then Exit;

    try
      if WinHttpSendRequest(hRequest, nil, 0, nil, 0, 0, 0) and WinHttpReceiveResponse(hRequest, nil) then
      begin
        dwSize := SizeOf(dwStatusCode);
        WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_STATUS_CODE or WINHTTP_QUERY_FLAG_NUMBER,
                            nil, @dwStatusCode, @dwSize, nil);
        if dwStatusCode = 200 then
        begin
          repeat
            dwDownloaded := 0;
            if not WinHttpReadData(hRequest, @Buffer, SizeOf(Buffer), @dwDownloaded) then Break;
            if dwDownloaded = 0 then Break;
            Stream.Write(Buffer, dwDownloaded);
          until dwDownloaded = 0;
        end;
        Result := (Stream.Size > 0);
      end;
    finally
      WinHttpCloseHandle(hRequest);
    end;
  finally
    WinHttpCloseHandle(hConnect);
  end;
end;



function GetOrLoadOrgLogo(const OrgName: string; const LogoURL: UnicodeString; TargetImage: TImage; hSession: HINTERNET): Boolean;
var
  CacheDir, FileName: string;
  ImgStream: TMemoryStream;
begin
  Result := False;
  if OrgName = '' then Exit;

  CacheDir := AppendPathDelim(ExtractFilePath(GetAppConfigFile(False)) + 'cache');
  if not DirectoryExists(CacheDir) then CreateDir(CacheDir);

  FileName := AppendPathDelim(CacheDir) + OrgName + '.png';

  if FileExists(FileName) then
  begin
    try
      TargetImage.Picture.LoadFromFile(FileName);
      Result := True;
      Exit;
    except
      // If the file is corrupt, proceed with the download
    end;
  end;

  // 2. Download if not present or corrupted
  if LogoURL <> '' then
  begin
    ImgStream := TMemoryStream.Create;
    try
      if DownloadToStream(hSession, LogoURL, ImgStream) then
      begin
        ImgStream.Position := 0;
        TargetImage.Picture.LoadFromStream(ImgStream);
        ImgStream.Position := 0;
        ImgStream.SaveToFile(FileName);
        Result := True;
      end;
    finally
      ImgStream.Free;
    end;
  end;
end;



function GetOrgDataAndLogoURL(const Nickname: string; out OrgName: string; out LogoURL: UnicodeString): Boolean;
var
  hSession, hConnect, hRequest: HINTERNET;
  ResponseData: TStringStream;
  Buffer: array[0..8191] of Byte;
  dwDownloaded: DWORD;
  Html: string;
  RegEx: TRegExpr;
begin
  Result := False;
  OrgName := ''; LogoURL := '';

  hSession := WinHttpOpen(StarToolsAgent, WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, nil, nil, 0);
  if not Assigned(hSession) then Exit;
  try
    hConnect := WinHttpConnect(hSession, RobertsSpaceIndustriesHost, INTERNET_DEFAULT_HTTPS_PORT, 0);
    if Assigned(hConnect) then
    try
      hRequest := WinHttpOpenRequest(hConnect, 'GET', PWideChar(ENCitizenPrefix + UnicodeString(Nickname)), nil, nil, nil, WINHTTP_FLAG_SECURE);
      if Assigned(hRequest) then
      try
        if WinHttpSendRequest(hRequest, nil, 0, nil, 0, 0, 0) and WinHttpReceiveResponse(hRequest, nil) then
        begin
          ResponseData := TStringStream.Create('');
          try
            repeat
              dwDownloaded := 0;
              if not WinHttpReadData(hRequest, @Buffer, SizeOf(Buffer), @dwDownloaded) then Break;
              ResponseData.Write(Buffer, dwDownloaded);
            until dwDownloaded = 0;
            Html := ResponseData.DataString;

            RegEx := TRegExpr.Create;
            try
              RegEx.ModifierS := True;
              // SID Extraction (OrgName)
              RegEx.Expression := RegExOrgSID;
              if RegEx.Exec(Html) then
              begin
                OrgName := Trim(RegEx.Match[1]);
                // Logo Path Extraction
                RegEx.Expression := RegExOrgLogoURL(OrgName);
                if RegEx.Exec(Html) then
                begin
                  LogoURL := HTTPSPrefix + RobertsSpaceIndustriesHost + UnicodeString(RegEx.Match[1]);
                  Result := True;
                end;
              end;
            finally
              RegEx.Free;
            end;
          finally
            ResponseData.Free;
          end;
        end;
      finally
        WinHttpCloseHandle(hRequest);
      end;
    finally
      WinHttpCloseHandle(hConnect);
    end;
  finally
    WinHttpCloseHandle(hSession);
  end;
end;



function GetOrLoadOrgLogoToStream(const OrgName: string; const LogoURL: UnicodeString; TargetStream: TStream): Boolean;
var
  FileName: string;
  hSession: HINTERNET;
begin
  Result := False;
  if not DirectoryExists(GetCacheDir) then
    ForceDirectories(GetCacheDir);
  FileName := GetCacheDir + OrgName + '.png';

  if FileExists(FileName) then
  begin
    try
      TMemoryStream(TargetStream).LoadFromFile(FileName);
      Exit(True);
    except
      //
    end;
  end;

  if LogoURL <> '' then
  begin
    hSession := WinHttpOpen(StarToolsAgent, WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, nil, nil, 0);
    if Assigned(hSession) then
    try
      if DownloadToStream(hSession, LogoURL, TargetStream) then
      begin
        TargetStream.Position := 0;
        TMemoryStream(TargetStream).SaveToFile(FileName);
        Result := True;
      end;
    finally
      WinHttpCloseHandle(hSession);
    end;
  end;
end;



function GetAvatarDataAndLogoURL(const Nickname: string; out LogoURL: UnicodeString): Boolean;
var
  hSession, hConnect, hRequest: HINTERNET;
  ResponseData: TStringStream;
  Buffer: array[0..8191] of Byte;
  dwDownloaded: DWORD;
  Html: string;
  RegEx: TRegExpr;
begin
  Result := False;
  LogoURL := '';

  hSession := WinHttpOpen(StarToolsAgent, WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, nil, nil, 0);
  if not Assigned(hSession) then Exit;
  try
    hConnect := WinHttpConnect(hSession, RobertsSpaceIndustriesHost, INTERNET_DEFAULT_HTTPS_PORT, 0);
    if Assigned(hConnect) then
    try
      hRequest := WinHttpOpenRequest(hConnect, 'GET', PWideChar(ENCitizenPrefix + UnicodeString(Nickname)), nil, nil, nil, WINHTTP_FLAG_SECURE);
      if Assigned(hRequest) then
      try
        if WinHttpSendRequest(hRequest, nil, 0, nil, 0, 0, 0) and WinHttpReceiveResponse(hRequest, nil) then
        begin
          ResponseData := TStringStream.Create('');
          try
            repeat
              dwDownloaded := 0;
              if not WinHttpReadData(hRequest, @Buffer, SizeOf(Buffer), @dwDownloaded) then Break;
              ResponseData.Write(Buffer, dwDownloaded);
            until dwDownloaded = 0;
            Html := ResponseData.DataString;

            RegEx := TRegExpr.Create;
            try
              RegEx.ModifierS := True;
              RegEx.Expression := RegExAvatarLogoURL;

              if RegEx.Exec(Html) then
              begin
                // User 'xxx    ' -> <img src="https://cdn.robertsspaceindustries.com/static/images/account/avatar_default_big.jpg" />  (1)
                // User JoeCrisix -> <img src="/media/ln4m651xxr7v2r/heap_infobox/38723888-4dba-41a9-B135-70ffb1803854.png" />          (2)
                //
                LogoURL := UnicodeString(RegEx.Match[1]);                         // (1)
                if Pos(HTTPSPrefix, RegEx.Match[1]) = 0 then
                  LogoURL := HTTPSPrefix + RobertsSpaceIndustriesHost + LogoURL;  // (2)
                Result := True;
              end;
            finally
              RegEx.Free;
            end;
          finally
            ResponseData.Free;
          end;
        end;
      finally
        WinHttpCloseHandle(hRequest);
      end;
    finally
      WinHttpCloseHandle(hConnect);
    end;
  finally
    WinHttpCloseHandle(hSession);
  end;
end;



function GetOrLoadAvatarLogoToStream(const Nickname: string; const LogoURL: UnicodeString; TargetStream: TStream): Boolean;
var
  FileName: string;
  hSession: HINTERNET;
begin
  Result := False;
  if not DirectoryExists(GetCacheDir) then
    ForceDirectories(GetCacheDir);
  FileName := GetCacheDir + Nickname + '.png';

  if FileExists(FileName) then
  begin
    try
      TMemoryStream(TargetStream).LoadFromFile(FileName);
      Exit(True);
    except
      //
    end;
  end;

  if LogoURL <> '' then
  begin
    hSession := WinHttpOpen(StarToolsAgent, WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, nil, nil, 0);
    if Assigned(hSession) then
    try
      if DownloadToStream(hSession, LogoURL, TargetStream) then
      begin
        TargetStream.Position := 0;
        TMemoryStream(TargetStream).SaveToFile(FileName);
        Result := True;
      end;
    finally
      WinHttpCloseHandle(hSession);
    end;
  end;
end;




end.
