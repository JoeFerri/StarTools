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
  Graphics, FPReadPNG, FPReadJPEG, LazFileUtils,
  VersionUnit;



{*
  Extract Organization SID by HTML

  @param(HTML HTML string)
  @returns(Organization SID)
}
function ExtractOrganizationSIDByHTML(const HTML: string): string;


{*
  Get User Organization

  @param(UserNickName User Nick Name)
  @param(UserOrganizationName User Organization Name)
  @param(UserAgent User Agent)
  @returns(True on success, False on failure)
}
function GetUserOrganization(const UserNickName: string; out UserOrganizationName: string; const UserAgent: UnicodeString = ''): Boolean;

{*}
function GetUserOrganizationAndLogo(const Username: string; out OrgName: string;
  TargetImage: TImage; const UserAgent: UnicodeString = ''): Boolean;


{*}
function GetOrgDataAndLogoURL(const Username: string; out OrgName: string; out LogoURL: string): Boolean;

{*}
function GetOrLoadOrgLogoToStream(const OrgName: string; const LogoURL: string; TargetStream: TStream): Boolean;




const
  StarToolsAgent = 'StarTools Agent/' + UnicodeString(StarToolsVersionMM);



implementation




// <p class="entry"><span class="label data8">Spectrum Identification (SID)</span><strong class="value data9">ORGANIZATION_NAME</strong></p>
function ExtractOrganizationSIDByHTML(const HTML: string): string;
var
  RegEx: TRegExpr;
begin
  Result := '';
  RegEx := TRegExpr.Create;
  try
    RegEx.Expression := '<span[^>]*>\s*Spectrum\s+Identification\s+\(SID\)\s*<\/span>\s*<strong[^>]*>\s*([^<\s]+)';
    RegEx.ModifierS := True;

    if RegEx.Exec(HTML) then
      Result := Trim(RegEx.Match[1]);
  finally
    RegEx.Free;
  end;
end;


function GetUserOrganization(const UserNickName: string; out UserOrganizationName: string; const UserAgent: UnicodeString = ''): Boolean;
var
  hSession, hConnect, hRequest: HINTERNET;
  dwStatusCode, dwSize, dwDownloaded: DWORD;
  wHost, wPath: UnicodeString;
  ResponseData: TStringStream;
  Buffer: array[0..8191] of Byte;
begin
  Result := False;
  UserOrganizationName := '';
  wHost := 'robertsspaceindustries.com';
  wPath := '/en/citizens/' + UnicodeString(UserNickName);

  hSession := WinHttpOpen(PWideChar(UserAgent), WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, nil, nil, 0);
  if not Assigned(hSession) then Exit;

  try
    hConnect := WinHttpConnect(hSession, PWideChar(wHost), INTERNET_DEFAULT_HTTPS_PORT, 0);
    if not Assigned(hConnect) then Exit;

    hRequest := WinHttpOpenRequest(hConnect, 'GET', PWideChar(wPath), nil, nil, nil, WINHTTP_FLAG_SECURE);
    if not Assigned(hRequest) then Exit;

    if WinHttpSendRequest(hRequest, nil, 0, nil, 0, 0, 0) and WinHttpReceiveResponse(hRequest, nil) then
    begin
      dwSize := SizeOf(dwStatusCode);
      WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_STATUS_CODE or WINHTTP_QUERY_FLAG_NUMBER, nil, @dwStatusCode, @dwSize, nil);

      if dwStatusCode = 200 then
      begin
        ResponseData := TStringStream.Create('');
        try
          // Data retrieval loop
          repeat
            dwSize := 0;
            if not WinHttpQueryDataAvailable(hRequest, @dwSize) then Break;
            if dwSize = 0 then Break;
            if dwSize > SizeOf(Buffer) then dwSize := SizeOf(Buffer);

            if WinHttpReadData(hRequest, @Buffer, dwSize, @dwDownloaded) then
              ResponseData.Write(Buffer, dwDownloaded);
          until dwDownloaded = 0;

          // Process the HTML with the Regex function
          UserOrganizationName := ExtractOrganizationSIDByHTML(ResponseData.DataString);

          // If UserOrganizationName is found, the operation is successful
          Result := (UserOrganizationName <> '');
        finally
          ResponseData.Free;
        end;
      end;
    end;
  finally
    WinHttpCloseHandle(hRequest);
    WinHttpCloseHandle(hConnect);
    WinHttpCloseHandle(hSession);
  end;
end;



function DownloadToStream(hSession: HINTERNET; const FullURL: UnicodeString; Stream: TStream): Boolean;
var
  hConnect, hRequest: HINTERNET;
  wHost, wPath: UnicodeString;
  p: Integer;
  dwSize, dwDownloaded: DWORD;
  Buffer: array[0..8191] of Byte;
begin
  Result := False;
  // Basic URL parsing: find host and path
  // Expected format: https://robertsspaceindustries.com/media/...
  wHost := 'robertsspaceindustries.com';
  p := Pos(wHost, FullURL);
  if p = 0 then Exit;

  wPath := Copy(FullURL, p + Length(wHost), Length(FullURL));

  hConnect := WinHttpConnect(hSession, PWideChar(wHost), INTERNET_DEFAULT_HTTPS_PORT, 0);
  if not Assigned(hConnect) then Exit;
  try
    hRequest := WinHttpOpenRequest(hConnect, 'GET', PWideChar(wPath), nil, nil, nil, WINHTTP_FLAG_SECURE);
    if not Assigned(hRequest) then Exit;
    try
      if WinHttpSendRequest(hRequest, nil, 0, nil, 0, 0, 0) and WinHttpReceiveResponse(hRequest, nil) then
      begin
        repeat
          dwDownloaded := 0;
          if not WinHttpReadData(hRequest, @Buffer, SizeOf(Buffer), @dwDownloaded) then Break;
          if dwDownloaded = 0 then Break;
          Stream.Write(Buffer, dwDownloaded);
        until dwDownloaded = 0;
        Result := (Stream.Size > 0);
      end;
    finally
      WinHttpCloseHandle(hRequest);
    end;
  finally
    WinHttpCloseHandle(hConnect);
  end;
end;


function GetOrLoadOrgLogo(const OrgName: string; const LogoURL: string; TargetImage: TImage; hSession: HINTERNET): Boolean;
var
  CacheDir, FileName: string;
  ImgStream: TMemoryStream;
begin
  Result := False;
  if OrgName = '' then Exit;

  // Definisci il percorso della cache
  CacheDir := AppendPathDelim(ExtractFilePath(GetAppConfigFile(False)) + 'cache');
  //CacheDir := AppendPathDelim(ExtractFilePath(ParamStr(0))) + 'cache';
  if not DirectoryExists(CacheDir) then CreateDir(CacheDir);

  FileName := AppendPathDelim(CacheDir) + OrgName + '.png';

  // 1. Controllo Cache locale
  if FileExists(FileName) then
  begin
    try
      TargetImage.Picture.LoadFromFile(FileName);
      Result := True;
      Exit;
    except
      // Se il file è corrotto, procediamo al download
    end;
  end;

  // 2. Download se non presente o corrotto
  if LogoURL <> '' then
  begin
    ImgStream := TMemoryStream.Create;
    try
      if DownloadToStream(hSession, UnicodeString(LogoURL), ImgStream) then
      begin
        ImgStream.Position := 0;
        // Carica nella TImage
        TargetImage.Picture.LoadFromStream(ImgStream);
        // Salva su disco per il prossimo avvio
        ImgStream.Position := 0;
        ImgStream.SaveToFile(FileName);
        Result := True;
      end;
    finally
      ImgStream.Free;
    end;
  end;
end;


function GetUserOrganizationAndLogo(const Username: string; out OrgName: string;
  TargetImage: TImage; const UserAgent: UnicodeString = ''): Boolean;
var
  hSession: HINTERNET;
  hConnect, hRequest: HINTERNET;
  dwSize, dwDownloaded: DWORD;
  wHost, wPath: UnicodeString;
  ResponseData: TStringStream;
  Buffer: array[0..8191] of Byte;
  Html, LogoPath: string;
  RegEx: TRegExpr;
  //ImgStream: TMemoryStream;
begin
  Result := False;
  OrgName := '';
  wHost := 'robertsspaceindustries.com';
  wPath := '/en/citizens/' + UnicodeString(Username);

  hSession := WinHttpOpen(PWideChar(UserAgent), WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, nil, nil, 0);
  if not Assigned(hSession) then Exit;

  try
    hConnect := WinHttpConnect(hSession, PWideChar(wHost), INTERNET_DEFAULT_HTTPS_PORT, 0);
    if not Assigned(hConnect) then Exit;
    try
      hRequest := WinHttpOpenRequest(hConnect, 'GET', PWideChar(wPath), nil, nil, nil, WINHTTP_FLAG_SECURE);
      if not Assigned(hRequest) then Exit;
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
              // 1. Extract SID
              RegEx.Expression := '<span[^>]*>\s*Spectrum\s+Identification\s+\(SID\)\s*<\/span>\s*<strong[^>]*>\s*([^<\s]+)';
              if RegEx.Exec(Html) then
              begin
                OrgName := Trim(RegEx.Match[1]);

                // 2. Extract Logo Path
                RegEx.Expression := '<a\s+href\s*=\s*"\/orgs\/' + OrgName + '"\s*>\s*<img\s+src\s*=\s*"([^>\s"]+)"';
                if RegEx.Exec(Html) then
                begin
                  LogoPath := RegEx.Match[1];
                  Result := GetOrLoadOrgLogo(OrgName, 'https://robertsspaceindustries.com' + LogoPath, TargetImage, hSession);
                  //ImgStream := TMemoryStream.Create;
                  //try
                  //  // Use UnicodeString for the full URL to avoid conversion warnings
                  //  if DownloadToStream(hSession, UnicodeString('https://robertsspaceindustries.com' + LogoPath), ImgStream) then
                  //  begin
                  //    ImgStream.Position := 0;
                  //    TargetImage.Picture.LoadFromStream(ImgStream);
                  //    Result := True;
                  //  end;
                  //finally
                  //  ImgStream.Free;
                  //end;
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



// Restituisce solo i metadati estratti dall'HTML
function GetOrgDataAndLogoURL(const Username: string; out OrgName: string; out LogoURL: string): Boolean;
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
    hConnect := WinHttpConnect(hSession, 'robertsspaceindustries.com', INTERNET_DEFAULT_HTTPS_PORT, 0);
    if Assigned(hConnect) then
    try
      hRequest := WinHttpOpenRequest(hConnect, 'GET', PWideChar(UnicodeString('/en/citizens/' + Username)), nil, nil, nil, WINHTTP_FLAG_SECURE);
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
              // Estrazione SID (OrgName)
              RegEx.Expression := '<span[^>]*>\s*Spectrum\s+Identification\s+\(SID\)\s*<\/span>\s*<strong[^>]*>\s*([^<\s]+)';
              if RegEx.Exec(Html) then
              begin
                OrgName := Trim(RegEx.Match[1]);
                // Estrazione Logo Path
                RegEx.Expression := '<a\s+href\s*=\s*"\/orgs\/' + OrgName + '"\s*>\s*<img\s+src\s*=\s*"([^>\s"]+)"';
                if RegEx.Exec(Html) then
                begin
                  LogoURL := 'https://robertsspaceindustries.com' + RegEx.Match[1];
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



// Gestisce Cache e Download senza toccare la UI
function GetOrLoadOrgLogoToStream(const OrgName: string; const LogoURL: string; TargetStream: TStream): Boolean;
var
  CacheDir, FileName: string;
  hSession: HINTERNET;
begin
  Result := False;
  CacheDir := AppendPathDelim(ExtractFilePath(GetAppConfigFile(False)) + 'cache');
  if not DirectoryExists(CacheDir) then CreateDir(CacheDir);
  FileName := AppendPathDelim(CacheDir) + OrgName + '.png';

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
      if DownloadToStream(hSession, UnicodeString(LogoURL), TargetStream) then
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
