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
  Classes, SysUtils, Windows, WinHttp, RegExpr;



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




end.
