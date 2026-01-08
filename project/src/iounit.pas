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



{* IO Unit }
unit IOUnit;

{$mode objfpc}{$H+}{$J-}{$R+}

interface

uses
  LazFileUtils, SysUtils;



const
  {* The name of the StarCitizen process. }
  StarCitizenProcessName = 'starcitizen.exe';





{* @returns(The directory of the executable.) }
function GetExeDir: string; inline;

{* @returns(The directory of the data files.) }
function GetDataDir: string; inline;        

{* @returns(The directory of the saves files.) }
function GetSavesDir: string; inline;

{* @returns(The path to the configuration file.) }
function GetConfigFilePath: String; inline;

{* @returns(The directory of the configuration file.) }
function GetConfigDir: String; inline;

{* @returns(The directory of the cache files.) }
function GetCacheDir: string; inline;

{* Initializes the paths. }
procedure InitPaths;






implementation


var
  FExeDir         : String;
  FDataDir        : String; 
  FSavesDir       : String;

  FConfigFilePath : String;
  FConfigDir      : String;

  FCacheDir       : String;




function GetExeDir: String;   begin Result := FExeDir; end;

function GetDataDir: String;  begin Result := FDataDir; end;    

function GetSavesDir: String;  begin Result := FSavesDir; end;

function GetConfigFilePath: String;  begin Result := FConfigFilePath; end;

function GetConfigDir: String;  begin Result := FConfigDir; end;

function GetCacheDir: String; begin Result := FCacheDir; end;



procedure InitPaths;
begin
  //FExeDir   := AppendPathDelim(ExtractFilePath(Application.ExeName));
  FExeDir   := AppendPathDelim(ExtractFilePath(ParamStr(0)));

  FDataDir  := AppendPathDelim(FExeDir + 'data');

  FSavesDir := AppendPathDelim(FExeDir + 'saves');

  {$IFDEF MSWINDOWS}
    FConfigFilePath := GetAppConfigFile(False);      // C:\Users\<user>\AppData\Local\StarTools\StarTools.cfg;
  {$ELSE}
    FConfigFilePath := GetAppConfigFile(False,True); // /home/<user>/.config/StarTools/StarTools.cfg;
  {$ENDIF}

  FConfigDir := AppendPathDelim(ExtractFilePath(FConfigFilePath));
  if not DirectoryExists(FConfigDir) then
    ForceDirectories(FConfigDir);

  FCacheDir := AppendPathDelim(FConfigDir + 'cache');
  if not DirectoryExists(FCacheDir) then
    ForceDirectories(FCacheDir);
end;




initialization
FExeDir         := '';
FDataDir        := '';

FConfigFilePath := '';
FConfigDir      := '';

FCacheDir       := '';




end.
