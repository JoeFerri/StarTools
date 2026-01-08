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


{* Version Unit }
unit VersionUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;


const                   // TODO: Make this dynamic
  {* Major Version Value }
  StarToolsVersionMajor = '1';
  {* Minor Version Value }
  StarToolsVersionMinor = '1';
  {* Patch Version Value }
  StarToolsVersionPatch = '0';

  {* Pre-Release Version Value }
  StarToolsVersionPreRelease = 'alpha';
  {* Major.Minor Version Value }
  StarToolsVersionMM = StarToolsVersionMajor + '.' + StarToolsVersionMinor;
  {* Major.Minor.Patch Version Value }
  StarToolsVersionMMP = StarToolsVersionMM + '.' + StarToolsVersionPatch;

  {* StarTools Version }
  StarToolsVersion = StarToolsVersionMMP + '-' + StarToolsVersionPreRelease;




implementation





end.

