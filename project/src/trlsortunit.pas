{
  This file is part of StarTools.
  StarTools is a fan-made suite of tools for Star Citizen.

  Copyright (c) 2025-2026 Giuseppe Ferri <jfinfoit@gmail.com>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  See the file LICENSE, included in this distribution,
  for details about the copyright.

 **********************************************************************}


{* TRL Sort Unit }
unit TRLSortUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  PanelRowUnit,
  SCUxSizeUnit, ContractUnit;



type
  TPanelRowIndexedRecord = record
    Index: Integer;
    PanelRow: TPanelRow;
  end;

  TPanelRowIndexedRecordList = specialize TList<TPanelRowIndexedRecord>;

  TPanelRowIndexedRecordArray = array of TPanelRowIndexedRecord;

  TPanelRowIndexedRecordCompareFunc = function(constref Left, Right: TPanelRowIndexedRecord): Integer;


  procedure ToggleSortSideDownGroupId;

  procedure ToggleSortSideDownLoadingStation;

  procedure ToggleSortSideDownUnloadingStation;

  procedure ToggleSortSideDownCommodities;

  procedure ToggleSortSideDownSCU;

  procedure ToggleSortSideDownSCUMaxSize;


  function SortGroupIDCompare(constref Left, Right: TPanelRowIndexedRecord): Integer;

  function SortLoadingStationCompare(constref Left, Right: TPanelRowIndexedRecord): Integer;

  function SortUnloadingStationCompare(constref Left, Right: TPanelRowIndexedRecord): Integer;

  function SortCommoditiesCompare(constref Left, Right: TPanelRowIndexedRecord): Integer;

  function SortSCUCompare(constref Left, Right: TPanelRowIndexedRecord): Integer;

  function SortSCUMaxSizeCompare(constref Left, Right: TPanelRowIndexedRecord): Integer;






implementation

var
  _SortSideDownGroupId: Boolean;
  _SortSideDownLoadingStation: Boolean;
  _SortSideDownUnloadingStation: Boolean;
  _SortSideDownCommodities: Boolean;
  _SortSideDownSCU: Boolean;
  _SortSideDownSCUMaxSize: Boolean;




procedure ToggleSortSideDownGroupId;
begin
  _SortSideDownGroupId := not _SortSideDownGroupId;
end;

procedure ToggleSortSideDownLoadingStation;
begin
  _SortSideDownLoadingStation := not _SortSideDownLoadingStation;
end;

procedure ToggleSortSideDownUnloadingStation;
begin
  _SortSideDownUnloadingStation := not _SortSideDownUnloadingStation;
end;

procedure ToggleSortSideDownCommodities;
begin
  _SortSideDownCommodities := not _SortSideDownCommodities;
end;

procedure ToggleSortSideDownSCU;
begin
  _SortSideDownSCU := not _SortSideDownSCU;
end;

procedure ToggleSortSideDownSCUMaxSize;
begin
  _SortSideDownSCUMaxSize := not _SortSideDownSCUMaxSize;
end;



function SortGroupIDCompare(constref Left, Right: TPanelRowIndexedRecord): Integer;
begin
  if Left.PanelRow.GroupId = Right.PanelRow.GroupId then
  begin
    if Left.PanelRow.HasNoData = Right.PanelRow.HasNoData then
    begin
      Result := 0;
      Exit;
    end;
    
    if Left.PanelRow.HasNoData then Result := 1 else Result := -1;
    Exit;
  end;

  if Left.PanelRow.GroupId = 0 then
  begin
    Result := 1;
    Exit;
  end;

  if Right.PanelRow.GroupId = 0 then
  begin
    Result := -1;
    Exit;
  end;

  if _SortSideDownGroupId then
  begin
    Result := Left.PanelRow.GroupId - Right.PanelRow.GroupId;
  end
  else
  begin
    Result := Right.PanelRow.GroupId - Left.PanelRow.GroupId;
  end;
end;


function SortLoadingStationCompare(constref Left, Right: TPanelRowIndexedRecord): Integer;
begin
  if Left.PanelRow.LoadingStationName = Right.PanelRow.LoadingStationName then
  begin
    if Left.PanelRow.HasNoData = Right.PanelRow.HasNoData then
    begin
      Result := 0;
      Exit;
    end;
    
    if Left.PanelRow.HasNoData then Result := 1 else Result := -1;
    Exit;
  end;

  if Left.PanelRow.LoadingStationName = '' then
  begin
    Result := 1;
    Exit;
  end;

  if Right.PanelRow.LoadingStationName = '' then
  begin
    Result := -1;
    Exit;
  end;

  if _SortSideDownLoadingStation then
  begin
    Result := String.Compare(Left.PanelRow.LoadingStationName, Right.PanelRow.LoadingStationName);
  end
  else
  begin
    Result := String.Compare(Right.PanelRow.LoadingStationName, Left.PanelRow.LoadingStationName);
  end;
end;


function SortUnloadingStationCompare(constref Left, Right: TPanelRowIndexedRecord): Integer;
begin
  if Left.PanelRow.UnloadingStationName = Right.PanelRow.UnloadingStationName then
  begin
    if Left.PanelRow.HasNoData = Right.PanelRow.HasNoData then
    begin
      Result := 0;
      Exit;
    end;
    
    if Left.PanelRow.HasNoData then Result := 1 else Result := -1;
    Exit;
  end;

  if Left.PanelRow.UnloadingStationName = '' then
  begin
    Result := 1;
    Exit;
  end;

  if Right.PanelRow.UnloadingStationName = '' then
  begin
    Result := -1;
    Exit;
  end;

  if _SortSideDownUnloadingStation then
  begin
    Result := String.Compare(Left.PanelRow.UnloadingStationName, Right.PanelRow.UnloadingStationName);
  end
  else
  begin
    Result := String.Compare(Right.PanelRow.UnloadingStationName, Left.PanelRow.UnloadingStationName);
  end;
end;


function SortCommoditiesCompare(constref Left, Right: TPanelRowIndexedRecord): Integer;
begin
  if Left.PanelRow.Commodity = Right.PanelRow.Commodity then
  begin
    if Left.PanelRow.HasNoData = Right.PanelRow.HasNoData then
    begin
      Result := 0;
      Exit;
    end;
    
    if Left.PanelRow.HasNoData then Result := 1 else Result := -1;
    Exit;
  end;

  if Left.PanelRow.Commodity = '' then
  begin
    Result := 1;
    Exit;
  end;

  if Right.PanelRow.Commodity = '' then
  begin
    Result := -1;
    Exit;
  end;

  if _SortSideDownCommodities then
  begin
    Result := String.Compare(Left.PanelRow.Commodity, Right.PanelRow.Commodity);
  end
  else
  begin
    Result := String.Compare(Right.PanelRow.Commodity, Left.PanelRow.Commodity);
  end;
end;


function SortSCUCompare(constref Left, Right: TPanelRowIndexedRecord): Integer;
begin
  if Left.PanelRow.SCU = Right.PanelRow.SCU then
  begin
    if Left.PanelRow.HasNoData = Right.PanelRow.HasNoData then
    begin
      Result := 0;
      Exit;
    end;
    
    if Left.PanelRow.HasNoData then Result := 1 else Result := -1;
    Exit;
  end;

  if Left.PanelRow.SCU = 0 then
  begin
    Result := 1;
    Exit;
  end;

  if Right.PanelRow.SCU = 0 then
  begin
    Result := -1;
    Exit;
  end;

  if _SortSideDownSCU then
  begin
    Result := Left.PanelRow.SCU - Right.PanelRow.SCU;
  end
  else
  begin
    Result := Right.PanelRow.SCU - Left.PanelRow.SCU;
  end;
end;


function SortSCUMaxSizeCompare(constref Left, Right: TPanelRowIndexedRecord): Integer;
var
  LeftSCUMaxSize, RightSCUMaxSize: Integer;
begin
  LeftSCUMaxSize  := SCUSizeArray[Left.PanelRow.SCUMaxSize];
  RightSCUMaxSize := SCUSizeArray[Right.PanelRow.SCUMaxSize];

  if LeftSCUMaxSize = RightSCUMaxSize then
  begin
    if Left.PanelRow.HasNoData = Right.PanelRow.HasNoData then
    begin
      Result := 0;
      Exit;
    end;
    
    if Left.PanelRow.HasNoData then Result := 1 else Result := -1;
    Exit;
  end;

  if _SortSideDownSCUMaxSize then
  begin
    Result := LeftSCUMaxSize - RightSCUMaxSize;
  end
  else
  begin
    Result := RightSCUMaxSize - LeftSCUMaxSize;
  end;
end;






initialization
_SortSideDownGroupId := True;
_SortSideDownLoadingStation := True;
_SortSideDownUnloadingStation := True;
_SortSideDownCommodities := True;
_SortSideDownSCU := True;
_SortSideDownSCUMaxSize := True;



finalization



end.

