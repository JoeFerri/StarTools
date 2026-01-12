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


{* Tools for viewing and entering data relating to a commercial route. }
unit PanelRowUnit;

{$mode ObjFPC}{$H+}{$J-}{$R+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, Dialogs,
  Generics.Collections,
  ContractUnit, SCUxSizeUnit,
  ConsoleUnit, ContractDBUnit,
  LCLType, Spin;


type
  { TPanelRow }
  {*
    A panel comprising components for displaying and entering data relating to a commercial route.
    By default, the panel has the Visible field set to False.
  }
  TPanelRow = class(TPanel)
    {* Callback of the @code(onChange()) event of the @link(SpinEditGroupID) component }
    procedure SpinEditGroupIDChange(Sender: TObject);

    {* Callback of the @code(onChange()) event of the @link(ComboBoxLoadingStation) component }
    procedure ComboBoxLoadingStationChange(Sender: TObject);

    {* Callback of the @code(onKeyDown()) event of the @link(ComboBoxLoadingStation) component }
    procedure ComboBoxLoadingStationKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    {* Callback of the @code(onExit()) event of the @link(ComboBoxLoadingStation) component}
    procedure ComboBoxLoadingStationExit(Sender: TObject);

    {* Callback of the @code(onChange()) event of the @link(ComboBoxUnloadingStation) component }
    procedure ComboBoxUnloadingStationChange(Sender: TObject);

    {* Callback of the @code(onKeyDown()) event of the @link(ComboBoxUnloadingStation) component }
    procedure ComboBoxUnloadingStationKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    {* Callback of the @code(onExit()) event of the @link(ComboBoxUnloadingStation) component }
    procedure ComboBoxUnloadingStationExit(Sender: TObject);

    {* Callback of the @code(onChange()) event of the @link(ComboBoxCommodities) component }
    procedure ComboBoxCommoditiesChange(Sender: TObject);

    {* Callback of the @code(onKeyDown()) event of the @link(ComboBoxCommodities) component }
    procedure ComboBoxCommoditiesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    {* Callback of the @code(onExit()) event of the @link(ComboBoxCommodities) component }
    procedure ComboBoxCommoditiesExit(Sender: TObject);

    {* Callback of the @code(onChange()) event of the @link(SpinEditSCU) component }
    procedure SpinEditSCUChange(Sender: TObject);
    
    {* Callback of the @code(onChange()) event of the @link(ComboBoxSCUMaxSize) component }
    procedure ComboBoxSCUMaxSizeChange(Sender: TObject);

    {* Callback of the @code(onChange()) event of the @link(CheckBoxDone) component }
    procedure CheckBoxDoneChange(Sender: TObject);

    {* Callback of the @code(onChange()) event of the @link(CheckBoxShowHideRow) component }
    procedure CheckBoxShowHideRowChange(Sender: TObject);



    {*
      Notification when a single panel/leg changes. Sender = TContractDB.
      @param(Sender The sender object.)
      @param(PanelID The panel ID.)
      @param(AItem The field that changed.)
      @param(ATradeRouteLeg The trade route leg.)
    }
    procedure PanelLegNotify(const Sender: TObject; const PanelID: Integer; const AItem: TTradeRouteLegItem; const ATradeRouteLeg: TTradeRouteLeg);

    {*
      Notification when a contract changes. Sender = TContractDB.
      @param(Sender The sender object.)
      @param(GroupID The group ID.)
      @param(AContract The contract.)
    }
    procedure PanelContractNotify(const Sender: TObject; const GroupID: Integer; const AContract: TContract);
  private
    FID: Integer;
    FContractDB: TContractDB;

    FGroupIDEnabled: Boolean;

    {* String records for filling in @code(TCheckBox) }
    StringListRecord: TItemsStringListRecord;

    {* Pointer to the callback procedure linked to events. }
    FTradeRouteLegCallback: TTradeRouteLegChanged;

    {* Assign each @code(TEditN) the respective value of @code(TradeRouteLeg.SCUxSize). }
    procedure SetEditN(Empty: Boolean);

    {* Set the visibility of each @code(TEditN). }
    procedure SetEditNVisibility;

    {* Create the components and insert them into the panel. }
    procedure CreateComponents;

    {*
      Initialise @link(TItemStringListRecord) by freeing the previous memory if necessary.
      @param(AStringListRecord The string lists for filling in @code(TCheckBox) components.)
    }
    procedure InitItemStringList(AStringListRecord: TItemsStringListRecord);

    {*
      Notify the ContractDB that a panel/leg has changed if ContractDB is assigned.
      @param(AItem The field that changed.)
    }
    procedure TryContractDBPanelChanged(const AItem: TTradeRouteLegItem);

    {* Retrieve the leg of the trade route represented by this panel. }
    function GetTradeRouteLeg : TTradeRouteLeg;

    {* Enable or disable the Group ID field based on the conformity of the trade route leg. }
    procedure GroupIdEnabledCheck; overload;

    {*
      Enable or disable the Group ID field based on the conformity of the trade route leg.
      @param(AEnabled Enable or disable the Group ID field.)
    }
    procedure GroupIdEnabledCheck(const AEnabled: Boolean); overload;

    {* @returns(The group ID of the trade route leg represented by this panel.) }
    function GetGroupId : Integer;
    
    {* @returns(The loading station name of the trade route leg represented by this panel.) }
    function GetLoadingStationName : string;
    
    {* @returns(The unloading station name of the trade route leg represented by this panel.) }
    function GetUnloadingStationName : string;
    
    {* @returns(The name of the goods of the trade route leg represented by this panel.) }
    function GetCommodity : string;
    
    {* @returns(The total number of SCUs of the trade route leg represented by this panel.) }
    function GetSCU : Integer;
    
    {* @returns(The maximum container size of the trade route leg represented by this panel.) }
    function GetSCUMaxSize : TSCUSize;
    
    {* @returns(The route completion flag of the trade route leg represented by this panel.) }
    function GetDone : Boolean;
    
    {* @returns(The route visibility flag of the trade route leg represented by this panel.) }
    function GetHide : Boolean;
    
    {* @returns(@link(TSCUxSizeRecord) of the trade route leg represented by this panel.) }
    function GetSCUxSize : TSCUxSizeRecord;

  public
    {* Generic grouping identifier.}
    SpinEditGroupID: TSpinEdit;

    {* Crate loading station name. }
    ComboBoxLoadingStation: TComboBox;

    {* Crate unloading station name. }
    ComboBoxUnloadingStation: TComboBox;

    {* Name of the goods. }
    ComboBoxCommodities: TComboBox;

    {* Total number of SCUs to be delivered. }
    SpinEditSCU: TSpinEdit;

    {* Maximum container size. }
    ComboBoxSCUMaxSize: TComboBox;

    {* Route completion flag. }
    CheckBoxDone: TCheckBox;

    {* Route visibility flag. }
    CheckBoxShowHideRow: TCheckBox;

    {* Number of 32-size crates. }
    Edit32: TEdit;

    {* Number of 24-size crates. }
    Edit24: TEdit;

    {* Number of 16-size crates. }
    Edit16: TEdit;

    {* Number of 08-size crates. }
    Edit08: TEdit;

    {* Number of 04-size crates. }
    Edit04: TEdit;

    {* Number of 02-size crates. }
    Edit02: TEdit;

    {* Number of 01-size crates. }
    Edit01: TEdit;


    {*
      Generic record grouping identifier.
      Panels with the same GroupId belong to the same contract leg.
    }
    property GroupId: Integer read GetGroupId;

    {* Crate loading station name. }
    property LoadingStationName: String read GetLoadingStationName;

    {* Crate unloading station name. }
    property UnloadingStationName: String read GetUnloadingStationName;

    {* Name of the goods. }
    property Commodity: String read GetCommodity;

    {* Total number of SCUs to be delivered. }
    property SCU: Integer read GetSCU;

    {* Maximum container size. }
    property SCUMaxSize: TSCUSize read GetSCUMaxSize;

    {* Route completion flag. }
    property Done: Boolean read GetDone;

    {* Route visibility flag. }
    property Hide: Boolean read GetHide;

    {* Record to keep track of the number of boxes for each SCU size. }
    property SCUxSize: TSCUxSizeRecord read GetSCUxSize;


    {* Leg of Trade Route }
    property TradeRouteLeg: TTradeRouteLeg read GetTradeRouteLeg;

    {* Unique identifier. }
    property ID: Integer read FID;       

    {* Pointer to the Contract Database. }
    property ContractDB: TContractDB read FContractDB;

    {* Register this panel in @code(ContractDB). }
    procedure RegisterPanel(AContractDB: TContractDB);

    {* Unregister this panel from @code(ContractDB). }
    procedure UnregisterPanel;

    {* Reset @code(StringListRecord) with new data and update the items in @code(ComboBox) }
    procedure ReloadDataListAndSetComboBox(AStringListRecord: TItemsStringListRecord);

    {* Reassign the contents of visual components. }
    procedure TradeRouteLegReload(ATradeRouteLeg: TTradeRouteLeg);

    {* Delete the contents of the visual components. }
    procedure Clear;

    {* Change the anchor component. }
    procedure ChangeAnchorComponent(AAnchorComponent: TControl);

    {* Set the contents of the visual components and notify the ContractDB. }
    procedure SetValue(const AItem: TTradeRouteLegItem; const ATradeRouteLeg: TTradeRouteLeg);

    {* Check if the contents of the visual components are empty. }
    function HasNoData: Boolean;

    {* Create an instance of @link(TPanelRow)

      @param AOwner The owner component, must be a @code(TWinControl).
      @param ACallback Pointer to the callback procedure linked to events.
      @param AAnchorComponent The component to anchor this panel below. If nil, anchors to the main form.
      @param AStringListRecord The string lists for filling in @code(TCheckBox) components.
      @param AConsoleServer Pointer to the console server for logging.
    }
    constructor Create(AOwner: TComponent; ACallback: TTradeRouteLegChanged;
                        AAnchorComponent: TControl; AStringListRecord: TItemsStringListRecord;
                          AConsoleServer: TConsoleReaderThread); reintroduce;

    {* Destroy the instance. }
    destructor Destroy; override;
  end;


  {* Stack type of TPanelRow. }
  TPanelRowStack = specialize TStack<TPanelRow>;

  {* List type of TPanelRow. }
  TPanelRowList  = specialize TList<TPanelRow>;

  {* Array type of TPanelRow. }
  TPanelRowArray = array of TPanelRow;





implementation

var
  {* Console Server }
  Console: TConsoleReaderThread;

const
  Spacing = 5;

  Spacing2 = 10;



function TPanelRow.HasNoData: Boolean;
begin
  Result := (
    (SpinEditGroupID.Value = 0) and
    (ComboBoxLoadingStation.ItemIndex = -1) and
    (ComboBoxUnloadingStation.ItemIndex = -1) and
    (ComboBoxCommodities.ItemIndex = -1) and
    (SpinEditSCU.Value = 0)
  );
end;



procedure TPanelRow.SetValue(const AItem: TTradeRouteLegItem; const ATradeRouteLeg: TTradeRouteLeg);
begin
  case AItem of
    eGroupId: begin
      SpinEditGroupID.Value := ATradeRouteLeg.GroupId;
    end;
    eLoadingStationName: begin
      ComboBoxLoadingStation.ItemIndex := ComboBoxLoadingStation.Items.IndexOf(ATradeRouteLeg.LoadingStationName);
      ComboBoxLoadingStationChange(Self);
    end;
    eUnloadingStationName: begin
      ComboBoxUnloadingStation.ItemIndex := ComboBoxUnloadingStation.Items.IndexOf(ATradeRouteLeg.UnloadingStationName);
      ComboBoxUnloadingStationChange(Self);
    end;
    eCommodity: begin
      ComboBoxCommodities.ItemIndex := ComboBoxCommodities.Items.IndexOf(ATradeRouteLeg.Commodity);
      ComboBoxCommoditiesChange(Self);
    end;
    eSCU: begin
      SpinEditSCU.Value := ATradeRouteLeg.SCU;
    end;
    eSCUMaxSize: begin
      ComboBoxSCUMaxSize.ItemIndex := ComboBoxSCUMaxSize.Items.IndexOf(IntToStr(SCUSizeArray[ATradeRouteLeg.SCUMaxSize]));
      ComboBoxSCUMaxSizeChange(Self);
    end;
    eDone: begin
      CheckBoxDone.Checked := ATradeRouteLeg.Done;
    end;
    eHide: begin
      CheckBoxShowHideRow.Checked := ATradeRouteLeg.Hide;
    end;
  end;

  GroupIdEnabledCheck;
end;



procedure TPanelRow.GroupIdEnabledCheck;
begin
  if TContractLeg.IsConformedTradeRouteLeg(TradeRouteLeg) then
  begin
    FGroupIdEnabled := True;
    SpinEditGroupID.Enabled := True;
  end
  else begin
    FGroupIdEnabled := False;
    SpinEditGroupID.Enabled := False;
    SpinEditGroupID.Value := 0;
    FTradeRouteLegCallback(Self, eGroupId);
  end;
end;



procedure TPanelRow.GroupIdEnabledCheck(const AEnabled: Boolean);
begin
  FGroupIdEnabled := AEnabled;
  SpinEditGroupID.Enabled := AEnabled;
  SpinEditGroupID.Value := 0;
  FTradeRouteLegCallback(Self, eGroupId);
end;



function TPanelRow.GetGroupId : Integer;
begin
  Result := SpinEditGroupID.Value;
end;

function TPanelRow.GetLoadingStationName : string;
begin
  Result := ComboBoxLoadingStation.Text;
end;

function TPanelRow.GetUnloadingStationName : string;
begin
  Result := ComboBoxUnloadingStation.Text;
end;

function TPanelRow.GetCommodity : string;
begin
  Result := ComboBoxCommodities.Text;
end;

function TPanelRow.GetSCU : Integer;
begin
  Result := SpinEditSCU.Value;
end;

function TPanelRow.GetSCUMaxSize : TSCUSize;
begin
  Result := TSCUxSizeRecord.GetSizeValue(StrToInt(ComboBoxSCUMaxSize.Text));
end;

function TPanelRow.GetDone : Boolean;
begin
  Result := CheckBoxDone.Checked;
end;

function TPanelRow.GetHide : Boolean;
begin
  Result := CheckBoxShowHideRow.Checked;
end;

function TPanelRow.GetSCUxSize : TSCUxSizeRecord;
begin
  Result := Default(TSCUxSizeRecord);
  TSCUxSizeRecord.SetSCUxSizeFromSCU(SpinEditSCU.Value, GetSCUMaxSize, Result);
end;



procedure TPanelRow.PanelLegNotify(const Sender: TObject; const PanelID: Integer; const AItem: TTradeRouteLegItem; const ATradeRouteLeg: TTradeRouteLeg);
begin
  Console.DebugLog('TPanelRow.TPanelLegNotify', 'PanelID: ' + IntToStr(PanelID));
  Console.DebugLog('TPanelRow.TPanelLegNotify', 'TradeRouteLegItem: ' + TradeRouteLegItemArray[AItem]);

  SetValue(AItem, ATradeRouteLeg);

  FTradeRouteLegCallback(Self, AItem);
end;



procedure TPanelRow.PanelContractNotify(const Sender: TObject; const GroupID: Integer; const AContract: TContract);
begin
  Console.DebugLog('TPanelRow.PanelContractNotify', 'GroupID: ' + IntToStr(GroupID));
  Console.DebugLog('TPanelRow.PanelContractNotify', Format('Contract ID: %d', [AContract.ID]));

  FTradeRouteLegCallback(Self, eIgnored);
end;



function TPanelRow.GetTradeRouteLeg : TTradeRouteLeg;
begin
  Result := Default(TTradeRouteLeg);
  Result.GroupId := SpinEditGroupID.Value;
  Result.LoadingStationName := ComboBoxLoadingStation.Text;
  Result.UnloadingStationName := ComboBoxUnloadingStation.Text;
  Result.Commodity := ComboBoxCommodities.Text;
  Result.SCU := SpinEditSCU.Value;
  Result.SCUMaxSize := TSCUxSizeRecord.GetSizeValue(StrToIntDef(ComboBoxSCUMaxSize.Text, 0));
  Result.Done := CheckBoxDone.Checked;
  Result.Hide := CheckBoxShowHideRow.Checked;
  TSCUxSizeRecord.SetSCUxSizeFromSCU(SpinEditSCU.Value, Result.SCUMaxSize, Result.SCUxSize);
end;



procedure TPanelRow.InitItemStringList(AStringListRecord: TItemsStringListRecord);
begin
  if Assigned(StringListRecord.StationNames) then
  begin
    StringListRecord.StationNames.Free;
    StringListRecord.StationNames := nil;
  end;
  if Assigned(StringListRecord.Commodities) then
  begin
    StringListRecord.Commodities.Free;
    StringListRecord.Commodities := nil;
  end;

  StringListRecord := AStringListRecord;

  StringListRecord.StationNames := TStringList.Create;
  StringListRecord.Commodities  := TStringList.Create;
  StringListRecord.StationNames.Assign(AStringListRecord.StationNames);
  StringListRecord.Commodities.Assign(AStringListRecord.Commodities);
end;



procedure TPanelRow.TryContractDBPanelChanged(const AItem: TTradeRouteLegItem);
begin
  if FContractDB <> nil then
  begin
    FContractDB.PanelChanged(ID, AItem, TradeRouteLeg);
  end;
end;



procedure TPanelRow.RegisterPanel(AContractDB: TContractDB);
begin
  if AContractDB <> nil then
  begin
    FContractDB := AContractDB; 
    FID := FContractDB.RegisterPanel(@PanelLegNotify, @PanelContractNotify);
  end;
end;



procedure TPanelRow.UnregisterPanel;
begin
  if FContractDB <> nil then
  begin
    FContractDB.UnregisterPanel(FID);
    FContractDB := nil;
  end;
end;



procedure TPanelRow.ReloadDataListAndSetComboBox(AStringListRecord: TItemsStringListRecord);
var
  OldValue: string;
begin
  InitItemStringList(AStringListRecord);

  OldValue := ComboBoxLoadingStation.Text;
  ComboBoxLoadingStation.Items.BeginUpdate;
  try
    ComboBoxLoadingStation.Items.Clear;
    ComboBoxLoadingStation.Items.AddStrings(StringListRecord.StationNames);
  finally
    ComboBoxLoadingStation.Items.EndUpdate;
  end;
  ComboBoxLoadingStation.ItemIndex := ComboBoxLoadingStation.Items.IndexOf(OldValue);
  ComboBoxLoadingStationChange(Self);

  OldValue := ComboBoxUnloadingStation.Text;
  ComboBoxUnloadingStation.Items.BeginUpdate;
  try
    ComboBoxUnloadingStation.Items.Clear;
    ComboBoxUnloadingStation.Items.AddStrings(StringListRecord.StationNames);
  finally
    ComboBoxUnloadingStation.Items.EndUpdate;
  end;
  ComboBoxUnloadingStation.ItemIndex := ComboBoxUnloadingStation.Items.IndexOf(OldValue);
  ComboBoxUnloadingStationChange(Self);

  OldValue := ComboBoxCommodities.Text;
  ComboBoxCommodities.Items.BeginUpdate;
  try
    ComboBoxCommodities.Items.Clear;
    ComboBoxCommodities.Items.AddStrings(StringListRecord.Commodities);
  finally
    ComboBoxCommodities.Items.EndUpdate;
  end;
  ComboBoxCommodities.ItemIndex := ComboBoxCommodities.Items.IndexOf(OldValue);
  ComboBoxCommoditiesChange(Self);
end;



procedure TPanelRow.TradeRouteLegReload(ATradeRouteLeg: TTradeRouteLeg);
begin
  ComboBoxLoadingStation.ItemIndex := ComboBoxLoadingStation.Items.IndexOf(ATradeRouteLeg.LoadingStationName);
  ComboBoxLoadingStationChange(Self);

  ComboBoxUnloadingStation.ItemIndex := ComboBoxUnloadingStation.Items.IndexOf(ATradeRouteLeg.UnloadingStationName);
  ComboBoxUnloadingStationChange(Self);

  ComboBoxCommodities.ItemIndex := ComboBoxCommodities.Items.IndexOf(ATradeRouteLeg.Commodity);
  ComboBoxCommoditiesChange(Self);

  SpinEditSCU.Value := ATradeRouteLeg.SCU;

  ComboBoxSCUMaxSize.ItemIndex := ComboBoxSCUMaxSize.Items.IndexOf(IntToStr(SCUSizeArray[ATradeRouteLeg.SCUMaxSize]));
  if ComboBoxSCUMaxSize.ItemIndex < 0 then
    ComboBoxSCUMaxSize.ItemIndex := 0; // ASSERT: no empty list
  ComboBoxSCUMaxSizeChange(Self);

  CheckBoxDone.Checked := ATradeRouteLeg.Done;

  CheckBoxShowHideRow.Checked := ATradeRouteLeg.Hide;
  
  SpinEditGroupID.Value := ATradeRouteLeg.GroupId;
end;



procedure TPanelRow.Clear;
begin
  SpinEditGroupID.Value := 0;

  ComboBoxLoadingStation.ItemIndex := -1;
  ComboBoxLoadingStationChange(Self);

  ComboBoxUnloadingStation.ItemIndex := -1;
  ComboBoxUnloadingStationChange(Self);

  ComboBoxCommodities.ItemIndex := -1;
  ComboBoxCommoditiesChange(Self);

  SpinEditSCU.Value := 0;

  ComboBoxSCUMaxSize.ItemIndex := 0;
  ComboBoxSCUMaxSizeChange(Self);

  CheckBoxDone.Checked := False;

  CheckBoxShowHideRow.Checked := False;
end;



procedure TPanelRow.SetEditN(Empty: Boolean);
var
  ATradeRouteLeg: TTradeRouteLeg;
begin
  if Empty then
  begin
    Edit32.Text := '';
    Edit24.Text := '';
    Edit16.Text := '';
    Edit08.Text := '';
    Edit04.Text := '';
    Edit02.Text := '';
    Edit01.Text := '';
  end
  else
  begin
    ATradeRouteLeg := GetTradeRouteLeg;
    Edit32.Text := IntToStr(ATradeRouteLeg.SCUxSize.SCUSize32);
    Edit24.Text := IntToStr(ATradeRouteLeg.SCUxSize.SCUSize24);
    Edit16.Text := IntToStr(ATradeRouteLeg.SCUxSize.SCUSize16);
    Edit08.Text := IntToStr(ATradeRouteLeg.SCUxSize.SCUSize08);
    Edit04.Text := IntToStr(ATradeRouteLeg.SCUxSize.SCUSize04);
    Edit02.Text := IntToStr(ATradeRouteLeg.SCUxSize.SCUSize02);
    Edit01.Text := IntToStr(ATradeRouteLeg.SCUxSize.SCUSize01);
  end;
end;



procedure TPanelRow.SetEditNVisibility;
var
  ASCUxSize: TSCUxSizeRecord;
  Visibility: Boolean;
begin
  ASCUxSize := GetTradeRouteLeg.SCUxSize;
  Visibility := False;

  Visibility := Visibility or (ASCUxSize.TotalSCU = 0) or (ASCUxSize.SCUSize32 > 0);
  Edit32.Visible := Visibility;

  Visibility := Visibility or (ASCUxSize.TotalSCU = 0) or (ASCUxSize.SCUSize24 > 0);
  Edit24.Visible := Visibility;

  Visibility := Visibility or (ASCUxSize.TotalSCU = 0) or (ASCUxSize.SCUSize16 > 0);
  Edit16.Visible := Visibility;

  Visibility := Visibility or (ASCUxSize.TotalSCU = 0) or (ASCUxSize.SCUSize08 > 0);
  Edit08.Visible := Visibility;
  
  Visibility := Visibility or (ASCUxSize.TotalSCU = 0) or (ASCUxSize.SCUSize04 > 0);
  Edit04.Visible := Visibility;
  
  Visibility := Visibility or (ASCUxSize.TotalSCU = 0) or (ASCUxSize.SCUSize02 > 0);
  Edit02.Visible := Visibility;
  
  Visibility := Visibility or (ASCUxSize.TotalSCU = 0) or (ASCUxSize.SCUSize01 > 0);
  Edit01.Visible := Visibility;
end;



procedure TPanelRow.SpinEditGroupIDChange(Sender: TObject);
begin
  TryContractDBPanelChanged(eGroupId);
end;



procedure ComboBoxControlItemExit(CB: TComboBox);
var
  S: String;
  Index: Integer;
begin
  if CB.Items.IndexOf(CB.Text) < 0 then
  begin
    S := CB.Text;
    S := S.Trim;
    Index := CB.Items.IndexOf(S);
    if Index < 0 then
    begin
      CB.ItemIndex := -1;
      CB.Text := '';
    end
    else begin
      CB.ItemIndex := Index;
      CB.Text := S;
    end;
    CB.OnChange(nil);
  end;
end;



procedure TPanelRow.ComboBoxLoadingStationExit(Sender: TObject);
begin
  ComboBoxControlItemExit(ComboBoxLoadingStation);
  TryContractDBPanelChanged(eLoadingStationName);
end;



procedure TPanelRow.ComboBoxUnloadingStationExit(Sender: TObject);
begin
  ComboBoxControlItemExit(ComboBoxUnloadingStation);
  TryContractDBPanelChanged(eUnloadingStationName);
end;



procedure TPanelRow.ComboBoxCommoditiesExit(Sender: TObject);
begin
  ComboBoxControlItemExit(ComboBoxCommodities);
  TryContractDBPanelChanged(eCommodity);
end;



procedure TPanelRow.ComboBoxLoadingStationChange(Sender: TObject);
begin
  if ComboBoxLoadingStation.Items.IndexOf(ComboBoxLoadingStation.Text) >= 0 then
    TryContractDBPanelChanged(eLoadingStationName)
  else
    GroupIdEnabledCheck(False);
  FTradeRouteLegCallback(Self, eLoadingStationName);
end;



procedure TPanelRow.ComboBoxUnloadingStationChange(Sender: TObject);
begin
  if ComboBoxUnloadingStation.Items.IndexOf(ComboBoxUnloadingStation.Text) >= 0 then
    TryContractDBPanelChanged(eUnloadingStationName)
  else
    GroupIdEnabledCheck(False);
  FTradeRouteLegCallback(Self, eUnloadingStationName);
end;



procedure TPanelRow.ComboBoxCommoditiesChange(Sender: TObject);
begin
  if ComboBoxCommodities.Items.IndexOf(ComboBoxCommodities.Text) >= 0 then
    TryContractDBPanelChanged(eCommodity)
  else
    GroupIdEnabledCheck(False);
  FTradeRouteLegCallback(Self, eCommodity);
end;



procedure TPanelRow.ComboBoxLoadingStationKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Shift := Shift;
  case Key of
    VK_RETURN:
      begin
        SetFocus;
        Key := 0;
      end
  else
    begin
    end;
  end;
end;



procedure TPanelRow.ComboBoxUnloadingStationKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Shift := Shift;
  case Key of
    VK_RETURN:
      begin
        SetFocus;
        Key := 0;
      end
  else
    begin
    end;
  end;
end;



procedure TPanelRow.ComboBoxCommoditiesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Shift := Shift;
  case Key of
    VK_RETURN:
      begin
        SetFocus;
        Key := 0;
      end
  else
    begin
    end;
  end;
end;



procedure TPanelRow.SpinEditSCUChange(Sender: TObject);
begin
  if SpinEditSCU.Value > 0 then
    SetEditN(False)
  else
    SetEditN(True);

  SetEditNVisibility;

  TryContractDBPanelChanged(eSCU);
  FTradeRouteLegCallback(Self, eSCU);
end;



procedure TPanelRow.ComboBoxSCUMaxSizeChange(Sender: TObject);
begin
  Console.DebugLog('TPanelRow.ComboBoxSCUMaxSizeChange', 'Text: ' + ComboBoxSCUMaxSize.Text);
  if SpinEditSCU.Value > 0 then
  begin
    SetEditN(False);
  end;

  SetEditNVisibility;

  TryContractDBPanelChanged(eSCUMaxSize);
  FTradeRouteLegCallback(Self, eSCUMaxSize);
end;



procedure TPanelRow.CheckBoxDoneChange(Sender: TObject);
begin
  if CheckBoxDone.Checked then
  begin
    BevelInner := bvLowered;
    BevelOuter := bvLowered;

    SpinEditGroupID.Enabled := False;
    ComboBoxLoadingStation.Enabled := False;
    ComboBoxUnloadingStation.Enabled := False;
    ComboBoxCommodities.Enabled := False;
    SpinEditSCU.Enabled := False;
    ComboBoxSCUMaxSize.Enabled := False;

    Edit32.Enabled := False;
    Edit24.Enabled := False;
    Edit16.Enabled := False;
    Edit08.Enabled := False;
    Edit04.Enabled := False;
    Edit02.Enabled := False;
    Edit01.Enabled := False;
  end
  else
  begin
    BevelInner := bvNone;
    BevelOuter := bvNone;

    if FGroupIDEnabled then SpinEditGroupID.Enabled := True;
    ComboBoxLoadingStation.Enabled := True;
    ComboBoxUnloadingStation.Enabled := True;
    ComboBoxCommodities.Enabled := True;
    SpinEditSCU.Enabled := True;
    ComboBoxSCUMaxSize.Enabled := True;

    Edit32.Enabled := True;
    Edit24.Enabled := True;
    Edit16.Enabled := True;
    Edit08.Enabled := True;
    Edit04.Enabled := True;
    Edit02.Enabled := True;
    Edit01.Enabled := True;
  end;

  TryContractDBPanelChanged(eDone);
  FTradeRouteLegCallback(Self, eDone);
end;



procedure TPanelRow.CheckBoxShowHideRowChange(Sender: TObject);
begin
  if CheckBoxShowHideRow.Checked then
  begin
    SpinEditGroupID.Hide;
    ComboBoxLoadingStation.Hide;
    ComboBoxUnloadingStation.Hide;
    ComboBoxCommodities.Hide;
    SpinEditSCU.Hide;
    ComboBoxSCUMaxSize.Hide;
    CheckBoxDone.Hide;

    Edit32.Hide;
    Edit24.Hide;
    Edit16.Hide;
    Edit08.Hide;
    Edit04.Hide;
    Edit02.Hide;
    Edit01.Hide;
  end
  else
  begin
    SpinEditGroupID.Show;
    ComboBoxLoadingStation.Show;
    ComboBoxUnloadingStation.Show;
    ComboBoxCommodities.Show;
    SpinEditSCU.Show;
    ComboBoxSCUMaxSize.Show;
    CheckBoxDone.Show;

    Edit32.Show;
    Edit24.Show;
    Edit16.Show;
    Edit08.Show;
    Edit04.Show;
    Edit02.Show;
    Edit01.Show;
  end;

  TryContractDBPanelChanged(eHide);
  FTradeRouteLegCallback(Self, eHide);
end;



procedure TPanelRow.ChangeAnchorComponent(AAnchorComponent: TControl);
begin
  if Assigned(AAnchorComponent) then
  begin
    AnchorSideTop.Control := AAnchorComponent;
    AnchorSideTop.Side := asrBottom;
    BorderSpacing.Top := Spacing;
  end
  else
  begin
    AnchorSideTop.Control := Parent;
    AnchorSideTop.Side := asrTop;
    BorderSpacing.Top := 65;
  end;
end;



constructor TPanelRow.Create(AOwner: TComponent; ACallback: TTradeRouteLegChanged;
                                AAnchorComponent: TControl; AStringListRecord: TItemsStringListRecord;
                                  AConsoleServer: TConsoleReaderThread);
begin
  inherited Create(AOwner);

  Self.DisableAlign;

  try
    FGroupIDEnabled := False;

    Self.Visible := False;
    Self.DoubleBuffered := True;

    FContractDB := nil;
    Console := AConsoleServer;

    StringListRecord := default(TItemsStringListRecord);
    StringListRecord.StationNames := nil;
    StringListRecord.Commodities := nil;

    FID := -1;

    FTradeRouteLegCallback := ACallback;

    if not (AOwner is TWinControl) then
      raise EInvalidOperation.CreateFmt(
        '%s requires TWinControl as owner, got %s',
        [ClassName, AOwner.ClassName]
      );
    
    Parent := TWinControl(AOwner);

    Anchors := [akLeft, akRight, akTop];
    AnchorSideLeft.Control := TControl(AOwner);
    AnchorSideLeft.Side := asrTop;
    AnchorSideRight.Control := TControl(AOwner);
    AnchorSideRight.Side := asrBottom;

    ChangeAnchorComponent(AAnchorComponent);

    Height := 30;

    //BevelColor := clTeal;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    BorderSpacing.Around := Spacing;

    CreateComponents;

    ReloadDataListAndSetComboBox(AStringListRecord);
  finally
    Self.EnableAlign;
  end;
end;


destructor TPanelRow.Destroy;
begin
  StringListRecord.StationNames.Free;
  StringListRecord.StationNames := nil;
  StringListRecord.Commodities.Free;
  StringListRecord.Commodities := nil;

  inherited Destroy;
end;



procedure TPanelRow.CreateComponents;
  {* Create the subsequent @code(TEdit) component in cascade. }
  procedure SetupEdit(var E: TEdit; AnchorTo: TControl; Side: TAnchorSideReference; Spacing: Integer);
  begin
    E := TEdit.Create(Self);
    E.Parent := Self;
    E.Width := 44;
    E.ReadOnly := True;
    E.NumbersOnly := True;
    E.Anchors := [akTop, akRight];
    
    E.AnchorSideRight.Control := AnchorTo;
    E.AnchorSideRight.Side := Side;
    E.AnchorSideTop.Control := SpinEditGroupID;
    E.AnchorSideTop.Side := asrCenter;
    E.BorderSpacing.Right := Spacing;
  end;
begin
  //--------------------------------------------------------------------------
  // Contract ID
  //--------------------------------------------------------------------------
  SpinEditGroupID := TSpinEdit.Create(self);
  SpinEditGroupID.Parent := self;
  SpinEditGroupID.Left := 5;
  SpinEditGroupID.Top := 0;
  SpinEditGroupID.Width := 59;
  SpinEditGroupID.AnchorSideTop.Control := Self;
  SpinEditGroupID.AnchorSideTop.Side := asrCenter;
  SpinEditGroupID.OnChange := @SpinEditGroupIDChange;
  SpinEditGroupID.MaxLength := 6;
  SpinEditGroupID.MaxValue := 999999;
  SpinEditGroupID.MinValue := 0;
  SpinEditGroupID.Enabled := False;

  //--------------------------------------------------------------------------
  // Loading Station
  //--------------------------------------------------------------------------
  ComboBoxLoadingStation := TComboBox.Create(self);
  ComboBoxLoadingStation.Parent := self;
  ComboBoxLoadingStation.Left := SpinEditGroupID.Left + SpinEditGroupID.Width + 10;
  ComboBoxLoadingStation.Top := 0;
  ComboBoxLoadingStation.Width := 220;
  ComboBoxLoadingStation.AutoComplete := True;
  ComboBoxLoadingStation.AnchorSideTop.Control := SpinEditGroupID;
  ComboBoxLoadingStation.AnchorSideTop.Side := asrCenter;
  ComboBoxLoadingStation.OnChange := @ComboBoxLoadingStationChange;
  ComboBoxLoadingStation.OnExit := @ComboBoxLoadingStationExit;
  ComboBoxLoadingStation.OnKeyDown := @ComboBoxLoadingStationKeyDown;
  //ComboBoxLoadingStation.OnEditingDone := @ComboBoxLoadingStationEditingDone;

  //--------------------------------------------------------------------------
  // Unloading Station
  //--------------------------------------------------------------------------
  ComboBoxUnloadingStation := TComboBox.Create(self);
  ComboBoxUnloadingStation.Parent := self;
  ComboBoxUnloadingStation.Left := ComboBoxLoadingStation.Left + ComboBoxLoadingStation.Width + 10;
  ComboBoxUnloadingStation.Top := 0;
  ComboBoxUnloadingStation.Width := 220;
  ComboBoxUnloadingStation.AutoComplete := True;
  ComboBoxUnloadingStation.AnchorSideTop.Control := SpinEditGroupID;
  ComboBoxUnloadingStation.AnchorSideTop.Side := asrCenter;
  ComboBoxUnloadingStation.OnChange := @ComboBoxUnloadingStationChange;
  ComboBoxUnloadingStation.OnExit := @ComboBoxUnloadingStationExit;
  ComboBoxUnloadingStation.OnKeyDown := @ComboBoxUnloadingStationKeyDown;

  //--------------------------------------------------------------------------
  // Commodities
  //--------------------------------------------------------------------------
  ComboBoxCommodities := TComboBox.Create(self);
  ComboBoxCommodities.Parent := self;
  ComboBoxCommodities.Left := ComboBoxUnloadingStation.Left + ComboBoxUnloadingStation.Width + 10;
  ComboBoxCommodities.Top := 0;
  ComboBoxCommodities.Width := 170;
  ComboBoxCommodities.AutoComplete := True;
  ComboBoxCommodities.AnchorSideTop.Control := SpinEditGroupID;
  ComboBoxCommodities.AnchorSideTop.Side := asrCenter;
  ComboBoxCommodities.OnChange := @ComboBoxCommoditiesChange;
  ComboBoxCommodities.OnExit := @ComboBoxCommoditiesExit;
  ComboBoxCommodities.OnKeyDown := @ComboBoxCommoditiesKeyDown;

  //--------------------------------------------------------------------------
  // SCU
  //--------------------------------------------------------------------------
  SpinEditSCU := TSpinEdit.Create(self);
  SpinEditSCU.Parent := self;
  SpinEditSCU.Left := ComboBoxCommodities.Left + ComboBoxCommodities.Width + 10;
  SpinEditSCU.Top := 0;
  SpinEditSCU.Width := 59;
  SpinEditSCU.NumbersOnly := True;
  SpinEditSCU.AnchorSideTop.Control := SpinEditGroupID;
  SpinEditSCU.AnchorSideTop.Side := asrCenter;
  SpinEditSCU.OnChange := @SpinEditSCUChange;
  SpinEditSCU.MaxLength := 6;  
  SpinEditSCU.MaxValue := 999999;
  SpinEditSCU.MinValue := 0;

  //--------------------------------------------------------------------------
  // Max Size
  //--------------------------------------------------------------------------
  ComboBoxSCUMaxSize := TComboBox.Create(self);
  ComboBoxSCUMaxSize.Parent := self;
  ComboBoxSCUMaxSize.Left := SpinEditSCU.Left + SpinEditSCU.Width + 10;
  ComboBoxSCUMaxSize.Top := 0;
  ComboBoxSCUMaxSize.Width := 44;

  ComboBoxSCUMaxSize.Items.AddStrings(['32', '24', '16', '8', '4', '2', '1']);
  ComboBoxSCUMaxSize.ItemIndex := 0;
  ComboBoxSCUMaxSize.ReadOnly := True;
  ComboBoxSCUMaxSize.AnchorSideTop.Control := SpinEditGroupID;
  ComboBoxSCUMaxSize.AnchorSideTop.Side := asrCenter;
  ComboBoxSCUMaxSize.OnChange := @ComboBoxSCUMaxSizeChange;

  //--------------------------------------------------------------------------
  // Done
  //--------------------------------------------------------------------------
  CheckBoxDone := TCheckBox.Create(self);
  CheckBoxDone.Parent := self;
  CheckBoxDone.Left := ComboBoxSCUMaxSize.Left + ComboBoxSCUMaxSize.Width + 30;
  CheckBoxDone.Top := 0;
  CheckBoxDone.AnchorSideTop.Control := ComboBoxSCUMaxSize;
  CheckBoxDone.AnchorSideTop.Side := asrCenter;
  CheckBoxDone.OnChange := @CheckBoxDoneChange;

  //--------------------------------------------------------------------------
  // Show/Hide Row
  //--------------------------------------------------------------------------
  CheckBoxShowHideRow := TCheckBox.Create(self);
  CheckBoxShowHideRow.Parent := self;
  CheckBoxShowHideRow.Left := CheckBoxDone.Left + 58;
  CheckBoxShowHideRow.Top := 0;
  CheckBoxShowHideRow.AnchorSideTop.Control := ComboBoxSCUMaxSize;
  CheckBoxShowHideRow.AnchorSideTop.Side := asrCenter;
  CheckBoxShowHideRow.OnChange := @CheckBoxShowHideRowChange;

  //--------------------------------------------------------------------------
  // Edit 32-01 fields (aligned on the right)
  //--------------------------------------------------------------------------
  SetupEdit(Edit01, Self, asrBottom, Spacing);

  SetupEdit(Edit02, Edit01, asrLeft, Spacing2);
  SetupEdit(Edit04, Edit02, asrLeft, Spacing2);
  SetupEdit(Edit08, Edit04, asrLeft, Spacing2);
  SetupEdit(Edit16, Edit08, asrLeft, Spacing2);
  SetupEdit(Edit24, Edit16, asrLeft, Spacing2);
  SetupEdit(Edit32, Edit24, asrLeft, Spacing2);
end;







initialization



finalization



end.

