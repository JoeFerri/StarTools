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


{* SCUxSizeForm Unit }
unit SCUxSizeFormUnit;

{$mode objfpc}{$H+}{$J-}{$R+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls,
  IniPropStorage,
  Menus, ActnList,
  fpjson, jsonparser,
  Generics.Defaults,
  LCLType, ComCtrls, Spin, BGRACustomDrawn, Windows,
  StateFormUnit,
  ContractUnit, PanelRowUnit, SCUxSizeUnit, SizeDialogUnit, ContractDBUnit,
  ContractViewUnit,
  TRLSortUnit,
  ConsoleUnit, MainServiceUnit;

const
  {* Minimum number of @link(TPanelRow). }
  NPanelRowMin: Integer = 8;
  {* Maximum number of @link(TPanelRow). }
  NPanelRowMax: Integer = 32;

type
  { TFormSCUxSize }
  TFormSCUxSize = class(TForm)
    ActionShowConsoleSettings: TAction;
    ActionOpenCommodities: TAction;
    ActionOpenStations: TAction;
    ActionReloadDataList: TAction;
    ActionSave: TAction;
    ActionLoad: TAction;
    ActionList: TActionList;
    ApplicationProperties: TApplicationProperties;
    Bevel1: TBevel;
    Bevel10: TBevel;
    Bevel11: TBevel;
    Bevel12: TBevel;
    Bevel13: TBevel;
    Bevel14: TBevel;
    Bevel15: TBevel;
    Bevel16: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    BevelTotals: TBevel;
    BevelTotalsFiltered: TBevel;
    CheckBoxRoutesShown: TCheckBox;
    CheckBoxRoutesDone: TCheckBox;
    CheckBoxRoutesGroup: TCheckBox;
    CoolBarTopLeftMenu: TCoolBar;
    EditSCUTotal01: TEdit;
    EditSCUTotal02: TEdit;
    EditSCUTotal04: TEdit;
    EditSCUTotal08: TEdit;
    EditSCUTotalFiltered01: TEdit;
    EditSCUTotalFiltered02: TEdit;
    EditSCUTotalFiltered04: TEdit;
    EditSCUTotalFiltered08: TEdit;
    EditSCUTotalFiltered16: TEdit;
    EditSCUTotalFiltered24: TEdit;
    EditSCUTotalFiltered32: TEdit;
    EditSCUTotalDone: TEdit;
    EditSCUTotal16: TEdit;
    EditSCUTotalToDo: TEdit;
    EditSCUTotal24: TEdit;
    EditSCUTotal32: TEdit;
    EditSCUTotal: TEdit;
    GroupBoxTotalFilters: TGroupBox;
    ImageListMenu: TImageList;
    ImageWarning_1: TImage;
    ImageWarning_2: TImage;
    ImageCancelGroupID: TImage;
    ImageCancelLoadingStation: TImage;
    ImageCancelDone: TImage;
    ImageShow: TImage;
    ImageCancelUnloadingStation: TImage;
    ImageCancelCommodities: TImage;
    ImageSetSCUMaxSize: TImage;
    ImageSortSCU: TImage;
    ImageSortGroupID: TImage;
    ImageAddRow: TImage;
    ImageDeleteRow: TImage;
    ImageSortLoadingStation: TImage;
    ImageSortUnloadingStation: TImage;
    ImageSortCommodities: TImage;
    ImageSortSCUMaxSize: TImage;
    ImageCancelSCU: TImage;
    IniPropStorage: TIniPropStorage;
    LabelTotalSCU: TLabel;
    Label32: TLabel;
    Label24: TLabel;
    Label16: TLabel;
    Label08: TLabel;
    Label04: TLabel;
    Label02: TLabel;
    Label01: TLabel;
    LabelShow: TLabel;
    LabelSCUMaxSize: TLabel;
    LabelSCU: TLabel;
    LabelCommodities: TLabel;
    LabelGroupID: TLabel;
    LabelDone: TLabel;
    LabelTotalDone: TLabel;
    LabelTotalFilteredSCUN: TLabel;
    LabelTotalToDo: TLabel;
    LabelTotalSCUN: TLabel;
    LabelUnloadingStation: TLabel;
    LabelLoadingStation: TLabel;
    MainMenuItemOpenStations: TMenuItem;
    MainMenuItemOpenCommodities: TMenuItem;
    MainMenuItemShowConsoleSettings: TMenuItem;
    MenuMain: TMainMenu;
    MainMenuItemFile: TMenuItem;
    PanelTotalFilters: TPanel;
    PanelTopMenu: TPanel;
    PopupMenuItemReloadDataList: TMenuItem;
    MainMenuItemLoad: TMenuItem;
    MainMenuItemSave: TMenuItem;
    MainMenuItemSettings: TMenuItem;
    MainMenuItemReloadDataList: TMenuItem;
    PopupMenuItemFile: TMenuItem;
    PopupMenuItemLoad: TMenuItem;
    PopupMenuItemPreferences: TMenuItem;
    PopupMenuItemSave: TMenuItem;
    OpenDialog: TOpenDialog;
    PanelTotals: TPanel;
    PanelRows: TPanel;
    PopupMenuMain: TPopupMenu;
    SaveDialog: TSaveDialog;
    SaveDialog1: TSaveDialog;
    SpinEditRoutesGroup: TSpinEdit;
    ToolBarTopMenuOp: TToolBar;
    ToolBarTopMenuWin: TToolBar;
    ToolBarTopMenuDev: TToolBar;
    ToolButtonStationsList: TToolButton;
    ToolButtonCommoditiesList: TToolButton;
    ToolButtonClearAll: TToolButton;
    ToolButtonContractView: TToolButton;
    ToolButtonCustomSort: TToolButton;
    ToolButtonShowDone: TToolButton;
    ToolButtonTest: TToolButton;
    ToolButtonAlwaysShowOnTop: TToolButton;
    ToolButton3: TToolButton;
    ToolButtonConsole: TToolButton;
    procedure ActionLoadExecute(Sender: TObject);
    procedure ActionOpenCommoditiesExecute(Sender: TObject);
    procedure ActionOpenStationsExecute(Sender: TObject);
    procedure ActionReloadDataListExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionShowConsoleSettingsExecute(Sender: TObject);
    procedure ApplicationPropertiesActivate(Sender: TObject);
    procedure ApplicationPropertiesDeactivate(Sender: TObject);
    procedure CheckBoxRoutesDoneChange(Sender: TObject);
    procedure CheckBoxRoutesGroupChange(Sender: TObject);
    procedure CheckBoxRoutesShownChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);

    procedure ImageAddRowClick(Sender: TObject);
    procedure ImageCancelCommoditiesClick(Sender: TObject);
    procedure ImageCancelGroupIDClick(Sender: TObject);
    procedure ImageCancelDoneClick(Sender: TObject);
    procedure ImageCancelLoadingStationClick(Sender: TObject);
    procedure ImageCancelSCUClick(Sender: TObject);
    procedure ImageShowClick(Sender: TObject);
    procedure ImageCancelUnloadingStationClick(Sender: TObject);
    procedure ImageDeleteRowClick(Sender: TObject);
    procedure ImageSetSCUMaxSizeClick(Sender: TObject);
    procedure ImageSortCommoditiesClick(Sender: TObject);
    procedure ImageSortGroupIDClick(Sender: TObject);
    procedure ImageSortLoadingStationClick(Sender: TObject);
    procedure ImageSortSCUMaxSizeClick(Sender: TObject);
    procedure ImageSortSCUClick(Sender: TObject);
    procedure ImageSortUnloadingStationClick(Sender: TObject);
    procedure SpinEditRoutesGroupChange(Sender: TObject);
    procedure ToolButtonAlwaysShowOnTopClick(Sender: TObject);
    procedure ToolButtonCommoditiesListClick(Sender: TObject);
    procedure ToolButtonContractViewClick(Sender: TObject);
    procedure ToolButtonClearAllClick(Sender: TObject);
    procedure ToolButtonConsoleClick(Sender: TObject);
    procedure ToolButtonCustomSortClick(Sender: TObject);
    procedure ToolButtonShowHideDoneClick(Sender: TObject);
    procedure ToolButtonStationsListClick(Sender: TObject);
    procedure ToolButtonTestClick(Sender: TObject);
  private
    {* State of the Form }
    State: TState;

    {* Main Service }
    FMain: IMainService;

    PanelRowStack: TPanelRowStack;

    
    {*
      Handles the @bold(WM_DISPLAYCHANGE) system message.
    
      This procedure is automatically triggered when the user changes 
      the screen resolution or the monitor configuration.
      
      @param(Message Contains the parameters of the message sent by Windows. 
              It is passed to the @italic(inherited) method to maintain standard behavior.)
    }
    procedure WMDisplayChange(var Message: TMessage); message WM_DISPLAYCHANGE;

    {*
      Updates the UI fields with the cumulative totals of SCU container sizes.
      
      This procedure iterates through all active rows in the @code(PanelRowStack), 
      aggregates the container distribution data from each @code(TradeRouteLeg), 
      and displays the final counts in the corresponding @code(EditSCUTotal) controls.
    }
    procedure SetEditSCUTotalN;

    {*
      Handles event notifications triggered by changes in the GUI fields of a @link(TPanelRow).
      
      This observer-pattern method reacts to user input within individual panel rows. 
      It identifies which specific property of the @link(TTradeRouteLeg) has changed 
      and updates the application state, including UI synchronization with the 
      contract view and aggregate SCU calculations.
      
      @param(Sender The object that triggered the change, typically a @code(TPanelRow) instance.)
      @param(AItem The specific data field (@link(TTradeRouteLegItem)) that was modified.)
    }
    procedure HandleTradeRouteLegChange(Sender: TObject; AItem: TTradeRouteLegItem);

    {*
      Sorts and visually rearranges the panel rows based on a custom comparison function.
      
      @param(CompareFunc The function pointer used to define the sorting criteria (e.g., by ID, Name, or Size).)
    }
    procedure ImageSorterClick(CompareFunc: TPanelRowIndexedRecordCompareFunc);

    {*
      Refreshes the contractual information displayed in the @link(TFormContractView).
    
      This procedure gathers the string representations of all contracts currently 
      stored in the database, formats them with visual separators, and pushes 
      the aggregated list to the contract view window.
    }
    procedure UpdateTextContractView;

    {*
      Checks if all rows in the current stack are empty or contain only default values.
    
      This utility function iterates through the @code(PanelRowStack) to determine 
      if there is any meaningful data entered by the user. A row is considered "empty" 
      if all its core @link(TTradeRouteLeg) fields (Group ID, Stations, Commodity, and SCU) 
      are at their default/zero state.
      
      @returns(@code(True) if all rows are empty or if the stack is empty; 
              @code(False) otherwise)
    }
    function PanelRowsNoData: Boolean;
  public
    {*
      Create the Form.
      @param(TheOwner The owner of the Form.)
      @param(AMainService The main service.)
    }
    constructor Create(TheOwner: TComponent; const AMainService: IMainService); reintroduce;

    {* Destructor. }
    destructor Destroy; override;

  end;
 
//var
  {* FormSCUxSize }
  //FormSCUxSize: TFormSCUxSize;






implementation

var
  _State: TState = TState.UnCreated;
  _StringListRecord: TItemsStringListRecord;

  _HideDone: Boolean;

  FormContractView: TFormContractView;




procedure TFormSCUxSize.WMDisplayChange(var Message: TMessage);
begin
  inherited;

  Application.ProcessMessages;

  FMain.ValidateMonitorSettings;
end;



procedure TFormSCUxSize.SetEditSCUTotalN;
var
  SCUxSize: TSCUxSizeRecord;
  APanelRow: TPanelRow;
  TradeRouteLeg: TTradeRouteLeg;
begin
  SCUxSize := default(TSCUxSizeRecord);
  for APanelRow in PanelRowStack do
  begin
    TradeRouteLeg := APanelRow.TradeRouteLeg;
    if TradeRouteLeg.SCU > 0 then
      SCUxSize := SCUxSize + TradeRouteLeg.SCUxSize;
  end;

  EditSCUTotal32.Text := IntToStr(SCUxSize.SCUSize32);
  EditSCUTotal24.Text := IntToStr(SCUxSize.SCUSize24);
  EditSCUTotal16.Text := IntToStr(SCUxSize.SCUSize16);
  EditSCUTotal08.Text := IntToStr(SCUxSize.SCUSize08);
  EditSCUTotal04.Text := IntToStr(SCUxSize.SCUSize04);
  EditSCUTotal02.Text := IntToStr(SCUxSize.SCUSize02);
  EditSCUTotal01.Text := IntToStr(SCUxSize.SCUSize01);
end;



procedure TFormSCUxSize.UpdateTextContractView;
var
  Details: TStringList;
  Contract: TContract;
begin
  if Assigned(FMain.ContractDB) and Assigned(FormContractView) then
  begin
    Details := TStringList.Create;

    for Contract in FMain.ContractDB.Contracts do
    begin
      Details.Add('________________________________________________________________');
      Details.Add(Contract.ToString);
    end;

    FormContractView.UpdateDetails(Details);

    Details.Free;
    Details := nil;
  end;
end;



procedure TFormSCUxSize.HandleTradeRouteLegChange(Sender: TObject; AItem: TTradeRouteLegItem);
var
  APanelRow: TPanelRow;
  InputCheck: Int64;
  TradeRouteLeg: TTradeRouteLeg;
begin

  if Assigned(FormContractView) then
    UpdateTextContractView;

  if Sender is TPanelRow then
  begin
    APanelRow := Sender as TPanelRow;
    case AItem of
      eGroupId:
        begin
          //? ignored
        end;

      eLoadingStationName:
        begin
          //? ignored 
        end;

      eUnloadingStationName:
        begin
          //? ignored
        end;

      eCommodity:
        begin
          //? ignored
        end;

      eSCU, eDone, eUndefined:
        begin
          InputCheck := 0;
          for APanelRow in PanelRowStack do
          begin
            TradeRouteLeg := APanelRow.TradeRouteLeg;
            if TradeRouteLeg.SCU > 0 then
              InputCheck += TradeRouteLeg.SCU;
          end;
          if InputCheck < MaxInt then
          begin
            EditSCUTotal.Text := IntToStr(InputCheck);
          end
          else
          begin
            EditSCUTotal.Text := IntToStr(MaxInt);
          end;

          InputCheck := 0;
          for APanelRow in PanelRowStack do
          begin
            TradeRouteLeg := APanelRow.TradeRouteLeg;
            if TradeRouteLeg.Done and (TradeRouteLeg.SCU > 0) then
              InputCheck += TradeRouteLeg.SCU;
          end;
          if InputCheck < MaxInt then
          begin
            EditSCUTotalDone.Text := IntToStr(InputCheck);
          end
          else
          begin
            EditSCUTotalDone.Text := IntToStr(MaxInt);
          end;

          InputCheck := StrToInt(EditSCUTotal.Text) - StrToInt(EditSCUTotalDone.Text);
          EditSCUTotalToDo.Text := IntToStr(InputCheck);

          SetEditSCUTotalN;
        end;

      eSCUMaxSize:
        begin
          SetEditSCUTotalN;
        end;

      eHide:
        begin
          //? ignored
        end;

      eSCUxSize:
        begin
          //? ignored
        end;

      else
        begin
          //? ignored
        end;
    end;
  end;
end;



procedure LoadListToStringList(const FileName: String; SL: TStringList);
var
  j: Integer;
  Line: String;
begin
  if not FileExists(FileName) then Exit;

  SL.LoadFromFile(FileName);

  // Cleans empty lines and unnecessary spaces
  for j := SL.Count - 1 downto 0 do
  begin
    Line := Trim(SL[j]);
    if Line = '' then
      SL.Delete(j)
    else
      SL[j] := Line;
  end;
end;



{$R *.lfm}

{ TFormSCUxSize }


procedure TFormSCUxSize.ApplicationPropertiesActivate(Sender: TObject);
begin
  // @code(_State) is initialised before @code(FormSCUxSize) is created.
  // It is used as additional protection against an inconsistent @code(State).
  if (_State <> TState.UnCreated) and (State <> TState.Focused) then
    begin
      case State of
        TState.Ready, TState.UnFocused:
          begin
            State:= TState.Focused;
          end;
      end;
    end;
end;



procedure TFormSCUxSize.ApplicationPropertiesDeactivate(Sender: TObject);
begin
  State:= TState.UnFocused;
end; 



procedure TFormSCUxSize.CheckBoxRoutesShownChange(Sender: TObject);
begin
  // TODO
end;



procedure TFormSCUxSize.CheckBoxRoutesDoneChange(Sender: TObject);
begin
  // TODO
end;



procedure TFormSCUxSize.CheckBoxRoutesGroupChange(Sender: TObject);
begin
  // TODO
end;   



procedure TFormSCUxSize.SpinEditRoutesGroupChange(Sender: TObject);
begin
  // TODO
end;



procedure TFormSCUxSize.ActionLoadExecute(Sender: TObject);
var
  JSONRoot, JSONRow: TJSONObject;
  JSONRows: TJSONArray;
  JSONData: TJSONData;
  JSONString: TStringList;
  i, RowCount, RowCountPre: Integer;
  FileName: String;
  TradeRouteLeg: TTradeRouteLeg;
  PanelRowArray: TPanelRowArray;
  PanelRow: TPanelRow;
begin
  if DirectoryExists(FMain.GetPathLoadExecuteDir) then
    OpenDialog.InitialDir := FMain.GetPathLoadExecuteDir
  else begin
    OpenDialog.InitialDir := GetUserDir;
    FMain.SetPathLoadExecuteDir(GetUserDir);
  end;

  FMain.Console.DebugLog('TFormSCUxSize.ActionLoadExecute', 'OpenDialog.InitialDir = ' + OpenDialog.InitialDir);
  FMain.Console.DebugLog('TFormSCUxSize.ActionLoadExecute', 'PathLoadExecuteDir = ' + FMain.GetPathLoadExecuteDir);

  if not OpenDialog.Execute then Exit;

  FileName := OpenDialog.FileName;
  FMain.SetPathLoadExecuteDir(ExtractFilePath(FileName));

  FMain.SavePathLoadExecuteDir;

  FMain.Console.DebugLog('TFormSCUxSize.ActionLoadExecute', Format('FileName = %s', [FileName]));
  FMain.Console.DebugLog('TFormSCUxSize.ActionLoadExecute', Format('Dir: %s', [FMain.GetPathLoadExecuteDir]));

  if not FileExists(FileName) then
  begin
    ShowMessage('File not found: ' + FileName);
    Exit;
  end;

  JSONString := TStringList.Create;
  try
    JSONString.LoadFromFile(FileName);
    JSONData := GetJSON(JSONString.Text);

    if not (JSONData is TJSONObject) then
    begin
      ShowMessage('The file does not contain a valid JSON object.');
      JSONData.Free;
      JSONData := nil;
      Exit;
    end;

    JSONRoot := TJSONObject(JSONData);

    if not JSONRoot.Find('Rows', JSONData) then
    begin
      ShowMessage('The "Rows" field is missing from the JSON file.');
      JSONRoot.Free;
      JSONRoot := nil;
      Exit;
    end;

    if not (JSONData is TJSONArray) then
    begin
      ShowMessage('"Rows" is not a valid JSON array.');
      JSONRoot.Free;
      JSONRoot := nil;
      Exit;
    end;

    JSONRows := TJSONArray(JSONData);

    for PanelRow in PanelRowStack do
      PanelRow.Clear;

    RowCount := JSONRows.Count;
    if RowCount > NPanelRowMax then
      RowCount := NPanelRowMax;
    RowCountPre := PanelRowStack.Count;

    if RowCount > RowCountPre then
    begin
      for i := RowCountPre to RowCount-1 do
      begin
        ImageAddRowClick(Self);
      end;
      // -----------------------------------------------------------------------
    end;

    PanelRowArray := PanelRowStack.ToArray;
    for i := 0 to RowCount-1 do // process only the initial rows
    begin
      if not (JSONRows.Items[i] is TJSONObject) then Continue;
      JSONRow := TJSONObject(JSONRows.Items[i]);

      TradeRouteLeg := default(TTradeRouteLeg);
      TradeRouteLeg.GroupId              := JSONRow.Get('Group_Id', 0);
      TradeRouteLeg.LoadingStationName   := JSONRow.Get('Loading_Station', '');
      TradeRouteLeg.UnloadingStationName := JSONRow.Get('Unloading_Station', '');
      TradeRouteLeg.Commodity            := JSONRow.Get('Commodity', '');
      TradeRouteLeg.SCU                  := JSONRow.Get('SCU', 0);
      TradeRouteLeg.SCUMaxSize           := TSCUxSizeRecord.GetSizeValue(JSONRow.Get('SCUMaxSize', SCUSizeIntDefault));
      TradeRouteLeg.Done                 := JSONRow.Get('Done', False);
      TradeRouteLeg.Hide                 := JSONRow.Get('Hide', False);

      TSCUxSizeRecord.SetSCUxSizeFromSCU(TradeRouteLeg.SCU, TradeRouteLeg.SCUMaxSize, TradeRouteLeg.SCUxSize);

      PanelRowArray[i].TradeRouteLegReload(TradeRouteLeg);
    end;

    //ShowMessage('Data successfully uploaded from: ' + ExtractFileName(FileName));
  finally
    JSONString.Free;
    JSONString := nil;
    if Assigned(JSONRoot) then
    begin
      JSONRoot.Free;
      JSONRoot := nil;
    end;
  end;
end;



procedure TFormSCUxSize.ActionOpenCommoditiesExecute(Sender: TObject);
var
  PathToFile: string;
begin
  PathToFile := ExtractFilePath(ParamStr(0)) + FMain.GetFileNameCommoditiesList;
  ShellExecute(0, 'open', PChar(PathToFile), nil, nil, SW_SHOWNORMAL);
end;



procedure TFormSCUxSize.ActionOpenStationsExecute(Sender: TObject);
var
  PathToFile: string;
begin
  PathToFile := ExtractFilePath(ParamStr(0)) + FMain.GetFileNameStationsList;
  ShellExecute(0, 'open', PChar(PathToFile), nil, nil, SW_SHOWNORMAL);
end;



procedure TFormSCUxSize.ActionSaveExecute(Sender: TObject);
var
  JSONRoot, JSONTotal, JSONRow: TJSONObject;
  JSONArrayRows: TJSONArray;
  i: Integer;
  FileName, TimeStamp: String;
  JSONString: TStringList;
  OverwriteConfirm: Integer;
  PanelRowArray: TPanelRowArray;
begin
  JSONRoot := TJSONObject.Create;

  // --- Totals ---
  JSONTotal := TJSONObject.Create;
  JSONTotal.Add('SCUTotal',   StrToIntDef(EditSCUTotal.Text, 0));
  JSONTotal.Add('SCUTotal32', StrToIntDef(EditSCUTotal32.Text, 0));
  JSONTotal.Add('SCUTotal24', StrToIntDef(EditSCUTotal24.Text, 0));
  JSONTotal.Add('SCUTotal16', StrToIntDef(EditSCUTotal16.Text, 0));
  JSONTotal.Add('SCUTotal08', StrToIntDef(EditSCUTotal08.Text, 0));
  JSONTotal.Add('SCUTotal04', StrToIntDef(EditSCUTotal04.Text, 0));
  JSONTotal.Add('SCUTotal02', StrToIntDef(EditSCUTotal02.Text, 0));
  JSONTotal.Add('SCUTotal01', StrToIntDef(EditSCUTotal01.Text, 0));
  JSONRoot.Add('TotalSCU', JSONTotal);

  // --- Rows ---
  JSONArrayRows := TJSONArray.Create;

  PanelRowArray := PanelRowStack.ToArray;
  for i := 0 to High(PanelRowArray) do
  begin
    JSONRow := TJSONObject.Create;

    JSONRow.Add('Index',       i);
    JSONRow.Add('Group_Id',          PanelRowArray[i].TradeRouteLeg.GroupId);
    JSONRow.Add('Loading_Station',   PanelRowArray[i].TradeRouteLeg.LoadingStationName);
    JSONRow.Add('Unloading_Station', PanelRowArray[i].TradeRouteLeg.UnloadingStationName);
    JSONRow.Add('Commodity',         PanelRowArray[i].TradeRouteLeg.Commodity);
    JSONRow.Add('SCU',               PanelRowArray[i].TradeRouteLeg.SCU);
    JSONRow.Add('SCUMaxSize',        SCUSizeArray[PanelRowArray[i].TradeRouteLeg.SCUMaxSize]);

    JSONRow.Add('Done',              PanelRowArray[i].TradeRouteLeg.Done);
    JSONRow.Add('Hide',              PanelRowArray[i].TradeRouteLeg.Hide);

    JSONRow.Add('32', PanelRowArray[i].TradeRouteLeg.SCUxSize.SCUSize32);
    JSONRow.Add('24', PanelRowArray[i].TradeRouteLeg.SCUxSize.SCUSize24);
    JSONRow.Add('16', PanelRowArray[i].TradeRouteLeg.SCUxSize.SCUSize16);
    JSONRow.Add('08', PanelRowArray[i].TradeRouteLeg.SCUxSize.SCUSize08);
    JSONRow.Add('04', PanelRowArray[i].TradeRouteLeg.SCUxSize.SCUSize04);
    JSONRow.Add('02', PanelRowArray[i].TradeRouteLeg.SCUxSize.SCUSize02);
    JSONRow.Add('01', PanelRowArray[i].TradeRouteLeg.SCUxSize.SCUSize01);

    JSONArrayRows.Add(JSONRow);
  end;

  JSONRoot.Add('Rows', JSONArrayRows);

  TimeStamp := FormatDateTime('yyyy_mm_dd_hh_nn', Now);
  FileName := 'SCUxSize_' + TimeStamp + '.json';
  SaveDialog.FileName := FileName;

  if DirectoryExists(FMain.GetPathSaveExecuteDir) then
    SaveDialog.InitialDir := FMain.GetPathSaveExecuteDir
  else begin
    SaveDialog.InitialDir := GetUserDir;
    FMain.SetPathSaveExecuteDir(GetUserDir);
  end;   

  FMain.Console.DebugLog('TFormSCUxSize.ActionSaveExecute', 'SaveDialog.InitialDir = ' + SaveDialog.InitialDir);
  FMain.Console.DebugLog('TFormSCUxSize.ActionSaveExecute', 'PathSaveExecuteDir = ' + FMain.GetPathSaveExecuteDir);

  if SaveDialog.Execute then
  begin
    FileName := SaveDialog.FileName;
    FMain.SetPathSaveExecuteDir(ExtractFilePath(FileName));

    FMain.SavePathSaveExecuteDir;

    if FileExists(FileName) then
    begin
      OverwriteConfirm := MessageDlg('File already exists',
        'The "' + ExtractFileName(FileName) + '" file already exists.' + sLineBreak +
        'Do you want to overwrite it?',
        mtConfirmation, [mbYes, mbNo], 0);
      if OverwriteConfirm <> mrYes then
      begin
        //ShowMessage('Save cancelled by the user.');
        JSONRoot.Free;
        JSONRoot := nil;
        Exit;
      end;
    end;

    JSONString := TStringList.Create;
    try
      JSONString.Text := JSONRoot.FormatJSON([foUseTabchar], 1);
      JSONString.SaveToFile(FileName);
      //ShowMessage('Data saved in: ' + FileName);
    finally
      JSONString.Free;
      JSONString := nil;
      JSONRoot.Free;
      JSONRoot := nil;
    end;
  end
  else begin
    JSONRoot.Free;
    JSONRoot := nil;
  end;
end;



procedure TFormSCUxSize.ActionShowConsoleSettingsExecute(Sender: TObject);
begin
  FMain.ShowConsoleSettings(Self);
end;



procedure TFormSCUxSize.ActionReloadDataListExecute(Sender: TObject);
var
  APanelRow: TPanelRow;
  AppPath, FileStations, FileCommodities: String;
begin
  AppPath := ExtractFilePath(Application.ExeName);
  FileStations := AppPath + FMain.GetFileNameStationsList;
  FileCommodities := AppPath + FMain.GetFileNameCommoditiesList;

  _StringListRecord.StationNames.Clear;
  LoadListToStringList(FileStations, _StringListRecord.StationNames);
  _StringListRecord.Commodities.Clear;
  LoadListToStringList(FileCommodities, _StringListRecord.Commodities);

  for APanelRow in PanelRowStack do
  begin
    ApanelRow.ReloadDataListAndSetComboBox(_StringListRecord);
  end;
end;



procedure TFormSCUxSize.ImageCancelGroupIDClick(Sender: TObject);
var
  PanelRow: TPanelRow;
  TradeRouteLeg: TTradeRouteLeg;
begin
  TradeRouteLeg := Default(TTradeRouteLeg);
  TradeRouteLeg.GroupId := 0;  
  for PanelRow in PanelRowStack do
  begin
    PanelRow.SetValue(eGroupId, TradeRouteLeg);
  end;
end;



procedure TFormSCUxSize.ImageCancelLoadingStationClick(Sender: TObject);
var
  PanelRow: TPanelRow;
  TradeRouteLeg: TTradeRouteLeg;
begin
  TradeRouteLeg := Default(TTradeRouteLeg);
  TradeRouteLeg.LoadingStationName := '';  
  for PanelRow in PanelRowStack do
  begin
    PanelRow.SetValue(eLoadingStationName, TradeRouteLeg);
  end;
end;



procedure TFormSCUxSize.ImageCancelUnloadingStationClick(Sender: TObject);
var
  PanelRow: TPanelRow;
  TradeRouteLeg: TTradeRouteLeg;
begin
  TradeRouteLeg := Default(TTradeRouteLeg);
  TradeRouteLeg.UnloadingStationName := '';
  for PanelRow in PanelRowStack do
  begin
    PanelRow.SetValue(eUnloadingStationName, TradeRouteLeg);
  end;
end;



procedure TFormSCUxSize.ImageCancelCommoditiesClick(Sender: TObject);
var
  PanelRow: TPanelRow;
  TradeRouteLeg: TTradeRouteLeg;
begin
  TradeRouteLeg := Default(TTradeRouteLeg);
  TradeRouteLeg.Commodity := '';
  for PanelRow in PanelRowStack do
  begin
    PanelRow.SetValue(eCommodity, TradeRouteLeg);
  end;
end;



procedure TFormSCUxSize.ImageCancelSCUClick(Sender: TObject);
var
  PanelRow: TPanelRow;
  TradeRouteLeg: TTradeRouteLeg;
begin
  TradeRouteLeg := Default(TTradeRouteLeg);
  TradeRouteLeg.SCU := 0;
  for PanelRow in PanelRowStack do
  begin
    PanelRow.SetValue(eSCU, TradeRouteLeg);
  end;
end;



procedure TFormSCUxSize.ImageCancelDoneClick(Sender: TObject);
var
  PanelRow: TPanelRow;
  TradeRouteLeg: TTradeRouteLeg;
begin
  TradeRouteLeg := Default(TTradeRouteLeg);
  TradeRouteLeg.Done := False;
  for PanelRow in PanelRowStack do
  begin
    PanelRow.SetValue(eDone, TradeRouteLeg);
  end;
end;



procedure TFormSCUxSize.ImageShowClick(Sender: TObject);
var
  PanelRow: TPanelRow;
  TradeRouteLeg: TTradeRouteLeg;
begin
  TradeRouteLeg := Default(TTradeRouteLeg);
  TradeRouteLeg.Hide := False;
  for PanelRow in PanelRowStack do
  begin
    PanelRow.SetValue(eHide, TradeRouteLeg);
  end;
end;



procedure TFormSCUxSize.ImageSetSCUMaxSizeClick(Sender: TObject);
var
  FormSizeDialog: TFormSizeDialog;
  SelectedValue: Integer;
  TradeRouteLeg: TTradeRouteLeg;
  PanelRowArray: TPanelRowArray;
  Ids: array of Integer;
  i: Integer;
begin
  TradeRouteLeg := Default(TTradeRouteLeg);
  FormSizeDialog := TFormSizeDialog.Create(Self, SCUSizeIntArray);
  Ids := nil;

  try
    if FormSizeDialog.ShowModal = mrOK then
    begin
      SelectedValue := FormSizeDialog.SelectedSize;

      if SelectedValue > 0 then
      begin
        PanelRowArray := PanelRowStack.ToArray;
        SetLength(Ids, PanelRowStack.Count);
        TradeRouteLeg.GroupId := 0;
        TradeRouteLeg.SCUMaxSize := TSCUxSizeRecord.GetSizeValue(SelectedValue);

        for i := 0 to High(Ids) do
        begin
          Ids[i] := PanelRowArray[i].GroupId;
          PanelRowArray[i].SetValue(eGroupId, TradeRouteLeg);
          PanelRowArray[i].SetValue(eSCUMaxSize, TradeRouteLeg);
        end;

        for i := 0 to High(Ids) do
        begin
          TradeRouteLeg.GroupId := Ids[i];
          PanelRowArray[i].SetValue(eGroupId, TradeRouteLeg);
        end;
      end;
    end;
  finally
    FormSizeDialog.Free;
  end;
end;



procedure TFormSCUxSize.ImageAddRowClick(Sender: TObject);
var
  PanelRow: TPanelRow;
begin
  if (PanelRowStack.Count < NPanelRowMax) and (PanelRowStack.Count >= NPanelRowMin) then
  begin
    PanelRows.DisableAlign;
    try
      PanelRow := TPanelRow.Create(PanelRows, @HandleTradeRouteLegChange, PanelRowStack.Peek, _StringListRecord, FMain.Console);
      PanelRowStack.Push(PanelRow);
      PanelRow.Visible := True;
      PanelRow.RegisterPanel(FMain.ContractDB);
      Self.Height := Self.Height + PanelRowStack.Peek.Height + 10;
    finally
      PanelRows.EnableAlign;
    end;
  end;
end;



procedure TFormSCUxSize.ImageDeleteRowClick(Sender: TObject);
var
  PanelRow: TPanelRow;
begin
  if PanelRowStack.Count > NPanelRowMin then
  begin
    Self.Height := Self.Height - PanelRowStack.Peek.Height - 10;
    PanelRow := PanelRowStack.Peek;
    //PanelRow.Clear;
    PanelRow.UnregisterPanel;
    PanelRowStack.Pop;
    PanelRow.Free;
    PanelRow := nil;
    UpdateTextContractView;
  end;
end;



procedure TFormSCUxSize.ImageSortGroupIDClick(Sender: TObject);
var
  GroupId: Integer;
  Same: Boolean;
  PanelRow: TPanelRow;
begin
  // Check if all GroupId are the same
  GroupId := -1;
  Same := True;
  for PanelRow in PanelRowStack do
  begin
    if GroupId = -1 then
      GroupId := PanelRow.GroupId
    else if GroupId <> PanelRow.GroupId then
    begin
      Same := False;
      Break;
    end;
  end;
  if Same then Exit;

  ImageSorterClick(@SortGroupIDCompare);
  ToggleSortSideDownGroupId;
end;



procedure TFormSCUxSize.ImageSortLoadingStationClick(Sender: TObject);
var
  LoadingStationName: String;
  Same: Boolean;
  PanelRow: TPanelRow;
begin
  // Check if all LoadingStationName are the same
  LoadingStationName := '^$^';
  Same := True;
  for PanelRow in PanelRowStack do
  begin
    if LoadingStationName = '^$^' then
      LoadingStationName := PanelRow.LoadingStationName
    else if LoadingStationName <> PanelRow.LoadingStationName then
    begin
      Same := False;
      Break;
    end;
  end;
  if Same then Exit;

  ImageSorterClick(@SortLoadingStationCompare);
  ToggleSortSideDownLoadingStation;
end;



procedure TFormSCUxSize.ImageSortUnloadingStationClick(Sender: TObject);
var
  UnloadingStationName: String;
  Same: Boolean;
  PanelRow: TPanelRow;
begin
  // Check if all UnloadingStationName are the same
  UnloadingStationName := '^$^';
  Same := True;
  for PanelRow in PanelRowStack do
  begin
    if UnloadingStationName = '^$^' then
      UnloadingStationName := PanelRow.UnloadingStationName
    else if UnloadingStationName <> PanelRow.UnloadingStationName then
    begin
      Same := False;
      Break;
    end;
  end;
  if Same then Exit;

  ImageSorterClick(@SortUnloadingStationCompare);
  ToggleSortSideDownUnloadingStation;
end;



procedure TFormSCUxSize.ImageSortCommoditiesClick(Sender: TObject);
var
  Commodity: String;
  Same: Boolean;
  PanelRow: TPanelRow;
begin
  // Check if all Commodities are the same
  Commodity := '^$^';
  Same := True;
  for PanelRow in PanelRowStack do
  begin
    if Commodity = '^$^' then
      Commodity := PanelRow.Commodity
    else if Commodity <> PanelRow.Commodity then
    begin
      Same := False;
      Break;
    end;
  end;
  if Same then Exit;

  ImageSorterClick(@SortCommoditiesCompare);
  ToggleSortSideDownCommodities;
end;



procedure TFormSCUxSize.ImageSortSCUClick(Sender: TObject);
var
  SCU: Integer;
  Same: Boolean;
  PanelRow: TPanelRow;
begin
  // Check if all SCU are the same
  SCU := -1;
  Same := True;
  for PanelRow in PanelRowStack do
  begin
    if SCU = -1 then
      SCU := PanelRow.SCU
    else if SCU <> PanelRow.SCU then
    begin
      Same := False;
      Break;
    end;
  end;
  if Same then Exit;

  ImageSorterClick(@SortSCUCompare);
  ToggleSortSideDownSCU;
end;



procedure TFormSCUxSize.ImageSortSCUMaxSizeClick(Sender: TObject);
var
  SCUMaxSize: TSCUSize;
  Same: Boolean;
  First: Boolean;
  PanelRow: TPanelRow;
begin
  // Check if all SCUMaxSize are the same
  First := True;
  Same := True;
  for PanelRow in PanelRowStack do
  begin
    if First then
    begin
      First := False;
      SCUMaxSize := PanelRow.SCUMaxSize;
    end
    else if SCUMaxSize <> PanelRow.SCUMaxSize then
    begin
      Same := False;
      Break;
    end;
  end;
  if Same then Exit;

  ImageSorterClick(@SortSCUMaxSizeCompare);
  ToggleSortSideDownSCUMaxSize;
end;



function TFormSCUxSize.PanelRowsNoData: Boolean;
var
  PanelRow: TPanelRow;
  TradeRouteLeg: TTradeRouteLeg;
begin
  Result := True;
  TradeRouteLeg := Default(TTradeRouteLeg);

  for PanelRow in PanelRowStack do
  begin
    TradeRouteLeg := PanelRow.TradeRouteLeg;
    if not (
      (TradeRouteLeg.GroupId = 0) and
      (TradeRouteLeg.LoadingStationName = '') and
      (TradeRouteLeg.UnloadingStationName = '') and
      (TradeRouteLeg.Commodity = '') and
      (TradeRouteLeg.SCU = 0)
    ) then begin
      Result := False;
      Exit;
    end;
  end;
end;



procedure TFormSCUxSize.ImageSorterClick(CompareFunc: TPanelRowIndexedRecordCompareFunc);
var
  PanelRow: TPanelRow;
  PanelRowPrev: TPanelRow;
  PanelRowIndexedRecord: TPanelRowIndexedRecord;
  PanelRowIndexedRecordList: TPanelRowIndexedRecordList;
  index: Integer;
begin
  FMain.Console.DebugLog('TFormSCUxSize.ImageSorterClick', 'ImageSorterClick');
  if (PanelRowStack.Count > 0) and (not PanelRowsNoData) then
  begin
    index := 0;
    PanelRowIndexedRecordList := TPanelRowIndexedRecordList.Create;

    FMain.Console.DebugLog('TFormSCUxSize.ImageSorterClick', Format('PanelRowStack.Count = %d', [PanelRowStack.Count]));

    while PanelRowStack.Count > 0 do
    begin
      PanelRow := PanelRowStack.Pop;
      PanelRowIndexedRecord.PanelRow := PanelRow;
      PanelRowIndexedRecord.Index := index;
      index += 1;
      PanelRowIndexedRecordList.Add(PanelRowIndexedRecord);
      FMain.Console.DebugLog('TFormSCUxSize.ImageSorterClick', Format('PanelRow.ID = %d    Index = %d', [PanelRow.ID, index]));
    end;

    PanelRowIndexedRecordList.Sort(specialize TComparer<TPanelRowIndexedRecord>.Construct(CompareFunc));

    for PanelRowIndexedRecord in PanelRowIndexedRecordList do
    begin
      PanelRowStack.Push(PanelRowIndexedRecord.PanelRow);
      FMain.Console.DebugLog('TFormSCUxSize.ImageSorterClick', Format('PanelRow.ID = %d    Index = %d', [PanelRowIndexedRecord.PanelRow.ID, PanelRowIndexedRecord.Index]));
    end;

    PanelRowIndexedRecordList.Free; 
    PanelRowIndexedRecordList := nil;

    PanelRows.DisableAlign;
    PanelRowPrev := nil;
    for PanelRow in PanelRowStack do
    begin
      PanelRow.ChangeAnchorComponent(PanelRowPrev);
      PanelRowPrev := PanelRow;
      FMain.Console.DebugLog('TFormSCUxSize.ImageSorterClick', Format('PanelRow.ID = %d', [PanelRow.ID]));
    end;
    PanelRows.EnableAlign;
  end;
end;



procedure TFormSCUxSize.ToolButtonAlwaysShowOnTopClick(Sender: TObject);
begin
  if Self.FormStyle = fsNormal then
    Self.FormStyle := fsSystemStayOnTop
  else        
    Self.FormStyle := fsNormal;
end;



procedure TFormSCUxSize.ToolButtonStationsListClick(Sender: TObject);
begin
  // TODO
end;



procedure TFormSCUxSize.ToolButtonCommoditiesListClick(Sender: TObject);
begin
  // TODO
end;



procedure TFormSCUxSize.ToolButtonContractViewClick(Sender: TObject);
begin
  FormContractView.Visible := not FormContractView.Visible;
end;



procedure TFormSCUxSize.ToolButtonClearAllClick(Sender: TObject);
var
  PanelRow: TPanelRow;
begin
  while PanelRowStack.Count > NPanelRowMin do
    ImageDeleteRowClick(Self);
  for PanelRow in PanelRowStack do
    PanelRow.Clear;
end;



procedure TFormSCUxSize.ToolButtonConsoleClick(Sender: TObject);
begin
  FMain.ShowHideConsole;
end;



procedure TFormSCUxSize.ToolButtonCustomSortClick(Sender: TObject);
begin
  // TODO: implement custom sort
  ShowMessage('This feature has not been implemented yet.');
end;



procedure TFormSCUxSize.ToolButtonShowHideDoneClick(Sender: TObject);
var
  PanelRow: TPanelRow;
begin
  if ToolButtonShowDone.ImageIndex = 7 then
    ToolButtonShowDone.ImageIndex := 6
  else ToolButtonShowDone.ImageIndex := 7;

  for PanelRow in PanelRowStack do
  begin
    if PanelRow.TradeRouteLeg.Done then
      PanelRow.CheckBoxShowHideRow.Checked := not _HideDone;
  end;

  _HideDone := not _HideDone;
end;



procedure TFormSCUxSize.ToolButtonTestClick(Sender: TObject);
begin
  if Assigned(FMain.ContractDB) then
    FMain.ContractDB.Test;
end;



procedure TFormSCUxSize.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Shift := Shift;
  case Key of
    VK_F1:
      begin
        FMain.FormKeyDownPressConsole;
        Key := 0;
      end
  else
    begin
    end;
  end;
end;



procedure TFormSCUxSize.FormKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    '\':
      begin
        FMain.FormKeyDownPressConsole;
        Key := #0;
      end
  else
    begin
    end;
  end;
end;



constructor TFormSCUxSize.Create(TheOwner: TComponent; const AMainService: IMainService);
begin
  inherited Create(TheOwner);

  State:= TState.Created;
  _State:= TState.Created;

  FMain := AMainService;

  FormContractView := TFormContractView.Create(nil);
  FormContractView.Visible := False;

  PanelRowStack := TPanelRowStack.Create;
end;



procedure TFormSCUxSize.FormCreate(Sender: TObject);
var
  i: Integer;
  PanelRow: TPanelRow;
  PanelRowPrev: TPanelRow;
begin
  if State = TState.Created then // ASSERT: Always True
    begin
      State:= TState.Ready;
      ApplicationPropertiesActivate(Sender);
    end;

  FMain.Console.NoticeLog('TFormSCUxSize.FormCreate', 'START');

  PanelRowPrev := nil;
  for i := 0 to (NPanelRowMin -1) do begin
    PanelRow := TPanelRow.Create(PanelRows, @HandleTradeRouteLegChange, PanelRowPrev, _StringListRecord, FMain.Console);
    PanelRow.Visible := True;
    PanelRow.RegisterPanel(FMain.ContractDB);
    PanelRowPrev := PanelRow;
    PanelRowStack.Push(PanelRowPrev);
  end;

  ActionReloadDataListExecute(Self);


  OpenDialog.Title := 'Select SCUxSize file to upload';
  OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);
  OpenDialog.Filter := 'JSON files|*.json';

  SaveDialog.InitialDir := ExtractFilePath(Application.ExeName);
  SaveDialog.DefaultExt := 'json';
  SaveDialog.Filter := 'JSON files|*.json';
end;



procedure TFormSCUxSize.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //
end;



procedure TFormSCUxSize.FormDestroy(Sender: TObject);
var
  PanelRow: TPanelRow;
  i: Integer;
begin
  FormContractView.Free;
  FormContractView := nil;

  for i := 0 to PanelRowStack.Count-1 do
  begin
    PanelRow := PanelRowStack.Pop;
    PanelRow.UnregisterPanel;
    // PanelRow.Free; // freed in TPanelRows
  end;

  PanelRowStack.Free;
  PanelRowStack := nil;

  FMain := nil;
end;


destructor TFormSCUxSize.Destroy;
begin
  //
  inherited Destroy;
end;







initialization
_StringListRecord := default(TItemsStringListRecord);
_StringListRecord.StationNames := TStringList.Create;
_StringListRecord.Commodities := TStringList.Create;

_HideDone := False;





finalization
_StringListRecord.StationNames.Free;
_StringListRecord.StationNames := nil;
_StringListRecord.Commodities.Free;
_StringListRecord.Commodities := nil;


end.

