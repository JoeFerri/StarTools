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


{* Main Unit }
unit MainUnit;

{$mode objfpc}{$H+}{$J-}{$R+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls,
  IniPropStorage,
  Menus, ActnList,
  fpjson, jsonparser,
  Generics.Defaults,
  LCLType, ComCtrls, Windows,
  StateFormUnit,
  ContractUnit, ContractDBUnit,
  TRLSortUnit, SCUxSizeFormUnit,
  ConsoleUnit, FormUnit,
  ConsoleSettingsDialogUnit, MainServiceUnit,
  InfoUnit,
  VersionUnit;


type
  { TFormMain }
  TFormMain = class(TForm, IConsoleInputHandler, IMainService)
    ActionAbout: TAction;
    ActionShowConsoleSettings: TAction;
    ActionList: TActionList;
    ApplicationProperties: TApplicationProperties;
    Bevel16: TBevel;
    BitBtnSCUxSize: TBitBtn;
    CoolBarTopLeftMenu: TCoolBar;
    ImageOrganization: TImage;
    ImageAvatar: TImage;
    ImageAGPL: TImage;
    ImageBanner: TImage;
    ImageInfoTime_1: TImage;
    ImageInfoTime_2: TImage;
    ImageListMenu: TImageList;
    ImageStarCitizenLogoLeft: TImage;
    ImageStarCitizenLogoRight: TImage;
    ImageStarCitizenLogoGreyLeft: TImage;
    ImageStarCitizenLogoGreyRight: TImage;
    ImageWarning_1: TImage;
    ImageWarning_2: TImage;
    IniPropStorage: TIniPropStorage;
    LabelStarCitizenActivity: TLabel;
    LabelTime: TLabel;
    MainMenuItemShowConsoleSettings: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemAiuto: TMenuItem;
    MenuMain: TMainMenu;
    PanelOrganization: TPanel;
    PanelTop: TPanel;
    PanelAvatar: TPanel;
    PanelTopMenu: TPanel;
    PanelBanner: TPanel;
    PanelTime: TPanel;
    PanelStarCitizenActivity: TPanel;
    PopupMenuItemShowConsoleSettings: TMenuItem;
    MainMenuItemSettings: TMenuItem;
    PopupMenuItemSettings: TMenuItem;
    PanelMainHeader: TPanel;
    PanelMainTools: TPanel;
    PopupMenuMain: TPopupMenu;
    SaveDialog1: TSaveDialog;
    StaticTextTime: TStaticText;
    StaticTextTimeStamp: TStaticText;
    Timer1: TTimer;
    ToolBarTopMenuWin: TToolBar;
    ToolBarTopMenuDev: TToolBar;
    ToolButtonTest: TToolButton;
    ToolButtonAlwaysShowOnTop: TToolButton;
    ToolButton3: TToolButton;
    ToolButtonConsole: TToolButton;
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionShowConsoleSettingsExecute(Sender: TObject);
    procedure ApplicationPropertiesActivate(Sender: TObject);
    procedure ApplicationPropertiesDeactivate(Sender: TObject);
    procedure BitBtnSCUxSizeClick(Sender: TObject);
    procedure BitBtnShowConsoleSettingsApplyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);

    procedure ToolButtonAlwaysShowOnTopClick(Sender: TObject);
    procedure ToolButtonConsoleClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolButtonTestClick(Sender: TObject);
  private
    {* State of the Main Form }
    State: TState;

    {*
      Handles the @bold(WM_DISPLAYCHANGE) system message.
    
      This procedure is automatically triggered when the user changes 
      the screen resolution or the monitor configuration.
      
      @param(Message Contains the parameters of the message sent by Windows. 
              It is passed to the @italic(inherited) method to maintain standard behavior.)
      
      @bold(Technical Notes:)
      The operating system and the VCL/LCL @code(Screen) global object 
      might not immediately reflect the new hardware parameters. 
      Calling @code(Application.ProcessMessages) allows the message queue 
      to be flushed, ensuring that @link(ValidateMonitorSettings) 
      operates on updated data.
    }
    procedure WMDisplayChange(var Message: TMessage); message WM_DISPLAYCHANGE;
  public
    {* Console Server }
    function Console : TConsoleReaderThread;

    {* Contract Database }
    function ContractDB: TContractDB;   

    {*
      Updates the position of the main form based on the current positioning mode.
    
      The method calculates the new coordinates (X, Y) to center the form within 
      the work area of the target monitor, excluding taskbars and docked toolbars.
      
      @param(AForm The form instance to be repositioned. Currently, the method 
            internal logic is tied to the main form, but is designed for 
            future generalization.)
            
      @bold(Positioning Modes:)
      @unorderedList(
        @item(@code(fpmDefault): No action is taken; the form remains at its current position.)
        @item(@code(fpmFollowActiveMonitor): The form is centered on the monitor where 
              it is currently located.)
        @item(@code(fpmSpecificMonitor): The form is moved to the monitor specified 
              by the global index @code(_SelectedFormMainMonitorIndex).)
      )
    }
    procedure UpdateMainFormPosition(AForm: TForm);  


    {*
      Validates and corrects monitor-related settings to prevent errors after hardware changes.
      
      This procedure acts as a safety check (fail-safe). If a previously selected 
      monitor is no longer available (e.g., unplugged), it resets the positioning 
      mode and index to their default values.
      
      @bold(Technical Notes:)
      This method is typically called during application startup or in response 
      to a @code(WM_DISPLAYCHANGE) message.
    }
    procedure ValidateMonitorSettings;

    {*
      Resets all application settings to their predefined default values in the INI storage.
    
      This method bypasses the current session variables and writes the 
      @code(ValueDefault) constants directly to the storage.
      
      @bold(Note:) A call to @code(IniPropStorage.Save) is performed at the end 
      to ensure changes are flushed to the physical disk.
    }
    procedure IniPropStorageRestore;

    {*
      Persists the current session variables into the INI storage.
    
      This method takes the values currently held in the private fields
      and writes them to the @link(IniPropStorage) component.
      
      @bold(Note:) Enumerated types are stored as integers using the @code(Ord()) function 
      to ensure compatibility with the INI format.
    }
    procedure IniPropStorageSetAllOptionsToDefault;

    {*
      Restores session variables from the INI storage into memory.
      
      This procedure reads the values from the storage and populates the application's 
      internal state. It includes fallback logic to handle missing keys or corrupted data.
      
      @bold(Technical Notes:)
      The range checking on integers is critical to prevent @italic(Access Violations)
      or undefined behavior when an INI file is manually edited with invalid values.
    }
    procedure IniPropStorageSetAllOptions;

    {* @returns(The path to the load execute directory.) }
    function GetPathLoadExecuteDir : String;

    {* Sets the path to the load execute directory. }
    procedure SetPathLoadExecuteDir(Path: String);

    {* Saves the path to the load execute directory. }
    procedure SavePathLoadExecuteDir;

    {* @returns(The path to the save execute directory.) }
    function GetPathSaveExecuteDir : String;

    {* Sets the path to the save execute directory. }
    procedure SetPathSaveExecuteDir(Path: String);

    {* Saves the path to the save execute directory. }
    procedure SavePathSaveExecuteDir;

    {* @returns(The name of the commodities list file.) }
    function GetFileNameCommoditiesList : String;

    {* Sets the name of the commodities list file. }
    procedure SetFileNameCommoditiesList(Path: String);

    {* Saves the name of the commodities list file. }
    procedure SaveFileNameCommoditiesList;     

    {* @returns(The name of the stations list file.)}
    function GetFileNameStationsList : String;

    {* Sets the name of the stations list file.}
    procedure SetFileNameStationsList(Path: String);

    {* Saves the name of the stations list file.}
    procedure SaveFileNameStationsList;

    {* Shows the console settings form. }
    procedure ShowConsoleSettings(Sender: TObject);

    {* Shows or hides the console. }
    procedure ShowHideConsole;

    {*
      Helper method to manage the visibility and focus of the external console.
    
      If the console is currently visible, it forces it to the foreground. 
      Otherwise, it triggers the console display logic (simulating a click 
      on the console tool button).
    }
    procedure FormKeyDownPressConsole;    



    {*
      Executes commands received from an external shell.
      
      Since the main application is a GUI and does not directly handle standard 
      input (stdin) or standard output (stdout), this procedure acts as a bridge. 
      It processes commands forwarded from an external shell process.
      
      @param(S The command string sent by the user through the external shell.)
    }
    procedure ConsoleInputExecute(const S: string);



    {*
      Constructor.
      @param(TheOwner The component that owns this form.)
    }
    constructor Create(TheOwner: TComponent); override;

    {* Destructor. }
    destructor Destroy; override;
  end;





var
  {* Main Form }
  FormMain: TFormMain;






implementation




const
  FileNameStationsListKey    = 'File_Name_Stations_List';
  FileNameCommoditiesListKey = 'File_Name_Commodities_List';
  FileNameContractorsListKey = 'File_Name_Contractors_List';

  FileNameStationsListValueDefault    = './data/stations/stations.list';
  FileNameCommoditiesListValueDefault = './data/commodities/commodities.list';
  FileNameContractorsListValueDefault = './data/contractors/contractors.list';

  PathLoadExecuteDirKey = 'Path_Load_Execute_Dir';
  PathLoadExecuteDirValueDefault = './';
  PathSaveExecuteDirKey = 'Path_Save_Execute_Dir';
  PathSaveExecuteDirValueDefault = './';

  ConsoleCommandsArray: array[0..1] of string =  ('"exit" -> close the tool',
                                                  '"hide" -> hides the console');

  FormMainPosModeKey = 'Form_Main_Pos_Mode';
  FormMainPosModeValueDefault = fpmDefault;
  SelectedFormMainMonitorIndexKey = 'Selected_Form_Main_Monitor_Index';
  SelectedFormMainMonitorIndexValueDefault = 0;

  ConsolePosModeKey = 'Console_Pos_Mode';
  ConsolePosModeValueDefault = cpmFollowMain;
  SelectedConsoleMonitorIndexKey = 'Selected_Console_Monitor_Index';
  SelectedConsoleMonitorIndexValueDefault = 0;





var
  _StartTime: TDateTime;
  _State: TState = TState.UnCreated;

  _ConsoleHide: Boolean;

  _FileNameStationsList: String;
  _FileNameCommoditiesList: String;
  _FileNameContractorsList: String;

  _PathLoadExecuteDir: String;
  _PathSaveExecuteDir: String;

  _ConsolePosMode: TConsolePosMode;
  _SelectedConsoleMonitorIndex: Integer;

  _FormMainPosMode: TFormPosMode;
  _SelectedFormMainMonitorIndex: Integer;


  FormSCUxSize: TFormSCUxSize;

  {* Console Server }
  _Console: TConsoleReaderThread;

  {* Contract Database }
  _ContractDB: TContractDB;




{ IMainService }


function TFormMain.Console : TConsoleReaderThread;
begin
  Result := _Console;
end;



function TFormMain.ContractDB: TContractDB;
begin
  Result := _ContractDB;
end;



procedure TFormMain.IniPropStorageSetAllOptionsToDefault;
//var
//  temp: string;
begin
  //temp:= StringReplace(prop, LineEnding, INI_NEWLINE, [rfReplaceAll]);

  IniPropStorage.WriteString(FileNameStationsListKey,          FileNameStationsListValueDefault);
  IniPropStorage.WriteString(FileNameCommoditiesListKey,       FileNameCommoditiesListValueDefault);
  IniPropStorage.WriteString(FileNameContractorsListKey,       FileNameContractorsListValueDefault);
  IniPropStorage.WriteString(PathLoadExecuteDirKey,            PathLoadExecuteDirValueDefault);
  IniPropStorage.WriteString(PathSaveExecuteDirKey,            PathSaveExecuteDirValueDefault);
  
  IniPropStorage.WriteInteger(FormMainPosModeKey,              Ord(FormMainPosModeValueDefault));
  IniPropStorage.WriteInteger(SelectedFormMainMonitorIndexKey, SelectedFormMainMonitorIndexValueDefault);

  IniPropStorage.WriteInteger(ConsolePosModeKey,               Ord(ConsolePosModeValueDefault));
  IniPropStorage.WriteInteger(SelectedConsoleMonitorIndexKey,  SelectedConsoleMonitorIndexValueDefault);

  IniPropStorage.Save;
end;


procedure TFormMain.IniPropStorageSetAllOptions;
begin
  IniPropStorage.WriteString(FileNameStationsListKey,          _FileNameStationsList);
  IniPropStorage.WriteString(FileNameCommoditiesListKey,       _FileNameCommoditiesList);
  IniPropStorage.WriteString(FileNameContractorsListKey,       _FileNameContractorsList);
  IniPropStorage.WriteString(PathLoadExecuteDirKey,            _PathLoadExecuteDir);
  IniPropStorage.WriteString(PathSaveExecuteDirKey,            _PathSaveExecuteDir);

  IniPropStorage.WriteInteger(FormMainPosModeKey,              Ord(_FormMainPosMode));
  IniPropStorage.WriteInteger(SelectedFormMainMonitorIndexKey,   _SelectedFormMainMonitorIndex);

  IniPropStorage.WriteInteger(ConsolePosModeKey,               Ord(_ConsolePosMode));
  IniPropStorage.WriteInteger(SelectedConsoleMonitorIndexKey,   _SelectedConsoleMonitorIndex);

  IniPropStorage.Save;
end;


procedure TFormMain.IniPropStorageRestore;
var
  StoredValue: Integer;
begin
  IniPropStorage.Restore;

  _FileNameStationsList    := IniPropStorage.ReadString(FileNameStationsListKey, FileNameStationsListValueDefault);
  if _FileNameStationsList = '' then _FileNameStationsList := FileNameStationsListValueDefault;

  _FileNameCommoditiesList := IniPropStorage.ReadString(FileNameCommoditiesListKey, FileNameCommoditiesListValueDefault);
  if _FileNameCommoditiesList = '' then _FileNameCommoditiesList := FileNameCommoditiesListValueDefault;

  _FileNameContractorsList := IniPropStorage.ReadString(FileNameContractorsListKey, FileNameContractorsListValueDefault);
  if _FileNameContractorsList = '' then _FileNameContractorsList := FileNameContractorsListValueDefault;

  _PathLoadExecuteDir      := IniPropStorage.ReadString(PathLoadExecuteDirKey, PathLoadExecuteDirValueDefault);
  if _PathLoadExecuteDir = '' then _PathLoadExecuteDir := PathLoadExecuteDirValueDefault;

  _PathSaveExecuteDir      := IniPropStorage.ReadString(PathSaveExecuteDirKey, PathSaveExecuteDirValueDefault);
  if _PathSaveExecuteDir = '' then _PathSaveExecuteDir := PathSaveExecuteDirValueDefault;
  
  //
  StoredValue := IniPropStorage.ReadInteger(FormMainPosModeKey, Ord(FormMainPosModeValueDefault));
  
  if (StoredValue >= Ord(Low(TFormPosMode))) and (StoredValue <= Ord(High(TFormPosMode))) then
    _FormMainPosMode := TFormPosMode(StoredValue)
  else
    _FormMainPosMode := FormMainPosModeValueDefault;

  _SelectedFormMainMonitorIndex := IniPropStorage.ReadInteger(SelectedFormMainMonitorIndexKey, SelectedFormMainMonitorIndexValueDefault);
  if _SelectedFormMainMonitorIndex >= Screen.MonitorCount then
    _SelectedFormMainMonitorIndex := 0;

  //
  StoredValue := IniPropStorage.ReadInteger(ConsolePosModeKey, Ord(ConsolePosModeValueDefault));
  
  if (StoredValue >= Ord(Low(TConsolePosMode))) and (StoredValue <= Ord(High(TConsolePosMode))) then
    _ConsolePosMode := TConsolePosMode(StoredValue)
  else
    _ConsolePosMode := ConsolePosModeValueDefault;

  _SelectedConsoleMonitorIndex := IniPropStorage.ReadInteger(SelectedConsoleMonitorIndexKey, SelectedConsoleMonitorIndexValueDefault);
  if _SelectedConsoleMonitorIndex >= Screen.MonitorCount then
    _SelectedConsoleMonitorIndex := 0;
end;


// TODO generalizzare in UpdateFormPosition e gestire tutte le TForm del progetto
procedure TFormMain.UpdateMainFormPosition(AForm: TForm);  
var
  TargetMonitor: TMonitor;
  NewX, NewY: Integer;
begin
  if _FormMainPosMode = fpmDefault then Exit;

  if _FormMainPosMode = fpmFollowActiveMonitor then
    TargetMonitor := Screen.MonitorFromWindow(Handle)
  else
    TargetMonitor := Screen.Monitors[_SelectedFormMainMonitorIndex];

  NewX := TargetMonitor.WorkareaRect.Left + (TargetMonitor.WorkareaRect.Width - Width) div 2;
  NewY := TargetMonitor.WorkareaRect.Top + (TargetMonitor.WorkareaRect.Height - Height) div 2;

  SetBounds(NewX, NewY, Width, Height);
end;



procedure TFormMain.ValidateMonitorSettings;
var
  AChanged: Boolean;
begin
  AChanged := False;

  if (_FormMainPosMode = fpmSpecificMonitor) and
      (_SelectedFormMainMonitorIndex >= Screen.MonitorCount) then
  begin
    _FormMainPosMode := FormMainPosModeValueDefault;
    _SelectedFormMainMonitorIndex := SelectedFormMainMonitorIndexValueDefault;
    AChanged := True;
  end;

  if (_ConsolePosMode = cpmSpecificMonitor) and
      (_SelectedConsoleMonitorIndex >= Screen.MonitorCount) then
  begin
    _ConsolePosMode := ConsolePosModeValueDefault;
    _SelectedConsoleMonitorIndex := SelectedConsoleMonitorIndexValueDefault;
    AChanged := True;
  end;

  if AChanged then
  begin
    IniPropStorageSetAllOptions;

    UpdateMainFormPosition(Self); // TODO generalizzare in UpdateFormPosition

    if not _ConsoleHide then
      Console.CSShow(_ConsolePosMode, Self, _SelectedConsoleMonitorIndex);
  end;
end;



function TFormMain.GetPathLoadExecuteDir : String;      
begin
  Result := _PathLoadExecuteDir;
end;



procedure TFormMain.SetPathLoadExecuteDir(Path: String);   
begin
  _PathLoadExecuteDir := Path;
end;



procedure TFormMain.SavePathLoadExecuteDir;         
begin
  IniPropStorage.WriteString(PathLoadExecuteDirKey, _PathLoadExecuteDir);
  IniPropStorage.Save;
end;



function TFormMain.GetPathSaveExecuteDir : String;      
begin
  Result := _PathSaveExecuteDir;
end;



procedure TFormMain.SetPathSaveExecuteDir(Path: String); 
begin
  _PathSaveExecuteDir := Path;
end;



procedure TFormMain.SavePathSaveExecuteDir;   
begin
  IniPropStorage.WriteString(PathSaveExecuteDirKey, _PathSaveExecuteDir);
  IniPropStorage.Save;   
end;



function TFormMain.GetFileNameCommoditiesList : String;  
begin
  Result := _FileNameCommoditiesList;
end;



procedure TFormMain.SetFileNameCommoditiesList(Path: String); 
begin
  _FileNameCommoditiesList := Path;
end;



procedure TFormMain.SaveFileNameCommoditiesList;  
begin
  IniPropStorage.WriteString(FileNameCommoditiesListKey, _FileNameCommoditiesList);
  IniPropStorage.Save;
end;



function TFormMain.GetFileNameStationsList : String;
begin
  Result := _FileNameStationsList;
end;



procedure TFormMain.SetFileNameStationsList(Path: String);   
begin
  _FileNameStationsList := Path;
end;



procedure TFormMain.SaveFileNameStationsList;         
begin
  IniPropStorage.WriteString(FileNameStationsListKey, _FileNameStationsList);
  IniPropStorage.Save;
end;



procedure TFormMain.ShowConsoleSettings(Sender: TObject);   
begin
  ActionShowConsoleSettingsExecute(Sender);
end;



procedure TFormMain.ShowHideConsole;   
begin
  ToolButtonConsoleClick(Self);
end;



procedure TFormMain.FormKeyDownPressConsole;
begin
  if not _ConsoleHide then
  begin
    Console.CSSetForegroundWindow;
  end
  else begin
    ToolButtonConsoleClick(Self);
  end;
end;




{$R *.lfm}

{ TFormMain }


procedure TFormMain.WMDisplayChange(var Message: TMessage);
begin
  inherited;

  // The system needs a moment to update the Screen object.
  // It is often useful to call validation after a very short delay or while processing messages.
  Application.ProcessMessages;

  ValidateMonitorSettings;
end;



procedure TFormMain.ApplicationPropertiesActivate(Sender: TObject);
begin
  // @code(_State) is initialised before @code(FormMain) is created.
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



procedure TFormMain.ApplicationPropertiesDeactivate(Sender: TObject);
begin
  State:= TState.UnFocused;
end;



procedure TFormMain.BitBtnSCUxSizeClick(Sender: TObject);
begin
  FormSCUxSize.Show;
end;



procedure ConsoleSettingsApply(Dlg: TFormConsoleSettings);
begin
  case Dlg.ComboBoxMonitors.ItemIndex of
    0: _ConsolePosMode := cpmDefault;
    1: _ConsolePosMode := cpmFollowMain;
    else
    begin
      _ConsolePosMode := cpmSpecificMonitor;
      _SelectedConsoleMonitorIndex := Dlg.ComboBoxMonitors.ItemIndex - 2;
    end;
  end;
end;



procedure TFormMain.BitBtnShowConsoleSettingsApplyClick(Sender: TObject);
var
  Dlg: TFormConsoleSettings;
begin
  if Sender is TControl then
  begin
    Dlg := TFormConsoleSettings(GetParentForm(TControl(Sender)));

    ConsoleSettingsApply(Dlg);

    if not _ConsoleHide then
      Console.CSShow(_ConsolePosMode, Self, _SelectedConsoleMonitorIndex);
  end;
end;



procedure TFormMain.ActionShowConsoleSettingsExecute(Sender: TObject);
var
  Dlg: TFormConsoleSettings;
  i: Integer;
begin
  Dlg := TFormConsoleSettings.Create(Self);
  try
    Dlg.BitBtnApply.OnClick := @BitBtnShowConsoleSettingsApplyClick;

    Dlg.ComboBoxMonitors.Items.Clear;
    Dlg.ComboBoxMonitors.Items.Add('System Default');
    Dlg.ComboBoxMonitors.Items.Add('Follow Main Window (Centered)');
    for i := 0 to Screen.MonitorCount - 1 do
      Dlg.ComboBoxMonitors.Items.Add(Format('Monitor %d (%dx%d)',
        [i + 1, Screen.Monitors[i].Width, Screen.Monitors[i].Height]));

    case _ConsolePosMode of
      cpmDefault: Dlg.ComboBoxMonitors.ItemIndex := 0;
      cpmFollowMain: Dlg.ComboBoxMonitors.ItemIndex := 1;
      cpmSpecificMonitor: Dlg.ComboBoxMonitors.ItemIndex := _SelectedConsoleMonitorIndex + 2;
    end;

    if Dlg.ShowModal = mrOk then
    begin
      ConsoleSettingsApply(Dlg);
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TFormMain.ActionAboutExecute(Sender: TObject);
var
  InfoDialog: TInfoForm;
begin
  InfoDialog := TInfoForm.Create(nil);
  try
    InfoDialog.ShowModal;
  finally
    InfoDialog.Free;
  end;
end;



procedure TFormMain.ToolButtonAlwaysShowOnTopClick(Sender: TObject); // BUG
begin
  if Self.FormStyle = fsNormal then
    Self.FormStyle := fsSystemStayOnTop
  else        
    Self.FormStyle := fsNormal;
end;



procedure TFormMain.ToolButtonConsoleClick(Sender: TObject);
begin
  if _ConsoleHide then
  begin
    Console.CSShow(_ConsolePosMode, Self, _SelectedConsoleMonitorIndex);
  end else Console.CSHide;
  _ConsoleHide := not _ConsoleHide;
end;



procedure TFormMain.Timer1Timer(Sender: TObject);
var
  CurrentTime: TDateTime;
begin
  CurrentTime := Now;

  StaticTextTime.Caption := FormatDateTime('hh" : "nn" : "ss', CurrentTime);

  StaticTextTimeStamp.Caption := FormatDateTime('hh"h " nn"m " ss"s"', CurrentTime - _StartTime);
end;



procedure TFormMain.ToolButtonTestClick(Sender: TObject);
begin
  _ContractDB.Test;
end;



procedure TFormMain.ConsoleInputExecute(const S: string);
var
  hForm: HWND;
begin
  _Console.CSInput(S);

  case S of
    'exit':
      Application.Terminate;
    'hide':
      begin
        if Self.WindowState = wsMinimized then
        begin
          Self.WindowState := wsNormal;
        end;
        ToolButtonConsoleClick(Self);

        hForm := Self.Handle;
        if hForm <> 0 then
        begin
          SetForegroundWindow(hForm);
          Self.SetFocus;
        end;
      end
    else
      begin
        // do nothing
      end;
  end;
end;



procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Shift := Shift;
  case Key of
    VK_F1:
      begin
        FormKeyDownPressConsole;
        Key := 0;
      end
  else
    begin
    end;
  end;
end;


procedure TFormMain.FormKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    '\':
      begin
        FormKeyDownPressConsole;
        Key := #0;
      end
  else
    begin
    end;
  end;
end;



constructor TFormMain.Create(TheOwner: TComponent);var
  ConfigPath: string;
begin
  inherited Create(TheOwner);

  State:= TState.Created;
  _State:= TState.Created;

  IniPropStorage.IniSection:= 'Application';
  {$IFDEF LINUX}
  ConfigPath := GetAppConfigFile(False,True); // /home/<user>/.config/StarTools/StarTools.cfg;
  {$ENDIF}
  {$IFDEF WINDOWS}
  ConfigPath := GetAppConfigFile(False);      // C:\Users\<user>\AppData\Local\StarTools\StarTools.cfg;
  {$ENDIF}
  IniPropStorage.IniFileName := ConfigPath; 
  if not FileExists(ConfigPath) then
  begin
    ForceDirectories(ExtractFilePath(ConfigPath));
    IniPropStorageSetAllOptionsToDefault();
  end;   
  IniPropStorageRestore();
end;



procedure TFormMain.FormCreate(Sender: TObject);
var
  i: Integer;
  S: String;
  CommandLine: String;
begin
  if State = TState.Created then // ASSERT: Always True
    begin
      State:= TState.Ready;
      ApplicationPropertiesActivate(Sender);
    end;


  // Console Server ---------------------------------
  S := LineEnding +
          'Star Tools ' + StarToolsVersion + ' by JF' + LineEnding + LineEnding +
          '  Console commands:' + LineEnding;
  for CommandLine in ConsoleCommandsArray do
    S += '    ' + CommandLine + LineEnding;

  _Console := TConsoleReaderThread.Create(Self as IConsoleInputHandler, S);
  _Console.CSHide;
  _ConsoleHide := True;

  if ParamCount > 0 then
  begin
    S := 'Parameters: ';
    for i := 1 to ParamCount do
    begin
      S := S + ParamStr(i);
      if i < ParamCount then S:= S + ', ';
    end;
    _Console.NoticeLog('TFormMain.FormCreate', S);
  end;
  // ------------------------------------------------
  _Console.NoticeLog('TFormMain.FormCreate', 'Program START');
  // ------------------------------------------------
  _ContractDB := TContractDB.Create(_Console);
  _Console.DebugLog('TFormMain.FormCreate', 'ContractDB created');
  // ------------------------------------------------
  TContract.SetConsoleServer(_Console);
  // ------------------------------------------------   

  FormSCUxSize := TFormSCUxSize.Create(nil, Self as IMainService);

  ValidateMonitorSettings;
  UpdateMainFormPosition(Self); // TODO generalizzare in UpdateFormPosition


  // C:\Users\<user>\AppData\Local\StarTools\StarTools.cfg
  _Console.DebugLog('TFormMain.FormCreate', 'IniFile = ' + IniPropStorage.IniFileName);
end;


procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  CurrentMonitor: TMonitor;
  i: Integer;
begin
  CurrentMonitor := Screen.MonitorFromWindow(Handle);

  _SelectedFormMainMonitorIndex := 0;
    for i := 0 to Screen.MonitorCount - 1 do
    begin
      if Screen.Monitors[i] = CurrentMonitor then
      begin
        _SelectedFormMainMonitorIndex := i;
        Break;
      end;
    end;

  if _SelectedFormMainMonitorIndex = 0 then
    _FormMainPosMode := fpmDefault
  else
    _FormMainPosMode := fpmSpecificMonitor;

  IniPropStorageSetAllOptions;
end;


procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FormSCUxSize.Free;
  FormSCUxSize := nil;

  _ContractDB.Free;
  _ContractDB := nil;
end;


destructor TFormMain.Destroy;
begin
  // Free Console for last
  if Assigned(_Console) then
  begin
    _Console.Free;
    _Console := nil;
  end;

  inherited Destroy;
end;


initialization
_StartTime := Now;

_Console := nil;

_ContractDB := nil;

_ConsoleHide := True;

_FileNameStationsList := '';
_FileNameCommoditiesList := '';
_FileNameContractorsList := '';

_PathLoadExecuteDir := '';
_PathSaveExecuteDir := '';

_FormMainPosMode := fpmDefault;
_SelectedFormMainMonitorIndex := 0;

_ConsolePosMode := cpmFollowMain;
_SelectedConsoleMonitorIndex := 0;




finalization


end.

