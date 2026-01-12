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


{* Main Service Unit }
unit MainServiceUnit;

{$mode objfpc}{$H+}{$J-}{$R+}

interface

uses
  Classes, SysUtils, Forms,
  ConsoleUnit,
  ContractDBUnit;



type
  {*
    @abstract Interface
  }
  IMainService = interface
    ['{F60F6353-A3B1-46A1-82AF-54F1BE568E4C}']

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
      and writes them to the @code(IniPropStorage) component.
      
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


  end;



implementation

end.

