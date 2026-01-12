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



{* Console Settings Dialog Unit }
unit ConsoleSettingsDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  Buttons;

type

  { TFormConsoleSettings }

  TFormConsoleSettings = class(TForm)
    BitBtnApply: TBitBtn;
    ButtonPanel: TButtonPanel;
    ComboBoxMonitors: TComboBox;
    GroupBoxView: TGroupBox;
    StaticTextConsoleWindowPosition: TStaticText;
  private

  public

  end;

//var
//  FormConsoleSettings: TFormConsoleSettings;

implementation

{$R *.lfm}

end.

