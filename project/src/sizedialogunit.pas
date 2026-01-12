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


{* Size Dialog Unit }
unit SizeDialogUnit;

{$mode ObjFPC}{$H+}{$R+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ButtonPanel;

type

  { TFormSizeDialog }

  TFormSizeDialog = class(TForm)
    ButtonPanelSizeDialog: TButtonPanel;
    PanelSizeDialog: TPanel;
    RadioGroupSizeDialog: TRadioGroup;
  procedure ButtonCancelClick(Sender: TObject);
  procedure ButtonOKClick(Sender: TObject);
  private
    FSelectedSize: Integer;
    FSelectedSizeDefault: Integer;
  public
    property SelectedSize: Integer read FSelectedSize;
    constructor Create(AOwner: TComponent; const ASizes: array of Integer); reintroduce;

  public

  end;

//var
//  FormSizeDialog: TFormSizeDialog;

implementation

{$R *.lfm}


procedure TFormSizeDialog.ButtonOKClick(Sender: TObject);
begin
  if RadioGroupSizeDialog.ItemIndex <> -1 then
    FSelectedSize := StrToIntDef(RadioGroupSizeDialog.Items[RadioGroupSizeDialog.ItemIndex], FSelectedSizeDefault)
  else
    FSelectedSize := FSelectedSizeDefault;

  ModalResult := mrOK;
end;


procedure TFormSizeDialog.ButtonCancelClick(Sender: TObject);
begin
  FSelectedSize := -1;
  ModalResult := mrCancel;
end;



constructor TFormSizeDialog.Create(AOwner: TComponent; const ASizes: array of Integer);
 var
   i: Integer;
begin
  inherited Create(AOwner);

  RadioGroupSizeDialog.Items.BeginUpdate;
  try
    RadioGroupSizeDialog.Items.Clear;
    for i := Low(ASizes) to High(ASizes) do
      RadioGroupSizeDialog.Items.Add(IntToStr(ASizes[i]));
  finally
    RadioGroupSizeDialog.Items.EndUpdate;
  end;

  if RadioGroupSizeDialog.Items.Count > 0 then
  begin
    RadioGroupSizeDialog.ItemIndex := 0;
    FSelectedSizeDefault := StrToInt(RadioGroupSizeDialog.Items[0]);
  end
  else begin
    RadioGroupSizeDialog.ItemIndex := -1;
    FSelectedSizeDefault := -1;
  end;
  
  FSelectedSize := FSelectedSizeDefault;
end;



end.

