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



{* Contract View Unit }
unit ContractViewUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons;

type

  { TFormContractView }

  TFormContractView = class(TForm)
    BitBtnSave: TBitBtn;
    MemoContracts: TMemo;
    PanelMain: TPanel;
    PanelMenu: TPanel;
    SaveDialogDetails: TSaveDialog;
    procedure BitBtnSaveClick(Sender: TObject);
  private

  public
    procedure UpdateDetails(const Details: TStringList);
  end;

//var
//  FormContractView: TFormContractView;

implementation

{$R *.lfm}

procedure TFormContractView.BitBtnSaveClick(Sender: TObject);
begin
  if SaveDialogDetails.Execute then
    MemoContracts.Lines.SaveToFile(SaveDialogDetails.FileName);
end;


procedure TFormContractView.UpdateDetails(const Details: TStringList);
begin
  MemoContracts.Lines.Assign(Details);
end;



end.

