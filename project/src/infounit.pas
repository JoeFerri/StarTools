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



{* Info Unit }
unit InfoUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Buttons,
  ExtCtrls, StdCtrls, LCLIntf, VersionUnit;

type

  { TInfoForm }

  TInfoForm = class(TForm)
    BtnClose: TBitBtn;
    GitHubLabelRepository: TLabel;
    ImageVersionLogo: TImage;
    LabelVersionTitle: TLabel;
    GitHubLabelFullLegalNotices: TLabel;
    PanelBottomInformation: TPanel;
    StaticTextRepository: TStaticText;
    StaticTextFullLegalNotice: TStaticText;
    VersionLabel: TLabel;
    MemoInformation: TMemo;
    PageControl1: TPageControl;
    PanelBottom: TPanel;
    TabSheetVersion: TTabSheet;
    TabSheetInformation: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GitHubLabelFullLegalNoticesClick(Sender: TObject);
    procedure GitHubLabelFullLegalNoticesMouseEnter(Sender: TObject);
    procedure GitHubLabelFullLegalNoticesMouseLeave(Sender: TObject);
    procedure GitHubLabelRepositoryClick(Sender: TObject);
    procedure GitHubLabelRepositoryMouseEnter(Sender: TObject);
    procedure GitHubLabelRepositoryMouseLeave(Sender: TObject);
  private

  public

  end;

var
  InfoForm: TInfoForm;

implementation

{$R *.lfm}

{ TInfoForm }

procedure TInfoForm.FormCreate(Sender: TObject);
const
  OLD_VERSION_PATTERN = 'Version: m.m.p';
var
  NewVersionString: string;
begin
  VersionLabel.Caption:= 'Version: ' + StarToolsVersion;

  NewVersionString := 'Version: ' + StarToolsVersion;
  MemoInformation.Lines.BeginUpdate;
  try
    MemoInformation.Text := StringReplace(
      MemoInformation.Text,
      OLD_VERSION_PATTERN,
      NewVersionString,
      [rfReplaceAll]//, rfIgnoreCase]
    );
  finally
    MemoInformation.Lines.EndUpdate;
  end;
end;

procedure TInfoForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TInfoForm.GitHubLabelFullLegalNoticesClick(Sender: TObject);
begin
  OpenURL(GitHubLabelFullLegalNotices.Caption);
end;

procedure TInfoForm.GitHubLabelFullLegalNoticesMouseEnter(Sender: TObject);
begin
  GitHubLabelFullLegalNotices.Font.Color := clRed;
  GitHubLabelFullLegalNotices.Font.Style := [fsUnderline];
  GitHubLabelFullLegalNotices.Cursor     := crHandPoint;
end;

procedure TInfoForm.GitHubLabelFullLegalNoticesMouseLeave(Sender: TObject);
begin
  GitHubLabelFullLegalNotices.Font.Color := clBlue;
  GitHubLabelFullLegalNotices.Font.Style := [];
  GitHubLabelFullLegalNotices.Cursor     := crDefault;
end;

procedure TInfoForm.GitHubLabelRepositoryClick(Sender: TObject);
begin
  OpenURL(GitHubLabelRepository.Caption);
end;

procedure TInfoForm.GitHubLabelRepositoryMouseEnter(Sender: TObject);
begin
  GitHubLabelRepository.Font.Color := clRed;
  GitHubLabelRepository.Font.Style := [fsUnderline];
  GitHubLabelRepository.Cursor     := crHandPoint;
end;

procedure TInfoForm.GitHubLabelRepositoryMouseLeave(Sender: TObject);
begin
  GitHubLabelRepository.Font.Color := clBlue;
  GitHubLabelRepository.Font.Style := [];
  GitHubLabelRepository.Cursor     := crDefault;
end;

end.

