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



{* User Nick Name Unit }
unit UserNickNameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ButtonPanel;

type

  { TFormUserNickName }

  TFormUserNickName = class(TForm)
    ButtonPanel: TButtonPanel;
    EditUserNickName: TEdit;
    ImageUserAvatar: TImage;
    LabelUserNickName: TLabel;
    PanelTop: TPanel;
  private
    {* User Nick Name }
    function GetNickName: String;

    {*
      Sets the user nick name.
      @param(ANickName The user nick name.)
    }
    procedure SetNickName(const ANickName: String);

  public
    {* User Nick Name }
    property NickName : String read GetNickName write SetNickName;

  end;




//var
//  FormUserNickName: TFormUserNickName;






implementation


function TFormUserNickName.GetNickName : String;
begin
  Result := EditUserNickName.Text;
end;



procedure TFormUserNickName.SetNickName(const ANickName: String);
begin
  EditUserNickName.Text := ANickName;
end;

{$R *.lfm}

end.

