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



{*}
unit SplashScreenUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, BCListBox,
  Windows, LCLIntf, LCLType, StdCtrls,
  BGRASpriteAnimation, BCPanel, BGRABitmap, BGRABitmapTypes;


type

  { TFormSplashScreen }

  TFormSplashScreen = class(TForm)
    BCPanel1: TBCPanel;
    BGRASpriteAnimationHourglass: TBGRASpriteAnimation;
    PanelMessage: TPanel;
    PanelTop: TPanel;
    StaticTextMessage: TStaticText;
    procedure FormCreate(Sender: TObject);
  private
    procedure MakeTransparent;
    function GetMessage: String;
    procedure SetMessage(AMessage: String);
  public
    property Message: String read GetMessage write SetMessage;

    procedure ResetMessage;
  end;



const
  MessageDefault = 'Loading...';



//var
//  FormSplashScreen: TFormSplashScreen;

implementation



procedure TFormSplashScreen.MakeTransparent;
var
  ExStyle: LONG;
begin
  ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);

  SetWindowLong(Handle, GWL_EXSTYLE, ExStyle or WS_EX_LAYERED);

  SetLayeredWindowAttributes(Handle, ColorToRGB(cl3DDkShadow), 255, LWA_COLORKEY);
end;



function TFormSplashScreen.GetMessage: String;
begin
  Result := StaticTextMessage.Caption;
end;



procedure TFormSplashScreen.SetMessage(AMessage: String);
begin
  StaticTextMessage.Caption := AMessage;
end;



procedure TFormSplashScreen.ResetMessage;
begin
  StaticTextMessage.Caption := MessageDefault;
end;


{$R *.lfm}

procedure TFormSplashScreen.FormCreate(Sender: TObject);
begin
  ResetMessage;
  MakeTransparent;
end;

end.

