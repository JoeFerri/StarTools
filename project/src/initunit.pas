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



{*}
unit InitUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, WebUnit;


type
  TInitializationThreadCallback = procedure of object;



  TInitializationThread = class(TThread)
  private
    FErrorMsg: string;
    FErrorCode: Integer;
    FCallback: TInitializationThreadCallback;
  protected
    procedure Execute; override;
  public
    property ErrorMessage: string read FErrorMsg;

    property ErrorCode: Integer read FErrorCode;

    constructor Create(ACallback: TInitializationThreadCallback; ATerminate: TNotifyEvent); reintroduce;
  end;




implementation



procedure TInitializationThread.Execute;
begin
  try
    FCallback;
  except
    on E: Exception do
    begin
      FErrorMsg := E.Message;
      FErrorCode := 1;
    end;
  end;
end;



constructor TInitializationThread.Create(ACallback: TInitializationThreadCallback; ATerminate: TNotifyEvent);
begin
  inherited Create(True);

  FCallback := ACallback;
  OnTerminate := ATerminate;
  FErrorMsg := '';
  FErrorCode := 0;
  FreeOnTerminate := True;
end;



end.

