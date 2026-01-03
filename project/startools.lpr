program startools;

{$mode objfpc}{$H+}{$R+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils,
  Forms, MainUnit, VersionUnit
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='StarTools';
  Application.Scaled:=True;

  DefaultFormatSettings.DecimalSeparator := '.';

  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

