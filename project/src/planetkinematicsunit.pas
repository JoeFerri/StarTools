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
unit PlanetKinematicsUnit;

{$mode ObjFPC}{$H+}{$J-}{$R+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, Buttons, DateTimePicker, GeometryUnit, Math;

type

  { TFormOM1OM2Center }

  TFormOM1OM2Center = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    BitBtnHelp: TBitBtn;
    DateTimePickerOM5Time_1: TDateTimePicker;
    DateTimePickerOM5Time_2: TDateTimePicker;
    EditAngularDisplacement_deg: TEdit;
    EditAngularVelocity: TEdit;
    EditRotationPeriod_hhmmss: TEdit;
    EditAngularDisplacement_rad: TEdit;
    EditRotationPeriod_s: TEdit;
    EditTangentialSpeed: TEdit;
    EditRotationRate: TEdit;
    EditTimeInterval: TEdit;
    EditOM5Time_1: TEdit;
    EditOM5Time_2: TEdit;
    EditOM1OM2Distance: TEdit;
    EditColatitude_grad: TEdit;
    EditColatitude_rad: TEdit;
    EditAveragePlanetRadius: TEdit;
    EditLatitude_grad: TEdit;
    EditLatitude_rad: TEdit;
    EditLongitude_grad: TEdit;
    EditLongitude_rad: TEdit;
    EditPlanetCenter_x: TEdit;
    EditDistance_d: TEdit;
    EditPlanetCenter_y: TEdit;
    EditPlanetCenter_z: TEdit;
    FloatSpinEditOM1_x: TFloatSpinEdit;
    FloatSpinEditAverageAltitude: TFloatSpinEdit;
    FloatSpinEditOM5_1_x: TFloatSpinEdit;
    FloatSpinEditOM1_y: TFloatSpinEdit;
    FloatSpinEditOM5_1_y: TFloatSpinEdit;
    FloatSpinEditOM1_z: TFloatSpinEdit;
    FloatSpinEditOM5_1_z: TFloatSpinEdit;
    FloatSpinEditOM2_x: TFloatSpinEdit;
    FloatSpinEditOM5_2_x: TFloatSpinEdit;
    FloatSpinEditOM2_y: TFloatSpinEdit;
    FloatSpinEditOM5_2_y: TFloatSpinEdit;
    FloatSpinEditOM2_z: TFloatSpinEdit;
    FloatSpinEditOM5_2_z: TFloatSpinEdit;
    GroupBoxOM5Results: TGroupBox;
    GroupBoxOM5: TGroupBox;
    GroupBoxOM12Results: TGroupBox;
    GroupBoxOM12: TGroupBox;
    Panel1: TPanel;
    Panel2: TPanel;
    StaticTextAngularDisplacement_deg: TStaticText;
    StaticTextRotationPeriod_s: TStaticText;
    StaticTextTimeInterval_s: TStaticText;
    StaticTextTangentialSpeed_ms: TStaticText;
    StaticTextAngularVelocity_rads: TStaticText;
    StaticTextAngularVelocity: TStaticText;
    StaticTextRotationPeriod_hhmmss: TStaticText;
    StaticTextRotationPeriod: TStaticText;
    StaticTextAngularDisplacement: TStaticText;
    StaticTextAngularDisplacement_rad: TStaticText;
    StaticTextTangentialSpeed: TStaticText;
    StaticTextRotationRate: TStaticText;
    StaticTextRotationRate_ds: TStaticText;
    StaticTextTimeInterval: TStaticText;
    StaticTextOM5Time_1: TStaticText;
    StaticTextOM1OM2Distance_m: TStaticText;
    StaticTextOM1OM2Distance: TStaticText;
    StaticTextDegrees: TStaticText;
    StaticTextAveragePlanetRadius_m: TStaticText;
    StaticTextOM5Time_2: TStaticText;
    StaticTextOM5_1_m: TStaticText;
    StaticTextOM5_2_m: TStaticText;
    StaticTextOM5_1: TStaticText;
    StaticTextOM5_2: TStaticText;
    StaticTextOM5_x: TStaticText;
    StaticTextOM5_y: TStaticText;
    StaticTextOM5_z: TStaticText;
    StaticTextRadiants: TStaticText;
    StaticTextDistance_d_m: TStaticText;
    StaticTextPlanetCenter_m: TStaticText;
    StaticTextOM1_m: TStaticText;
    StaticTextOM2_m: TStaticText;
    StaticTextAverageAltitude_m: TStaticText;
    StaticTextAveragePlanetRadius: TStaticText;
    StaticTextAverageAltitude: TStaticText;
    StaticTextPlanetCenter_x: TStaticText;
    StaticTextPlanetCenter_y: TStaticText;
    StaticTextPlanetCenter_z: TStaticText;
    StaticTextColatitude: TStaticText;
    StaticTextLatitude: TStaticText;
    StaticTextLongitude: TStaticText;
    StaticTextDistance_d: TStaticText;
    StaticTextOM_x: TStaticText;
    StaticTextOM_y: TStaticText;
    StaticTextOM_z: TStaticText;
    StaticTextPlanetCenter: TStaticText;
    StaticTextOM1: TStaticText;
    StaticTextOM2: TStaticText;
    procedure BitBtnHelpClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure OM1OM2CoordUpdate(Sender: TObject);
    procedure OM5ValuesUpdate(Sender: TObject);
    procedure CopyOnRightClickMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DateTimePickerOM5Time_1Change(Sender: TObject);
    procedure DateTimePickerOM5Time_2Change(Sender: TObject);
    procedure EditOM5Time_1Change(Sender: TObject);
    procedure EditOM5Time_2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FSystemOriginAxes: TGPoint3D;
    FOM1: TGPoint3D;       
    FOM2: TGPoint3D;
    FPlanetCenter: TGPoint3D;
    FOM1OM2Distance: TDistance;
    FAverageAltitude: TDistance;
    FAveragePlanetRadius: TDistance;
    FDistanceOC: TDistance;
    FColatitude: TColatitude;
    FLatitude: TLatitude;
    FLongitude: TLongitude;

    FIsUpdating: Boolean;

    procedure HintInit;
  public
    property SystemOriginAxes: TGPoint3D read FSystemOriginAxes;

  end;

//var
//  FormOM1OM2Center: TFormOM1OM2Center;

implementation



{$R *.lfm}

{ TFormOM1OM2Center }


procedure TFormOM1OM2Center.HintInit;
begin
  // Abilita la visualizzazione degli hint per il form
  Self.ShowHint := True;

  { --- INPUT SECTION: OM1 & OM2 (Poles) --- }
  FloatSpinEditOM1_x.Hint := 'X coordinate of OM1 (typically North Pole). Defines the rotation axis origin.';
  FloatSpinEditOM1_y.Hint := 'Y coordinate of OM1 (typically North Pole). Defines the rotation axis origin.';
  FloatSpinEditOM1_z.Hint := 'Z coordinate of OM1 (typically North Pole). Defines the rotation axis origin.';

  FloatSpinEditOM2_x.Hint := 'X coordinate of OM2 (typically South Pole). Used with OM1 to calculate the planet center.';
  FloatSpinEditOM2_y.Hint := 'Y coordinate of OM2 (typically South Pole). Used with OM1 to calculate the planet center.';
  FloatSpinEditOM2_z.Hint := 'Z coordinate of OM2 (typically South Pole). Used with OM1 to calculate the planet center.';

  FloatSpinEditAverageAltitude.Hint := 'Average altitude [h] of Orbital Markers. Used to derive the physical planet radius: r = (Distance_OM1_OM2 / 2) - h.';

  { --- INPUT SECTION: OM5 (Rotation Tracking) --- }
  FloatSpinEditOM5_1_x.Hint := 'X coordinate of OM5 at time t1. First sample for rotation measurement.';
  FloatSpinEditOM5_1_y.Hint := 'Y coordinate of OM5 at time t1. First sample for rotation measurement.';
  FloatSpinEditOM5_1_z.Hint := 'Z coordinate of OM5 at time t1. First sample for rotation measurement.';

  FloatSpinEditOM5_2_x.Hint := 'X coordinate of OM5 at time t2. Second sample to calculate angular displacement (Δθ).';
  FloatSpinEditOM5_2_y.Hint := 'Y coordinate of OM5 at time t2. Second sample to calculate angular displacement (Δθ).';
  FloatSpinEditOM5_2_z.Hint := 'Z coordinate of OM5 at time t2. Second sample to calculate angular displacement (Δθ).';

  DateTimePickerOM5Time_1.Hint := 'Visual selector for timestamp t1. Synchronized with the ctime text field for Star Citizen log compatibility.';
  DateTimePickerOM5Time_2.Hint := 'Visual selector for timestamp t2. Synchronized with the ctime text field for Star Citizen log compatibility.';

  EditOM5Time_1.Hint := 'Timestamp t1 of the first OM5 position. Format: "Www Mmm dd hh:mm:ss yyyy" (ctime standard).';
  EditOM5Time_2.Hint := 'Timestamp t2 of the second OM5 position. The Δt between t1 and t2 defines the calculation precision.';

  { --- OUTPUT SECTION: Planet Geometry --- }
  EditPlanetCenter_x.Hint := 'Calculated X coordinate of the planet center (C) in the global system origin.';
  EditPlanetCenter_y.Hint := 'Calculated Y coordinate of the planet center (C) in the global system origin.';
  EditPlanetCenter_z.Hint := 'Calculated Z coordinate of the planet center (C) in the global system origin.';

  EditDistance_d.Hint := 'Linear distance [d] from the Star (0,0,0) to the planet center.';
  EditOM1OM2Distance.Hint := 'Straight-line distance between polar markers. Represents the diameter of the OM orbital shell.';
  EditAveragePlanetRadius.Hint := 'The physical radius of the planet (r). Essential for calculating surface tangential speeds.';

  EditColatitude_grad.Hint := 'Colatitude (φ) in degrees: the angle measured from the North Pole (Positive Z-axis). φ = 0° at North Pole, 90° at Equator.';
  EditColatitude_rad.Hint  := 'Colatitude (φ) in radians: the angle measured from the North Pole (Positive Z-axis).';

  EditLatitude_grad.Hint   := 'Geographic Latitude (Φ) in degrees: the angle from the equatorial plane. Positive for North, negative for South.';
  EditLatitude_rad.Hint    := 'Geographic Latitude (Φ) in radians: the angle from the equatorial plane.';

  EditLongitude_grad.Hint  := 'Geographic Longitude (θ) in degrees: the angle in the XY plane. Used to determine the prime meridian offset.';
  EditLongitude_rad.Hint   := 'Geographic Longitude (θ) in radians: the angle in the XY plane.';

  { --- OUTPUT SECTION: Kinematics (Results) --- }
  EditAngularVelocity.Hint := 'Angular Velocity (ω) in rad/s. The fundamental rate of rotation used for all orbital mechanics.';
  EditRotationPeriod_hhmmss.Hint := 'Time for a full 360° rotation (T = 2π / ω). Expressed in HH:MM:SS.';
  EditRotationPeriod_s.Hint := 'Time for a full 360° rotation (T = 2π / ω). Expressed in seconds.';
  EditAngularDisplacement_rad.Hint := 'The angle (Δθ) in radians traveled by the planet between t1 and t2.';
  EditAngularDisplacement_deg.Hint := 'The angle (Δθ) in degrees traveled by the planet between t1 and t2.';
  EditTangentialSpeed.Hint := 'Equatorial Tangential Velocity (veq = ω * r). Speed of the ground sliding beneath a stationary observer at the equator.';
  EditRotationRate.Hint := 'Rotation speed in degrees per second. Useful for manual ship orientation and turret tracking.';
  EditTimeInterval.Hint := 'The elapsed time (Δt) in seconds between your two coordinate captures.';
end;



procedure TFormOM1OM2Center.OM1OM2CoordUpdate(Sender: TObject);
begin
  FOM1 := TGPoint3D.Create(
    FloatSpinEditOM1_x.Value,
    FloatSpinEditOM1_y.Value,
    FloatSpinEditOM1_z.Value
  );
  FOM2 := TGPoint3D.Create(
    FloatSpinEditOM2_x.Value,
    FloatSpinEditOM2_y.Value,
    FloatSpinEditOM2_z.Value
  );

  FPlanetCenter := TGPoint3D.Create(
    FOM1.X,
    FOM1.Y,
    (FOM1.Z + FOM2.Z) / 2
  );

  FDistanceOC := FSystemOriginAxes.DistanceTo(FPlanetCenter);

  FAverageAltitude := FloatSpinEditAverageAltitude.Value;

  FOM1OM2Distance := FOM1.DistanceTo(FOM2);

  FAveragePlanetRadius := (FOM1OM2Distance / 2) - FAverageAltitude;

  FColatitude := FPlanetCenter.Colatitude(FDistanceOC);

  FLatitude := TGPoint3D.CoToLatitude(FColatitude);

  FLongitude := FPlanetCenter.Longitude;

  // Update UI

  EditPlanetCenter_x.Text := FloatToStr(FPlanetCenter.X);
  EditPlanetCenter_y.Text := FloatToStr(FPlanetCenter.Y);
  EditPlanetCenter_z.Text := FloatToStr(FPlanetCenter.Z);

  EditDistance_d.Text     := FloatToStr(RoundTo(FDistanceOC, -4));

  EditColatitude_rad.Text     := FColatitude.TofString;
  EditLatitude_rad.Text       := FLatitude.TofString;
  EditLongitude_rad.Text      := FLongitude.TofString;

  EditColatitude_grad.Text     := FColatitude.ToDegrees.TofString;
  EditLatitude_grad.Text       := FLatitude.ToDegrees.TofString;
  EditLongitude_grad.Text      := FLongitude.ToDegrees.TofString;

  EditOM1OM2Distance.Text := FloatToStr(RoundTo(FOM1OM2Distance, -4));
  EditAveragePlanetRadius.Text := FloatToStr(RoundTo(FAveragePlanetRadius, -4));

  OM5ValuesUpdate(Sender);
end;



procedure TFormOM1OM2Center.BitBtnHelpClick(Sender: TObject);
var
  HelpForm: TForm;
  HelpMemo: TMemo;
  HelpText: TStringList;
begin
  HelpForm := TForm.Create(nil);
  HelpText := TStringList.Create;
  try
    HelpForm.Caption := 'StarTools - Planet Kinematics Guide';
    HelpForm.SetBounds(100, 100, 700, 600);
    HelpForm.Position := poScreenCenter;
    HelpForm.BorderStyle := bsDialog;

    HelpMemo := TMemo.Create(HelpForm);
    HelpMemo.Parent := HelpForm;
    HelpMemo.Align := alClient;
    HelpMemo.ReadOnly := True;
    HelpMemo.ScrollBars := ssAutoVertical;
    HelpMemo.Font.Name := 'Courier New'; // Font monospaziato per tabelle e simboli
    HelpMemo.Font.Size := 10;

    { --- Inizio Contenuto Guida --- }
    HelpText.Add('STARTOOLS: PLANET ROTATION & GEOMETRY GUIDE');
    HelpText.Add('===========================================');
    HelpText.Add('');
    HelpText.Add('1. THEORY OF OPERATION');
    HelpText.Add('   This tool calculates the rotational kinematics of a celestial body');
    HelpText.Add('   using the Global Coordinate System (GCS) provided by Star Citizen.');
    HelpText.Add('   By sampling a fixed orbital point (OM-5) at two different times,');
    HelpText.Add('   we derive the angular velocity vector and the physical radius.');
    HelpText.Add('');
    HelpText.Add('2. SETUP INSTRUCTIONS (STEP-BY-STEP)');       
    HelpText.Add('   [STEP 0] - Essential In-Game Commands:');
    HelpText.Add('   - Press "Enter" to open the game chat.');
    HelpText.Add('   - Use "/showlocation" in chat to copy your current coordinates');
    HelpText.Add('     (relative to the star) to the system clipboard.');
    HelpText.Add('   - Press "\" (backslash) to open the game console/shell.');
    HelpText.Add('   - Use "r_DisplayInfo 3" in the console to show real-time');
    HelpText.Add('     coordinates and system statistics in the overlay.');
    HelpText.Add('   - Use "r_DisplayInfo 0" in the console to hide the overlay.');
    HelpText.Add('');
    HelpText.Add('   [STEP A] - Define the Planet Center:');
    HelpText.Add('   - Copy/Paste coordinates of OM-1 (North Pole) and OM-2 (South Pole).');
    HelpText.Add('   - Input the Average Altitude of the OM shell (e.g., 438812m).');
    HelpText.Add('   - Result: The tool calculates the Center (C) and Physical Radius (r).');
    HelpText.Add('');
    HelpText.Add('   [STEP B] - Measurement Samples:');
    HelpText.Add('   - Go to Orbital Marker 5 (OM-5).');
    HelpText.Add('   - Paste the coordinates and the exact timestamp into "OM5 Time 1".');
    HelpText.Add('   - Wait at least 20-40 seconds for better precision.');
    HelpText.Add('   - Repeat the process for "OM5 Time 2".');
    HelpText.Add('');
    HelpText.Add('3. FIELD EXPLANATIONS');
    HelpText.Add('   --- Kinematics ---');
    HelpText.Add('   * Angular Velocity (ω): The core physical constant (rad/s).');
    HelpText.Add('   * Rotation Period (T): Time for a full 360° turn (HH:MM:SS).');
    HelpText.Add('   * Angular Displacement (Δθ): The arc traveled between samples.');
    HelpText.Add('');
    HelpText.Add('   --- Practical Navigation ---');
    HelpText.Add('   * Tangential Speed (veq): How fast the ground moves at the equator.');
    HelpText.Add('     Crucial for atmospheric entry and hovering.');
    HelpText.Add('   * Rotation Rate (deg/s): Useful for manual tracking and alignment.');
    HelpText.Add('');
    HelpText.Add('4. COORDINATE SYSTEMS');
    HelpText.Add('   - Colatitude (φ): Angle from the Z-axis (0° at North Pole).');
    HelpText.Add('   - Latitude (Φ): Angle from the Equator (Positive = North).');
    HelpText.Add('   - Longitude (θ): Angle in the XY plane.');
    HelpText.Add('');
    HelpText.Add('5. TECHNICAL NOTES');
    HelpText.Add('   - Time Format: Use the ctime standard (e.g., Sun Jan 18 2026).');
    HelpText.Add('   - Precision: Sampling over longer intervals reduces the error');
    HelpText.Add('     caused by floating-point truncation in the /showlocation output.');
    HelpText.Add('');
    HelpText.Add('===========================================');
    HelpText.Add('StarTools (c) 2025-2026');

    HelpMemo.Lines.Assign(HelpText);
    HelpForm.ShowModal;
  finally
    HelpText.Free;
    HelpForm.Free;
  end;
end;



procedure TFormOM1OM2Center.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Assigned(Owner) then
  begin
    TForm(Owner).SetFocus;
    TForm(Owner).Show;
  end;
end;



procedure TFormOM1OM2Center.OM5ValuesUpdate(Sender: TObject);
var
  P1, P2: TGPoint3D;
  T1, T2: TDateTime;
  Rotation: TRotationResult;
  DeltaT_Sec: Double;
  DeltaTheta: TRadiants;
  V1, V2: TGPoint3D;
begin
  // Evitiamo calcoli se i dati non sono completi o se siamo in fase di aggiornamento GUI
  if FIsUpdating then Exit;

  try
    // 1. Acquisizione coordinate cartesiane OM5 ai due tempi
    P1 := TGPoint3D.Create(
      FloatSpinEditOM5_1_x.Value,
      FloatSpinEditOM5_1_y.Value,
      FloatSpinEditOM5_1_z.Value
    );
    P2 := TGPoint3D.Create(
      FloatSpinEditOM5_2_x.Value,
      FloatSpinEditOM5_2_y.Value,
      FloatSpinEditOM5_2_z.Value
    );

    // 2. Acquisizione tempi (usiamo i picker che sono sincronizzati con gli edit)
    T1 := DateTimePickerOM5Time_1.DateTime;
    T2 := DateTimePickerOM5Time_2.DateTime;

    // 3. Calcolo intervallo temporale
    DeltaT_Sec := Abs(T2 - T1) * SecsPerDay; // Conversione TDateTime (giorni) in secondi

    if DeltaT_Sec < 1.0 then // Evitiamo divisioni per zero o campionamenti troppo vicini
    begin
      EditTimeInterval.Text := 'Invalid interval';
      Exit;
    end;

    // 4. Calcolo dello spostamento angolare (DeltaTheta)
    // Trasliamo P1 e P2 rispetto al centro del pianeta calcolato in OM1OM2CoordUpdate
    V1 := P1 - FPlanetCenter;
    V2 := P2 - FPlanetCenter;
    DeltaTheta := V1.AngleTo(V2);

    // 5. Esecuzione calcoli tramite la funzione di GeometryUnit
    Rotation := TGPoint3D.CalculateRotation(P1, P2, FPlanetCenter, T1, T2);

    // 6. Aggiornamento Campi Output
    EditTimeInterval.Text        := FloatToStr(RoundTo(DeltaT_Sec, -2));
    EditAngularDisplacement_rad.Text := FloatToStr(RoundTo(DeltaTheta.Value, -6));
    EditAngularDisplacement_deg.Text := FloatToStr(RoundTo(DeltaTheta.Value * (180/Pi), -2));

    EditAngularVelocity.Text     := FloatToStr(Rotation.Omega);
    EditRotationRate.Text        := FloatToStr(RoundTo(Rotation.DegreesPerSec, -8));

    // Velocità Tangenziale all'equatore: v = omega * r
    // Usiamo il raggio calcolato FAveragePlanetRadius
    EditTangentialSpeed.Text     := FloatToStr(RoundTo(Rotation.Omega * FAveragePlanetRadius, -4));

    // Periodo di rotazione formattato
    if Rotation.Period > 0 then
    begin
      EditRotationPeriod_hhmmss.Text := FormatDateTime('hh:nn:ss', Rotation.Period / SecsPerDay);
      EditRotationPeriod_s.Text := FloatToStr(RoundTo(Rotation.Period, -2));
    end
    else
      EditRotationPeriod_hhmmss.Text := 'N/A';

  except
    on E: Exception do
    begin
      // In caso di errore (es. coordinate non valide), puliamo i campi o mostriamo errore
      EditAngularVelocity.Text := 'Error';
    end;
  end;
end;


procedure TFormOM1OM2Center.CopyOnRightClickMouseDown(Sender: TObject;  // BUG
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
  begin
    if Sender is TEdit then
    begin
      TEdit(Sender).SelectAll;
      TEdit(Sender).CopyToClipboard;
    end
    else if Sender is TSpinEdit then
    begin
      TSpinEdit(Sender).SelectAll;
      TSpinEdit(Sender).CopyToClipboard;
    end;
  end;
end;



procedure TFormOM1OM2Center.DateTimePickerOM5Time_1Change(Sender: TObject);
begin
  if FIsUpdating then Exit;
  FIsUpdating := True;
  try
    // Formattazione manuale per garantire il formato standard ctime
    // Default format: "ddd mmm dd hh:nn:ss yyyy"
    EditOM5Time_1.Text := FormatDateTime('ddd mmm dd hh:nn:ss yyyy', DateTimePickerOM5Time_1.DateTime, GDefaultFormatSettings);
  finally
    FIsUpdating := False;
  end;

  OM5ValuesUpdate(Sender);
end;


procedure TFormOM1OM2Center.DateTimePickerOM5Time_2Change(Sender: TObject);
begin
  if FIsUpdating then Exit;
  FIsUpdating := True;
  try
    EditOM5Time_2.Text := FormatDateTime('ddd mmm dd hh:nn:ss yyyy', DateTimePickerOM5Time_2.DateTime, GDefaultFormatSettings);
  finally
    FIsUpdating := False;
  end;

  OM5ValuesUpdate(Sender);
end;

procedure TFormOM1OM2Center.EditOM5Time_1Change(Sender: TObject);
var
  ParsedDT: TDateTime;
begin
  if FIsUpdating then Exit;
  try
    ParsedDT := ParseCTime(EditOM5Time_1.Text);
    FIsUpdating := True;
    try
      DateTimePickerOM5Time_1.DateTime := ParsedDT;
    finally
      FIsUpdating := False;
    end;
  except
    // Silent fail durante la digitazione per non interrompere l'utente
  end;

  OM5ValuesUpdate(Sender);
end;

procedure TFormOM1OM2Center.EditOM5Time_2Change(Sender: TObject);
var
  ParsedDT: TDateTime;
begin
  if FIsUpdating then Exit;
  try
    ParsedDT := ParseCTime(EditOM5Time_2.Text);
    FIsUpdating := True;
    try
      DateTimePickerOM5Time_2.DateTime := ParsedDT;
    finally
      FIsUpdating := False;
    end;
  except   
    // Silent fail durante la digitazione per non interrompere l'utente
  end;

  OM5ValuesUpdate(Sender);
end;





procedure TFormOM1OM2Center.FormCreate(Sender: TObject);
begin
  FSystemOriginAxes    := TGPoint3D.Create(0,0,0);
  FOM1                 := TGPoint3D.Create(0,0,0);
  FOM2                 := TGPoint3D.Create(0,0,0);
  FPlanetCenter        := TGPoint3D.Create(0,0,0);
  FOM1OM2Distance      := 0;
  FAverageAltitude     := 0;
  FAveragePlanetRadius := 0;
  FDistanceOC          := 0;
  FColatitude          := TColatitude.Create(0);
  FLatitude            := TLatitude.Create(0);
  FLongitude           := TLongitude.Create(0);

  FIsUpdating          := False;

  HintInit;   

  DateTimePickerOM5Time_1.DateTime := Now;
  DateTimePickerOM5Time_2.DateTime := Now;    
  EditOM5Time_1.Text := FormatDateTime('ddd mmm dd hh:nn:ss yyyy', DateTimePickerOM5Time_1.DateTime, GDefaultFormatSettings);
  EditOM5Time_2.Text := FormatDateTime('ddd mmm dd hh:nn:ss yyyy', DateTimePickerOM5Time_2.DateTime, GDefaultFormatSettings);
end;




initialization


end.

