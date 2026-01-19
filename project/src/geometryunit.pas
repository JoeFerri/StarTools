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
unit GeometryUnit;

{$mode ObjFPC}{$H+}{$J-}{$R+}
{$modeswitch advancedrecords}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, Math, DateUtils;


type
  TRadiants = record
    public
      Value: Double; //* assume che l'angolo sia compreso tra -2π e 2π

      constructor Create(const AValue: Double); overload;
  end;

  TDegrees = record
    public
      Value: Double; //* assume che l'angolo sia compreso tra -360° e 360°

      constructor Create(const AValue: Double); overload;
  end;

  TRadiantsHelper = type helper for TRadiants
    private
      class function GetEpsilon: TRadiants; static;
      class procedure SetEpsilon(AEpsilon: TRadiants); static;
    public
      class property Epsilon: TRadiants read GetEpsilon write SetEpsilon;

      function ToDegrees: TDegrees;
      function SameValue(const AValue: TRadiants; const AEpsilon: TRadiants): Boolean; overload;
      function SameValue(const AValue: TRadiants): Boolean; overload;
      function ToString(const ADigits: Integer = 12): String;
      function TofString(const ADigits: Integer = 12): String;

      constructor Create(const ADegrees: TDegrees); overload;
  end;


  TDegreesHelper = type helper for TDegrees
    function ToRadians: TRadiants;
    function ToString(const ADigits: Integer = 2): String;
    function TofString(const ADigits: Integer = 2): String;
    
    constructor Create(const ARadiants: TRadiants); overload;
  end;


  TColatitude = TRadiants;   // in radians
  TLatitude   = TRadiants;   // in radians
  TLongitude  = TRadiants;   // in radians
  TDistance   = Double;      // in meters


  TRotationResult = record
    Omega: Double;      // rad/s
    Period: Double;     // secondi
    DegreesPerSec: Double;
  end;


  TGPoint3D = record
    X, Y, Z: Double;
    public
      class operator +(const A, B: TGPoint3D): TGPoint3D;
      class operator -(const A, B: TGPoint3D): TGPoint3D;

      constructor Create(const AX, AY, AZ: Double);
  end;

  TGPoint3DHelper = record helper for TGPoint3D
    function DistanceTo(const APoint: TGPoint3D): TDistance;
    function Colatitude(const ADistance: TDistance): TColatitude;
    function Latitude(const ADistance: TDistance): TLatitude; overload;
    function Latitude(): TLatitude; overload;
    function Longitude(): TLongitude;

    function Magnitude: Double;
    function DotProduct(const V: TGPoint3D): Double;
    function AngleTo(const V: TGPoint3D): TRadiants;

    class function CoToLatitude(const AColatitude: TColatitude): TLatitude; static;

    class function CalculateRotation(const P1, P2, Center: TGPoint3D; const T1, T2: TDateTime): TRotationResult; static;
    function ToString(const ADigits: Integer = 4): String;
  end;

  TGSphere = record
    Center: TGPoint3D;
    Radius: Double;
  end;





  function ParseCTime(const ATimeStr: String): TDateTime;





const
  GPoint3D0: TGPoint3D = (X: 0; Y: 0; Z: 0);


var
  GDefaultFormatSettings : TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'd/m/y';
    LongDateFormat: 'dd" "mmmm" "yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );



implementation

var 
  _RadiantsEpsilon: TRadiants;



{ TRadiants }

constructor TRadiants.Create(const AValue: Double);
begin
  Value := AValue;
end;


{ TRadiantsHelper }


constructor TRadiantsHelper.Create(const ADegrees: TDegrees);
begin
  Value := ADegrees.Value * Pi / 180;
end;


function TRadiantsHelper.ToDegrees: TDegrees;
begin
  Result := TDegrees.Create(Self);
end;


function TRadiantsHelper.SameValue(const AValue: TRadiants; const AEpsilon: TRadiants): Boolean;
begin
  Result := math.SameValue(Self.Value, AValue.Value, AEpsilon.Value);
end;

function TRadiantsHelper.SameValue(const AValue: TRadiants): Boolean;
begin
  Result := math.SameValue(Self.Value, AValue.Value, _RadiantsEpsilon.Value);
end;


function TRadiantsHelper.ToString(const ADigits: Integer = 12): String;
begin
  Result := Format('%.' + IntToStr(ADigits) + 'f', [Self.Value]);
end;


class function TRadiantsHelper.GetEpsilon: TRadiants;
begin
  Result := _RadiantsEpsilon;
end;

class procedure TRadiantsHelper.SetEpsilon(AEpsilon: TRadiants); 
begin
  _RadiantsEpsilon := AEpsilon;
end;

function TRadiantsHelper.TofString(const ADigits: Integer = 12): String;
var
  Factor: Double;
  Numerator, Denom: Integer;
  BestDenom: Integer;
  Found: Boolean;
  i: Integer;
  const Denominators: array[0..4] of Integer = (1, 2, 3, 4, 6);
begin
  Found := False;
  BestDenom := 1;
  Factor := Self.Value / Pi;

  for i := 0 to High(Denominators) do
  begin
    Denom := Denominators[i];
    Numerator := Round(Factor * Denom);
    if math.SameValue(Factor * Denom, Numerator, _RadiantsEpsilon.Value) then
    begin
      BestDenom := Denom;
      Found := True;
      Break;
    end;
  end;

  if not Found then
    Exit(Self.ToString(ADigits));

  Numerator := Round(Factor * BestDenom);

  if Numerator = 0 then
    Exit('0');

  Result := '';

  if Numerator < 0 then
    Result := '-';

  if Abs(Numerator) = 1 then
    Result := Result + 'π'
  else
    Result := Result + IntToStr(Abs(Numerator)) + 'π';

  if BestDenom > 1 then
    Result := Result + '/' + IntToStr(BestDenom);
end;


{ TDegrees }

constructor TDegrees.Create(const AValue: Double);
begin
  Value := AValue;
end;



{ TDegreesHelper }


constructor TDegreesHelper.Create(const ARadiants: TRadiants);
begin
  Value := ARadiants.Value * 180 / Pi;
end;


function TDegreesHelper.ToRadians: TRadiants;
begin
  Result := TRadiants.Create(Self);
end;


function TDegreesHelper.ToString(const ADigits: Integer = 2): String;
begin
  Result := Format('%.' + IntToStr(ADigits) + 'f', [Self.Value]);
end;


function TDegreesHelper.TofString(const ADigits: Integer = 2): String;
begin
  Result := Format('%.' + IntToStr(ADigits) + 'f °', [Self.Value]);
end;



{ TGPoint3DHelper }

class operator TGPoint3D.+(const A, B: TGPoint3D): TGPoint3D;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;


class operator TGPoint3D.-(const A, B: TGPoint3D): TGPoint3D;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;


function TGPoint3DHelper.DistanceTo(const APoint: TGPoint3D): TDistance;
begin
  Result := Sqrt(Sqr(APoint.X - Self.X) + Sqr(APoint.Y - Self.Y) + Sqr(APoint.Z - Self.Z));
end;


function TGPoint3DHelper.Colatitude(const ADistance: TDistance): TColatitude;
begin
  Result := TColatitude.Create(0);
  if ADistance = 0 then
    Result := TColatitude.Create(Pi/2)
  else
    Result := TColatitude.Create(ArcCos(Self.Z / ADistance));
end;


function TGPoint3DHelper.Latitude(const ADistance: TDistance): TLatitude;
begin
  Result := TGPoint3D.CoToLatitude(Self.Colatitude(ADistance));
end;


function TGPoint3DHelper.Latitude: TLatitude;
begin
  Result := TGPoint3D.CoToLatitude(Self.Colatitude(Self.DistanceTo(GPoint3D0)));
end;


class function TGPoint3DHelper.CoToLatitude(const AColatitude: TColatitude): TLatitude;
begin
  Result := TLatitude.Create((Pi/2) - AColatitude.Value);
end;


function TGPoint3DHelper.Longitude: TLongitude;
begin
  Result := TLongitude.Create(0);
  if Self.Y <> 0 then
    Result := TLongitude.Create(ArcTan2(Self.Y, Self.X));
end;


function TGPoint3DHelper.ToString(const ADigits: Integer = 4): String;
begin
  Result := Format('%.' + IntToStr(ADigits) + 'f,%.' + IntToStr(ADigits) + 'f,%.' + IntToStr(ADigits) + 'f', [X, Y, Z]);
end;


{ TGPoint3DHelper rotation and angle calculations }

function TGPoint3DHelper.Magnitude: Double;
begin
  Result := Sqrt(Sqr(Self.X) + Sqr(Self.Y) + Sqr(Self.Z));
end;



function TGPoint3DHelper.DotProduct(const V: TGPoint3D): Double;
begin
  Result := (Self.X * V.X) + (Self.Y * V.Y) + (Self.Z * V.Z);
end;



function TGPoint3DHelper.AngleTo(const V: TGPoint3D): TRadiants;
var
  Dot, MagProd: Double;
begin
  Result := TRadiants.Create(0);
  MagProd := Self.Magnitude * V.Magnitude;
  if MagProd < 1e-9 then Exit;
  
  Dot := Self.DotProduct(V) / MagProd;
  
  // Clamp for floating point precision errors
  if Dot > 1.0 then Dot := 1.0 else if Dot < -1.0 then Dot := -1.0;
  
  Result := TRadiants.Create(ArcCos(Dot));
end;



{ Global Rotation Calculation Function }

class function TGPoint3DHelper.CalculateRotation(const P1, P2, Center: TGPoint3D; const T1, T2: TDateTime): TRotationResult;
var
  V1, V2: TGPoint3D;
  DeltaTheta: TRadiants;
  DeltaT: Double; // Seconds
begin
  Result := Default(TRotationResult);
  // Vectors relative to the center of the planet
  V1 := P1 - Center;
  V2 := P2 - Center;
  
  // DeltaT in seconds (TDateTime is in days)
  DeltaT := Abs(T2 - T1) * SecsPerDay;//86400;
  
  if DeltaT = 0.1 then 
  begin
    Result.Omega := 0;
    Exit;
  end;

  // Calculation of the angle between the two vectors
  DeltaTheta := V1.AngleTo(V2);
  
  Result.Omega := DeltaTheta.Value / DeltaT;
  Result.DegreesPerSec := Result.Omega * (180 / Pi);
  
  if Result.Omega <> 0 then
    Result.Period := (2 * Pi) / Result.Omega
  else
    Result.Period := 0;
end;


{ Star Citizen Time Parsing Utility }
function ParseCTime(const ATimeStr: String): TDateTime;
var
  MonthStr: String;
  y, m, d, hh, nn, ss: Word;
  const MonthNames: array[1..12] of string = 
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
begin
  { Formato SC: "Sun Jan 18 09:54:01 2026" }
  { index:       123 567 9012345678901234 }
  
  MonthStr := Copy(ATimeStr, 5, 3);
  m := 0;
  for m := 1 to 12 do
    if MonthNames[m] = MonthStr then Break;

  d  := StrToIntDef(Copy(ATimeStr, 9, 2), 1);
  hh := StrToIntDef(Copy(ATimeStr, 12, 2), 0);
  nn := StrToIntDef(Copy(ATimeStr, 15, 2), 0);
  ss := StrToIntDef(Copy(ATimeStr, 18, 2), 0);
  y  := StrToIntDef(Copy(ATimeStr, 21, 4), YearOf(Now));

  if (m = 0) or (d = 0) or (hh = 0) or (nn = 0) or (ss = 0) or (y = 0) then
  begin
    Result := 0;
    Exit;
  end;

  Result := EncodeDateTime(y, m, d, hh, nn, ss, 0);
end;


constructor TGPoint3D.Create(const AX, AY, AZ: Double);
begin
  X := AX;
  Y := AY;
  Z := AZ;
end;




initialization
_RadiantsEpsilon := TRadiants.Create(0.00001);




end.

