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


{*
  Provides tools for calculating and managing cargo crates divided by size.

  Currently, the sizes available in SCU are: 32, 24, 16, 8, 4, 2, 1.

  (SCU - Standard Cargo Unit)

  1 SCU @html(&equiv;) 1.25m x 1.25m x 1.25m = 1.953125m@html(<sup>3</sup>)
}
unit SCUxSizeUnit;

{$mode ObjFPC}{$H+}{$J-}{$R+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Dialogs;



type
  {* Type alias for representing sizes in Standard Cargo Units (SCU). }
  TSCU = Double;
const
  {* Constant used in calculations relating to the dimensions of crates. }
  SCU = 1.25 * 1.25 * 1.25;


type
  {* Enumeration for the size of Cargo Crates. }
  TSCUSize = (eSCUSize32, eSCUSize24, eSCUSize16, eSCUSize08, eSCUSize04, eSCUSize02, eSCUSize01);


type
  {* Type of utility for the constant @link(SCUSizeArray). }
  TSCUSizeArray = array[TSCUSize] of Integer;
const
  {* Array that maps @link(TSCUSize) enumeration values to their corresponding size in SCU. }
  SCUSizeArray: TSCUSizeArray = (32, 24, 16, 8, 4, 2, 1);


type
  {* Type of utility for the constant @link(SCUSizeIntArray). }
  TSCUSizeIntArray = array[0..6] of Integer;
const
  {* Constant used in calculations relating to the dimensions of crates. }
  SCUSizeIntArray: TSCUSizeIntArray = (32, 24, 16, 8, 4, 2, 1);


const
  {* Maximum size limit for crates. }
  SCUSizeDefault: TSCUSize = eSCUSize32;   
  {* Maximum size limit for crates. }
  SCUSizeIntDefault: Integer = 32;






type
  {* Record to keep track of the number of boxes for each SCU size.

    All properties should be >= 0.
  }
  TSCUxSizeRecord = record
    private
      FSCUSize32: Integer;
      FSCUSize24: Integer;
      FSCUSize16: Integer;
      FSCUSize08: Integer;
      FSCUSize04: Integer;
      FSCUSize02: Integer;
      FSCUSize01: Integer;

      FTotalSCU: Integer;

      {* Recalculate the total when a field is modified @code(SCUSizeN). }
      procedure RecalculateTotalSCU;

      // Writing methods (Setters) for each SCUSizeN property
      {* @raises(EArgumentOutOfRangeException if the value is < 0) }
      procedure SetSCUSize32(const Value: Integer);

      {* @raises(EArgumentOutOfRangeException if the value is < 0) }
      procedure SetSCUSize24(const Value: Integer);

      {* @raises(EArgumentOutOfRangeException if the value is < 0) }
      procedure SetSCUSize16(const Value: Integer);

      {* @raises(EArgumentOutOfRangeException if the value is < 0) }
      procedure SetSCUSize08(const Value: Integer);

      {* @raises(EArgumentOutOfRangeException if the value is < 0) }
      procedure SetSCUSize04(const Value: Integer);

      {* @raises(EArgumentOutOfRangeException if the value is < 0) }
      procedure SetSCUSize02(const Value: Integer);

      {* @raises(EArgumentOutOfRangeException if the value is < 0) }
      procedure SetSCUSize01(const Value: Integer);

      {* @raises(EArgumentOutOfRangeException if the value is not in (32, 24, 16, 8, 4, 2, 1)) }
      procedure SetTotalSCU(const InputSCU: Integer);

    public
      // Properties read from fields, but write via Setters to trigger recalculation.
      property SCUSize32: Integer read FSCUSize32 write SetSCUSize32; //* >= 0
      property SCUSize24: Integer read FSCUSize24 write SetSCUSize24; //* >= 0
      property SCUSize16: Integer read FSCUSize16 write SetSCUSize16; //* >= 0
      property SCUSize08: Integer read FSCUSize08 write SetSCUSize08; //* >= 0
      property SCUSize04: Integer read FSCUSize04 write SetSCUSize04; //* >= 0
      property SCUSize02: Integer read FSCUSize02 write SetSCUSize02; //* >= 0
      property SCUSize01: Integer read FSCUSize01 write SetSCUSize01; //* >= 0

      {* Total given by the sum of the products of the number of boxes and their respective sizes. }
      property TotalSCU: Integer read FTotalSCU; //* >= 0

      {* Overload of the sum operator between two @link(TSCUxSizeRecord) }
      class operator +(const ALeft: TSCUxSizeRecord; const ARight: TSCUxSizeRecord): TSCUxSizeRecord;
  end;


  {* Static helper for TSCUxSizeRecord. }
  { TSCUxSizeRecordHelper }
  TSCUxSizeRecordHelper = record helper for TSCUxSizeRecord
      public
        {* Returns the integer value associated with a TSCUSize (e.g. eSCUSize32 -> 32). }
        class function GetIntValue(const ASize: TSCUSize): Integer; static;

        {* Returns the TSCUSize associated with an Integer (e.g. 32 -> eSCUSize32). Generates an error if the input is invalid. }
        class function GetSizeValue(const AValue: Integer): TSCUSize; static;

        {* Returns True if an Integer has an associated TSCUSize, False otherwise. }
        class function IsValidSizeValue(const AValue: Integer): Boolean; static;

        {* Returns a @link(TSCUxSizeRecord) initialised with the number of crates needed to reach @code(InputSCU),
            limited to sizes less than or equal to @code(MaxSize). }
        class function GetSCUxSizeFromSCU(const InputSCU: Integer; const MaxSize: Integer): TSCUxSizeRecord; static; overload;

        {* Returns a @link(TSCUxSizeRecord) initialised with the number of crates needed to reach @code(InputSCU),
            limited to sizes less than or equal to @code(MaxSize). }
        class function GetSCUxSizeFromSCU(const InputSCU: Integer; const MaxSize: TSCUSize): TSCUxSizeRecord; static; overload;

        {* Initialise the @link(TSCUxSizeRecord) with the number of crates needed to reach @code(InputSCU),
            limited to sizes less than or equal to @code(MaxSize). }
        class procedure SetSCUxSizeFromSCU(const InputSCU: Integer; const MaxSize: Integer; var ASCUxSize: TSCUxSizeRecord); static; overload;

        {* Initialise the @link(TSCUxSizeRecord) with the number of crates needed to reach @code(InputSCU),
            limited to sizes less than or equal to @code(MaxSize). }
        class procedure SetSCUxSizeFromSCU(const InputSCU: Integer; const MaxSize: TSCUSize; var ASCUxSize: TSCUxSizeRecord); static; overload;
  end;





implementation

{ TSCUxSizeRecord }

class operator TSCUxSizeRecord.+(const ALeft: TSCUxSizeRecord; const ARight: TSCUxSizeRecord): TSCUxSizeRecord;
function SafeAdd(const A, B: Integer): Integer;
  var
    Total: Int64;
  begin
    Total := Int64(A) + Int64(B);
    if Total < MaxInt then
      Result := Total
    else
      Result := MaxInt;
  end;
begin
  Result := default(TSCUxSizeRecord);

  Result.FSCUSize32 := SafeAdd(ALeft.FSCUSize32, ARight.FSCUSize32);
  Result.FSCUSize24 := SafeAdd(ALeft.FSCUSize24, ARight.FSCUSize24);
  Result.FSCUSize16 := SafeAdd(ALeft.FSCUSize16, ARight.FSCUSize16);
  Result.FSCUSize08 := SafeAdd(ALeft.FSCUSize08, ARight.FSCUSize08);
  Result.FSCUSize04 := SafeAdd(ALeft.FSCUSize04, ARight.FSCUSize04);
  Result.FSCUSize02 := SafeAdd(ALeft.FSCUSize02, ARight.FSCUSize02);
  Result.FSCUSize01 := SafeAdd(ALeft.FSCUSize01, ARight.FSCUSize01);

  Result.RecalculateTotalSCU;
end;


procedure TSCUxSizeRecord.RecalculateTotalSCU;
var
  TotalCheck: Int64;
begin
  TotalCheck := 0;

  TotalCheck := TotalCheck + (Int64(FSCUSize32) * 32);
  TotalCheck := TotalCheck + (Int64(FSCUSize24) * 24);
  TotalCheck := TotalCheck + (Int64(FSCUSize16) * 16);
  TotalCheck := TotalCheck + (Int64(FSCUSize08) * 8);
  TotalCheck := TotalCheck + (Int64(FSCUSize04) * 4);
  TotalCheck := TotalCheck + (Int64(FSCUSize02) * 2);
  TotalCheck := TotalCheck +  Int64(FSCUSize01); // * 1

  if TotalCheck > MaxInt then
    FTotalSCU := MaxInt
  else
    FTotalSCU := TotalCheck;
end;


procedure TSCUxSizeRecord.SetSCUSize32(const Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateFmt(
      'SCUSize32 must be non-negative. Got: %d', [Value]
    );

  FSCUSize32 := Value;
  RecalculateTotalSCU;
end;

procedure TSCUxSizeRecord.SetSCUSize24(const Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateFmt(
      'SCUSize24 must be non-negative. Got: %d', [Value]
    );
  FSCUSize24 := Value;
  RecalculateTotalSCU;
end;

procedure TSCUxSizeRecord.SetSCUSize16(const Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateFmt(
      'SCUSize16 must be non-negative. Got: %d', [Value]
    );
  FSCUSize16 := Value;
  RecalculateTotalSCU;
end;

procedure TSCUxSizeRecord.SetSCUSize08(const Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateFmt(
      'SCUSize08 must be non-negative. Got: %d', [Value]
    );
  FSCUSize08 := Value;
  RecalculateTotalSCU;
end;

procedure TSCUxSizeRecord.SetSCUSize04(const Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateFmt(
      'SCUSize04 must be non-negative. Got: %d', [Value]
    );
  FSCUSize04 := Value;
  RecalculateTotalSCU;
end;

procedure TSCUxSizeRecord.SetSCUSize02(const Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateFmt(
      'SCUSize02 must be non-negative. Got: %d', [Value]
    );
  FSCUSize02 := Value;
  RecalculateTotalSCU;
end;

procedure TSCUxSizeRecord.SetSCUSize01(const Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateFmt(
      'SCUSize01 must be non-negative. Got: %d', [Value]
    );
  FSCUSize01 := Value;
  RecalculateTotalSCU;
end;


procedure TSCUxSizeRecord.SetTotalSCU(const InputSCU: Integer);
begin
  if InputSCU < 0 then
    raise EArgumentOutOfRangeException.CreateFmt(
      'InputSCU must be non-negative. Got: %d', [InputSCU]
    );

  TSCUxSizeRecord.SetSCUxSizeFromSCU(InputSCU, SCUSizeIntDefault, Self);
end;


{ TSCUxSizeHelper }

class function TSCUxSizeRecordHelper.GetIntValue(const ASize: TSCUSize): Integer;
begin
  Result := SCUSizeArray[ASize];
end;


class function TSCUxSizeRecordHelper.GetSizeValue(const AValue: Integer): TSCUSize;
begin
  case AValue of
    32: Result := eSCUSize32;
    24: Result := eSCUSize24;
    16: Result := eSCUSize16;
     8: Result := eSCUSize08;
     4: Result := eSCUSize04;
     2: Result := eSCUSize02;
     1: Result := eSCUSize01;
  else
    raise EArgumentOutOfRangeException.CreateFmt(
      'AValue can only be one of these values: 32, 24, 16, 8, 4, 2, 1. Got: %d', [AValue]
    );
  end;
end;


class function TSCUxSizeRecordHelper.IsValidSizeValue(const AValue: Integer): Boolean;
begin
  // True for 1,2,4,8,16,24,32
  Result := (AValue = 24) or (((AValue and (AValue - 1)) = 0) and (AValue <= 32) and (AValue > 0));
end;


class function TSCUxSizeRecordHelper.GetSCUxSizeFromSCU(const InputSCU: Integer; const MaxSize: Integer): TSCUxSizeRecord;
var
  ASCUxSize: TSCUxSizeRecord;
begin
  ASCUxSize := default(TSCUxSizeRecord);
  SetSCUxSizeFromSCU(InputSCU, MaxSize, ASCUxSize);
  Result := ASCUxSize;
end;


class function TSCUxSizeRecordHelper.GetSCUxSizeFromSCU(const InputSCU: Integer; const MaxSize: TSCUSize): TSCUxSizeRecord;
var
  ASCUxSize: TSCUxSizeRecord;
begin
  ASCUxSize := default(TSCUxSizeRecord);
  SetSCUxSizeFromSCU(InputSCU, MaxSize, ASCUxSize);
  Result := ASCUxSize;
end;


class procedure TSCUxSizeRecordHelper.SetSCUxSizeFromSCU(const InputSCU: Integer; const MaxSize: Integer; var ASCUxSize: TSCUxSizeRecord);
var
  RemainingSCU: Integer;
begin
  ASCUxSize := default(TSCUxSizeRecord);
  RemainingSCU := InputSCU;

  if MaxSize >= 32 then begin
    ASCUxSize.FSCUSize32 := RemainingSCU div 32;
    RemainingSCU := RemainingSCU mod 32;
  end;

  if MaxSize >= 24 then begin
    ASCUxSize.FSCUSize24 := RemainingSCU div 24;
    RemainingSCU := RemainingSCU mod 24;
  end;

  if MaxSize >= 16 then begin
    ASCUxSize.FSCUSize16 := RemainingSCU div 16;
    RemainingSCU := RemainingSCU mod 16;
  end;           

  if MaxSize >= 8 then begin
    ASCUxSize.FSCUSize08 := RemainingSCU div 8;
    RemainingSCU := RemainingSCU mod 8;
  end;

  if MaxSize >= 4 then begin
    ASCUxSize.FSCUSize04 := RemainingSCU div 4;
    RemainingSCU := RemainingSCU mod 4;
  end;

  if MaxSize >= 2 then begin
    ASCUxSize.FSCUSize02 := RemainingSCU div 2;
    RemainingSCU := RemainingSCU mod 2;
  end;

  ASCUxSize.FSCUSize01 := RemainingSCU;


  ASCUxSize.RecalculateTotalSCU;
end;


class procedure TSCUxSizeRecordHelper.SetSCUxSizeFromSCU(const InputSCU: Integer; const MaxSize: TSCUSize; var ASCUxSize: TSCUxSizeRecord);
begin
  SetSCUxSizeFromSCU(InputSCU, SCUSizeArray[MaxSize], ASCUxSize);
end;

end.

