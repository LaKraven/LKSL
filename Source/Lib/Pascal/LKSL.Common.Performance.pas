{
  LaKraven Studios Standard Library [LKSL]
  Copyright (c) 2014-2015, LaKraven Studios Ltd, All Rights Reserved

  Original Source Location: https://github.com/LaKraven/LKSL

  License:
    - You may use this library as you see fit, including use within commercial applications.
    - You may modify this library to suit your needs, without the requirement of distributing
      modified versions.
    - You may redistribute this library (in part or whole) individually, or as part of any
      other works.
    - You must NOT charge a fee for the distribution of this library (compiled or in its
      source form). It MUST be distributed freely.
    - This license and the surrounding comment block MUST remain in place on all copies and
      modified versions of this source code.
    - Modified versions of this source MUST be clearly marked, including the name of the
      person(s) and/or organization(s) responsible for the changes, and a SEPARATE "changelog"
      detailing all additions/deletions/modifications made.

  Disclaimer:
    - Your use of this source constitutes your understanding and acceptance of this
      disclaimer.
    - LaKraven Studios Ltd and its employees (including but not limited to directors,
      programmers and clerical staff) cannot be held liable for your use of this source
      code. This includes any losses and/or damages resulting from your use of this source
      code, be they physical, financial, or psychological.
    - There is no warranty or guarantee (implicit or otherwise) provided with this source
      code. It is provided on an "AS-IS" basis.

  Donations:
    - While not mandatory, contributions are always appreciated. They help keep the coffee
      flowing during the long hours invested in this and all other Open Source projects we
      produce.
    - Donations can be made via PayPal to PayPal [at] LaKraven (dot) Com
                                          ^  Garbled to prevent spam!  ^
}
unit LKSL.Common.Performance;

{$I LKSL.inc}

{
  About this unit:
    - This unit provides tools for Performance Metrics and is used throughout the LKSL.
}

interface

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs,
  {$ELSE}
    Classes, SysUtils, SyncObjs,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types,
  LKSL.Generics.Collections;

  {$I LKSL_RTTI.inc}

type
  { Forward Declarations }
  TLKPerformanceCounter = class;

  ///  <summary><c>Keeps track of Performance both Instant and Average, in units of Things Per Second.</c></summary>
  ///  <remarks>
  ///    <para><c>Note that this does NOT operate like a "Stopwatch", it merely takes the given Time Difference (Delta) Values to calculate smooth averages.</c></para>
  ///  </remarks>
  TLKPerformanceCounter = class(TLKPersistent)
  private
    FAverageOver: Cardinal;
    FAverageRate: LKFloat;
    FInstantRate: LKFloat;

    ///  <summary><c>A humble Dynamic Array, fixed to the specified "Average Over" value, containing each Sample used to determine the Average Rate.</c></summary>
    FSamples: Array of LKFloat;
    ///  <summary><c>The number of Samples currently being held. This will reach the "Average Over" value and stay there (unless the "Average Over" value changes)</c></summary>
    FSampleCount: Cardinal;
    ///  <summary><c>The Index of the NEXT Sample to be stored. This simply rolls around from 0 to N, replacing each oldest value.</c></summary>
    FSampleIndex: Integer;

    { Getters }
    function GetAverageOver: Cardinal;
    function GetAverageRate: LKFloat;
    function GetInstantRate: LKFloat;

    { Setters }
    procedure SetAverageOver(const AAverageOver: Cardinal = 10);
  public
    constructor Create(const AAverageOver: Cardinal); reintroduce;

    procedure AfterConstruction; override;

    procedure RecordSample(const AValue: LKFloat);
    procedure Reset;

    ///  <summary><c>The number of Samples over which to calculate the Average</c></summary>
    property AverageOver: Cardinal read GetAverageOver write SetAverageOver;
    ///  <summary><c>The Average Rate (based on the number of Samples over which to calculate it)</c></summary>
    property AverageRate: LKFloat read GetAverageRate;
    ///  <summary><c>The Instant Rate (based only on the last given Sample)</c></summary>
    property InstantRate: LKFloat read GetInstantRate;
  end;

implementation

{ TLKPerformanceCounter }

procedure TLKPerformanceCounter.AfterConstruction;
begin
  inherited;
  Reset;
end;

constructor TLKPerformanceCounter.Create(const AAverageOver: Cardinal);
begin
  inherited Create;
  FAverageOver := AAverageOver;
end;

function TLKPerformanceCounter.GetAverageOver: Cardinal;
begin
  Lock;
  try
    Result := FAverageOver;
  finally
    Unlock;
  end;
end;

function TLKPerformanceCounter.GetAverageRate: LKFloat;
begin
  Lock;
  try
    Result := FAverageRate;
  finally
    Unlock;
  end;
end;

function TLKPerformanceCounter.GetInstantRate: LKFloat;
begin
  Lock;
  try
    Result := FInstantRate;
  finally
    Unlock;
  end;
end;

procedure TLKPerformanceCounter.RecordSample(const AValue: LKFloat);
var
  I: Integer;
  LTotal: LKFloat;
begin
  Lock;
  try
    if AValue > 0 then // Can't divide by 0
    begin
      FInstantRate := 1 / AValue; // Calculate Instant Rate
      FSamples[FSampleIndex] := AValue; // Add this Sample
      Inc(FSampleIndex); // Increment the Sample Index
      if FSampleIndex > High(FSamples) then // If the next sample would be Out Of Bounds...
        FSampleIndex := 0; // ... roll back around to index 0
      if FSampleCount < FAverageOver then // If we haven't yet recorded the desired number of Samples...
        Inc(FSampleCount); // .. increment the Sample Count

      // Now we'll calculate the Average
      LTotal := 0;
      for I := 0 to FSampleCount - 1 do
        LTotal := LTotal + FSamples[I];
      if LTotal > 0 then // Can't divide by 0
        FAverageRate := 1 / (LTotal / FSampleCount);
    end;
  finally
    Unlock;
  end;
end;

procedure TLKPerformanceCounter.Reset;
begin
  SetLength(FSamples, FAverageOver);
  FSampleCount := 0;
  FSampleIndex := 0;
  FInstantRate := 0;
  FAverageRate := 0;
end;

procedure TLKPerformanceCounter.SetAverageOver(const AAverageOver: Cardinal);
begin
  Lock;
  try
    FAverageOver := AAverageOver;
    Reset;
  finally
    Unlock;
  end;
end;

end.
