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
unit LKSL.Threads.Base;

{
  About this unit:
    - This unit provides type declarations required for our "High Precision Threads"
    - This unit also provides the Abstract Base Implementation for the same.
}

interface

{$I LKSL.inc}

{$IFDEF FPC}
  {$IFDEF LKSL_MODE_FPC}
    {$mode objfpc}{$H+}
  {$ELSE}
    {$mode delphi}
  {$ENDIF LKSL_MODE_FPC}
{$ENDIF FPC}

uses
    {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
      {$IFDEF POSIX}Posix.Unistd,{$ENDIF} System.Classes, System.SysUtils, System.Diagnostics, System.Math, System.SyncObjs,
    {$ELSE}
      Classes, SysUtils, Math, SyncObjs, {$IFDEF FPC}LKSL.Common.Stopwatch,{$ELSE}Diagnostics,{$ENDIF FPC}
    {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types;

  {$IFNDEF FPC}
    {$I LKSL_RTTI.inc}
  {$ENDIF FPC}

type
  { Forward Declarations }
  TLKThread = class;

  { Enum Types }
  TLKThreadState = (tsRunning, tsPaused);

  {
    TLKThread
      - Abstract Base type for "High Precision Threads"
      - This Thread Type is ALWAYS "Looped" until Termination
      - Provides the Delta Time (time since the previous Tick) and precise Start Time for each "Tick"
      - Tick Rates are defined as the number of "Ticks per Second"
      - Provides integrated Tick Rate Limiting (0 = No Limit)
      - NOT INTENDED FOR USE AS A SINGLE-RUN "WORKER THREAD"

      - All Public Methods and Properties are THREAD-SAFE!
  }
  TLKThread = class abstract(TThread)
  private
    FLock: TLKCriticalSection;
    FNextTickTime: LKFloat;
    FThreadState: TLKThreadState;
    FTickRate: LKFloat; // The INSTANT Tick Rate (in "Ticks per Second")
    FTickRateAverage: LKFloat; // The AVERAGE Tick Rate (in "Ticks per Second")
    FTickRateAverageOver: LKFloat; // How much time (in seconds) the Average is calculated over
    FTickRateDesired: LKFloat; // The DESIRED rate at which you want the Thread to Tick (minimum)
    FTickRateExtra: LKFloat; // The number of extra ticks per second in excess of the DESIRED Tick Rate.
    FTickRateExtraTime: LKFloat; // The number of extra SECONDS available over the DESIRED Tick Rate.
    FTickRateExtraAverage: LKFloat; // Same as "FTickRateExtraTime" but Averaged over "FTickRateAverageOver"
    FTickRateExtraAverageTime: LKFloat; // Same as "FTickRateExtraTime" but Averaged over "FTickRateAverageOver"
    FTickRateLimit: LKFloat; // The current Tick Rate Limit (in "Ticks per Second"), 0 = no limit.
    FYieldAccumulatedTime: Boolean;

    function GetNextTickTime: LKFloat;
    function GetThreadState: TLKThreadState;
    function GetTickRate: LKFloat;
    function GetTickRateAverage: LKFloat;
    function GetTickRateAverageOver: LKFloat;
    function GetTickRateDesired: LKFloat;
    function GetTickRateExtraTicks: LKFloat;
    function GetTickRateExtraTime: LKFloat;
    function GetTickRateExtraTicksAverage: LKFloat;
    function GetTickRateExtraTimeAverage: LKFloat;
    function GetTickRateLimit: LKFloat;
    function GetYieldAccumulatedTime: Boolean;

    procedure SetThreadState(const AThreadState: TLKThreadState);
    procedure SetTickRate(const ATickRate: LKFloat); // Used internally!
    procedure SetTickRateAverage(const ATickRateAverage: LKFloat); // Used internally!
    procedure SetTickRateAverageOver(const AAverageOver: LKFloat);
    procedure SetTickRateDesired(const ADesiredRate: LKFloat);
    procedure SetTickRateExtraTicks(const AExtraTime: LKFloat); // Used internally!
    procedure SetTickRateExtraTicksAverage(const AExtraTimeAverage: LKFloat); // Used internally!
    procedure SetTickRateLimit(const ATickRateLimit: LKFloat);
    procedure SetYieldAccumulatedTime(const AYieldAccumulatedTime: Boolean);
  protected
    // Override "GetDefaultTickRateLimit" if you want to set a default limit (the default is 0 [no limit])
    function GetDefaultTickRateLimit: LKFloat; virtual;
    // Override "GetDefaultTickRateAverageOver" if you want to change the default Tick Rate Averaging Time (default = 2.00 seconds)
    function GetDefaultTickRateAverageOver: LKFloat; virtual;
    // Override "GetDefaultTickRateDesired" if you want to compute EXTRA Time between Ticks.
    // This Extra Time can be used during a Tick to selectively perform additional processing during a Tick.
    // One example in which this is useful is for graphics rendering, where Exta Time can be used for
    // post-processing computations.
    // Default = 0.00, which disables Extra Time Calculation
    function GetDefaultTickRateDesired: LKFloat; virtual;
    // Override "GetDefaultYieldAccumulatedTime" if you DON'T want accumulated (excess) time to be yielded in a single block (Default = True)
    // False = Yield time in small chunks
    // True = Yield all accumulated time in a single block
    function GetDefaultYieldAccumulatedTime: Boolean; virtual;
    // Override "GetInitialThreadState" if you want the Thread to be Paused on construction (the default is Running)
    function GetInitialThreadState: TLKThreadState; virtual;

    function CalculateExtraTime: LKFloat;

    // YOU MUST NOT override the TThread.Execute method in your descendants of TLKThread!!!!!!!!
    procedure Execute; override; final;

    // Override the "PreTick" procedure if your Thread needs to do something on EVERY cycle
    // NOTE: THIS METHOD IGNORES THE TICK RATE LIMIT!
    // "PreTick" is implemented by the Event Handler system (because the Event Queue needs to be processed regardless of the Tick Rate Limit)
    procedure PreTick(const ADelta, AStartTime: LKFloat); virtual;

    // Override the "Tick" procedure to implement your Thread's operational code.
    // ADelta = the time differential ("Delta") between the current Tick and the previous Tick
    // AStartTime = the Reference Time at which the current Tick began.
    // DON'T FORGET "INHERITED;" FIRST!!!!
    procedure Tick(const ADelta, AStartTime: LKFloat); virtual; abstract;
  public
    // Override "Create" to initialize any custom Members and Starting Values (DON'T FORGET "INHERITED;" FIRST!)
    constructor Create; virtual;
    // Override "Destroy" to finalize any custom Members (DON'T FORGET "INHERITED;" LAST!)
    destructor Destroy; override;

    // "Bump" forces the "Next Tick Time" to be "bumped up" to RIGHT NOW, forcing a rate-limited Thread waiting
    // between Ticks to perform a Tick immediately.
    // NOTE: This method ONLY works if "YieldAccumulatedTime" is set to FALSE!
    procedure Bump;

    // "Kill" Terminates the Thread, waits for the thread to be Terminated, then Frees it.
    // It's basically just a way of performing three actions in a single call (sugar-coating, if you like)
    procedure Kill; virtual;

    // "Lock" locks the Thread's global "Critical Section" using a "Spinlock"
    // (Public just in case you need to call it externally for some reason)
    procedure Lock; inline;
    // "Unlock" unlocks the Thread's global "Critical Section"
    // (Public just in case you need to call it externally for some reason)
    procedure Unlock; inline;

    property NextTickTime: LKFloat read GetNextTickTime;
    property ThreadState: TLKThreadState read GetThreadState write SetThreadState;
    property TickRate: LKFloat read GetTickRate;
    property TickRateAverage: LKFloat read GetTickRateAverage;
    property TickRateAverageOver: LKFloat read GetTickRateAverageOver write SetTickRateAverageOver;
    property TickRateDesired: LKFloat read GetTickRateDesired write SetTickRateDesired;
    property TickRateExtraTicks: LKFloat read GetTickRateExtraTicks;
    property TickRateExtraTime: LKFloat read GetTickRateExtraTime;
    property TickRateExtraTicksAverage: LKFloat read GetTickRateExtraTicksAverage;
    property TickRateExtraTimeAverage: LKFloat read GetTickRateExtraTimeAverage;
    property TickRateLimit: LKFloat read GetTickRateLimit write SetTickRateLimit;
    property YieldAccumulatedTime: Boolean read GetYieldAccumulatedTime write SetYieldAccumulatedTime;
  end;

// "GetReferenceTime" returns the current "Reference Time" (which is supremely high resolution)
function GetReferenceTime: LKFloat;

implementation

var
  ReferenceWatch: TStopwatch;

function GetReferenceTime: LKFloat;
begin
  Result := TStopwatch.GetTimeStamp / TStopwatch.Frequency;
end;

{ TLKThread }

procedure TLKThread.Bump;
begin
  Lock;
  try
    FNextTickTime := GetReferenceTime;
  finally
    Unlock;
  end;
end;

function TLKThread.CalculateExtraTime: LKFloat;
begin
  Result := NextTickTime - GetReferenceTime;
end;

constructor TLKThread.Create;
begin
  inherited Create(False);
  FLock := TLKCriticalSection.Create;
  FreeOnTerminate := False;
  FThreadState := GetInitialThreadState;
  FTickRateLimit := GetDefaultTickRateLimit;
  FTickRateAverageOver := GetDefaultTickRateAverageOver;
  FYieldAccumulatedTime := GetDefaultYieldAccumulatedTime;
  FTickRateDesired := GetDefaultTickRateDesired;
end;

destructor TLKThread.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TLKThread.Execute;
var
  {$IFNDEF POSIX}LSleepTime: Integer;{$ENDIF}
  LDelta, LCurrentTime: LKFloat;
  LTickRate, LTickRateLimit, LTickRateDesired, LTickRateAverage: LKFloat;
  LLastAverageCheckpoint, LNextAverageCheckpoint: LKFloat;
  LAverageTicks: Integer;
begin
  LCurrentTime := GetReferenceTime;
  Lock;
  try
  FNextTickTime := LCurrentTime;
  finally
    Unlock;
  end;
  LLastAverageCheckpoint := 0.00;
  LNextAverageCheckpoint := 0.00;
  LTickRate := 0.00;
  LAverageTicks := 0;
  while (not Terminated) do
  begin
    // Read once so we don't have to keep Acquiring the Lock!
    Lock;
    try
      LTickRateLimit := FTickRateLimit;
      LTickRateDesired := FTickRateDesired;
    finally
      Unlock;
    end;
    LCurrentTime := GetReferenceTime;
    LDelta := (LCurrentTime - FNextTickTime);

    // Rate Limiter
    if (LTickRateLimit > 0.00) then
      if (LDelta < ( 1 / LTickRateLimit)) then
        LDelta := (1 / LTickRateLimit);

    // Calculate INSTANT Tick Rate
    if LDelta > 0 then
    begin
      LTickRate := 1 / LDelta; // Calculate the current Tick Rate

      SetTickRate(LTickRate);

      // Calculate EXTRA time
      if LTickRateDesired > 0.00 then
        SetTickRateExtraTicks(LTickRate - LTickRateDesired)
      else
        SetTickRateExtraTicks(0.00);
    end;

    // Call "PreTick"
    PreTick(LDelta, LCurrentTime);

    if ThreadState = tsRunning then
    begin
      // Tick or Wait...
      if ((LCurrentTime >= FNextTickTime) and (LTickRateLimit > 0.00)) or (LTickRateLimit = 0.00) then
      begin

        // Calculate AVEARAGE Tick Rate
        if LCurrentTime >= LNextAverageCheckpoint then
        begin
          LNextAverageCheckpoint := LCurrentTime + TickRateAverageOver;
          LLastAverageCheckpoint := LCurrentTime;
          LAverageTicks := -1;
        end;
        Inc(LAverageTicks);
        if (LCurrentTime - LLastAverageCheckpoint > 0.00) then
        begin
          LTickRateAverage := LAverageTicks / (LCurrentTime - LLastAverageCheckpoint);
          SetTickRateAverage(LTickRateAverage);
          if LTickRateDesired > 0.00 then
            SetTickRateExtraTicksAverage(LTickRateAverage - LTickRateDesired)
          else
            SetTickRateExtraTicksAverage(0.00);
        end else
        begin
          SetTickRateAverage(LTickRate);
        end;
        Lock;
        try
          FNextTickTime := FNextTickTime + LDelta;
        finally
          Unlock;
        end;
        Tick(LDelta, LCurrentTime);
      end else
      begin
        if (not YieldAccumulatedTime) then
          {$IFDEF POSIX}usleep(1){$ELSE}Sleep(1){$ENDIF}
        else
        begin
          {$IFNDEF POSIX}
             // Windows does not support a "Sleep" resolution above 1ms (which sucks)
            LSleepTime := Floor(1000 * (FNextTickTime - LCurrentTime));
            if LSleepTime > 0 then
              TThread.Sleep(LSleepTime);
          {$ELSE}
            // POSIX platforms support higher-resolution Sleep times (yay)
            usleep(Floor((FNextTickTime - LCurrentTime)));
          {$ENDIF}
        end;
      end;
    end else
      TThread.Sleep(1);
  end;
end;

function TLKThread.GetDefaultTickRateLimit: LKFloat;
begin
  Result := 0.00;
end;

function TLKThread.GetDefaultYieldAccumulatedTime: Boolean;
begin
  Result := True;
end;

function TLKThread.GetDefaultTickRateAverageOver: LKFloat;
begin
  Result := 2.00;
end;

function TLKThread.GetDefaultTickRateDesired: LKFloat;
begin
  Result := 0.00;
end;

function TLKThread.GetInitialThreadState: TLKThreadState;
begin
  Result := tsRunning;
end;

function TLKThread.GetNextTickTime: LKFloat;
begin
  Lock;
  try
    Result := FNextTickTime;
  finally
    Unlock;
  end;
end;

function TLKThread.GetThreadState: TLKThreadState;
begin
  Lock;
  try
    Result := FThreadState;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRate: LKFloat;
begin
  Lock;
  try
    Result := FTickRate;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateAverage: LKFloat;
begin
  Lock;
  try
  Result := FTickRateAverage;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateAverageOver: LKFloat;
begin
  Lock;
  try
    Result := FTickRateAverageOver;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateDesired: LKFloat;
begin
  Lock;
  try
    Result := FTickRateDesired;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateExtraTicks: LKFloat;
begin
  Lock;
  try
    Result := FTickRateExtra;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateExtraTicksAverage: LKFloat;
begin
  Lock;
  try
    Result := FTickRateExtraAverage;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateExtraTime: LKFloat;
begin
  Lock;
  try
    Result := FTickRateExtraTime;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateExtraTimeAverage: LKFloat;
begin
  Lock;
  try
    Result := FTickRateExtraAverageTime;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateLimit: LKFloat;
begin
  Lock;
  try
    Result := FTickRateLimit;
  finally
    Unlock;
  end;
end;

function TLKThread.GetYieldAccumulatedTime: Boolean;
begin
  Lock;
  try
    Result := FYieldAccumulatedTime;
  finally
    Unlock;
  end;
end;

procedure TLKThread.Kill;
begin
  Terminate;
  WaitFor;
  Free;
end;

procedure TLKThread.Lock;
begin
  FLock.Acquire;
end;

procedure TLKThread.PreTick(const ADelta, AStartTime: LKFloat);
begin
  // Do nothing by default
end;

procedure TLKThread.SetThreadState(const AThreadState: TLKThreadState);
begin
  Lock;
  try
    FThreadState := AThreadState;
  finally
    Unlock;
  end;
end;

procedure TLKThread.SetTickRate(const ATickRate: LKFloat);
begin
  Lock;
  try
    FTickRate := ATickRate;
  finally
    Unlock;
  end;
end;

procedure TLKThread.SetTickRateAverage(const ATickRateAverage: LKFloat);
begin
  Lock;
  try
    FTickRateAverage := ATickRateAverage;
  finally
    Unlock;
  end;
end;

procedure TLKThread.SetTickRateAverageOver(const AAverageOver: LKFloat);
begin
  Lock;
  try
    FTickRateAverageOver := AAverageOver;
  finally
    Unlock;
  end;
end;

procedure TLKThread.SetTickRateDesired(const ADesiredRate: LKFloat);
begin
  Lock;
  try
    FTickRateDesired := ADesiredRate;
  finally
    Unlock;
  end;
end;

procedure TLKThread.SetTickRateExtraTicks(const AExtraTime: LKFloat);
begin
  Lock;
  try
    FTickRateExtra := AExtraTime;
    if FTickRateExtra > 0.00 then
      FTickRateExtraTime := (1 / FTickRate) * FTickRateExtra
    else if FTickRateExtra < 0.00 then
      FTickRateExtraTime := -((1 / -FTickRate) * FTickRateExtra)
    else
      FTickRateExtraTime := 0.00;
  finally
    Unlock;
  end;
end;

procedure TLKThread.SetTickRateExtraTicksAverage(const AExtraTimeAverage: LKFloat);
begin
  Lock;
  try
    FTickRateExtraAverage := AExtraTimeAverage;
    if FTickRateExtraAverage > 0.00 then
      FTickRateExtraAverageTime := (1 / FTickRateAverage) * FTickRateExtraAverage
    else if FTickRateExtraAverage < 0.00 then
      FTickRateExtraAverageTime := -((1 / -FTickRateAverage) * FTickRateExtraAverage)
    else
      FTickRateExtraAverageTime := 0.00;
  finally
    Unlock;
  end;
end;

procedure TLKThread.SetTickRateLimit(const ATickRateLimit: LKFloat);
begin
  Lock;
  try
    FTickRateLimit := ATickRateLimit;
    // If the Limit is LOWER than the defined "Desired" Rate, then we cannot desire MORE than the limit,
    // so we match the two.
    if (FTickRateLimit > 0) and (FTickRateLimit < FTickRateDesired) then
      FTickRateDesired := FTickRateLimit;
  finally
    Unlock;
  end;
end;

procedure TLKThread.SetYieldAccumulatedTime(const AYieldAccumulatedTime: Boolean);
begin
  Lock;
  try
    FYieldAccumulatedTime := AYieldAccumulatedTime;
  finally
    Unlock;
  end;
end;

procedure TLKThread.Unlock;
begin
  FLock.Release;
end;

initialization
  ReferenceWatch := TStopwatch.Create;

end.
