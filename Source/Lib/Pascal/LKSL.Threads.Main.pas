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
unit LKSL.Threads.Main;

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
    System.Classes, System.SysUtils, System.Diagnostics, System.Math, System.SyncObjs,
  {$ELSE}
    Classes, SysUtils, Math, SyncObjs, {$IFDEF FPC}LKSL.Common.Stopwatch, {$ELSE}Diagnostics,{$ENDIF FPC}
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types, LKSL.Common.SyncObjs, LKSL.Common.Performance;

  {$I LKSL_RTTI.inc}

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
    FInstanceGUID: TGUID;
    FLock: TLKCriticalSection;
    FNextTickTime: LKFloat;
    FPerformance: TLKPerformanceCounter;
    FThreadState: TLKThreadState;
    FTickRateDesired: LKFloat; // The DESIRED rate at which you want the Thread to Tick (minimum)
    FTickRateLimit: LKFloat; // The current Tick Rate Limit (in "Ticks per Second"), 0 = no limit.
    FThrottleInterval: Cardinal; // The current Throttling Interval (in Milliseconds)
    FWakeUp: TEvent;

    { Internal Methods }
    procedure AtomicIncrementNextTickTime(const AIncrementBy: LKFloat); inline;
    procedure InitializeTickVariables(var ACurrentTime, ALastAverageCheckpoint, ANextAverageCheckpoint, ATickRate: LKFloat; var AAverageTicks: Integer);
    procedure AtomicInitializeCycleValues(var ATickRateLimit, ATickRateDesired: LKFloat; var AThrottleInterval: Cardinal); inline;

    { Property Getters }
    function GetNextTickTime: LKFloat;
    function GetThreadState: TLKThreadState;
    function GetTickRate: LKFloat;
    function GetTickRateAverage: LKFloat;
    function GetTickRateAverageOver: Cardinal;
    function GetTickRateDesired: LKFloat;
    function GetTickRateLimit: LKFloat;
    function GetThrottleInterval: Cardinal;

    { Property Setters }
    procedure SetThreadState(const AThreadState: TLKThreadState);
    procedure SetTickRateAverageOver(const AAverageOver: Cardinal);
    procedure SetTickRateDesired(const ADesiredRate: LKFloat);
    procedure SetTickRateLimit(const ATickRateLimit: LKFloat);
    procedure SetThrottleInterval(const AThrottleInterval: Cardinal);
  protected
    ///  <summary><c>Override if you wish your inherited Type to enforce a Tick Rate Limit by Default.</c></summary>
    ///  <remarks>
    ///    <para>0 <c>= No Tick Rate Limit</c></para>
    ///    <para><c>Default = </c>0</para>
    ///  </remarks>
    function GetDefaultTickRateLimit: LKFloat; virtual;
    ///  <summary><c>Override if you wish to change the default Tick Rate Averaging Sample Count.</c></summary>
    ///  <remarks>
    ///    <para><c>Value is in Samples</c></para>
    ///    <para><c>Default = </c>10</para>
    ///  </remarks>
    function GetDefaultTickRateAverageOver: Cardinal; virtual;
    ///  <summary><c>Override if you wish your inherited Type to state a desired Tick Rate by Default.</c></summary>
    ///  <remarks>
    ///    <para>0 <c>= No Desired Rate</c></para>
    ///    <para><c>Default = </c>0</para>
    ///  </remarks>
    function GetDefaultTickRateDesired: LKFloat; virtual;
    ///  <summary><c>Override if you wish to define a different Throttling Interval (period in which to rest the Thread when waiting between Ticks)</c></summary>
    ///  <remarks>
    ///    <para><c>Minimum Value = </c>1</para>
    ///    <para><c>Default = </c>1</para>
    ///    <para><c>Values are in </c>MILLISECONDS</para>
    ///  </remarks>
    function GetDefaultThrottleInterval: Integer; virtual;
    ///  <summary><c>Defines whether the Thread should be Running or Paused upon Construction.</c></summary>
    ///  <remarks><c>Default = </c>tsRunning</remarks>
    function GetInitialThreadState: TLKThreadState; virtual;
    ///  <summary><c>Calculates how much "Extra Time" is available for the current Tick.</c></summary>
    ///  <remarks><c>Could be a negative number of the Thread is performing BELOW the desired rate!</c></remarks>
    function CalculateExtraTime: LKFloat;

    ///  <summary><c>You must NOT override "Execute" on descendants. See </c><see DisplayName="Tick" cref="LKSL.Threads.Main|TLKThread.Tick"/><c> instead!</c></summary>
    procedure Execute; override; final;

    ///  <summary><c>Override to implement code you need your Thread to perform on EVERY cycle (regardless of any Tick Rate Limit).</c></summary>
    ///  <param name="ADelta"><c>The time differential ("Delta") between the current Tick and the previous Tick.</c></param>
    ///  <param name="AStartTime"><c>The Reference Time at which the current Tick began.</c></param>
    ///  <remarks>
    ///    <para><c>Used extensively by the Event Engine.</c></para>
    ///    <para><c>Ignores any Tick Rate Limits.</c></para>
    ///  </remarks>
    procedure PreTick(const ADelta, AStartTime: LKFloat); virtual;
    ///  <summary><c>Override to implement your Thread's operational code.</c></summary>
    ///  <param name="ADelta"><c>The time differential ("Delta") between the current Tick and the previous Tick.</c></param>
    ///  <param name="AStartTime"><c>The Reference Time at which the current Tick began.</c></param>
    procedure Tick(const ADelta, AStartTime: LKFloat); virtual; abstract;
  public
    ///  <summary><c>Puts a Thread to sleep ONLY if there's enough time!</c></summary>
    class function SmartSleep(const ATimeToWait: LKFloat; const AThreshold: Cardinal): Boolean;
    constructor Create; virtual;
    destructor Destroy; override;

    ///  <summary><c>Forces the "Next Tick Time" to be bumped to RIGHT NOW. This will trigger the next Tick immediately regardless of any Rate Limit setting.</c></summary>
    procedure Bump;

    ///  <summary><c>Acquires the Thread's internal Critical Section.</c></summary>
    ///  <remarks>
    ///    <para><c>Call this if you need to Get/Set MULTIPLE Properties in a "Consistent State".</c></para>
    ///    <para><c>See </c><see DisplayName="Unlock" cref="LKSL.Threads.Main|TLKThread.Unlock"/><c> also.</c></para>
    ///    <para><c>See </c><see DisplayName="LockIfAvailable" cref="LKSL.Threads.Main|TLKThread.LockIfAvailable"/><c> also.</c></para>
    ///  </remarks>
    procedure Lock; //inline;
    ///  <summary><c>Acquires the Thread's internal Critical Section ONLY IF IT IS AVAILABLE.</c></summary>
    ///  <remarks>
    ///    <para><c>Returns </c>True<c> if the Lock has been successfully Acquired, </c>False<c> if it has NOT been Acquired.</c></para>
    ///    <para><c>Call this if you need to Get/Set MULTIPLE Properties in a "Consistent State".</c></para>
    ///    <para><c>See </c><see DisplayName="Lock" cref="LKSL.Threads.Main|TLKThread.Lock"/><c> also.</c></para>
    ///    <para><c>See </c><see DisplayName="Unlock" cref="LKSL.Threads.Main|TLKThread.Unlock"/><c> also.</c></para>
    ///  </remarks>
    function LockIfAvailable: Boolean; //inline;
    ///  <summary><c>Releases the Thread's internal Critical Section.</c></summary>
    ///  <remarks>
    ///    <para><c>Call this if you need to Get/Set MULTIPLE Properties in a "Consistent State".</c></para>
    ///    <para><c>See </c><see DisplayName="Lock" cref="LKSL.Threads.Main|TLKThread.Lock"/><c> also.</c></para>
    ///    <para><c>See </c><see DisplayName="LockIfAvailable" cref="LKSL.Threads.Main|TLKThread.LockIfAvailable"/><c> also.</c></para>
    ///  </remarks>
    procedure Unlock; //inline;

    ///  <summary><c>Places the Thread in an Inactive state, waiting for the signal to </c><see DisplayName="Wake" cref="LKSL.Threads.Main|TLKThread.Wake"/><c> the Thread.</c></summary>
    procedure Rest;
    ///  <summary><c>Wakes the Thread if it is an Inactive state (see </c><see DisplayName="Rest" cref="LKSL.Threads.Main|TLKThread.Rest"/><c> for details)</c></summary>
    procedure Wake;

    property InstanceGUID: TGUID read FInstanceGUID;
    ///  <summary><c>The Absolute Reference Time at which the next Tick will occur.</c></summary>
    property NextTickTime: LKFloat read GetNextTickTime;
    ///  <summary><c>The current State of the Thread (running or paused).</c></summary>
    property ThreadState: TLKThreadState read GetThreadState write SetThreadState;
    ///  <summary><c>The Absolute Rate (in Ticks Per Second [T/s]) at which the Thread is executing its Tick method.</c></summary>
    property TickRate: LKFloat read GetTickRate;
    ///  <summary><c>The Running Average Rate (in Ticks Per Second [T/s]) at which the Thread is executing its Tick method.</c></summary>
    property TickRateAverage: LKFloat read GetTickRateAverage;
    ///  <summary><c>The Time (in Seconds) over which to calculate the Running Average.</c></summary>
    property TickRateAverageOver: Cardinal read GetTickRateAverageOver write SetTickRateAverageOver;
    ///  <summary><c>The number of Ticks Per Second [T/s] you would LIKE the Thread to operate at.</c></summary>
    ///  <remarks><c>This value is used to calculate how much "Extra Time" (if any) is available on the current Tick.</c></remarks>
    property TickRateDesired: LKFloat read GetTickRateDesired write SetTickRateDesired;
    ///  <summary><c>The Absolute Tick Rate (in Ticks Per Second [T/s]) at which you wish the Thread to operate.</c></summary>
    ///  <remarks><c>There is no guarantee that the rate you specify here will be achievable. Slow hardware or an overloaded running environment may mean the thread operates below the specified rate.</c></remarks>
    property TickRateLimit: LKFloat read GetTickRateLimit write SetTickRateLimit;
    ///  <summary><c>The minimum amount of time that must be available between Ticks in order to Rest the Thread.</c></summary>
    ///  <remarks>
    ///    <para><c>Value is in </c>MILLISECONDS<c> (1 = 0.001 seconds)</c></para>
    ///    <para><c>Minimum value = </c>1</para>
    ///  </remarks>
    property ThrottleInterval: Cardinal read GetThrottleInterval write SetThrottleInterval;
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
const
  THREAD_STATES: Array[TLKThreadState] of Boolean = (True, False);
begin
  inherited Create(False);
  CreateGUID(FInstanceGUID);
  FPerformance := TLKPerformanceCounter.Create(GetDefaultTickRateAverageOver);
  FThrottleInterval := GetDefaultThrottleInterval;
  FLock := TLKCriticalSection.Create;
  FreeOnTerminate := False;
  FThreadState := GetInitialThreadState;
  FTickRateLimit := GetDefaultTickRateLimit;
  FTickRateDesired := GetDefaultTickRateDesired;
  FWakeUp := TEvent.Create(nil, True, THREAD_STATES[FThreadState], '');
end;

destructor TLKThread.Destroy;
begin
  FWakeUp.SetEvent;
  if not Terminated then
  begin
    Terminate;
    WaitFor;
  end;
  FWakeUp.Free;
  FLock.Free;
  FPerformance.Free;
  inherited;
end;

procedure TLKThread.InitializeTickVariables(var ACurrentTime, ALastAverageCheckpoint, ANextAverageCheckpoint, ATickRate: LKFloat; var AAverageTicks: Integer);
begin
  ACurrentTime := GetReferenceTime;
  Lock;
  try
  FNextTickTime := ACurrentTime;
  finally
    Unlock;
  end;
  ALastAverageCheckpoint := 0;
  ANextAverageCheckpoint := 0;
  ATickRate := 0;
  AAverageTicks := 0;
end;

procedure TLKThread.AtomicInitializeCycleValues(var ATickRateLimit, ATickRateDesired: LKFloat; var AThrottleInterval: Cardinal);
begin
  Lock;
  try
    ATickRateLimit := FTickRateLimit;
    ATickRateDesired := FTickRateDesired;
    AThrottleInterval := FThrottleInterval;
  finally
    Unlock;
  end;
end;

procedure TLKThread.AtomicIncrementNextTickTime(const AIncrementBy: LKFloat);
begin
  Lock;
  try
    FNextTickTime := FNextTickTime + AIncrementBy;
  finally
    Unlock;
  end;
end;

procedure TLKThread.Execute;
var
  LDelta, LCurrentTime: LKFloat;
  LTickRate, LTickRateLimit, LTickRateDesired: LKFloat;
  LLastAverageCheckpoint, LNextAverageCheckpoint: LKFloat;
  LAverageTicks: Integer;
  LThrottleInterval: Cardinal;
begin
  InitializeTickVariables(LCurrentTime, LLastAverageCheckpoint, LNextAverageCheckpoint, LTickRate, LAverageTicks);
  while (not Terminated) do
  begin
    if ThreadState = tsRunning then
    begin
      LCurrentTime := GetReferenceTime;
      AtomicInitializeCycleValues(LTickRateLimit, LTickRateDesired, LThrottleInterval);
      LDelta := (LCurrentTime - FNextTickTime);

      // Rate Limiter
      if ((LTickRateLimit > 0)) and (LDelta < ( 1 / LTickRateLimit)) then
        LDelta := (1 / LTickRateLimit);

      // Calculate INSTANT Tick Rate
      if LDelta > 0 then
      begin
        LTickRate := 1 / LDelta; // Calculate the current Tick Rate
        FPerformance.RecordSample(LTickRate);
      end;

      // Call "PreTick"
      PreTick(LDelta, LCurrentTime);

      // Tick or Wait...
      if ((LCurrentTime >= FNextTickTime) and (LTickRateLimit > 0)) or (LTickRateLimit = 0) then
      begin
        AtomicIncrementNextTickTime(LDelta);
        Tick(LDelta, LCurrentTime);
      end else
      begin
        if (FNextTickTime - GetReferenceTime >= LThrottleInterval / 1000) then
          TThread.Sleep(LThrottleInterval);
      end;
    end else
      FWakeUp.WaitFor(INFINITE);
  end;
end;

function TLKThread.GetDefaultTickRateLimit: LKFloat;
begin
  Result := 0;
end;

function TLKThread.GetDefaultThrottleInterval: Integer;
begin
  Result := 1;
end;

function TLKThread.GetDefaultTickRateAverageOver: Cardinal;
begin
  Result := 10;
end;

function TLKThread.GetDefaultTickRateDesired: LKFloat;
begin
  Result := 0;
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

function TLKThread.GetThrottleInterval: Cardinal;
begin
  Lock;
  try
    Result := FThrottleInterval;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRate: LKFloat;
begin
  Result := FPerformance.InstantRate;
end;

function TLKThread.GetTickRateAverage: LKFloat;
begin
  Result := FPerformance.AverageRate;
end;

function TLKThread.GetTickRateAverageOver: Cardinal;
begin
  Result := FPerformance.AverageOver;
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

function TLKThread.GetTickRateLimit: LKFloat;
begin
  Lock;
  try
    Result := FTickRateLimit;
  finally
    Unlock;
  end;
end;

procedure TLKThread.Lock;
begin
  FLock.Acquire;
end;

function TLKThread.LockIfAvailable: Boolean;
begin
  Result := FLock.TryEnter;
end;

procedure TLKThread.PreTick(const ADelta, AStartTime: LKFloat);
begin
  // Do nothing by default
end;

procedure TLKThread.Rest;
begin
  Lock;
  try
    FThreadState := tsPaused;
    FWakeUp.ResetEvent;
  finally
    Unlock;
  end;
end;

procedure TLKThread.SetThreadState(const AThreadState: TLKThreadState);
begin
  Lock;
  try
    FThreadState := AThreadState;
    case AThreadState of
      tsRunning: FWakeUp.SetEvent;
      tsPaused: FWakeUp.ResetEvent;
    end;
  finally
    Unlock;
  end;
end;

procedure TLKThread.SetThrottleInterval(const AThrottleInterval: Cardinal);
begin
  Lock;
  try
    if AThrottleInterval > 0 then
      FThrottleInterval := AThrottleInterval
    else
      FThrottleInterval := 1;
  finally
    Unlock;
  end;
end;

procedure TLKThread.SetTickRateAverageOver(const AAverageOver: Cardinal);
begin
  FPerformance.AverageOver := AAverageOver;
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

class function TLKThread.SmartSleep(const ATimeToWait: LKFloat; const AThreshold: Cardinal): Boolean;
begin
  Result := False;
  if (ATimeToWait >= AThreshold / 1000) then
  begin
    TThread.Sleep(AThreshold);
    Result := True;
  end;
end;

procedure TLKThread.Unlock;
begin
  FLock.Release;
end;

procedure TLKThread.Wake;
begin
  Lock;
  try
    FThreadState := tsRunning;
    FWakeUp.SetEvent;
  finally
    Unlock;
  end;
end;

initialization
  ReferenceWatch := TStopwatch.Create;

end.



