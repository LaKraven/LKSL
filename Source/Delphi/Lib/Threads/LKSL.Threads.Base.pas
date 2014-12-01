{
  LaKraven Studios Standard Library [LKSL]
  Copyright (c) 2014, LaKraven Studios Ltd, All Rights Reserved

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

{$I LKSL.inc}

{
  About this unit:
    - This unit provides type declarations required for our "High Precision Threads"
    - This unit also provides the Abstract Base Implementation for the same.

  Included Usage Demos:
    - "LKSL_Demo_HighPrecisionThreads" in the "\Demos\Delphi\<version>\High Precision Threading (HPT)"
      folder

  Changelog (latest changes first):
    30th November 2014:
      - Put "try/finally" blocks around all Lock requests (so if the code fails, the Lock will be released)
    19th September 2014:
      - Added Protected Function "CalculateExtraTime" which returns the number of seconds
        (double-precision) available between the time at which you place the call, and the time at which
        the next Tick is scheduled to occur.
    16th September 2014:
      - Added Property "TickRateDesired" which is used to define the rate you want the Thread to run at.
        This is NOT the same as "TickRateLimit"... it is used to calculate how much EXTRA time is
        available over the "desired rate" (or how far BELOW the "Desired Rate", in the event of negative
        values). The value should represent Ticks Per Second!
      - Added Property "TickRateExtraTicks" which (if "TickRateDesired > 0.00") returns the INSTANT number of
        Ticks Per Second in EXCESS of the Desired Rate.
      - Added Property "TickRateExtraTicksAverage" which (if "TickRateDesired > 0.00") returns the AVERAGE
        number of Ticks Per Second in EXCESS of the Desired Rate.
      - Added Property "TickRateExtraTime" which returns the number of SECONDS between the INSTANT Tick
        Rate and the DESIRED Tick Rate
      - Added Property "TickRateExtraTimeAverage" which is the same as "TickRateExtraTime" but Averaged
    8th September 2014:
      - Fixed a bug related to non-Windows platforms
    4th September 2014 (Second Commit):
      - Decenteralized "GetReferenceTime" so that all Threads share a common Reference Timer.
        This is good for synchronizing process timing between separate Threads.
      - Added new Method "PreTick", which is executed on EVERY cycle, ignoring the Tick Rate Limit.
        This is required for the "TLKEventThread" type defined in "LKSL.Events.Base.pas"
    4th September 2014 (First Commit):
      - Added new Method "GetDefaultYieldAccumulatedTime" to define whether all accumulated (excess) time
        should be yielded in a single block by default.
      - Added new Property "YieldAccumulatedTime" to control ad-hoc whether all accumulated (excess) time
        should be yielded in a single block.
      - Add new Property "NextTickTime", which is the Reference Time at (or after) which the next Tick
        will occur.
      - Added new Method "Bump", which (if "YieldAccumulatedTime" = False) forces the next Tick to occur
        immediately.
    3rd September 2014:
      - Prepared for Release.
}

interface

uses
  {$IFDEF POSIX}Posix.Unistd,{$ENDIF}
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.Diagnostics, System.Math, System.SyncObjs;
  {$ELSE}
    Classes, SysUtils, Diagnostics, Math, SyncObjs;
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}

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
    FLock: TCriticalSection;
    FNextTickTime: Double;
    FThreadState: TLKThreadState;
    FTickRate: Double; // The INSTANT Tick Rate (in "Ticks per Second")
    FTickRateAverage: Double; // The AVERAGE Tick Rate (in "Ticks per Second")
    FTickRateAverageOver: Double; // How much time (in seconds) the Average is calculated over
    FTickRateDesired: Double; // The DESIRED rate at which you want the Thread to Tick (minimum)
    FTickRateExtra: Double; // The number of extra ticks per second in excess of the DESIRED Tick Rate.
    FTickRateExtraTime: Double; // The number of extra SECONDS available over the DESIRED Tick Rate.
    FTickRateExtraAverage: Double; // Same as "FTickRateExtraTime" but Averaged over "FTickRateAverageOver"
    FTickRateExtraAverageTime: Double; // Same as "FTickRateExtraTime" but Averaged over "FTickRateAverageOver"
    FTickRateLimit: Double; // The current Tick Rate Limit (in "Ticks per Second"), 0 = no limit.
    FYieldAccumulatedTime: Boolean;

    function GetNextTickTime: Double;
    function GetThreadState: TLKThreadState;
    function GetTickRate: Double;
    function GetTickRateAverage: Double;
    function GetTickRateAverageOver: Double;
    function GetTickRateDesired: Double;
    function GetTickRateExtraTicks: Double;
    function GetTickRateExtraTime: Double;
    function GetTickRateExtraTicksAverage: Double;
    function GetTickRateExtraTimeAverage: Double;
    function GetTickRateLimit: Double;
    function GetYieldAccumulatedTime: Boolean;

    procedure SetThreadState(const AThreadState: TLKThreadState);
    procedure SetTickRate(const ATickRate: Double); // Used internally!
    procedure SetTickRateAverage(const ATickRateAverage: Double); // Used internally!
    procedure SetTickRateAverageOver(const AAverageOver: Double);
    procedure SetTickRateDesired(const ADesiredRate: Double);
    procedure SetTickRateExtraTicks(const AExtraTime: Double); // Used internally!
    procedure SetTickRateExtraTicksAverage(const AExtraTimeAverage: Double); // Used internally!
    procedure SetTickRateLimit(const ATickRateLimit: Double);
    procedure SetYieldAccumulatedTime(const AYieldAccumulatedTime: Boolean);
  protected
    // Override "GetDefaultTickRateLimit" if you want to set a default limit (the default is 0 [no limit])
    function GetDefaultTickRateLimit: Double; virtual;
    // Override "GetDefaultTickRateAverageOver" if you want to change the default Tick Rate Averaging Time (default = 2.00 seconds)
    function GetDefaultTickRateAverageOver: Double; virtual;
    // Override "GetDefaultTickRateDesired" if you want to compute EXTRA Time between Ticks.
    // This Extra Time can be used during a Tick to selectively perform additional processing during a Tick.
    // One example in which this is useful is for graphics rendering, where Exta Time can be used for
    // post-processing computations.
    // Default = 0.00, which disables Extra Time Calculation
    function GetDefaultTickRateDesired: Double; virtual;
    // Override "GetDefaultYieldAccumulatedTime" if you DON'T want accumulated (excess) time to be yielded in a single block (Default = True)
    // False = Yield time in small chunks
    // True = Yield all accumulated time in a single block
    function GetDefaultYieldAccumulatedTime: Boolean; virtual;
    // Override "GetInitialThreadState" if you want the Thread to be Paused on construction (the default is Running)
    function GetInitialThreadState: TLKThreadState; virtual;

    function CalculateExtraTime: Double;

    // YOU MUST NOT override the TThread.Execute method in your descendants of TLKThread!!!!!!!!
    procedure Execute; override; final;

    // Override the "PreTick" procedure if your Thread needs to do something on EVERY cycle
    // NOTE: THIS METHOD IGNORES THE TICK RATE LIMIT!
    // "PreTick" is implemented by the Event Handler system (because the Event Queue needs to be processed regardless of the Tick Rate Limit)
    procedure PreTick(const ADelta, AStartTime: Double); virtual;

    // Override the "Tick" procedure to implement your Thread's operational code.
    // ADelta = the time differential ("Delta") between the current Tick and the previous Tick
    // AStartTime = the Reference Time at which the current Tick began.
    // DON'T FORGET "INHERITED;" FIRST!!!!
    procedure Tick(const ADelta, AStartTime: Double); virtual; abstract;
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
    procedure Kill;

    // "Lock" locks the Thread's global "Critical Section" using a "Spinlock"
    // (Public just in case you need to call it externally for some reason)
    procedure Lock; inline;
    // "Unlock" unlocks the Thread's global "Critical Section"
    // (Public just in case you need to call it externally for some reason)
    procedure Unlock; inline;

    property NextTickTime: Double read GetNextTickTime;
    property ThreadState: TLKThreadState read GetThreadState write SetThreadState;
    property TickRate: Double read GetTickRate;
    property TickRateAverage: Double read GetTickRateAverage;
    property TickRateAverageOver: Double read GetTickRateAverageOver write SetTickRateAverageOver;
    property TickRateDesired: Double read GetTickRateDesired write SetTickRateDesired;
    property TickRateExtraTicks: Double read GetTickRateExtraTicks;
    property TickRateExtraTime: Double read GetTickRateExtraTime;
    property TickRateExtraTicksAverage: Double read GetTickRateExtraTicksAverage;
    property TickRateExtraTimeAverage: Double read GetTickRateExtraTimeAverage;
    property TickRateLimit: Double read GetTickRateLimit write SetTickRateLimit;
    property YieldAccumulatedTime: Boolean read GetYieldAccumulatedTime write SetYieldAccumulatedTime;
  end;

// "GetReferenceTime" returns the current "Reference Time" (which is supremely high resolution)
function GetReferenceTime: Double;

implementation

var
  ReferenceWatch: TStopwatch;

function GetReferenceTime: Double;
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

function TLKThread.CalculateExtraTime: Double;
begin
  Result := NextTickTime - GetReferenceTime;
end;

constructor TLKThread.Create;
begin
  inherited Create(False);
  FLock := TCriticalSection.Create;
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
  LDelta, LCurrentTime: Double;
  LTickRate, LTickRateLimit, LTickRateDesired, LTickRateAverage: Double;
  LLastAverageCheckpoint, LNextAverageCheckpoint: Double;
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

function TLKThread.GetDefaultTickRateLimit: Double;
begin
  Result := 0.00;
end;

function TLKThread.GetDefaultYieldAccumulatedTime: Boolean;
begin
  Result := True;
end;

function TLKThread.GetDefaultTickRateAverageOver: Double;
begin
  Result := 2.00;
end;

function TLKThread.GetDefaultTickRateDesired: Double;
begin
  Result := 0.00;
end;

function TLKThread.GetInitialThreadState: TLKThreadState;
begin
  Result := tsRunning;
end;

function TLKThread.GetNextTickTime: Double;
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

function TLKThread.GetTickRate: Double;
begin
  Lock;
  try
    Result := FTickRate;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateAverage: Double;
begin
  Lock;
  try
  Result := FTickRateAverage;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateAverageOver: Double;
begin
  Lock;
  try
    Result := FTickRateAverageOver;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateDesired: Double;
begin
  Lock;
  try
    Result := FTickRateDesired;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateExtraTicks: Double;
begin
  Lock;
  try
    Result := FTickRateExtra;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateExtraTicksAverage: Double;
begin
  Lock;
  try
    Result := FTickRateExtraAverage;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateExtraTime: Double;
begin
  Lock;
  try
    Result := FTickRateExtraTime;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateExtraTimeAverage: Double;
begin
  Lock;
  try
    Result := FTickRateExtraAverageTime;
  finally
    Unlock;
  end;
end;

function TLKThread.GetTickRateLimit: Double;
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

procedure TLKThread.PreTick(const ADelta, AStartTime: Double);
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

procedure TLKThread.SetTickRate(const ATickRate: Double);
begin
  Lock;
  try
    FTickRate := ATickRate;
  finally
    Unlock;
  end;
end;

procedure TLKThread.SetTickRateAverage(const ATickRateAverage: Double);
begin
  Lock;
  try
    FTickRateAverage := ATickRateAverage;
  finally
    Unlock;
  end;
end;

procedure TLKThread.SetTickRateAverageOver(const AAverageOver: Double);
begin
  Lock;
  try
    FTickRateAverageOver := AAverageOver;
  finally
    Unlock;
  end;
end;

procedure TLKThread.SetTickRateDesired(const ADesiredRate: Double);
begin
  Lock;
  try
    FTickRateDesired := ADesiredRate;
  finally
    Unlock;
  end;
end;

procedure TLKThread.SetTickRateExtraTicks(const AExtraTime: Double);
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

procedure TLKThread.SetTickRateExtraTicksAverage(const AExtraTimeAverage: Double);
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

procedure TLKThread.SetTickRateLimit(const ATickRateLimit: Double);
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
