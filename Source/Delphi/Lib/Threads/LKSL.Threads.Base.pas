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

{
  About this unit:
    - This unit provides type declarations required for our "High Precision Threads"
    - This unit also provides the Abstract Base Implementation for the same.

  Included Usage Demos:
    - "LKSL_Demo_HighPrecisionThreads" in the "\Demos\Delphi\<version>\High Precision Threading (HPT)"
      folder

  Changelog (latest changes first):
    4th September 2014:
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
  {$IFDEF POSIX}Posix.Unistd,{$ENDIF} System.Classes, System.SysUtils, System.Diagnostics,
  System.SyncObjs, System.Math;

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
    FStopwatch: TStopwatch;
    FThreadState: TLKThreadState;
    FTickRate: Double; // The INSTANT Tick Rate (in "Ticks per Second")
    FTickRateAverage: Double; // The AVERAGE Tick Rate (in "Ticks per Second")
    FTickRateAverageOver: Double; // How much time (in seconds) the Average is calculated over
    FTickRateLimit: Double; // The current Tick Rate Limit (in "Ticks per Second"), 0 = no limit.
    FYieldAccumulatedTime: Boolean;

    function GetNextTickTime: Double;
    function GetThreadState: TLKThreadState;
    function GetTickRate: Double;
    function GetTickRateAverage: Double;
    function GetTickRateAverageOver: Double;
    function GetTickRateLimit: Double;
    function GetYieldAccumulatedTime: Boolean;

    procedure SetThreadState(const AThreadState: TLKThreadState);
    procedure SetTickRate(const ATickRate: Double); // Used internally!
    procedure SetTickRateAverage(const ATickRateAverage: Double); // Used internally!
    procedure SetTickRateAverageOver(const AAverageOver: Double);
    procedure SetTickRateLimit(const ATickRateLimit: Double);
    procedure SetYieldAccumulatedTime(const AYieldAccumulatedTime: Boolean);
  protected
    // Override "GetDefaultTickRateLimit" if you want to set a default limit (the default is 0 [no limit])
    function GetDefaultTickRateLimit: Double; virtual;
    // Override "GetDefaultTickRateAverageOver" if you want to change the default Tick Rate Averaging Time (default = 2.00 seconds)
    function GetDefaultTickRateAverageOver: Double; virtual;
    // Override "GetDefaultYieldAccumulatedTime" if you DON'T want accumulated (excess) time to be yielded in a single block (Default = True)
    function GetDefaultYieldAccumulatedTime: Boolean; virtual;
    // Override "GetInitialThreadState" if you want the Thread to be Paused on construction (the default is Running)
    function GetInitialThreadState: TLKThreadState; virtual;
    // "GetReferenceTime" returns the current "Reference Time" (which is supremely high resolution)
    function GetReferenceTime: Double;

    // YOU MUST NOT override the TThread.Execute method in your descendants of TLKThread!!!!!!!!
    procedure Execute; override;

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
    procedure Lock;
    // "Unlock" unlocks the Thread's global "Critical Section"
    // (Public just in case you need to call it externally for some reason)
    procedure Unlock;

    property NextTickTime: Double read GetNextTickTime;
    property ThreadState: TLKThreadState read GetThreadState write SetThreadState;
    property TickRate: Double read GetTickRate;
    property TickRateAverage: Double read GetTickRateAverage;
    property TickRateAverageOver: Double read GetTickRateAverageOver write SetTickRateAverageOver;
    property TickRateLimit: Double read GetTickRateLimit write SetTickRateLimit;
    property YieldAccumulatedTime: Boolean read GetYieldAccumulatedTime write SetYieldAccumulatedTime;
  end;

implementation

{ TLKThread }

procedure TLKThread.Bump;
begin
  Lock;
  FNextTickTime := GetReferenceTime;
  Unlock;
end;

constructor TLKThread.Create;
begin
  inherited Create(False);
  FLock := TCriticalSection.Create;
  FStopwatch := TStopwatch.Create;
  FreeOnTerminate := False;
  FThreadState := GetInitialThreadState;
  FTickRateLimit := GetDefaultTickRateLimit;
  FTickRateAverageOver := GetDefaultTickRateAverageOver;
  FYieldAccumulatedTime := GetDefaultYieldAccumulatedTime;
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
  LTickRateLimit: Double;
  LLastAverageCheckpoint, LNextAverageCheckpoint: Double;
  LAverageTicks: Integer;
begin
  LCurrentTime := GetReferenceTime;
  Lock;
  FNextTickTime := LCurrentTime;
  Unlock;
  LLastAverageCheckpoint := 0.00;
  LNextAverageCheckpoint := 0.00;
  LAverageTicks := 0;
  while (not Terminated) do
  begin
    LTickRateLimit := TickRateLimit; // Read once so we don't have to keep Acquiring the Lock!
    LCurrentTime := GetReferenceTime;
    LDelta := (LCurrentTime - FNextTickTime);

    // Rate Limiter
    if (LTickRateLimit > 0) then
      if (LDelta < ( 1 / LTickRateLimit)) then
        LDelta := (1 / LTickRateLimit);

    // Calculate INSTANT Tick Rate
    if LDelta > 0 then
      SetTickRate((1 / LDelta)); // Calculate the current Tick Rate

    if ThreadState = tsRunning then
    begin
      // Tick or Wait...
      if ((LCurrentTime >= FNextTickTime) and (LTickRateLimit > 0)) or (LTickRateLimit  = 0) then
      begin

        // Calculate AVEARAGE Tick Rate
        if LCurrentTime >= LNextAverageCheckpoint then
        begin
          LNextAverageCheckpoint := LCurrentTime + TickRateAverageOver;
          LLastAverageCheckpoint := LCurrentTime;
          LAverageTicks := -1;
        end;
        Inc(LAverageTicks);
        if (LCurrentTime - LLastAverageCheckpoint > 0) then
          SetTickRateAverage(LAverageTicks / (LCurrentTime - LLastAverageCheckpoint))
        else
          SetTickRateAverage(GetTickRate);
        Lock;
        FNextTickTime := FNextTickTime + LDelta;
        Unlock;
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
            usleep(Floor((LNextTime - LCurrentTime)));
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

function TLKThread.GetInitialThreadState: TLKThreadState;
begin
  Result := tsRunning;
end;

function TLKThread.GetNextTickTime: Double;
begin
  Lock;
  Result := FNextTickTime;
  Unlock;
end;

function TLKThread.GetReferenceTime: Double;
begin
  Result := TStopwatch.GetTimeStamp / TStopwatch.Frequency;
end;

function TLKThread.GetThreadState: TLKThreadState;
begin
  Lock;
  Result := FThreadState;
  Unlock;
end;

function TLKThread.GetTickRate: Double;
begin
  Lock;
  Result := FTickRate;
  Unlock;
end;

function TLKThread.GetTickRateAverage: Double;
begin
  Lock;
  Result := FTickRateAverage;
  Unlock;
end;

function TLKThread.GetTickRateAverageOver: Double;
begin
  Lock;
  Result := FTickRateAverageOver;
  Unlock;
end;

function TLKThread.GetTickRateLimit: Double;
begin
  Lock;
  Result := FTickRateLimit;
  Unlock;
end;

function TLKThread.GetYieldAccumulatedTime: Boolean;
begin
  Lock;
  Result := FYieldAccumulatedTime;
  Unlock;
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

procedure TLKThread.SetThreadState(const AThreadState: TLKThreadState);
begin
  Lock;
  FThreadState := AThreadState;
  Unlock;
end;

procedure TLKThread.SetTickRate(const ATickRate: Double);
begin
  Lock;
  FTickRate := ATickRate;
  Unlock;
end;

procedure TLKThread.SetTickRateAverage(const ATickRateAverage: Double);
begin
  Lock;
  FTickRateAverage := ATickRateAverage;
  Unlock;
end;

procedure TLKThread.SetTickRateAverageOver(const AAverageOver: Double);
begin
  Lock;
  FTickRateAverageOver := AAverageOver;
  Unlock;
end;

procedure TLKThread.SetTickRateLimit(const ATickRateLimit: Double);
begin
  Lock;
  FTickRateLimit := ATickRateLimit;
  Unlock;
end;

procedure TLKThread.SetYieldAccumulatedTime(const AYieldAccumulatedTime: Boolean);
begin
  Lock;
  FYieldAccumulatedTime := AYieldAccumulatedTime;
  Unlock;
end;

procedure TLKThread.Unlock;
begin
  FLock.Release;
end;

end.
