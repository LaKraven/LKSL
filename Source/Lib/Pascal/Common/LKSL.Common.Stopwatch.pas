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
unit LKSL.Common.Stopwatch;

{$I ..\Common\LKSL.inc}

{$IFDEF FPC}
  {$IFDEF LKSL_MODE_FPC}
    {$mode objfpc}{$H+}
  {$ELSE}
    {$mode delphi}
  {$ENDIF LKSL_MODE_FPC}
{$ENDIF FPC}

interface

  uses
    SysUtils {$IFDEF LINUX},unixtype, linux{$ENDIF LINUX};

type

  { TStopWatch }

  TStopWatch = class
  private
    const
      C_THOUSAND = 1000;
     {$IFDEF LINUX}
        C_MILLION  = C_THOUSAND * C_THOUSAND;
        TicksPerNanoSecond   = 100;
     {$ENDIF LINUX}
      C_BILLION  = C_THOUSAND * C_THOUSAND * C_THOUSAND;
      TicksPerMilliSecond  =  10000;
      TicksPerSecond       = C_BILLION div 100;
    type
      TBaseMesure = {$IFDEF MSWINDOWS}Int64;{$ENDIF MSWINDOWS}{$IFDEF LINUX}TTimeSpec;{$ENDIF LINUX}
  strict private
    class var FFrequency : Int64;
    class var FIsHighResolution : Boolean;
  strict private
    FElapsed : Int64;
    FRunning : Boolean;
    FStartPosition : TBaseMesure;
  strict private
    procedure CheckInitialization();inline;
    function GetElapsedMilliseconds: Int64;
    function GetElapsedTicks: Int64;
  public
    class function Create() : TStopWatch;static;
    class function GetTimeStamp: Int64; static;
    class function StartNew() : TStopWatch;static;
    class property Frequency : Int64 read FFrequency;
    class property IsHighResolution : Boolean read FIsHighResolution;
    procedure Reset();
    procedure Start();
    procedure Stop();
    property ElapsedMilliseconds : Int64 read GetElapsedMilliseconds;
    property ElapsedTicks : Int64 read GetElapsedTicks;
    property IsRunning : Boolean read FRunning;
  end;

resourcestring
  sStopWatchNotInitialized = 'The StopWatch is not initialized.';

implementation

  {$IFDEF MSWINDOWS}
  uses
    Windows;
  {$ENDIF MSWINDOWS}

{ TStopWatch }

class function TStopWatch.Create(): TStopWatch;
  {$IFDEF LINUX}
    var
      r : TBaseMesure;
  {$ENDIF LINUX}
begin
  Result := nil;
  if (FFrequency = 0) then begin
    {$IFDEF MSWINDOWS}
      FIsHighResolution := QueryPerformanceFrequency(FFrequency);
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
      FIsHighResolution := (clock_getres(CLOCK_MONOTONIC,@r) = 0);
      FIsHighResolution := FIsHighResolution and (r.tv_nsec <> 0);
      if (r.tv_nsec <> 0) then
        FFrequency := C_BILLION div r.tv_nsec;
    {$ENDIF LINUX}
  end;
  FillChar(Result,SizeOf(Result),0);
end;

class function TStopWatch.GetTimeStamp: Int64;
  {$IF defined(POSIX) and not defined(MACOS)}
    var
      res: timespec;
  {$ENDIF}
begin
  Result := 0;
  {$IF defined(MSWINDOWS)}
    if FIsHighResolution then
      QueryPerformanceCounter(Result)
    else
      Result := GetTickCount * Int64(TicksPerMillisecond);
  {$ELSEIF defined(MACOS)}
    Result := Int64(AbsoluteToNanoseconds(mach_absolute_time) div 100);
  {$ELSEIF defined(POSIX)}
    clock_gettime(CLOCK_MONOTONIC, @res);
    Result := (Int64(1000000000) * res.tv_sec + res.tv_nsec) div 100;
  {$ENDIF}
end;

class function TStopWatch.StartNew() : TStopWatch;
begin
  Result := TStopWatch.Create();
  Result.Start();
end;

procedure TStopWatch.CheckInitialization();
begin
  if (FFrequency = 0) then
    raise Exception.Create(sStopWatchNotInitialized);
end;

function TStopWatch.GetElapsedMilliseconds: Int64;
begin
  {$IFDEF MSWINDOWS}
    Result := ElapsedTicks * TicksPerMilliSecond;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
    Result := FElapsed div C_MILLION;
  {$ENDIF LINUX}
end;

function TStopWatch.GetElapsedTicks: Int64;
begin
  CheckInitialization();
  {$IFDEF MSWINDOWS}
    Result := (FElapsed * TicksPerSecond) div FFrequency;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
    Result := FElapsed div TicksPerNanoSecond;
  {$ENDIF LINUX}
end;

procedure TStopWatch.Reset();
begin
  Stop();
  FElapsed := 0;
  FillChar(FStartPosition,SizeOf(FStartPosition),0);
end;

procedure TStopWatch.Start();
begin
  if FRunning then
    exit;
  FRunning := True;
  {$IFDEF MSWINDOWS}
    QueryPerformanceCounter(FStartPosition);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
    clock_gettime(CLOCK_MONOTONIC,@FStartPosition);
  {$ENDIF LINUX}
end;

procedure TStopWatch.Stop();
var
  locEnd : TBaseMesure = 0;
  {$IFDEF LINUX}
    s, n : Int64;
  {$ENDIF LINUX}
begin
  if not FRunning then
    exit;
  FRunning := False;
  {$IFDEF MSWINDOWS}
    QueryPerformanceCounter(locEnd);
    FElapsed := FElapsed + (UInt64(locEnd) - UInt64(FStartPosition));
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
    clock_gettime(CLOCK_MONOTONIC,@locEnd);
    if (locEnd.tv_nsec < FStartPosition.tv_nsec) then begin
      s := locEnd.tv_sec - FStartPosition.tv_sec - 1;
      n := C_BILLION + locEnd.tv_nsec - FStartPosition.tv_nsec;
    end else begin
      s := locEnd.tv_sec - FStartPosition.tv_sec;
      n := locEnd.tv_nsec - FStartPosition.tv_nsec;
    end;
    FElapsed := FElapsed + (s * C_BILLION) + n;
  {$ENDIF LINUX}
end;

end.
