{
  LaKraven Studios Standard Library [LKSL]
  Copyright (c) 2014-2016, Simon J Stuart, All Rights Reserved

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
    - Simon J Stuart and its employees (including but not limited to directors,
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
unit LKSL.Common.SyncObjs;

interface

{$I LKSL.inc}

{$REGION 'Unit About'}
  {
    About this unit:
      - This unit provides Locks and Interlocks for multi-threaded processes
      - This unit is used pervasively throughout the LKSL itself.
  }
{$ENDREGION}

{$DEFINE LKSL_LOCK_ALLEXCLUSIVE} // TODO -oSJS -cTLKReadWriteLock: Fix the deadlock issue when elevating a Read to a Write, then remove LKSL_LOCK_ALLEXCLUSIVE define

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs;
  {$ELSE}
    Classes, SysUtils, SyncObjs;
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}

{$I LKSL_RTTI.inc}

type
  { Forward Declarations }
  TLKCriticalSection = class;
  TLKReadWriteLock = class;

  { Enums }
  TLKReadWriteLockState = (lsWaiting, lsReading, lsWriting);

  ///  <summary><c>Avoids CPU caching (which causes problems)</c></summary>
  TLKCriticalSection = class(TCriticalSection)
  private
    {$HINTS OFF}FDummy : array [0..95] of Byte;{$HINTS ON}
  end;

  ///  <summary><c>Provies Two-State Locking</c></summary>
  ///  <remarks>
  ///    <c>Three States are:</c>
  ///    <para><c>Read - Holds back Write requests until relinquished.</c></para>
  ///    <para><c>Write - Holds back Read and other Write requests until relinquished.</c></para>
  ///  </remarks>
  TLKReadWriteLock = class(TPersistent)
  private
    {$IFDEF LKSL_LOCK_ALLEXCLUSIVE}
      FWriteLock: TLKCriticalSection;
    {$ELSE}
      FActiveThread: Cardinal; // ID of the current Thread holding the Write Lock
      FCountReads: {$IFDEF DELPHI}Int64{$ELSE}LongWord{$ENDIF DELPHI}; // Number of Read Operations in Progress
      FCountWrites: {$IFDEF DELPHI}Int64{$ELSE}LongWord{$ENDIF DELPHI}; // Number of Write Operations in Progress
      FLockState: Cardinal; // 0 = Waiting, 1 = Reading, 2 = Writing
      FWaitRead,
      FWaitWrite: TEvent;
      function GetLockState: TLKReadWriteLockState;
      function GetThreadMatch: Boolean;
      procedure SetActiveThread;
      procedure SetLockState(const ALockState: TLKReadWriteLockState);
      protected
        function AcquireReadActual: Boolean;
        function AcquireWriteActual: Boolean;
    {$ENDIF LKSL_LOCK_ALLEXCLUSIVE}
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AcquireRead;
    procedure AcquireWrite;
    procedure ReleaseRead;
    procedure ReleaseWrite;
    function TryAcquireRead: Boolean;
    function TryAcquireWrite: Boolean;

    {$IFNDEF LKSL_LOCK_ALLEXCLUSIVE}
      property LockState: TLKReadWriteLockState read GetLockState;
    {$ENDIF LKSL_LOCK_ALLEXCLUSIVE}
  end;

implementation

{ TLKReadWriteLock }

procedure TLKReadWriteLock.AcquireRead;
{$IFNDEF LKSL_LOCK_ALLEXCLUSIVE}
  var
    LAcquired: Boolean;
{$ENDIF LKSL_LOCK_ALLEXCLUSIVE}
begin
  {$IFDEF LKSL_LOCK_ALLEXCLUSIVE}
    FWriteLock.Enter;
  {$ELSE}
    repeat
      LAcquired := AcquireReadActual;
    until LAcquired;

  {$ENDIF LKSL_LOCK_ALLEXCLUSIVE}
end;

{$IFNDEF LKSL_LOCK_ALLEXCLUSIVE}
  function TLKReadWriteLock.AcquireReadActual: Boolean;
  begin
    Result := False;
    case GetLockState of
      lsWaiting, lsReading: begin
                              FWaitRead.ResetEvent;
                              SetLockState(lsReading);
                              {$IFDEF DELPHI}AtomicIncrement{$ELSE}InterlockedIncrement{$ENDIF DELPHI}(FCountReads);
                              Result := True;
                            end;
      lsWriting: begin
                   Result := GetThreadMatch;
                   if (not Result) then
                     FWaitWrite.WaitFor(500)
                   else
                     {$IFDEF DELPHI}AtomicIncrement{$ELSE}InterlockedIncrement{$ENDIF DELPHI}(FCountReads);
                 end;
    end;
  end;
{$ENDIF LKSL_LOCK_ALLEXCLUSIVE}

procedure TLKReadWriteLock.AcquireWrite;
{$IFNDEF LKSL_LOCK_ALLEXCLUSIVE}
  var
    LAcquired: Boolean;
{$ENDIF LKSL_LOCK_ALLEXCLUSIVE}
begin
  {$IFDEF LKSL_LOCK_ALLEXCLUSIVE}
    FWriteLock.Enter;
  {$ELSE}
    repeat
      LAcquired := AcquireWriteActual;
    until LAcquired;
  {$ENDIF LKSL_LOCK_ALLEXCLUSIVE}
end;

{$IFNDEF LKSL_LOCK_ALLEXCLUSIVE}
  function TLKReadWriteLock.AcquireWriteActual: Boolean;
  begin
    Result := False;
    case GetLockState of
      lsWaiting: begin
                   FWaitWrite.ResetEvent;
                   SetActiveThread;
                   SetLockState(lsWriting);
                   {$IFDEF DELPHI}AtomicIncrement{$ELSE}InterlockedIncrement{$ENDIF DELPHI}(FCountWrites);
                   Result := True;
                 end;
      lsReading: FWaitRead.WaitFor(500);
      lsWriting: begin
                   Result := GetThreadMatch;
                   if Result then
                     {$IFDEF DELPHI}AtomicIncrement{$ELSE}InterlockedIncrement{$ENDIF DELPHI}(FCountWrites);
                 end;
    end;
  end;
{$ENDIF LKSL_LOCK_ALLEXCLUSIVE}

constructor TLKReadWriteLock.Create;
begin
  inherited;
  {$IFDEF LKSL_LOCK_ALLEXCLUSIVE}
    FWriteLock := TLKCriticalSection.Create;
  {$ELSE}
    FWaitRead := TEvent.Create(nil, True, True, '');
    FWaitWrite := TEvent.Create(nil, True, True, '');
    FActiveThread := 0; // Since there's no Thread yet
    FCountReads := 0;
    FCountWrites := 0;
    FLockState := 0; // Waiting by default
  {$ENDIF LKSL_LOCK_ALLEXCLUSIVE}
end;

destructor TLKReadWriteLock.Destroy;
begin
  {$IFDEF LKSL_LOCK_ALLEXCLUSIVE}
    FWriteLock.Free;
  {$ELSE}
    FWaitRead.SetEvent;
    FWaitWrite.SetEvent;
    FWaitRead.Free;
    FWaitWrite.Free;
  {$ENDIF LKSL_LOCK_ALLEXCLUSIVE}
  inherited;
end;

{$IFNDEF LKSL_LOCK_ALLEXCLUSIVE}
  function TLKReadWriteLock.GetLockState: TLKReadWriteLockState;
  const
    LOCK_STATES: Array[0..2] of TLKReadWriteLockState = (lsWaiting, lsReading, lsWriting);
  begin
    Result := LOCK_STATES[{$IFDEF DELPHI}AtomicIncrement{$ELSE}InterlockedExchangeAdd{$ENDIF DELPHI}(FLockState, 0)];
  end;

  function TLKReadWriteLock.GetThreadMatch: Boolean;
  begin
    Result := {$IFDEF DELPHI}AtomicIncrement{$ELSE}InterlockedExchangeAdd{$ENDIF DELPHI}(FActiveThread, 0) = TThread.CurrentThread.ThreadID;
  end;
{$ENDIF LKSL_LOCK_ALLEXCLUSIVE}

procedure TLKReadWriteLock.ReleaseRead;
begin
  {$IFDEF LKSL_LOCK_ALLEXCLUSIVE}
    FWriteLock.Leave;
  {$ELSE}
    case GetLockState of
      lsWaiting: raise Exception.Create('Lock State not Read, cannot Release Read on a Waiting Lock!');
      lsReading: begin
                   if {$IFDEF DELPHI}AtomicDecrement{$ELSE}InterlockedDecrement{$ENDIF DELPHI}(FCountReads) = 0 then
                   begin
                     SetLockState(lsWaiting);
                     FWaitRead.SetEvent;
                   end;
                 end;
      lsWriting: begin
                   if (not GetThreadMatch) then
                     raise Exception.Create('Lock State not Read, cannot Release Read on a Write Lock!');
                 end;
    end;
  {$ENDIF LKSL_LOCK_ALLEXCLUSIVE}
end;

procedure TLKReadWriteLock.ReleaseWrite;
begin
  {$IFDEF LKSL_LOCK_ALLEXCLUSIVE}
    FWriteLock.Leave;
  {$ELSE}
    case GetLockState of
      lsWaiting: raise Exception.Create('Lock State not Write, cannot Release Write on a Waiting Lock!');
      lsReading: begin
                   if (not GetThreadMatch) then;
                     raise Exception.Create('Lock State not Write, cannot Release Write on a Read Lock!');
                 end;
      lsWriting: begin
                   if GetThreadMatch then
                   begin
                     if {$IFDEF DELPHI}AtomicDecrement{$ELSE}InterlockedDecrement{$ENDIF DELPHI}(FCountWrites) = 0 then
                     begin
                       SetLockState(lsWaiting);
                       {$IFDEF DELPHI}AtomicExchange{$ELSE}InterlockedExchange{$ENDIF DELPHI}(FActiveThread, 0);
                       FWaitWrite.SetEvent;
                     end;
                   end;
                 end;
    end;
  {$ENDIF LKSL_LOCK_ALLEXCLUSIVE}
end;

{$IFNDEF LKSL_LOCK_ALLEXCLUSIVE}
  procedure TLKReadWriteLock.SetActiveThread;
  begin
    {$IFDEF DELPHI}AtomicExchange{$ELSE}InterlockedExchange{$ENDIF DELPHI}(FActiveThread, TThread.CurrentThread.ThreadID);
  end;

  procedure TLKReadWriteLock.SetLockState(const ALockState: TLKReadWriteLockState);
  const
    LOCK_STATES: Array[TLKReadWriteLockState] of Integer = (0, 1, 2);
  begin
    {$IFDEF DELPHI}AtomicExchange{$ELSE}InterlockedExchange{$ENDIF DELPHI}(FLockState, LOCK_STATES[ALockState]);
  end;
{$ENDIF LKSL_LOCK_ALLEXCLUSIVE}

function TLKReadWriteLock.TryAcquireRead: Boolean;
begin
  {$IFDEF LKSL_LOCK_ALLEXCLUSIVE}
    Result := FWriteLock.TryEnter;
  {$ELSE}
    Result := AcquireReadActual;
  {$ENDIF LKSL_LOCK_ALLEXCLUSIVE}
end;

function TLKReadWriteLock.TryAcquireWrite: Boolean;
begin
  {$IFDEF LKSL_LOCK_ALLEXCLUSIVE}
    Result := FWriteLock.TryEnter;
  {$ELSE}
    Result := AcquireWriteActual;
  {$ENDIF LKSL_LOCK_ALLEXCLUSIVE}
end;

end.
