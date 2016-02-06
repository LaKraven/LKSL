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
unit LKSL.Common.Types;

{$I LKSL.inc}

{$IFDEF FPC}
  {$IFNDEF LKSL_SUPPRESS_VERSION_WARNING}
    {.$IF FPC_VERSION < 3}
      {.$ERROR 'FreePascal (FPC) 3.0 or above is required for the LKSL.'}
      {.$DEFINE LKSL_WARNING_VERSION}
    {.$IFEND FPC_VERSION}
  {$ENDIF LKSL_SUPPRESS_VERSION_WARNING}
{$ELSE}
  {$IFNDEF LKSL_SUPPRESS_VERSION_WARNING}
    {$IFNDEF DELPHIXE2_UP}
      {$MESSAGE WARN 'Delphi 2010 and XE are not regularly tested with the LKSL. Please report any issues on https://github.com/LaKraven/LKSL'}
      {$DEFINE LKSL_WARNING_VERSION}
    {$ENDIF DELPHIXE2_UP}
  {$ENDIF LKSL_SUPPRESS_VERSION_WARNING}
{$ENDIF FPC}

{$IFDEF LKSL_WARNING_VERSION}
  {$MESSAGE HINT 'Define "LKSL_SUPPRESS_VERSION_WARNING" in your project options to get rid of these messages'}
  {$UNDEF LKSL_WARNING_VERSION}
{$ENDIF LKSL_WARNING_VERSION}

{$IFNDEF LKSL_SUPPRESS_DEPRECATION_WARNING}
  // Nothing deprecated to warn about at this moment
  {$IFDEF LKSL_WARNING_DEPRECATION}
    {$MESSAGE HINT 'Define "LKSL_SUPPRESS_DEPRECATION_WARNING" in your project options to get rid of these messages'}
  {$ENDIF LKSL_WARNING_DEPRECATION}
{$ENDIF LKSL_SUPPRESS_DEPRECATION_WARNING}

{
  About this unit:
    - This unit provides fundamental abstract base types used throughout the LKSL
}

interface

{$DEFINE LKSL_LOCK_ALLEXCLUSIVE} // TODO -oSJS -cTLKReadWriteLock: Fix the deadlock issue when elevating a Read to a Write, then remove LKSL_LOCK_ALLEXCLUSIVE define

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs;
  {$ELSE}
    Classes, SysUtils, SyncObjs;
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}

  {$I LKSL_RTTI.inc}

type
  { Interface Forward Declarations }
  ILKInterface = interface;
  ILKReadWriteLock = interface;
  { Class Forward Declarations }
  TLKPersistent = class;
  TLKObject = class;
  TLKReadWriteLock = class;
  TLKInterfacedPersistent = class;
  TLKInterfacedObject = class;
  {$IFNDEF FPC}
    ILKComparer<T> = interface;
    ILKHolder<T> = interface;
    ILKObjectHolder<T: class> = interface;
    TLKComparer<T> = class;
    TLKHolder<T> = class;
    TLKObjectHolder<T: class> = class;
    ILKThreadSafeType<T> = interface;
    TLKThreadSafeType<T> = class;
  {$ENDIF FPC}

  { Enums }
  TLKReadWriteLockState = (lsWaiting, lsReading, lsWriting);

  {$IFDEF LKSL_FLOAT_SINGLE}
    ///  <summary><c>Single-Precision Floating Point Type.</c></summary>
    LKFloat = Single;
  {$ELSE}
    {$IFDEF LKSL_FLOAT_EXTENDED}
      ///  <summary><c>Extended-Precision Floating Point Type.</c></summary>
      LKFloat = Extended;
    {$ELSE}
      ///  <summary><c>Double-Precision Floating Point Type.</c></summary>
      LKFloat = Double; // This is our default
    {$ENDIF LKSL_FLOAT_DOUBLE}
  {$ENDIF LKSL_FLOAT_SINGLE}

  TLKGenericCallbackUnbound<T> = procedure(const Value: T);
  TLKGenericCallbackOfObject<T> = procedure(const Value: T) of Object;
  {$IFNDEF SUPPORTS_REFERENCETOMETHOD}
    TLKGenericCallbackAnonymous<T> = reference to procedure(const Value: T);
  {$ENDIF SUPPORTS_REFERENCETOMETHOD}

  { Exception Types }
  ELKException = class(Exception);

  ///  <summary><c>Absolute Base Interface for all LKSL-Defined Interfaces.</c></summary>
  ///  <remarks><c>Provides access to the Lock and Unlock methods.</c></remarks>
  ILKInterface = interface
  ['{CAC7A376-703A-4D55-BFBE-423CCAF8F43A}']
    procedure AcquireReadLock;
    procedure AcquireWriteLock;
    procedure ReleaseReadLock;
    procedure ReleaseWriteLock;
    function TryAcquireReadLock: Boolean;
    function TryAcquireWriteLock: Boolean;
  end;

  ILKReadWriteLock = interface
  ['{7E404157-629D-4FAE-B431-E4E55FB00160}']
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

  {
    TLKPersistent
      - Provides a "Critical Section" (or "Lock") to make members "Thread-Safe"
  }
  TLKPersistent = class(TPersistent)
  private
    FInstanceGUID: TGUID;
    FLock: TLKReadWriteLock;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AcquireReadLock; //inline;
    procedure AcquireWriteLock; //inline;
    procedure ReleaseReadLock; //inline;
    procedure ReleaseWriteLock; //inline;
    function TryAcquireReadLock: Boolean; //inline;
    function TryAcquireWriteLock: Boolean; //inline;

    property InstanceGUID: TGUID read FInstanceGUID;
  end;

  ///  <summary><c>Provies Two-State Locking</c></summary>
  ///  <remarks>
  ///    <c>Three States are:</c>
  ///    <para><c>Read - Holds back Write requests until relinquished.</c></para>
  ///    <para><c>Write - Holds back Read and other Write requests until relinquished.</c></para>
  ///  </remarks>
  TLKReadWriteLock = class(TInterfacedObject, ILKReadWriteLock)
  private
    {$IFDEF LKSL_LOCK_ALLEXCLUSIVE}
      FWriteLock: TCriticalSection;
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

  {
    TLKObject
      - Provides a "Critical Section" (or "Lock") to make members "Thread-Safe"
  }
  TLKObject = class(TObject)
  private
    FInstanceGUID: TGUID;
    FLock: TLKReadWriteLock;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AcquireReadLock; //inline;
    procedure AcquireWriteLock; //inline;
    procedure ReleaseReadLock; //inline;
    procedure ReleaseWriteLock; //inline;
    function TryAcquireReadLock: Boolean; //inline;
    function TryAcquireWriteLock: Boolean; //inline;

    property InstanceGUID: TGUID read FInstanceGUID;
  end;

  TLKInterfacedPersistent = class(TInterfacedPersistent, ILKInterface)
  private
    FInstanceGUID: TGUID;
    FLock: TLKReadWriteLock;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AcquireReadLock; //inline;
    procedure AcquireWriteLock; //inline;
    procedure ReleaseReadLock; //inline;
    procedure ReleaseWriteLock; //inline;
    function TryAcquireReadLock: Boolean; //inline;
    function TryAcquireWriteLock: Boolean; //inline;

    property InstanceGUID: TGUID read FInstanceGUID;
  end;

  TLKInterfacedObject = class(TInterfacedObject, ILKInterface)
  private
    FInstanceGUID: TGUID;
    FLock: TLKReadWriteLock;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AcquireReadLock; //inline;
    procedure AcquireWriteLock; //inline;
    procedure ReleaseReadLock; //inline;
    procedure ReleaseWriteLock; //inline;
    function TryAcquireReadLock: Boolean; //inline;
    function TryAcquireWriteLock: Boolean; //inline;

    property InstanceGUID: TGUID read FInstanceGUID;
  end;

  ///  <summary><c>Compares two values of the defined Generic Type.</c></summary>
  ILKComparer<T> = interface(ILKInterface)
  ['{3E6657DE-65CB-4106-9647-27F3E5BC88D6}']
    function AEqualToB(const A, B: T): Boolean;
    function AGreaterThanB(const A, B: T): Boolean;
    function AGreaterThanOrEqualB(const A, B: T): Boolean;
    function ALessThanB(const A, B: T): Boolean;
    function ALessThanOrEqualB(const A, B: T): Boolean;
  end;

  ///  <summary><c>A simple Reference Counted container.</c></summary>
  ILKHolder<T> = interface(ILKInterface)
  ['{280B501C-E16B-445F-9538-AFBB50301B13}']
    // Getters
    function GetItem: T;
    // Setters
    procedure SetItem(const AItem: T);
    // Properties
    property Item: T read GetItem write SetItem;
  end;

  ///  <summary><c>A Reference Counted Object container.</c></summary>
  ///  <remarks><c>Can take ownership of an Object and automatically destroy it when no references remain.</c></remarks>
  ILKObjectHolder<T: class> = interface(ILKHolder<T>)
  ['{57E82372-C304-449E-BA42-4679B5F15368}']
    // Getters
    function GetOwnsObject: Boolean;
    // Setters
    procedure SetOwnsObject(const AOwnsObject: Boolean);
    // Properties
    property OwnsObject: Boolean read GetOwnsObject write SetOwnsObject;
  end;

  ///  <summary><c>Compares two values of the defined Generic Type.</c></summary>
  TLKComparer<T> = class abstract(TLKInterfacedObject, ILKComparer<T>)
  public
    function AEqualToB(const A, B: T): Boolean; virtual; abstract;
    function AGreaterThanB(const A, B: T): Boolean; virtual; abstract;
    function AGreaterThanOrEqualB(const A, B: T): Boolean; virtual; abstract;
    function ALessThanB(const A, B: T): Boolean; virtual; abstract;
    function ALessThanOrEqualB(const A, B: T): Boolean; virtual; abstract;
  end;

  ILKFloatComparer = ILKComparer<LKFloat>;

  TLKFloatComparer = class(TLKComparer<LKFloat>, ILKFloatComparer)
    function AEqualToB(const A, B: LKFloat): Boolean; override;
    function AGreaterThanB(const A, B: LKFloat): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: LKFloat): Boolean; override;
    function ALessThanB(const A, B: LKFloat): Boolean; override;
    function ALessThanOrEqualB(const A, B: LKFloat): Boolean; override;
  end;

  ///  <summary><c>A simple Reference Counted container.</c></summary>
  TLKHolder<T> = class(TLKInterfacedObject, ILKHolder<T>)
  protected
    FItem: T;
    // Getters
    function GetItem: T;
    // Setters
    procedure SetItem(const AItem: T);
  public
    constructor Create(const AItem: T); reintroduce;
    // Properties
    property Item: T read GetItem write SetItem;
  end;

  ///  <summary><c>A Reference Counted Object container.</c></summary>
  ///  <remarks><c>Can take ownership of an Object and automatically destroy it when no references remain.</c></remarks>
  TLKObjectHolder<T: class> = class(TLKHolder<T>, ILKObjectHolder<T>)
  private
    FOwnsObject: Boolean;
    // Getters
    function GetOwnsObject: Boolean;
    // Setters
    procedure SetOwnsObject(const AOwnsObject: Boolean);
  public
    constructor Create(const AItem: T; const AOwnsObject: Boolean = True); reintroduce;
    destructor Destroy; override;
    // Properties
    property OwnsObject: Boolean read GetOwnsObject write SetOwnsObject;
  end;

  ///  <summary><c>A Thread-Safe Container for Basic Data Types</c></summary>
  ILKThreadSafeType<T> = interface
  ['{AEC2ED7C-4324-4795-B3C8-B2CD9BFB658B}']
    function GetValue: T;
    procedure SetValue(const AValue: T);

    procedure WithRead(const ACallback: TLKGenericCallbackUnbound<T>); overload;
    procedure WithRead(const ACallback: TLKGenericCallbackOfObject<T>); overload;
    {$IFNDEF SUPPORTS_REFERENCETOMETHOD}
      procedure WithRead(const ACallback: TLKGenericCallbackAnonymous<T>); overload;
    {$ENDIF SUPPORTS_REFERENCETOMETHOD}

    procedure WithWrite(const ACallback: TLKGenericCallbackUnbound<T>); overload;
    procedure WithWrite(const ACallback: TLKGenericCallbackOfObject<T>); overload;
    {$IFNDEF SUPPORTS_REFERENCETOMETHOD}
      procedure WithWrite(const ACallback: TLKGenericCallbackAnonymous<T>); overload;
    {$ENDIF SUPPORTS_REFERENCETOMETHOD}

    property Value: T read GetValue write SetValue;
  end;

  ///  <summary><c>A Thread-Safe Container for Basic Data Types</c></summary>
  TLKThreadSafeType<T> = class(TLKInterfacedObject, ILKThreadSafeType<T>)
  private
    FLock: TLKReadWriteLock;
    FValue: T;
    function GetValue: T;
    procedure SetValue(const AValue: T);
  public
    constructor Create; reintroduce; overload;
    constructor Create(const AValue: T); reintroduce; overload;
    destructor Destroy; override;

    procedure WithRead(const ACallback: TLKGenericCallbackUnbound<T>); overload;
    procedure WithRead(const ACallback: TLKGenericCallbackOfObject<T>); overload;
    {$IFNDEF SUPPORTS_REFERENCETOMETHOD}
      procedure WithRead(const ACallback: TLKGenericCallbackAnonymous<T>); overload;
    {$ENDIF SUPPORTS_REFERENCETOMETHOD}

    procedure WithWrite(const ACallback: TLKGenericCallbackUnbound<T>); overload;
    procedure WithWrite(const ACallback: TLKGenericCallbackOfObject<T>); overload;
    {$IFNDEF SUPPORTS_REFERENCETOMETHOD}
      procedure WithWrite(const ACallback: TLKGenericCallbackAnonymous<T>); overload;
    {$ENDIF SUPPORTS_REFERENCETOMETHOD}

    property Value: T read GetValue write SetValue;
  end;

implementation

{ TLKPersistent }

constructor TLKPersistent.Create;
begin
  inherited Create;
  FLock := TLKReadWriteLock.Create;
  CreateGUID(FInstanceGUID);
end;

destructor TLKPersistent.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TLKPersistent.AcquireReadLock;
begin
  FLock.AcquireRead;
end;

procedure TLKPersistent.AcquireWriteLock;
begin
  FLock.AcquireWrite;
end;

procedure TLKPersistent.ReleaseReadLock;
begin
  FLock.ReleaseRead;
end;

procedure TLKPersistent.ReleaseWriteLock;
begin
  FLock.ReleaseWrite;
end;

function TLKPersistent.TryAcquireReadLock: Boolean;
begin
  Result := FLock.TryAcquireRead;
end;

function TLKPersistent.TryAcquireWriteLock: Boolean;
begin
  Result := FLock.TryAcquireWrite;
end;

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
    FWriteLock := TCriticalSection.Create;
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

{ TLKObject }

constructor TLKObject.Create;
begin
  inherited Create;
  FLock := TLKReadWriteLock.Create;
  CreateGUID(FInstanceGUID);
end;

destructor TLKObject.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TLKObject.AcquireReadLock;
begin
  FLock.AcquireRead
end;

procedure TLKObject.AcquireWriteLock;
begin
  FLock.AcquireWrite;
end;

procedure TLKObject.ReleaseReadLock;
begin
  FLock.ReleaseRead;
end;

procedure TLKObject.ReleaseWriteLock;
begin
  FLock.ReleaseWrite;
end;

function TLKObject.TryAcquireReadLock: Boolean;
begin
  Result := FLock.TryAcquireRead;
end;

function TLKObject.TryAcquireWriteLock: Boolean;
begin
  Result := FLock.TryAcquireWrite;
end;

{ TLKInterfacedPersistent }

constructor TLKInterfacedPersistent.Create;
begin
  inherited Create;
  FLock := TLKReadWriteLock.Create;
  CreateGUID(FInstanceGUID);
end;

destructor TLKInterfacedPersistent.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TLKInterfacedPersistent.AcquireReadLock;
begin
  FLock.AcquireRead;
end;

procedure TLKInterfacedPersistent.AcquireWriteLock;
begin
  FLock.AcquireWrite;
end;

procedure TLKInterfacedPersistent.ReleaseReadLock;
begin
  FLock.ReleaseRead;
end;

procedure TLKInterfacedPersistent.ReleaseWriteLock;
begin
  FLock.ReleaseWrite;
end;

function TLKInterfacedPersistent.TryAcquireReadLock: Boolean;
begin
  Result := FLock.TryAcquireRead;
end;

function TLKInterfacedPersistent.TryAcquireWriteLock: Boolean;
begin
  Result := FLock.TryAcquireWrite;
end;

{ TLKInterfacedObject }

constructor TLKInterfacedObject.Create;
begin
  inherited Create;
  FLock := TLKReadWriteLock.Create;
  CreateGUID(FInstanceGUID);
end;

destructor TLKInterfacedObject.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TLKInterfacedObject.AcquireReadLock;
begin
  FLock.AcquireRead;
end;

procedure TLKInterfacedObject.AcquireWriteLock;
begin
  FLock.AcquireWrite;
end;

procedure TLKInterfacedObject.ReleaseReadLock;
begin
  FLock.ReleaseRead;
end;

procedure TLKInterfacedObject.ReleaseWriteLock;
begin
  FLock.ReleaseWrite;
end;

function TLKInterfacedObject.TryAcquireReadLock: Boolean;
begin
  Result := FLock.TryAcquireRead;
end;

function TLKInterfacedObject.TryAcquireWriteLock: Boolean;
begin
  Result := FLock.TryAcquireWrite;
end;

{ TLKFloatComparer }

function TLKFloatComparer.AEqualToB(const A, B: LKFloat): Boolean;
begin
  Result := (A = B);
end;

function TLKFloatComparer.AGreaterThanB(const A, B: LKFloat): Boolean;
begin
  Result := (A > B);
end;

function TLKFloatComparer.AGreaterThanOrEqualB(const A, B: LKFloat): Boolean;
begin
  Result := (A >= B);
end;

function TLKFloatComparer.ALessThanB(const A, B: LKFloat): Boolean;
begin
  Result := (A < B);
end;

function TLKFloatComparer.ALessThanOrEqualB(const A, B: LKFloat): Boolean;
begin
  Result := (A <= B);
end;

{ TLKHolder<T> }

constructor TLKHolder<T>.Create(const AItem: T);
begin
  inherited Create;
  FItem := AItem;
end;

function TLKHolder<T>.GetItem: T;
begin
  AcquireReadLock;
  try
    Result := FItem;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKHolder<T>.SetItem(const AItem: T);
begin
  AcquireWriteLock;
  try
    FItem := AItem;
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKObjectHolder<T> }

constructor TLKObjectHolder<T>.Create(const AItem: T; const AOwnsObject: Boolean = True);
begin
  inherited Create(AItem);
  FOwnsObject := AOwnsObject;
end;

destructor TLKObjectHolder<T>.Destroy;
begin
  if FOwnsObject then
    FItem.{$IFDEF SUPPORTS_DISPOSEOF}DisposeOf{$ELSE}Free{$ENDIF SUPPORTS_DISPOSEOF};
  inherited;
end;

function TLKObjectHolder<T>.GetOwnsObject: Boolean;
begin
  AcquireReadLock;
  try
    Result := FOwnsObject;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKObjectHolder<T>.SetOwnsObject(const AOwnsObject: Boolean);
begin
  AcquireWriteLock;
  try
    FOwnsObject := AOwnsObject;
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKThreadSafeType<T> }

constructor TLKThreadSafeType<T>.Create;
begin
  inherited Create;
  FLock := TLKReadWriteLock.Create;
end;

constructor TLKThreadSafeType<T>.Create(const AValue: T);
begin
  Create;
  FValue := AValue;
end;

destructor TLKThreadSafeType<T>.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TLKThreadSafeType<T>.GetValue: T;
begin
  FLock.AcquireRead;
  try
    Result := FValue;
  finally
    FLock.ReleaseRead;
  end;
end;

procedure TLKThreadSafeType<T>.SetValue(const AValue: T);
begin
  FLock.AcquireWrite;
  try
    FValue := AValue;
  finally
    FLock.ReleaseWrite;
  end;
end;

procedure TLKThreadSafeType<T>.WithRead(const ACallback: TLKGenericCallbackUnbound<T>);
begin
  FLock.AcquireRead;
  try
    ACallback(FValue);
  finally
    FLock.ReleaseRead;
  end;
end;

procedure TLKThreadSafeType<T>.WithRead(const ACallback: TLKGenericCallbackOfObject<T>);
begin
  FLock.AcquireRead;
  try
    ACallback(FValue);
  finally
    FLock.ReleaseRead;
  end;
end;

{$IFNDEF SUPPORTS_REFERENCETOMETHOD}
  procedure TLKThreadSafeType<T>.WithRead(const ACallback: TLKGenericCallbackAnonymous<T>);
  begin
    FLock.AcquireRead;
    try
      ACallback(FValue);
    finally
      FLock.ReleaseRead;
    end;
  end;
{$ENDIF SUPPORTS_REFERENCETOMETHOD}

procedure TLKThreadSafeType<T>.WithWrite(const ACallback: TLKGenericCallbackUnbound<T>);
begin
  FLock.AcquireWrite;
  try
    ACallback(FValue);
  finally
    FLock.ReleaseWrite;
  end;
end;

procedure TLKThreadSafeType<T>.WithWrite(const ACallback: TLKGenericCallbackOfObject<T>);
begin
  FLock.AcquireWrite;
  try
    ACallback(FValue);
  finally
    FLock.ReleaseWrite;
  end;
end;

{$IFNDEF SUPPORTS_REFERENCETOMETHOD}
  procedure TLKThreadSafeType<T>.WithWrite(const ACallback: TLKGenericCallbackAnonymous<T>);
  begin
    FLock.AcquireWrite;
    try
      ACallback(FValue);
    finally
      FLock.ReleaseWrite;
    end;
  end;
{$ENDIF SUPPORTS_REFERENCETOMETHOD}

end.
