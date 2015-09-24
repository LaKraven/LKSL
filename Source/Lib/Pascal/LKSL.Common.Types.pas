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

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs,
  {$ELSE}
    Classes, SysUtils, SyncObjs,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.SyncObjs;

  {$I LKSL_RTTI.inc}

type
  { Interface Forward Declarations }
  ILKInterface = interface;
  { Class Forward Declarations }
  TLKPersistent = class;
  TLKObject = class;
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
