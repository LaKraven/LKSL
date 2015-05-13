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
unit LKSL.Generics.CollectionsRedux;

//TODO -oSJS -cGenerics Redux: Reintegrate this unit as LKSL.Generics.Collections.pas

interface

{$I LKSL.inc}

{
  About this unit:
    - This unit provides useful enhancements for Generics types used in the LKSL.
}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs,
  {$ELSE}
    Classes, SysUtils, SyncObjs,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  {$IFNDEF FPC}
    Generics.Defaults,
    Generics.Collections,
  {$ELSE}
    fgl,
  {$ENDIF FPC}
  LKSL.Common.Types, LKSL.Common.SyncObjs,
  LKSL.Generics.Defaults;

  {$I LKSL_RTTI.inc}

type
  {$IFDEF FPC}
    TArray<T> = Array of T; // FreePascal doesn't have this defined by default (yet)
  {$ELSE}
    { Interface Forward Declarations }
    ILKArray<T> = interface;
    ILKArrayContainer<T> = interface;
    ILKComparer<T> = interface;
    ILKListSorter<T> = interface;
    ILKListExpander<T> = interface;
    ILKListCompactor<T> = interface;
    ILKList<T> = interface;
    { Class Forward Declarations }
    TLKArray<T> = class;
    TLKArrayContainer<T> = class;
    TLKComparer<T> = class;
    TLKListSorter<T> = class;
    TLKListExpander<T> = class;
    TLKListCompactor<T> = class;
    TLKList<T> = class;
  {$ENDIF FPC}

  { Exception Types }
  ELKGenericCollectionsException = class(ELKException);
    ELKGenericCollectionsLimitException = class(ELKGenericCollectionsException);
    ELKGenericCollectionsRangeException = class(ELKGenericCollectionsException);
    ELKGenericCollectionsKeyAlreadyExists = class(ELKGenericCollectionsException);
    ELKGenericCollectionsKeyNotFound = class(ELKGenericCollectionsException);

  ///  <summary><c>A Simple Generic Array with basic Management Methods.</c></summary>
  ILKArray<T> = interface(ILKInterface)
  ['{8950BA38-870B-49F8-92A5-59D30E8F1DDB}']
    // Getters
    function GetCapacity: Integer;
    function GetItem(const AIndex: Integer): T;
    // Setters
    procedure SetCapacity(const ACapacity: Integer);
    procedure SetItem(const AIndex: Integer; const AItem: T);
    // Management Methods
    procedure Clear;
    procedure Finalize(const AIndex, ACount: Integer);
    procedure Move(const AFromIndex, AToIndex, ACount: Integer);
    // Properties
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Items[const AIndex: Integer]: T read GetItem write SetItem;
  end;

  ILKArrayContainer<T> = interface(ILKInterface)
  ['{9E6880CA-D55C-4166-9B72-45CCA6900488}']
    function GetArray: ILKArray<T>;
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

  ILKListSorter<T> = interface(ILKInterface)
  ['{2644E14D-A7C9-44BC-B8DD-109EC0C0A0D1}']
    // Getters
    function GetComparer: ILKComparer<T>;
    // Setters
    procedure SetComparer(const AComparer: ILKComparer<T>);
    // Properties
    property Comparer: ILKComparer<T> read GetComparer write SetComparer;
  end;

  ILKListExpander<T> = interface(ILKInterface)
  ['{9B4D9541-96E4-4767-81A7-5565AC24F4A9}']

  end;

  ILKListCompactor<T> = interface(ILKInterface)
  ['{B72ECE0C-F629-4002-A84A-2F7FAEC122E0}']

  end;

  ILKList<T> = interface(ILKInterface)
  ['{FD2E0742-9079-4E03-BDA5-A39D5FAC80A0}']
    // Getters
    function GetCompactor: ILKListCompactor<T>;
    function GetExpander: ILKListExpander<T>;
    function GetSorter: ILKListSorter<T>;
    // Setters
    procedure SetCompactor(const ACompactor: ILKListCompactor<T>);
    procedure SetExpander(const AExpander: ILKListExpander<T>);
    procedure SetSorter(const ASorter: ILKListSorter<T>);
    // Properties
    property Compactor: ILKListCompactor<T> read GetCompactor write SetCompactor;
    property Expander: ILKListExpander<T> read GetExpander write SetExpander;
    property Sorter: ILKListSorter<T> read GetSorter write SetSorter;
  end;

  ///  <summary><c>A Simple Generic Array with basic Management Methods.</c></summary>
  TLKArray<T> = class(TLKInterfacedObject, ILKArray<T>)
  private
    FArray: TArray<T>;
    FCapacityInitial: Integer;
    // Getters
    function GetCapacity: Integer;
    function GetItem(const AIndex: Integer): T;
    // Setters
    procedure SetCapacity(const ACapacity: Integer);
    procedure SetItem(const AIndex: Integer; const AItem: T);
  public
    constructor Create(const ACapacity: Integer = 0); reintroduce;
    // Management Methods
    procedure Clear;
    procedure Finalize(const AIndex, ACount: Integer);
    procedure Move(const AFromIndex, AToIndex, ACount: Integer);
    // Properties
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Items[const AIndex: Integer]: T read GetItem write SetItem;
  end;

  TLKArrayContainer<T> = class(TLKInterfacedObject, ILKArrayContainer<T>)
  protected
    FArray: ILKArray<T>;
    function GetArray: ILKArray<T>;
  public
    constructor Create(const AArray: ILKArray<T>); reintroduce;
  end;

  TLKComparer<T> = class abstract(TLKInterfacedObject, ILKComparer<T>)
  public
    function AEqualToB(const A, B: T): Boolean; virtual; abstract;
    function AGreaterThanB(const A, B: T): Boolean; virtual; abstract;
    function AGreaterThanOrEqualB(const A, B: T): Boolean; virtual; abstract;
    function ALessThanB(const A, B: T): Boolean; virtual; abstract;
    function ALessThanOrEqualB(const A, B: T): Boolean; virtual; abstract;
  end;

  TLKListSorter<T> = class abstract(TLKArrayContainer<T>, ILKListSorter<T>)
  private
    FComparer: ILKComparer<T>;
    // Getters
    function GetComparer: ILKComparer<T>;
    // Setters
    procedure SetComparer(const AComparer: ILKComparer<T>);
  public
    // Properties
    property Comparer: ILKComparer<T> read GetComparer write SetComparer;
  end;

  TLKListExpander<T> = class abstract(TLKArrayContainer<T>, ILKListExpander<T>)

  end;

  TLKListCompactor<T> = class abstract(TLKArrayContainer<T>, ILKListCompactor<T>)

  end;

  TLKList<T> = class abstract(TLKInterfacedObject, ILKList<T>)
  private
    FArray: ILKArray<T>;
    FCompactor: ILKListCompactor<T>;
    FExpander: ILKListExpander<T>;
    FSorter: ILKListSorter<T>;

    // Getters
    function GetCompactor: ILKListCompactor<T>;
    function GetExpander: ILKListExpander<T>;
    function GetSorter: ILKListSorter<T>;

    // Setters
    procedure SetCompactor(const ACompactor: ILKListCompactor<T>);
    procedure SetExpander(const AExpander: ILKListExpander<T>);
    procedure SetSorter(const ASorter: ILKListSorter<T>);
  public
    constructor Create(const ACapacity: Integer = 0); reintroduce;
    destructor Destroy; override;

    // Properties
    property Compactor: ILKListCompactor<T> read GetCompactor write SetCompactor;
    property Expander: ILKListExpander<T> read GetExpander write SetExpander;
    property Sorter: ILKListSorter<T> read GetSorter write SetSorter;
  end;

implementation

{ TLKArray<T> }

procedure TLKArray<T>.Clear;
begin
  SetLength(FArray, FCapacityInitial);
end;

constructor TLKArray<T>.Create(const ACapacity: Integer);
begin
  inherited Create;
  FCapacityInitial := ACapacity;
  SetLength(FArray, ACapacity);
end;

procedure TLKArray<T>.Finalize(const AIndex, ACount: Integer);
begin
  AcquireWriteLock;
  try
    System.FillChar(FArray[AIndex], ACount * SizeOf(T), 0);
  finally
    ReleaseWriteLock;
  end;
end;

function TLKArray<T>.GetCapacity: Integer;
begin
  AcquireReadLock;
  try
    Result := Length(FArray);
  finally
    ReleaseReadLock;
  end;
end;

function TLKArray<T>.GetItem(const AIndex: Integer): T;
begin
  AcquireReadLock;
  try
    Result := FArray[AIndex];
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKArray<T>.Move(const AFromIndex, AToIndex, ACount: Integer);
begin
  AcquireWriteLock;
  try
    System.Move(FArray[AFromIndex], FArray[AToIndex], ACount * SizeOf(T));
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKArray<T>.SetCapacity(const ACapacity: Integer);
begin
  AcquireWriteLock;
  try
    SetLength(FArray, ACapacity);
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKArray<T>.SetItem(const AIndex: Integer; const AItem: T);
begin
  AcquireWriteLock;
  try
    FArray[AIndex] := AItem;
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKArrayContainer<T> }

constructor TLKArrayContainer<T>.Create(const AArray: ILKArray<T>);
begin
  FArray := AArray;
end;

function TLKArrayContainer<T>.GetArray: ILKArray<T>;
begin
  Result := FArray;
end;

{ TLKListSorter<T> }

function TLKListSorter<T>.GetComparer: ILKComparer<T>;
begin
  AcquireReadLock;
  try
    Result := FComparer;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKListSorter<T>.SetComparer(const AComparer: ILKComparer<T>);
begin
  AcquireWriteLock;
  try
    FComparer := AComparer;
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKListBase<T> }

constructor TLKList<T>.Create(const ACapacity: Integer);
begin
  inherited Create;
  FArray.Capacity := ACapacity;
end;

destructor TLKList<T>.Destroy;
begin

  inherited;
end;

function TLKList<T>.GetCompactor: ILKListCompactor<T>;
begin
  AcquireReadLock;
  try
    Result := FCompactor;
  finally
    ReleaseReadLock;
  end;
end;

function TLKList<T>.GetExpander: ILKListExpander<T>;
begin
  AcquireReadLock;
  try
    Result := FExpander;
  finally
    ReleaseReadLock;
  end;
end;

function TLKList<T>.GetSorter: ILKListSorter<T>;
begin
  AcquireReadLock;
  try
    Result := FSorter;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKList<T>.SetCompactor(const ACompactor: ILKListCompactor<T>);
begin
  AcquireWriteLock;
  try
    FCompactor := ACompactor;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKList<T>.SetExpander(const AExpander: ILKListExpander<T>);
begin
  AcquireWriteLock;
  try
    FExpander := AExpander;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKList<T>.SetSorter(const ASorter: ILKListSorter<T>);
begin
  AcquireWriteLock;
  try
    FSorter := ASorter;
  finally
    ReleaseWriteLock;
  end;
end;

end.
