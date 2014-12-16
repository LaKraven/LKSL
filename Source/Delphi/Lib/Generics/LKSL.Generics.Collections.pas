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
unit LKSL.Generics.Collections;

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
  Generics.Defaults,
  Generics.Collections,
  LKSL.Common.Types;

  {$I LKSL_RTTI.inc}

type
  { Forward Declaration }
  TLKArray<T> = class;
  TLKDictionary<TKey, TValue> = class;
  TLKList<T> = class;
  TLKObjectList<T: class> = class;
  TLKCenteredList<T> = class;
  TLKTreeNode<T> = class;

  { Enum Types }
  TLKListDirection = (ldLeft, ldRight);

  { Exception Types }
  ELKGenericCollectionsException = class(ELKException);
    ELKGenericCollectionsLimitException = class(ELKGenericCollectionsException);
    ELKGenericCollectionsRangeException = class(ELKGenericCollectionsException);

  {
    TLKArray<T>
      - Provides a Thread-Safe Lock (TCriticalSection)
      - A very simple "Managed Array" for items of the nominated type.
      - Items are UNSORTED
      - The Array expands by 1 each time a new item is added
      - The Array collapses by 1 each time an item is removed
      - If referencing the "ArrayRaw" property, don't forget to call "Lock" and "Unlock" manually!
  }
  TLKArray<T> = class(TLKPersistent)
  type
    TLKArrayType = Array of T;
  private
    FArray: TLKArrayType;
    FLock: TCriticalSection;

    // Getters
    function GetItem(const AIndex: Integer): T;

    // Setters
    procedure SetItem(const AIndex: Integer; const AItem: T);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure Unlock; inline;

    function Add(const AItem: T): Integer;
    procedure Clear; inline;
    procedure Delete(const AIndex: Integer); virtual;

    property ArrayRaw: TLKArrayType read FArray;
    property Items[const AIndex: Integer]: T read GetItem write SetItem;
  end;

  {
    TLKDictionary<TKey, TValue>
      - Provides a Thread-Safe Lock (TCriticalSection)
  }
  TLKDictionary<TKey, TValue> = class(TDictionary<TKey, TValue>)
  private
    FLock: TCriticalSection;
  public
    constructor Create(ACapacity: Integer = 0); reintroduce; overload;
    constructor Create(const AComparer: IEqualityComparer<TKey>); reintroduce; overload;
    constructor Create(ACapacity: Integer; const AComparer: IEqualityComparer<TKey>); reintroduce; overload;
    constructor Create(const Collection: TEnumerable<TPair<TKey,TValue>>); reintroduce; overload;
    constructor Create(const Collection: TEnumerable<TPair<TKey,TValue>>; const AComparer: IEqualityComparer<TKey>); reintroduce; overload;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure Unlock; inline;
  end;

  {
    TLKList<T>
      - Provides a Thread-Safe Lock (TCriticalSection)
  }
  TLKList<T> = class(TList<T>)
  private
    FLock: TCriticalSection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure Unlock; inline;
  end;

  {
    TLKObjectList<T: class>
      - Provides a Thread-Safe Lock (TCriticalSection)
  }
  TLKObjectList<T: class> = class(TObjectList<T>)
  private
    FLock: TCriticalSection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure Unlock; inline;
  end;

  {
    TLKCenteredList
      - A special Generic List object which allows the use of both positive and NEGATIVE indices!
      - Essentially, we're cheating: using two Arrays (and a singular instance to represent index 0)
        - Any LEFT (negative) assignment goes on the end of the LEFT Array, any RIGHT (positive)
          assignment goes on the end of the RIGHT Array.
        - In a sense, this means you have a combined Queue AND Stack acting like a singular Array.
  }
  TLKCenteredList<T> = class(TPersistent)
  private
    type
      TArrayOfT = Array of T;
  private
    // Counts
    FCountLeft: Integer;
    FCountRight: Integer;
    // Locks
    FLockCenter: TCriticalSection;
    FLockLeft: TCriticalSection;
    FLockRight: TCriticalSection;
    // Arrays (and Center)
    FArrayLeft: TArrayOfT;
    FArrayRight: TArrayOfT;
    FCenter: T;
    // Capacity
    FCapacityMultiplier: Single;
    FCapacityThreshold: Integer;
    // Necessary Other Stuff
    FCenterAssigned: Boolean;

    // Capacity Checks
    procedure CheckCapacityLeft;
    procedure CheckCapacityRight;
    // Deletes
    procedure DeleteCenter;
    procedure DeleteLeft(const AIndex: Integer);
    procedure DeleteRight(const AIndex: Integer);
    // Capacity Getters
    function GetCapacity: Integer; // GetCapacityLeft + GetCapacityRight + 1
    function GetCapacityLeft: Integer; // Size of FArrayLeft
    function GetCapacityRight: Integer; // Size of FArrayRight
    function GetCapacityMultiplier: Single;
    function GetCapacityThreshold: Integer;
    // Count Getters
    function GetCount: Integer; // GetCountLeft + GetCountRight + FCenterAssigned
    function GetCountLeft: Integer; // Number of Items in FArrayLeft
    function GetCountRight: Integer; // Number of Items in FArrayRight
    // Item Getters
    function GetItemByIndex(const AIndex: Integer): T;
    function GetItemCenter: T;
    function GetItemLeft(const AIndex: Integer): T;
    function GetItemRight(const AIndex: Integer): T;
    // Range Getters
    function GetRangeLow: Integer;
    function GetRangeHigh: Integer;
    // Insert Methods
    procedure InsertCenter(const AItem: T; const AShiftDirection: TLKListDirection = ldRight);
    procedure InsertLeft(const AItem: T; const AIndex: Integer; const AShiftDirection: TLKListDirection = ldRight);
    procedure InsertRight(const AItem: T; const AIndex: Integer; const AShiftDirection: TLKListDirection = ldRight);
    // General
    function TryCenterAvailable(const AItem: T): Boolean;

    // Capacity Setters
    procedure SetCapacityMultiplier(const AMultiplier: Single);
    procedure SetCapacityThreshold(const AThreshold: Integer);
    // Item Setters
    procedure SetItemByIndex(const AIndex: Integer; const AItem: T);
    procedure SetItemCenter(const AItem: T);
    procedure SetItemLeft(const AIndex: Integer; const AItem: T);
    procedure SetItemRight(const AIndex: Integer; const AItem: T);
  protected
    procedure Finalize(var AArray: TArrayOfT; const AIndex, ACount: Integer);
    procedure Move(var AArray: TArrayOfT; const AFromIndex, AToIndex, ACount: Integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Add(const AItem: T; const ADirection: TLKListDirection = ldRight): Integer; overload; virtual;
    procedure Add(const AItems: TArrayOfT; const ADirection: TLKListDirection = ldRight); overload; virtual;
    function AddLeft(const AItem: T): Integer; overload; virtual;
    procedure AddLeft(const AItems: TArrayOfT); overload;
    function AddRight(const AItem: T): Integer; overload; virtual;
    procedure AddRight(const AItems: TArrayOfT); overload;


    procedure Compact;

    procedure Delete(const AIndex: Integer); overload; virtual;
    procedure Delete(const AIndices: Array of Integer); overload; virtual;
    procedure DeleteRange(const ALow, AHigh: Integer); virtual;

    procedure Insert(const AItem: T; const AIndex: Integer; const AShiftDirection: TLKListDirection = ldRight);

    procedure Lock; inline;
    procedure LockCenter; inline;
    procedure LockLeft; inline;
    procedure LockRight; inline;

    procedure Unlock; inline;
    procedure UnlockCenter; inline;
    procedure UnlockLeft; inline;
    procedure UnlockRight; inline;

    procedure Swap(const AFrom, ATo: Integer);

    // Capacity Properties
    property Capacity: Integer read GetCapacity;
    property CapacityLeft: Integer read GetCapacityLeft;
    property CapacityRight: Integer read GetCapacityRight;
    property CapacityMultiplier: Single read GetCapacityMultiplier write SetCapacityMultiplier;
    property CapacityThreshold: Integer read GetCapacityThreshold write SetCapacityThreshold;
    // Count Properties
    property Count: Integer read GetCount;
    property CountLeft: Integer read GetCountLeft;
    property CountRight: Integer read GetCountRight;
    // Item Properties
    property Items[const AIndex: Integer]: T read GetItemByIndex write SetItemByIndex; default;
    // Range Properties
    property Low: Integer read GetRangeLow;
    property High: Integer read GetRangeHigh;
  end;


  {
    TLKTreeNode<T>
      - A special Generic Tree Object with thread-safe Locks
      - Acts as both the Root and Child node (dependant on whether or not it contains a Parent)
  }
  TLKTreeNode<T> = class(TLKObject)
  private type
    TLKValueCallback<V> = reference to procedure(const Value: V);
  private
    FParent: TLKTreeNode<T>;
    FChildren: TLKObjectList<TLKTreeNode<T>>;
    FValue: T;

    FDestroying: Boolean;

    procedure DoAncestorChanged;

    function GetRootNode: TLKTreeNode<T>;
    function GetChildCount: Integer; inline;
    function GetChildren(const AIndex: Integer): TLKTreeNode<T>; inline;
    function GetIndexAsChild: Integer; inline;

    function GetIsBranch: Boolean; inline;
    function GetIsRoot: Boolean; inline;
    function GetIsLeaf: Boolean; inline;
    function GetDepth: Integer;
  protected
    procedure AncestorChanged; virtual;
    procedure Destroying; virtual;

    procedure AddChild(const AIndex: Integer; const AChild: TLKTreeNode<T>); virtual;
    procedure RemoveChild(const AChild: TLKTreeNode<T>); virtual;

    procedure SetValue(const AValue: T); virtual;

    property IsDestroying: Boolean read FDestroying;
  public
    constructor Create(const AParent: TLKTreeNode<T>; const AValue: T); reintroduce; overload;
    constructor Create(const AParent: TLKTreeNode<T>); reintroduce; overload;
    constructor Create(const AValue: T); reintroduce; overload;
    constructor Create; overload; override;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure MoveTo(const ANewParent: TLKTreeNode<T>; const AIndex: Integer = -1); overload;
    procedure MoveTo(const AIndex: Integer); overload; inline;

    function IndexOf(const AChild: TLKTreeNode<T>): Integer; inline;

    procedure PreOrderWalk(const AAction: TLKValueCallback<TLKTreeNode<T>>); overload;
    procedure PreOrderWalk(const AAction: TLKValueCallback<T>); overload;

    procedure PostOrderWalk(const AAction: TLKValueCallback<TLKTreeNode<T>>); overload;
    procedure PostOrderWalk(const AAction: TLKValueCallback<T>); overload;

    property Depth: Integer read GetDepth;

    property Parent: TLKTreeNode<T> read FParent;
    property RootNode: TLKTreeNode<T> read GetRootNode;
    property ChildCount: Integer read GetChildCount;
    property Children[const AIndex: Integer]: TLKTreeNode<T> read GetChildren; default;
    property IndexAsChild: Integer read GetIndexAsChild;

    property IsBranch: Boolean read GetIsBranch;
    property IsRoot: Boolean read GetIsRoot;
    property IsLeaf: Boolean read GetIsLeaf;

    property Value: T read FValue write SetValue;
  end;

implementation

{ TLKArray<T> }

function TLKArray<T>.Add(const AItem: T): Integer;
begin
  Result := Length(FArray);
  SetLength(FArray, Result + 1);
  FArray[Result] := AItem;
end;

procedure TLKArray<T>.Clear;
begin
  SetLength(FArray, 0);
end;

constructor TLKArray<T>.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
end;

procedure TLKArray<T>.Delete(const AIndex: Integer);
var
  I: Integer;
begin
  Lock;
  try
    for I := AIndex to Length(FArray) - 2 do
      FArray[I] := FArray[I + 1];

    SetLength(FArray, Length(FArray) - 1);
  finally
    Unlock;
  end;
end;

destructor TLKArray<T>.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TLKArray<T>.GetItem(const AIndex: Integer): T;
begin
  Lock;
  try
    Result := FArray[AIndex];
  finally
    Unlock;
  end;
end;

procedure TLKArray<T>.Lock;
begin
  FLock.Acquire;
end;

procedure TLKArray<T>.SetItem(const AIndex: Integer; const AItem: T);
begin
  Lock;
  try
    FArray[AIndex] := AItem;
  finally
    Unlock;
  end;
end;

procedure TLKArray<T>.Unlock;
begin
  FLock.Release;
end;

{ TLKDictionary<TKey, TValue> }

constructor TLKDictionary<TKey, TValue>.Create(const AComparer: IEqualityComparer<TKey>);
begin
  FLock := TCriticalSection.Create;
  inherited Create(AComparer);
end;

constructor TLKDictionary<TKey, TValue>.Create(ACapacity: Integer);
begin
  FLock := TCriticalSection.Create;
  inherited Create(ACapacity);
end;

constructor TLKDictionary<TKey, TValue>.Create(ACapacity: Integer; const AComparer: IEqualityComparer<TKey>);
begin
  FLock := TCriticalSection.Create;
  inherited Create(ACapacity, AComparer);
end;

constructor TLKDictionary<TKey, TValue>.Create(const Collection: TEnumerable<TPair<TKey, TValue>>; const AComparer: IEqualityComparer<TKey>);
begin
  FLock := TCriticalSection.Create;
  inherited Create(Collection, AComparer);
end;

constructor TLKDictionary<TKey, TValue>.Create(const Collection: TEnumerable<TPair<TKey, TValue>>);
begin
  FLock := TCriticalSection.Create;
  inherited Create(Collection);
end;

destructor TLKDictionary<TKey, TValue>.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TLKDictionary<TKey, TValue>.Lock;
begin
  FLock.Acquire;
end;

procedure TLKDictionary<TKey, TValue>.Unlock;
begin
  FLock.Release;
end;

{ TLKList<T> }

constructor TLKList<T>.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TLKList<T>.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TLKList<T>.Lock;
begin
  FLock.Acquire;
end;

procedure TLKList<T>.Unlock;
begin
  FLock.Release;
end;

{ TLKObjectList<T> }

constructor TLKObjectList<T>.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TLKObjectList<T>.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TLKObjectList<T>.Lock;
begin
  FLock.Acquire;
end;

procedure TLKObjectList<T>.Unlock;
begin
  FLock.Release;
end;

{ TLKCenteredList<T> }

function TLKCenteredList<T>.Add(const AItem: T; const ADirection: TLKListDirection): Integer;
begin
  case ADirection of
    ldLeft: Result := AddLeft(AItem);
    ldRight: Result := AddRight(Aitem);
  end;
end;

procedure TLKCenteredList<T>.Add(const AItems: TArrayOfT; const ADirection: TLKListDirection);
var
  I: Integer;
begin
  for I := System.Low(AItems) to System.High(AItems) do
    Add(AItems[I], ADirection);
end;

function TLKCenteredList<T>.AddLeft(const AItem: T): Integer;
begin
  if (TryCenterAvailable(AItem)) then
    Result := 0
  else
  begin
    LockLeft;
    try
      Result := FCountLeft;
      FArrayLeft[Result] := AItem;
      Inc(FCountLeft);
      CheckCapacityLeft;
    finally
      UnlockLeft;
    end;
  end;
end;

procedure TLKCenteredList<T>.AddLeft(const AItems: TArrayOfT);
var
  I: Integer;
begin
  for I := System.Low(AItems) to System.High(AItems) do
    AddLeft(AItems[I]);
end;

function TLKCenteredList<T>.AddRight(const AItem: T): Integer;
begin
  if (TryCenterAvailable(AItem)) then
    Result := 0
  else
  begin
    LockRight;
    try
      Result := FCountRight;
      FArrayRight[Result] := AItem;
      Inc(FCountRight);
      CheckCapacityRight;
    finally
      UnlockRight;
    end;
  end;
end;

procedure TLKCenteredList<T>.AddRight(const AItems: TArrayOfT);
var
  I: Integer;
begin
  for I := System.Low(AItems) to System.High(AItems) do
    AddRight(AItems[I]);
end;

procedure TLKCenteredList<T>.CheckCapacityLeft;
begin
  LockLeft;
  try
    if ((Length(FArrayLeft) - FCountLeft) < FCapacityThreshold) then
      SetLength(FArrayLeft, Round(Length(FArrayLeft) * FCapacityMultiplier));
  finally
    UnlockLeft;
  end;
end;

procedure TLKCenteredList<T>.CheckCapacityRight;
begin
  LockRight;
  try
    if ((Length(FArrayRight) - FCountRight) < FCapacityThreshold) then
      SetLength(FArrayRight, Round(Length(FArrayRight) * FCapacityMultiplier));
  finally
    UnlockRight;
  end;
end;

procedure TLKCenteredList<T>.Compact;
begin
  Lock;
  try
    SetLength(FArrayLeft, (Length(FArrayLeft) - FCountLeft) + FCapacityThreshold + 1);
    SetLength(FArrayRight, (Length(FArrayRight) - FCountRight) + FCapacityThreshold + 1);
  finally
    Unlock;
  end;
end;

constructor TLKCenteredList<T>.Create;
begin
  inherited Create;
  FCenterAssigned := False;
  FCountLeft := 0;
  FCountRight := 0;
  FLockLeft := TCriticalSection.Create;
  FLockRight := TCriticalSection.Create;
  FLockCenter := TCriticalSection.Create;
  // We default the Capacity of the Left and Right Arrays to 10 each.
  // This equates to 21 default Capacity (including Center)
  SetLength(FArrayLeft, 10);
  SetLength(FArrayRight, 10);
  FCapacityThreshold := 5;
  FCapacityMultiplier := 1.5;
end;

procedure TLKCenteredList<T>.Delete(const AIndex: Integer);
begin
  if AIndex = 0 then
    DeleteCenter
  else if AIndex < 0 then
    DeleteLeft((-AIndex) - 1) // Invert AIndex to provide a positive index
  else
    DeleteRight(AIndex - 1);
end;

procedure TLKCenteredList<T>.Delete(const AIndices: array of Integer);
var
  I: Integer;
begin
  Lock;
  try
    for I := System.Low(AIndices) to System.High(AIndices) do
      Delete(AIndices[I]);
  finally
    Unlock;
  end;
end;

procedure TLKCenteredList<T>.DeleteCenter;
var
  I: Integer;
begin
  LockCenter;
  LockRight;
  try
    if not (FCenterAssigned) then
      raise ELKGenericCollectionsRangeException.Create('Index Out of Bounds: 0')
    else begin
      if FCountRight > 0 then
      begin
        FCenter := FArrayRight[0];
        FCenterAssigned := True;
        if FCountRight > 1 then
        begin
          Dec(FCountRight);
          Move(FArrayRight, 1, 0, FCountRight);
          Finalize(FArrayRight, FCountRight, 1);
        end else
        begin
          Dec(FCountRight);
          Finalize(FArrayRight, 0, 1); // Finalize the Right Array
        end;
      end else
      begin
        System.FillChar(FCenter, SizeOf(T), 0); // Finalize the Center
        FCenterAssigned := False;
      end;
    end;
  finally
    UnlockCenter;
    UnlockRight;
  end;
end;

procedure TLKCenteredList<T>.DeleteLeft(const AIndex: Integer);
var
  I: Integer;
begin
  LockLeft;
  try
    if AIndex < FCountLeft then
    begin
      Dec(FCountLeft);
      Move(FArrayLeft, AIndex + 1, AIndex, FCountLeft - AIndex);
      Finalize(FArrayLeft, FCountLeft, 1);
    end else
      raise ELKGenericCollectionsRangeException.CreateFmt('Index Out of Bounds: %d', [-(AIndex + 1)]);
  finally
    UnlockLeft;
  end;
end;

procedure TLKCenteredList<T>.DeleteRange(const ALow, AHigh: Integer);
var
  I, LOffset: Integer;
begin
  LOffset := 0;
  Lock;
  try
    for I := ALow to AHigh do
    begin
      Delete(I - LOffset);
      Inc(LOffset);
    end;
  finally
    Unlock;
  end;
end;

procedure TLKCenteredList<T>.DeleteRight(const AIndex: Integer);
var
  I: Integer;
begin
  LockRight;
  try
    if AIndex < FCountRight then
    begin
      Dec(FCountRight);
      Move(FArrayRight, AIndex + 1, AIndex, FCountRight - AIndex);
      Finalize(FArrayRight, FCountRight, 1);
    end else
      raise ELKGenericCollectionsRangeException.CreateFmt('Index Out of Bounds: %d', [AIndex + 1]);
  finally
    UnlockRight;
  end;
end;

destructor TLKCenteredList<T>.Destroy;
begin
  FLockLeft.Free;
  FLockRight.Free;
  FLockCenter.Free;
  inherited;
end;

procedure TLKCenteredList<T>.Finalize(var AArray: TArrayOfT; const AIndex, ACount: Integer);
begin
  System.FillChar(AArray[AIndex], ACount * SizeOf(T), 0);
end;

function TLKCenteredList<T>.GetCapacity: Integer;
begin
  Result := Length(FArrayLeft) + Length(FArrayRight) + 1;
end;

function TLKCenteredList<T>.GetCapacityLeft: Integer;
begin
  LockLeft;
  try
    Result := Length(FArrayLeft);
  finally
    UnlockLeft;
  end;
end;

function TLKCenteredList<T>.GetCapacityMultiplier: Single;
begin
  Lock;
  try
    Result := FCapacityMultiplier;
  finally
    Unlock;
  end;
end;

function TLKCenteredList<T>.GetCapacityRight: Integer;
begin
  LockRight;
  try
    Result := Length(FArrayRight);
  finally
    UnlockRight;
  end;
end;

function TLKCenteredList<T>.GetCapacityThreshold: Integer;
begin
  Lock;
  try
    Result := FCapacityThreshold;
  finally
    Unlock;
  end;
end;

function TLKCenteredList<T>.GetCount: Integer;
const
  // Ordinarily, if Center isn't assigned, then the Count will be 0 anyway!
  // There is the possibility of exceptions to this general rule, however!
  CENTER_ADDITION: Array[Boolean] of Integer = (0, 1);
begin
  Lock;
  try
    Result := GetCountLeft + GetCountRight + CENTER_ADDITION[FCenterAssigned];
  finally
    Unlock;
  end;
end;

function TLKCenteredList<T>.GetCountLeft: Integer;
begin
  LockLeft;
  try
    Result := FCountLeft;
  finally
    UnlockLeft;
  end;
end;

function TLKCenteredList<T>.GetCountRight: Integer;
begin
  LockRight;
  try
    Result := FCountRight;
  finally
    UnlockRight;
  end;
end;

function TLKCenteredList<T>.GetItemByIndex(const AIndex: Integer): T;
begin
  if AIndex = 0 then
    Result := GetItemCenter
  else if AIndex < 0 then
    Result := GetItemLeft((-AIndex) - 1)
  else
    Result := GetItemRight(AIndex - 1);
end;

function TLKCenteredList<T>.GetItemCenter: T;
begin
  LockCenter;
  try
    if FCenterAssigned then
      Result := FCenter
    else
      raise ELKGenericCollectionsRangeException.Create('Index Out of Bounds: 0');
  finally
    UnlockCenter;
  end;
end;

function TLKCenteredList<T>.GetItemLeft(const AIndex: Integer): T;
begin
  LockLeft;
  try
    if AIndex < FCountLeft then
      Result := FArrayLeft[AIndex]
    else
      raise ELKGenericCollectionsRangeException.CreateFmt('Index Out of Bounds: %d', [-(AIndex + 1)]);
  finally
    UnlockLeft;
  end;
end;

function TLKCenteredList<T>.GetItemRight(const AIndex: Integer): T;
begin
  LockRight;
  try
    if AIndex < FCountRight then
      Result := FArrayRight[AIndex]
    else
      raise ELKGenericCollectionsRangeException.CreateFmt('Index Out of Bounds: %d', [AIndex]);
  finally
    UnlockRight;
  end;
end;

function TLKCenteredList<T>.GetRangeHigh: Integer;
begin
  LockRight;
  try
    Result := FCountRight;
    if (not FCenterAssigned) then
      Dec(Result);
  finally
    UnlockRight;
  end;
end;

function TLKCenteredList<T>.GetRangeLow: Integer;
begin
  LockLeft;
  try
    Result := -FCountLeft;
  finally
    UnlockLeft;
  end;
end;

procedure TLKCenteredList<T>.Insert(const AItem: T; const AIndex: Integer; const AShiftDirection: TLKListDirection = ldRight);
begin
  if AIndex = 0 then
    InsertCenter(AItem, AShiftDirection)
  else if AIndex < 0 then
    InsertLeft(AItem, (-AIndex) - 1, AShiftDirection)
  else
    InsertRight(AItem, AIndex - 1, AShiftDirection);
end;

procedure TLKCenteredList<T>.InsertCenter(const AItem: T; const AShiftDirection: TLKListDirection = ldRight);
begin
  LockCenter;
  try
    if not (TryCenterAvailable(AItem)) then
    begin
      case AShiftDirection of
        ldLeft: begin
                  LockLeft;
                  try
                    if FCountLeft > 0 then
                    begin
                      Move(FArrayLeft, 0, 1, FCountLeft);
                      FArrayLeft[0] := FCenter;
                      Inc(FCountLeft);
                      CheckCapacityLeft;
                    end;
                  finally
                    UnlockLeft;
                  end;
                end;
        ldRight: begin
                   LockRight;
                   try
                     if FCountRight > 0 then
                     begin
                       Move(FArrayRight, 0, 1, FCountRight);
                       FArrayRight[0] := FCenter;
                       Inc(FCountRight);
                       CheckCapacityRight;
                     end;
                   finally
                     UnlockRight;
                   end;
                 end;
      end;
      FCenter := AItem;
      FCenterAssigned := True;
    end;
  finally
    UnlockCenter;
  end;
end;

procedure TLKCenteredList<T>.InsertLeft(const AItem: T; const AIndex: Integer; const AShiftDirection: TLKListDirection = ldRight);
begin
  if AIndex > FCountLeft then
    raise ELKGenericCollectionsRangeException.CreateFmt('Index %d Out of Bounds.', [-(AIndex + 1)])
  else
  begin
    case AShiftDirection of
      ldLeft: begin
                LockLeft;
                try
                  // We're actually shifting to the RIGHT (remember, it's inverted)
                  Move(FArrayLeft, AIndex, AIndex + 1, FCountLeft - AIndex);
                  FArrayLeft[AIndex] := AItem;
                  Inc(FCountLeft);
                  CheckCapacityLeft;
                finally
                  UnlockLeft;
                end;
              end;
      ldRight: begin
                 LockRight;
                 try
                   // We're actually shifting to the LEFT (remember, it's inverted)
                   LockCenter;
                   try
                     InsertRight(FCenter, 0); // Move the Center to position 0 of the RIGHT Array
                     FCenter := FArrayLeft[0]; // Move the Right-most item in the Left Array to Center
                     FCenterAssigned := True;
                   finally
                     UnlockCenter;
                   end;
                   Move(FArrayLeft, 1, 0, AIndex);
                   FArrayLeft[AIndex] := AItem; // Assign the Item to the index
                   CheckCapacityRight;
                 finally
                   UnlockRight;
                 end;
               end;
    end;
  end;
end;

procedure TLKCenteredList<T>.InsertRight(const AItem: T; const AIndex: Integer; const AShiftDirection: TLKListDirection = ldRight);
begin
  if AIndex > FCountRight then
    raise ELKGenericCollectionsRangeException.CreateFmt('Index %d Out of Bounds.', [AIndex + 1])
  else
  begin
    case AShiftDirection of
      ldLeft: begin
                LockLeft;
                try
                  LockCenter;
                  try
                    InsertLeft(FCenter, 0, ldLeft); // Move the Center position to 0 of the LEFT Array
                    FCenter := FArrayRight[0]; // Move the left-most item in the Left Array to Center
                    FCenterAssigned := True;
                  finally
                    UnlockCenter;
                  end;
                  Move(FArrayRight, 1, 0, AIndex);
                  FArrayRight[AIndex] := AItem;
                  CheckCapacityLeft;
                finally
                  UnlockLeft;
                end;
              end;
      ldRight: begin
                 LockRight;
                 try
                   Move(FArrayRight, AIndex, AIndex + 1, FCountRight - AIndex);
                   FArrayRight[AIndex] := AItem;
                   Inc(FCountRight);
                   CheckCapacityRight;
                 finally
                   UnlockRight;
                 end;
               end;
    end;
  end;
end;

procedure TLKCenteredList<T>.Lock;
begin
  LockCenter;
  LockLeft;
  LockRight;
end;

procedure TLKCenteredList<T>.LockCenter;
begin
  FLockCenter.Acquire;
end;

procedure TLKCenteredList<T>.LockLeft;
begin
  FLockLeft.Acquire;
end;

procedure TLKCenteredList<T>.LockRight;
begin
  FLockRight.Acquire;
end;

procedure TLKCenteredList<T>.Move(var AArray: TArrayOfT; const AFromIndex, AToIndex, ACount: Integer);
begin
  System.Move(AArray[AFromIndex], AArray[AToIndex], ACount * SizeOf(T));
end;

procedure TLKCenteredList<T>.SetCapacityMultiplier(const AMultiplier: Single);
begin
  // Sanity Check on Multiplier
  if AMultiplier < 1.10 then
    raise ELKGenericCollectionsLimitException.Create('Minimum Capacity Multiplier is 1.10')
  else if AMultiplier > 3.00 then
    raise ELKGenericCollectionsLimitException.Create('Maximum Capacity Multiplier is 3.00');
  // If we got this far, we're ready to change our Multiplier
  Lock;
  try
    FCapacityMultiplier := AMultiplier;
  finally
    Unlock;
  end;
end;

procedure TLKCenteredList<T>.SetCapacityThreshold(const AThreshold: Integer);
begin
  // Sanity Check on Threshold
  if AThreshold < 5 then
    raise ELKGenericCollectionsLimitException.Create('Minimum Capacity Threshold is 5');
  // If we got this far, we're ready to change our Threshold
  Lock;
  try
    FCapacityThreshold := AThreshold;
    CheckCapacityLeft; // Adjust the Left Array Capacity if necessary
    CheckCapacityRight; // Adjust the Right Array Capacity if necessary
  finally
    Unlock;
  end;
end;

procedure TLKCenteredList<T>.SetItemByIndex(const AIndex: Integer; const AItem: T);
begin
  if AIndex = 0 then
    SetItemCenter(AItem)
  else if AIndex < 0 then
    SetItemLeft((-AIndex) - 1, AItem)
  else
    SetItemRight(AIndex - 1, AItem);
end;

procedure TLKCenteredList<T>.SetItemCenter(const AItem: T);
begin
  LockCenter;
  try
    FCenter := AItem;
    FCenterAssigned := True;
  finally
    UnlockCenter;
  end;
end;

procedure TLKCenteredList<T>.SetItemLeft(const AIndex: Integer; const AItem: T);
begin
  LockLeft;
  try
    if AIndex < FCountLeft then
      FArrayLeft[AIndex] := AItem
    else
      raise ELKGenericCollectionsRangeException.CreateFmt('Index Out of Range: %d', [-(AIndex + 1)]);
  finally
    UnlockLeft;
  end;
end;

procedure TLKCenteredList<T>.SetItemRight(const AIndex: Integer; const AItem: T);
begin
  LockRight;
  try
    if AIndex < FCountRight then
      FArrayRight[AIndex] := AItem
    else
      raise ELKGenericCollectionsRangeException.CreateFmt('Index Out of Bounds: %d', [AIndex]);
  finally
    UnlockRight;
  end;
end;

procedure TLKCenteredList<T>.Swap(const AFrom, ATo: Integer);
var
  LTemp: T;
  LFrom, LTo: ^T;
begin
  if AFrom = 0 then
    LFrom := @FCenter
  else if AFrom < 0 then
    LFrom := @FArrayLeft[-(AFrom) - 1]
  else
    LFrom := @FArrayRight[AFrom - 1];

  if ATo = 0 then
    LTo := @FCenter
  else if ATo < 0 then
    LTo := @FArrayLeft[-(ATo) - 1]
  else
    LTo := @FArrayRight[ATo - 1];

    LTemp := LFrom^;
    LFrom^ := LTo^;
    LTo^ := LTemp;
end;

function TLKCenteredList<T>.TryCenterAvailable(const AItem: T): Boolean;
begin
  Result := False;
  LockCenter;
  try
    if not (FCenterAssigned) then
    begin
      Result := True;
      FCenter := AItem;
      FCenterAssigned := True;
    end;
  finally
    UnlockCenter;
  end;
end;

procedure TLKCenteredList<T>.Unlock;
begin
  UnlockCenter;
  UnlockLeft;
  UnlockRight;
end;

procedure TLKCenteredList<T>.UnlockCenter;
begin
  FLockCenter.Release;
end;

procedure TLKCenteredList<T>.UnlockLeft;
begin
  FLockLeft.Release;
end;

procedure TLKCenteredList<T>.UnlockRight;
begin
  FLockRight.Release;
end;

{ TLKTreeNode<T> }

constructor TLKTreeNode<T>.Create(const AParent: TLKTreeNode<T>; const AValue: T);
begin
  inherited Create;

  FParent := AParent;
  FChildren := TLKObjectList<TLKTreeNode<T>>.Create;
  FValue := AValue;
end;

constructor TLKTreeNode<T>.Create(const AParent: TLKTreeNode<T>);
begin
  Create(AParent, Default(T));
end;

constructor TLKTreeNode<T>.Create(const AValue: T);
begin
  Create(nil, AValue);
end;

constructor TLKTreeNode<T>.Create;
begin
  Create(nil, Default(T));
end;

destructor TLKTreeNode<T>.Destroy;
begin
  FChildren.Free;

  inherited;
end;

procedure TLKTreeNode<T>.DoAncestorChanged;
begin
  PreOrderWalk(procedure(const Node: TLKTreeNode<T>)
               begin
                 Node.AncestorChanged;
               end);
end;

function TLKTreeNode<T>.GetRootNode: TLKTreeNode<T>;
begin
  Lock;
  try
    if IsRoot then
      Result := Self
    else
      Result := Parent.RootNode;
  finally
    Unlock;
  end;
end;

function TLKTreeNode<T>.GetChildCount: Integer;
begin
  Lock;
  try
    Result := FChildren.Count;
  finally
    Unlock;
  end;
end;

function TLKTreeNode<T>.GetChildren(const AIndex: Integer): TLKTreeNode<T>;
begin
  Lock;
  try
    Result := FChildren[AIndex];
  finally
    Unlock;
  end;
end;

function TLKTreeNode<T>.GetIndexAsChild: Integer;
begin
  if Parent = nil then
    Result := -1
  else
  begin
    Lock;
    try
      Result := Parent.IndexOf(Self);
    finally
      Unlock;
    end;
  end;
end;

function TLKTreeNode<T>.GetIsRoot: Boolean;
begin
  Lock;
  try
    Result := Parent = nil;
  finally
    Unlock;
  end;
end;

function TLKTreeNode<T>.GetIsBranch: Boolean;
begin
  Lock;
  try
    Result := (not GetIsRoot) and (not GetIsLeaf);
  finally
    Unlock;
  end;
end;

function TLKTreeNode<T>.GetIsLeaf: Boolean;
begin
  Lock;
  try
    Result := FChildren.Count = 0;
  finally
    Unlock;
  end;
end;

function TLKTreeNode<T>.GetDepth: Integer;
var
  Ancestor: TLKTreeNode<T>;
begin
  Lock;
  try
    Ancestor := Parent;
    Result := 0;

    while Ancestor <> nil do
    begin
      Inc(Result);
      Ancestor := Ancestor.Parent;
    end;
  finally
    Unlock;
  end;
end;

procedure TLKTreeNode<T>.Destroying;
begin
  FDestroying := True;
end;

procedure TLKTreeNode<T>.AddChild(const AIndex: Integer; const AChild: TLKTreeNode<T>);
begin
  if AIndex < 0 then
    FChildren.Add(AChild)
  else
    FChildren.Insert(AIndex, AChild);
end;

procedure TLKTreeNode<T>.RemoveChild(const AChild: TLKTreeNode<T>);
begin
  FChildren.Remove(AChild);
end;

procedure TLKTreeNode<T>.SetValue(const AValue: T);
begin
  Lock;
  try
    FValue := AValue;
  finally
    Unlock;
  end;
end;

procedure TLKTreeNode<T>.AfterConstruction;
begin
  inherited;

  if Parent <> nil then
    Parent.AddChild(-1, Self);

  DoAncestorChanged;
end;

procedure TLKTreeNode<T>.AncestorChanged;
begin
  // Do nothing (yet)
end;

procedure TLKTreeNode<T>.BeforeDestruction;
begin
  inherited;

  if not IsDestroying then
    PreOrderWalk(procedure(const Node: TLKTreeNode<T>)
                 begin
                   Node.Destroying;
                 end);

  if (Parent <> nil) and (not Parent.IsDestroying) then
    Parent.RemoveChild(Self);
end;

procedure TLKTreeNode<T>.MoveTo(const ANewParent: TLKTreeNode<T>; const AIndex: Integer = -1);
begin
  if (Parent = nil) and (ANewParent = nil) then
    Exit;

  Lock;
  try
    if Parent = ANewParent then
    begin
      if AIndex <> IndexAsChild then
      begin
        Parent.FChildren.Remove(Self);
        if AIndex < 0 then
          Parent.FChildren.Add(Self)
        else
          Parent.FChildren.Insert(AIndex, Self);
      end;
    end else
    begin
      if Parent <> nil then
        Parent.RemoveChild(Self);

      FParent := ANewParent;

      if Parent <> nil then
        Parent.AddChild(AIndex, Self);

      DoAncestorChanged;
    end;
  finally
    Unlock;
  end;
end;

procedure TLKTreeNode<T>.MoveTo(const AIndex: Integer);
begin
  MoveTo(Parent, AIndex);
end;

function TLKTreeNode<T>.IndexOf(const AChild: TLKTreeNode<T>): Integer;
begin
  Lock;
  try
    Result := FChildren.IndexOf(AChild);
  finally
    Unlock;
  end;
end;

procedure TLKTreeNode<T>.PreOrderWalk(const AAction: TLKValueCallback<TLKTreeNode<T>>);
var
  Index: Integer;
begin
  Lock;
  try
    AAction(Self);
    for Index := 0 to ChildCount-1 do
      Children[Index].PreOrderWalk(AAction);
  finally
    Unlock;
  end;
end;

procedure TLKTreeNode<T>.PreOrderWalk(const AAction: TLKValueCallback<T>);
begin
  Lock;
  try
    PreOrderWalk(procedure(const Node: TLKTreeNode<T>)
                 begin
                   AAction(Node.Value);
                 end);
  finally
    Unlock;
  end;
end;

procedure TLKTreeNode<T>.PostOrderWalk(const AAction: TLKValueCallback<TLKTreeNode<T>>);
var
  LIndex: Integer;
begin
  Lock;
  try
    for LIndex := 0 to ChildCount-1 do
      Children[LIndex].PostOrderWalk(AAction);
    AAction(Self);
  finally
    Unlock;
  end;
end;

procedure TLKTreeNode<T>.PostOrderWalk(const AAction: TLKValueCallback<T>);
begin
  Lock;
  try
    PostOrderWalk(procedure(const Node: TLKTreeNode<T>)
                  begin
                    AAction(Node.Value);
                  end);
  finally
    Unlock;
  end;
end;

end.
