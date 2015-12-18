{
  LaKraven Studios Standard Library [LKSL]
  Copyright (c) 2014-2015, Simon J Stuart, All Rights Reserved

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
    TArray<T> = Array of T;
  {$ELSE}
    { Interface Forward Declarations }
    ILKComparer<T> = interface;
    ILKArray<T> = interface;
    ILKObjectArray<T: class> = interface;
    ILKListBase<T> = interface;
    ILKList<T> = interface;
    ILKObjectList<T: class> = interface;
    ILKSortedListBase<T> = interface;
    ILKSortedList<T> = interface;
    ILKSortedObjectList<T: class> = interface;
    ILKTreeNode<T> = interface;
    ILKTreeObjectNode<T: class> = interface;
    { Class Forward Declaration }
    TLKComparer<T> = class;
    TLKArray<T> = class;
    TLKObjectArray<T: class> = class;
    TLKDictionary<TKey, TValue> = class;
//    TLKHashMap<TKey, TValue> = class;
    TLKListBase<T> = class;
    TLKList<T> = class;
    TLKObjectList<T: class> = class;
    TLKSortedListBase<T> = class;
    TLKSortedList<T> = class;
    TLKSortedObjectList<T: class> = class;
    TLKLookupListBase<TKey, TValue> = class;
    TLKLookupList<TKey, TValue> = class;
    TLKObjectLookupList<TKey, TValue: class> = class;
    TLKTreeNode<T> = class;
    TLKTreeObjectNode<T: class> = class;
  {$ENDIF FPC}

  { Enum Types }
  TLKListDirection = (ldLeft, ldRight);
  TLKListSortOrder = (soAscending, soDescending);
  TLKListFinalizeMode = (fmNoFinalize, fmFinalize);
  TLKListSmartCompactMode = (scmNoSmartCompact, scmSmartCompact);

  { Callback Types }
  {$IFDEF SUPPORTS_REFERENCETOMETHOD}
    ///  <summary><c>Used by TLKListBase (and descendants) to Iterate Items in the List.</c></summary>
    TLKListIterateCallbackAnon<T> = reference to procedure(const AIndex: Integer; const AItem: T);
    ///  <summary><c>Used by TLKTreeNode (and descendants) to Iterate Node Values in the Tree.</c></summary>
    TLKTreeNodeValueCallbackAnon<V> = reference to procedure(const Value: V);
  {$ENDIF SUPPORTS_REFERENCETOMETHOD}
  ///  <summary><c>Used by TLKListBase (and descendants) to Iterate Items in the List.</c></summary>
  TLKListIterateCallbackOfObject<T> = procedure(const AIndex: Integer; const AItem: T) of object;
  ///  <summary><c>Used by TLKTreeNode (and descendants) to Iterate Node Values in the Tree.</c></summary>
  TLKTreeNodeValueCallbackOfObject<V> = procedure(const Value: V) of object;
  ///  <summary><c>Used by TLKListBase (and descendants) to Iterate Items in the List.</c></summary>
  TLKListIterateCallbackUnbound<T> = procedure(const AIndex: Integer; const AItem: T);
  ///  <summary><c>Used by TLKTreeNode (and descendants) to Iterate Node Values in the Tree.</c></summary>
  TLKTreeNodeValueCallbackUnbound<V> = procedure(const Value: V);

  { Exception Types }
  ELKGenericCollectionsException = class(ELKException);
    ELKGenericCollectionsLimitException = class(ELKGenericCollectionsException);
    ELKGenericCollectionsRangeException = class(ELKGenericCollectionsException);
    ELKGenericCollectionsKeyAlreadyExists = class(ELKGenericCollectionsException);
    ELKGenericCollectionsKeyNotFound = class(ELKGenericCollectionsException);

  ILKComparer<T> = interface(ILKInterface)
  ['{2E102784-D058-44D6-9104-915FE4BCE3FD}']
    function AEqualToB(const A, B: T): Boolean;
    function AGreaterThanB(const A, B: T): Boolean;
    function AGreaterThanOrEqualB(const A, B: T): Boolean;
    function ALessThanB(const A, B: T): Boolean;
    function ALessThanOrEqualB(const A, B: T): Boolean;
  end;

  ILKArray<T> = interface(ILKInterface)
  ['{04C10AB7-8438-4EAF-BB11-F19F8590F3D8}']
    function GetItem(const AIndex: Integer): T;
    procedure SetItem(const AIndex: Integer; const AItem: T);

    function Add(const AItem: T): Integer;
    procedure Clear;
    procedure Delete(const AIndex: Integer);

    property Items[const AIndex: Integer]: T read GetItem write SetItem;
  end;

  ILKObjectArray<T: class> = interface(ILKArray<T>)
  ['{BCBDF151-09FD-4E0E-8311-4483957AE63E}']
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const AOwnsObjects: Boolean);

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  ILKListBase<T> = interface(ILKInterface)
  ['{FC258E80-7236-4F7D-A113-D2FB1856122E}']
    function GetCapacity: Integer;
    function GetCapacityMultiplier: Single;
    function GetCapacityThreshold: Integer;
    // Getters - Other
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetItemByIndex(const AIndex: Integer): T;

    // Setters - Capacity
    procedure SetCapacityMultiplier(const AMultiplier: Single);
    procedure SetCapacityThreshold(const AThreshold: Integer);

    function Add(const AItem: T): Integer; overload;
    procedure Add(const AItems: Array of T); overload;

    procedure Clear(const ACompact: Boolean = True);
    procedure Compact;

    ///  <summary><c>Delete a specific Item at the given Index.</c></summary>
    procedure Delete(const AIndex: Integer); overload;
    ///  <summary><c>Alias of </c>DeleteRange</summary>
    procedure Delete(const AFirstIndex, ACount: Integer); overload;
    ///  <summary><c>Delete a Range of Items from the given Index.</c></summary>
    procedure DeleteRange(const AIndex, ACount: Integer);

    function Remove(const AItem: T): Integer; overload;
    procedure Remove(const AItems: Array of T); overload;

    {$IFDEF SUPPORTS_REFERENCETOMETHOD}
      procedure Iterate(const AIterateCallback: TLKListIterateCallbackAnon<T>; const AIterateDirection: TLKListDirection = ldRight); overload;
      procedure Iterate(const AIterateCallback: TLKListIterateCallbackAnon<T>; const AFrom, ATo: Integer); overload;
    {$ENDIF SUPPORTS_REFERENCETOMETHOD}
    procedure Iterate(const AIterateCallback: TLKListIterateCallbackOfObject<T>; const AIterateDirection: TLKListDirection = ldRight); overload;
    procedure Iterate(const AIterateCallback: TLKListIterateCallbackOfObject<T>; const AFrom, ATo: Integer); overload;
    procedure Iterate(const AIterateCallback: TLKListIterateCallbackUnbound<T>; const AIterateDirection: TLKListDirection = ldRight); overload;
    procedure Iterate(const AIterateCallback: TLKListIterateCallbackUnbound<T>; const AFrom, ATo: Integer); overload;

    function Contains(const AItem: T): Boolean; overload;
    function Contains(const AItems: Array of T): Boolean; overload;
    function IndexOf(const AItem: T): Integer;

    procedure Sort(const AComparer: ILKComparer<T>; const ASortOrder: TLKListSortOrder = soAscending);

    ///  <summary><c>The number of Slots presently allocated for the List.</c></summary>
    ///  <remarks><c>Read-Only</c></remarks>
    property Capacity: Integer read GetCapacity;
    ///  <summary><c>By what Factor to increase the Size of the List when the Capacity Threshold is reached.</c></summary>
    property CapacityMultiplier: Single read GetCapacityMultiplier write SetCapacityMultiplier;
    ///  <summary><c>The Minimum number of Vacant Slots in the List before the List is Expanded.</c></summary>
    property CapacityThreshold: Integer read GetCapacityThreshold write SetCapacityThreshold;
    ///  <summary><c>The number of Items presently occupying the List.</c></summary>
    ///  <remarks><c>Read-Only</c></remarks>
    property Count: Integer read GetCount;
    ///  <summary><c>Returns </c>True<c> if the List contains no Items.</c></summary>
    property IsEmpty: Boolean read GetIsEmpty;
    ///  <summary><c>Retrieves the Item at the given Index</c></summary>
    property Items[const AIndex: Integer]: T read GetItemByIndex; default;
  end;

  ILKList<T> = interface(ILKListBase<T>)
  ['{D25C4FC1-1E33-43FA-942D-E4CC6A92CB3D}']
    procedure Insert(const AItem: T; const AIndex: Integer);
  end;

  ILKObjectList<T: class> = interface(ILKList<T>)
  ['{59B2887B-6857-4A20-BF4C-5613C62BF957}']
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const AOwnsObjects: Boolean);

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  ILKSortedListBase<T> = interface(ILKInterface)
  ['{E1C7F61C-6CBF-4595-A35E-F66AA3DAC6DE}']

  end;

  ILKSortedList<T> = interface(ILKSortedListBase<T>)
  ['{7653B5EA-EABC-43A6-A12D-59493985D802}']

  end;

  ILKSortedObjectList<T: class> = interface(ILKSortedList<T>)
  ['{A2D03B64-B84A-4F8A-831F-925093F2940C}']

  end;

  ILKTreeNode<T> = interface(ILKInterface)
  ['{5FAFB0C6-172D-4DB3-A044-4B593C08194B}']

  end;

  ILKTreeObjectNode<T: class> = interface(ILKTreeNode<T>)
  ['{D24E33FD-1959-48A8-8FB0-79E9CABA786D}']

  end;

  {
    TLKComparer<T>
      - Used to dictate how to determine the order of Items in a List/Array when Sorting it.
      - Allows you to provide Dynamic Sorting Behaviour at Runtime!
  }
  TLKComparer<T> = class abstract(TLKInterfacedObject, ILKComparer<T>)
  public
    // Parity Checks
    function AEqualToB(const A, B: T): Boolean; virtual; abstract;
    function AGreaterThanB(const A, B: T): Boolean; virtual; abstract;
    function AGreaterThanOrEqualB(const A, B: T): Boolean; virtual; abstract;
    function ALessThanB(const A, B: T): Boolean; virtual; abstract;
    function ALessThanOrEqualB(const A, B: T): Boolean; virtual; abstract;
  end;

  ///  <summary><c>A very simple "Managed Array" for items of the nominated Type.</c></summary>
  ///  <remarks>
  ///    <para><c>Array expands by one each time an Item is Added.</c></para>
  ///    <para><c>Array collapses by 1 each time an Item is Removed.</c></para>
  ///    <para>ArrayRaw <c>property violates thread-safe Locking!</c></para>
  ///  </remarks>
  TLKArray<T> = class(TLKInterfacedObject, ILKArray<T>)
  type
    TLKArrayType = Array of T;
  private
    FArray: TLKArrayType;

    // Getters
    function GetItem(const AIndex: Integer): T;

    // Setters
    procedure SetItem(const AIndex: Integer; const AItem: T);
  public
    function Add(const AItem: T): Integer;
    procedure Clear; virtual;
    procedure Delete(const AIndex: Integer); virtual;

    property ArrayRaw: TLKArrayType read FArray;
    property Items[const AIndex: Integer]: T read GetItem write SetItem;
  end;

  ///  <summary><c>A very simple "Managed Object Array" for items of the nominated Type.</c></summary>
  ///  <remarks>
  ///    <para><c>Can take ownership of Object Instances in the List, and manage their lifetime</c></para>
  ///    <para><c>Array expands by one each time an Item is Added.</c></para>
  ///    <para><c>Array collapses by 1 each time an Item is Removed.</c></para>
  ///    <para>ArrayRaw <c>property violates thread-safe Locking!</c></para>
  ///  </remarks>
  TLKObjectArray<T: class> = class(TLKArray<T>, ILKObjectArray<T>)
  private
    FOwnsObjects: Boolean;
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const AOwnsObjects: Boolean);
  public
    constructor Create(const AOwnsObjects: Boolean = True); reintroduce;
    destructor Destroy; override;

    procedure Clear; override;
    procedure Delete(const AIndex: Integer); override;

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  {
    TLKDictionary<TKey, TValue>
      - Provides a Thread-Safe Lock (TLKCriticalSection)
  }
  TLKDictionary<TKey, TValue> = class({$IFDEF FPC}TFPGMap{$ELSE}TDictionary{$ENDIF FPC}<TKey, TValue>)
    private
      FLock: TLKCriticalSection;
    public
      {$IFDEF FPC}
        constructor Create; reintroduce;
      {$ELSE}
        constructor Create(ACapacity: Integer = 0); reintroduce; overload;
        constructor Create(const AComparer: IEqualityComparer<TKey>); reintroduce; overload;
        constructor Create(ACapacity: Integer; const AComparer: IEqualityComparer<TKey>); reintroduce; overload;
        constructor Create(const Collection: TEnumerable<TPair<TKey,TValue>>); reintroduce; overload;
        constructor Create(const Collection: TEnumerable<TPair<TKey,TValue>>; const AComparer: IEqualityComparer<TKey>); reintroduce; overload;
      {$ENDIF FPC}
      destructor Destroy; override;

      {$IFDEF FPC}
        function TryGetValue(const AKey: TKey; var AValue: TValue): Boolean;
        function ContainsKey(const AKey: TKey): Boolean;
      {$ENDIF FPC}

      procedure Lock; inline;
      procedure Unlock; inline;
    end;

  ///  <summary><c>Thread-safe Hashed Key/Value Map.</c></summary>
  ///  <remarks><c>Note that this is not ready for use at this time!</c></remarks>
{  TLKHashMap<TKey, TValue> = class(TLKPersistent)
  private type
    TItem = record
      HashCode: Integer;
      Key: TKey;
      Value: TValue;
    end;
    TItemArray = Array of TItem;
  private
    FItems: TItemArray;
    FCapacityMultiplier: Single;
    FCapacityThreshold: Integer;
    FCount: Integer;
    FLock: TLKCriticalSection;

    function Hash(const AKey: TKey): Integer;
  end;}

  {
    TLKListBase<T>
      - Abstract Base Type for TLKList[variation]<T> types
  }
  TLKListBase<T> = class abstract(TLKInterfacedObject, ILKListBase<T>)
  private type
    TArrayOfT = Array of T;
  private
    FArray: TArrayOfT;
    FCapacityMultiplier: Single;
    FCapacityThreshold: Integer;
    {$IFNDEF FPC}
      FComparer: IComparer<T>;
    {$ENDIF FPC}
    FCount: Integer;
    // Getters - Capacity
    function GetCapacity: Integer;
    function GetCapacityMultiplier: Single;
    function GetCapacityThreshold: Integer;
    // Getters - Other
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetItemByIndex(const AIndex: Integer): T;

    // Setters - Capacity
    procedure SetCapacityMultiplier(const AMultiplier: Single);
    procedure SetCapacityThreshold(const AThreshold: Integer);
    procedure SetItemByIndex(const AIndex: Integer; const AItem: T);

    procedure InitializeArray;
  protected
    // Add
    function AddActual(const AItem: T): Integer; virtual;
    // Remove
    function RemoveActual(const AItem: T): Integer;
    // Capacity Management
    procedure CheckCapacity; overload; inline;
    procedure CheckCapacity(const ATotalRequiredCapacity: Integer); overload;
    // Array Management
    procedure Finalize(const AIndex, ACount: Integer);
    procedure Move(const AFromIndex, AToIndex, ACount: Integer);
    // Delete
    procedure DeleteActual(const AIndex: Integer; const AFinalizeMode: TLKListFinalizeMode; const ASmartCompactMode: TLKListSmartCompactMode);
    // Insert
    procedure InsertActual(const AItem: T; const AIndex: Integer); inline;
    // Quick Sort
    procedure QuickSort(const AComparer: ILKComparer<T>; ALow, AHigh: Integer; const ASortOrder: TLKListSortOrder = soAscending);
    // Smart Compact
    procedure CompactActual;
    procedure SmartCompact;
    // Validation Methods
    function ValidateIndexInRange(const AIndex: Integer): Boolean;
    function ValidateRangeInArray(const AFrom, ATo: Integer): Boolean;
  public
    constructor Create; overload; override;
    {$IFNDEF FPC}
      constructor Create(const AComparer: IComparer<T>); reintroduce; overload; virtual;
    {$ENDIF FPC}
    destructor Destroy; override;

    function Add(const AItem: T): Integer; overload; virtual;
    procedure Add(const AItems: Array of T); overload;

    procedure Clear(const ACompact: Boolean = True); virtual;
    procedure Compact; inline;

    ///  <summary><c>Delete a specific Item at the given Index.</c></summary>
    procedure Delete(const AIndex: Integer); overload; virtual;
    ///  <summary><c>Alias of </c>DeleteRange</summary>
    procedure Delete(const AFirstIndex, ACount: Integer); overload; inline;
    ///  <summary><c>Delete a Range of Items from the given Index.</c></summary>
    procedure DeleteRange(const AIndex, ACount: Integer); virtual;

    function Remove(const AItem: T): Integer; overload; inline;
    procedure Remove(const AItems: Array of T); overload;

    {$IFDEF SUPPORTS_REFERENCETOMETHOD}
      procedure Iterate(const AIterateCallback: TLKListIterateCallbackAnon<T>; const AIterateDirection: TLKListDirection = ldRight); overload;
      procedure Iterate(const AIterateCallback: TLKListIterateCallbackAnon<T>; const AFrom, ATo: Integer); overload;
    {$ENDIF SUPPORTS_REFERENCETOMETHOD}
    procedure Iterate(const AIterateCallback: TLKListIterateCallbackOfObject<T>; const AIterateDirection: TLKListDirection = ldRight); overload;
    procedure Iterate(const AIterateCallback: TLKListIterateCallbackOfObject<T>; const AFrom, ATo: Integer); overload;
    procedure Iterate(const AIterateCallback: TLKListIterateCallbackUnbound<T>; const AIterateDirection: TLKListDirection = ldRight); overload;
    procedure Iterate(const AIterateCallback: TLKListIterateCallbackUnbound<T>; const AFrom, ATo: Integer); overload;

    function Contains(const AItem: T): Boolean; overload; inline;
    function Contains(const AItems: Array of T): Boolean; overload;
    function IndexOf(const AItem: T): Integer;

    procedure Sort(const AComparer: ILKComparer<T>; const ASortOrder: TLKListSortOrder = soAscending);

    ///  <summary><c>The number of Slots presently allocated for the List.</c></summary>
    ///  <remarks><c>Read-Only</c></remarks>
    property Capacity: Integer read GetCapacity;
    ///  <summary><c>By what Factor to increase the Size of the List when the Capacity Threshold is reached.</c></summary>
    property CapacityMultiplier: Single read GetCapacityMultiplier write SetCapacityMultiplier;
    ///  <summary><c>The Minimum number of Vacant Slots in the List before the List is Expanded.</c></summary>
    property CapacityThreshold: Integer read GetCapacityThreshold write SetCapacityThreshold;
    ///  <summary><c>The number of Items presently occupying the List.</c></summary>
    ///  <remarks><c>Read-Only</c></remarks>
    property Count: Integer read GetCount;
    ///  <summary><c>Returns </c>True<c> if the List contains no Items.</c></summary>
    property IsEmpty: Boolean read GetIsEmpty;
    ///  <summary><c>Retrieves the Item at the given Index</c></summary>
    property Items[const AIndex: Integer]: T read GetItemByIndex write SetItemByIndex; default;
  end;

  {
    TLKList<T>
      - A simple Generic List Type (light-weight)
  }
  TLKList<T> = class(TLKListBase<T>, ILKList<T>)
  public
    procedure Insert(const AItem: T; const AIndex: Integer);
  end;

  {
    TLKObjectList<T: class>
      - A version of TLKList<T> capable of handling ownership of Class Instances
  }
  TLKObjectList<T: class> = class(TLKList<T>, ILKObjectList<T>)
  private
    FOwnsObjects: Boolean;
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const AOwnsObjects: Boolean);
  public
    constructor Create(const AOwnsObjects: Boolean = True); overload;
    {$IFNDEF FPC}
      constructor Create(const AComparer: IComparer<T>; const AOwnsObjects: Boolean = True); overload;
    {$ENDIF FPC}
    destructor Destroy; override;

    procedure Clear(const ACompact: Boolean = True); override;
    procedure Delete(const AIndex: Integer); override;

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  {
    TLKSortedListBase<T>
      - A TLKListBase<T> with Sorted Insertion
      - Abstract methods need to be implemented on type-specific descendants
  }
  TLKSortedListBase<T> = class abstract(TLKListBase<T>, ILKSortedListBase<T>)
  protected
    // Parity Checks
    function AEqualToB(const A, B: T): Boolean; virtual; abstract;
    function AGreaterThanB(const A, B: T): Boolean; virtual; abstract;
    function AGreaterThanOrEqualB(const A, B: T): Boolean; virtual; abstract;
    function ALessThanB(const A, B: T): Boolean; virtual; abstract;
    function ALessThanOrEqualB(const A, B: T): Boolean; virtual; abstract;
    // Critical Utility Methods
    function GetSortedPosition(const AItem: T): Integer;
  end;

  {
    TLKSortedList<T>
      - An implementation of TLKSortedListBase<T> supporting Duplicate Sorted Keys
      - Abstract methods need to be implemented on type-specific descendants
  }
  TLKSortedList<T> = class abstract(TLKSortedListBase<T>, ILKSortedList<T>)
  protected
    function AddActual(const AItem: T): Integer; override;
  end;

  {
    TLKSortedObjectList<T: class; TKey>
      - A version of TLKSortedList<T> capable of handling ownership of Class Instances
      - Abstract methods need to be implemented on type-specific descendants
  }
  TLKSortedObjectList<T: class> = class abstract(TLKSortedListBase<T>, ILKSortedObjectList<T>)
  private
    FOwnsObjects: Boolean;
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const AOwnsObjects: Boolean);
  public
    constructor Create(const AOwnsObjects: Boolean = True); reintroduce; overload;
    {$IFNDEF FPC}
      constructor Create(const AComparer: IComparer<T>; const AOwnsObjects: Boolean = True); reintroduce; overload;
    {$ENDIF FPC}
    destructor Destroy; override;

    procedure Clear(const ACompact: Boolean = True); override;
    procedure Delete(const AIndex: Integer); override;

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  TLKLookupListBase<TKey, TValue> = class(TLKListBase<TValue>)

  end;

  TLKLookupList<TKey, TValue> = class(TLKLookupListBase<TKey, TValue>)

  end;

  TLKObjectLookupList<TKey, TValue: class> = class(TLKLookupList<TKey, TValue>)

  end;
{$IFNDEF FPC}
  {
    TLKTreeNode<T>
      - A special Generic Tree Object with thread-safe Locks
      - Acts as both the Root and Child node (dependant on whether or not it contains a Parent)
  }
  TLKTreeNode<T> = class(TLKInterfacedObject, ILKTreeNode<T>)
  private type
    TLKTreeNodeT = TLKTreeNode<T>;
  private
    FParent: TLKTreeNode<T>;
    FChildren: TLKObjectList<TLKTreeNodeT>;
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

    {$IFDEF SUPPORTS_REFERENCETOMETHOD}
      procedure PreOrderWalk(const AAction: TLKTreeNodeValueCallbackAnon<TLKTreeNode<T>>); overload;
      procedure PreOrderWalk(const AAction: TLKTreeNodeValueCallbackAnon<T>); overload;
    {$ENDIF SUPPORTS_REFERENCETOMETHOD}
    procedure PreOrderWalk(const AAction: TLKTreeNodeValueCallbackOfObject<TLKTreeNode<T>>); overload;
    procedure PreOrderWalk(const AAction: TLKTreeNodeValueCallbackOfObject<T>); overload;
    procedure PreOrderWalk(const AAction: TLKTreeNodeValueCallbackUnbound<TLKTreeNode<T>>); overload;
    procedure PreOrderWalk(const AAction: TLKTreeNodeValueCallbackUnbound<T>); overload;

    {$IFDEF SUPPORTS_REFERENCETOMETHOD}
      procedure PostOrderWalk(const AAction: TLKTreeNodeValueCallbackAnon<TLKTreeNode<T>>); overload;
      procedure PostOrderWalk(const AAction: TLKTreeNodeValueCallbackAnon<T>); overload;
    {$ENDIF SUPPORTS_REFERENCETOMETHOD}
    procedure PostOrderWalk(const AAction: TLKTreeNodeValueCallbackOfObject<TLKTreeNode<T>>); overload;
    procedure PostOrderWalk(const AAction: TLKTreeNodeValueCallbackOfObject<T>); overload;
    procedure PostOrderWalk(const AAction: TLKTreeNodeValueCallbackUnbound<TLKTreeNode<T>>); overload;
    procedure PostOrderWalk(const AAction: TLKTreeNodeValueCallbackUnbound<T>); overload;

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

  {
    TLKTreeObjectNode<T: class>
      - Special variant of TLKTreeNode<T> for Objects
      - Has the ability to OWN its Values (so they are destroyed with the Node)
  }
  TLKTreeObjectNode<T: class> = class(TLKTreeNode<T>, ILKTreeObjectNode<T>)
  private
    FOwnsObject: Boolean;
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const AOwnsObjects: Boolean);
  public
    constructor Create(const AParent: TLKTreeNode<T>; const AValue: T); reintroduce; overload;
    constructor Create(const AParent: TLKTreeNode<T>); reintroduce; overload;
    constructor Create(const AValue: T); reintroduce; overload;
    constructor Create; reintroduce; overload;
    destructor Destroy; override;

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;
{$ENDIF FPC}
const
  LKSL_LIST_CAPACITY_DEFAULT = 10;
  LKSL_LIST_MULTIPLIER_MINIMUM = 1.10;
  LKSL_LIST_MULTIPLIER_MAXIMUM = 3.00;
  LKSL_LIST_MULTIPLIER_DEFAULT = 1.5;
  LKSL_LIST_THRESHOLD_MINIMUM = 5;
  LKSL_LIST_THRESHOLD_DEFAULT = LKSL_LIST_THRESHOLD_MINIMUM;

implementation

{ TLKArray<T> }

function TLKArray<T>.Add(const AItem: T): Integer;
begin
  AcquireWriteLock;
  try
    Result := Length(FArray);
    SetLength(FArray, Result + 1);
    FArray[Result] := AItem;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKArray<T>.Clear;
begin
  AcquireWriteLock;
  try
    SetLength(FArray, 0);
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKArray<T>.Delete(const AIndex: Integer);
var
  I: Integer;
begin
  AcquireWriteLock;
  try
    for I := AIndex to Length(FArray) - 2 do
      FArray[I] := FArray[I + 1];

    SetLength(FArray, Length(FArray) - 1);
  finally
    ReleaseWriteLock;
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

procedure TLKArray<T>.SetItem(const AIndex: Integer; const AItem: T);
begin
  AcquireWriteLock;
  try
    FArray[AIndex] := AItem;
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKObjectArray<T> }

procedure TLKObjectArray<T>.Clear;
var
  I: Integer;
begin
  AcquireWriteLock;
  try
    if FOwnsObjects then
      for I := Low(FArray) to High(FArray) do
        FArray[I].{$IFDEF SUPPORTS_DISPOSEOF}DisposeOf{$ELSE}Free{$ENDIF SUPPORTS_DISPOSEOF};
    inherited;
  finally
    ReleaseWriteLock;
  end;
end;

constructor TLKObjectArray<T>.Create(const AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

procedure TLKObjectArray<T>.Delete(const AIndex: Integer);
begin
  AcquireWriteLock;
  try
    if FOwnsObjects then
      FArray[AIndex].{$IFDEF SUPPORTS_DISPOSEOF}DisposeOf{$ELSE}Free{$ENDIF SUPPORTS_DISPOSEOF};
    inherited;
  finally
    ReleaseWriteLock;
  end;
end;

destructor TLKObjectArray<T>.Destroy;
begin

  inherited;
end;

function TLKObjectArray<T>.GetOwnsObjects: Boolean;
begin
  AcquireReadLock;
  try
    Result := FOwnsObjects;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKObjectArray<T>.SetOwnsObjects(const AOwnsObjects: Boolean);
begin
  AcquireWriteLock;
  try
    FOwnsObjects := AOwnsObjects;
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKDictionary<TKey, TValue> }

{$IFDEF FPC}
  constructor TLKDictionary<TKey, TValue>.Create;
  begin
    FLock := TLKCriticalSection.Create;
    inherited Create;
  end;

  function TLKDictionary<TKey, TValue>.TryGetValue(const AKey: TKey; var AValue: TValue): Boolean;
  var
    LIndex: Integer;
  begin
    Result := Find(AKey, LIndex);
    if Result then
      AValue := Data[LIndex];
  end;

  function TLKDictionary<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
  begin
    Result := IndexOf(AKey) > -1;
  end;
{$ELSE}
  constructor TLKDictionary<TKey, TValue>.Create(const AComparer: IEqualityComparer<TKey>);
  begin
    FLock := TLKCriticalSection.Create;
    inherited Create(AComparer);
  end;

  constructor TLKDictionary<TKey, TValue>.Create(ACapacity: Integer);
  begin
    FLock := TLKCriticalSection.Create;
    inherited Create(ACapacity);
  end;

  constructor TLKDictionary<TKey, TValue>.Create(ACapacity: Integer; const AComparer: IEqualityComparer<TKey>);
  begin
    FLock := TLKCriticalSection.Create;
    inherited Create(ACapacity, AComparer);
  end;

  constructor TLKDictionary<TKey, TValue>.Create(const Collection: TEnumerable<TPair<TKey, TValue>>; const AComparer: IEqualityComparer<TKey>);
  begin
    FLock := TLKCriticalSection.Create;
    inherited Create(Collection, AComparer);
  end;

  constructor TLKDictionary<TKey, TValue>.Create(const Collection: TEnumerable<TPair<TKey, TValue>>);
  begin
    FLock := TLKCriticalSection.Create;
    inherited Create(Collection);
  end;
{$ENDIF FPC}

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

{ TLKHashMap<TKey, TValue> }
{
function TLKHashMap<TKey, TValue>.Hash(const AKey: TKey): Integer;
const
  POSMASK = not Integer($80000000);
begin
  Result := POSMASK and ((POSMASK and LKSuperFastHash(@AKey, SizeOf(AKey))) + 1);
end;
}
{ TLKListBase<T> }

function TLKListBase<T>.Add(const AItem: T): Integer;
begin
  AddActual(AItem);
end;

procedure TLKListBase<T>.CheckCapacity;
begin
  CheckCapacity(Count);
end;

procedure TLKListBase<T>.Add(const AItems: Array of T);
var
  I: Integer;
begin
  for I := Low(AItems) to High(AItems) do
    AddActual(AItems[I]);
end;

procedure TLKListBase<T>.CheckCapacity(const ATotalRequiredCapacity: Integer);
begin
  AcquireWriteLock;
  try
    if ((Length(FArray) - ATotalRequiredCapacity) < FCapacityThreshold) then
    begin
      if (Round(Length(FArray) * FCapacityMultiplier)) > (ATotalRequiredCapacity + FCapacityThreshold) then
        SetLength(FArray, Round(Length(FArray) * FCapacityMultiplier)) // Expand using the Multiplier
      else
        SetLength(FArray, ATotalRequiredCapacity + FCapacityThreshold); // Expand to ensure everything fits
    end;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKListBase<T>.Clear(const ACompact: Boolean = True);
begin
  Finalize(0, FCount);
  FCount := 0;
  if ACompact then
    SetLength(FArray, LKSL_LIST_CAPACITY_DEFAULT);
end;

procedure TLKListBase<T>.Compact;
begin
  CompactActual;
end;

procedure TLKListBase<T>.CompactActual;
begin
  AcquireWriteLock;
  try
    SetLength(FArray, FCount + FCapacityThreshold + Round(FCapacityMultiplier));
  finally
    ReleaseWriteLock;
  end;
end;

function TLKListBase<T>.Contains(const AItems: Array of T): Boolean;
var
  I: Integer;
begin
  AcquireReadLock;
  try
    Result := True; // Optimistic
    for I := Low(AItems) to High(AItems) do
    begin
      Result := IndexOf(AItems[I]) >= 0;
      if (not Result) then
        Break; // No point looking further if one of the items isn't present
    end;
  finally
    ReleaseReadLock;
  end;
end;

constructor TLKListBase<T>.Create;
begin
  {$IFDEF FPC}
    inherited Create;
    InitializeArray;
  {$ELSE}
    Create(nil);
  {$ENDIF FPC}
end;

{$IFNDEF FPC}
  constructor TLKListBase<T>.Create(const AComparer: IComparer<T>);
  begin
    inherited Create;
    FComparer := AComparer;
    if FComparer = nil then
      FComparer := TComparer<T>.Default;
    InitializeArray;
  end;
{$ENDIF FPC}

function TLKListBase<T>.Contains(const AItem: T): Boolean;
begin
  Result := IndexOf(AItem) >= 0;
end;

procedure TLKListBase<T>.Delete(const AIndex: Integer);
begin
  DeleteActual(AIndex, fmFinalize, scmSmartCompact);
end;

procedure TLKListBase<T>.Delete(const AFirstIndex, ACount: Integer);
begin
  DeleteRange(AFirstIndex, ACount);
end;

procedure TLKListBase<T>.DeleteActual(const AIndex: Integer; const AFinalizeMode: TLKListFinalizeMode; const ASmartCompactMode: TLKListSmartCompactMode);
begin
  AcquireWriteLock;
  try
    if ValidateIndexInRange(AIndex) then
    begin
      Dec(FCount);
      if AFinalizeMode = fmFinalize then
      begin
        Move(AIndex + 1, AIndex, FCount - AIndex);
        Finalize(FCount, 1);
      end;
      if ASmartCompactMode = scmSmartCompact then
        SmartCompact;
    end;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKListBase<T>.DeleteRange(const AIndex, ACount: Integer);
var
  I: Integer;
begin
  AcquireWriteLock;
  try
    for I := AIndex + ACount downto AIndex do
      Delete(I);
  finally
    ReleaseWriteLock;
  end;
end;

destructor TLKListBase<T>.Destroy;
begin

  inherited;
end;

procedure TLKListBase<T>.Finalize(const AIndex, ACount: Integer);
begin
  System.FillChar(FArray[AIndex], ACount * SizeOf(T), 0);
end;

function TLKListBase<T>.GetCapacity: Integer;
begin
  AcquireReadLock;
  try
  Result := Length(FArray);
  finally
    ReleaseReadLock;
  end;
end;

function TLKListBase<T>.GetCapacityMultiplier: Single;
begin
  AcquireReadLock;
  try
    Result := FCapacityMultiplier;
  finally
    ReleaseReadLock;
  end;
end;

function TLKListBase<T>.GetCapacityThreshold: Integer;
begin
  AcquireReadLock;
  try
    Result := FCapacityThreshold;
  finally
    ReleaseReadLock;
  end;
end;

function TLKListBase<T>.GetCount: Integer;
begin
  AcquireReadLock;
  try
    Result := FCount;
  finally
    ReleaseReadLock;
  end;
end;

function TLKListBase<T>.GetIsEmpty: Boolean;
begin
  Result := (Count = 0);
end;

function TLKListBase<T>.GetItemByIndex(const AIndex: Integer): T;
begin
  AcquireReadLock;
  try
    if (AIndex > -1) and (AIndex <= FCount) then
      Result := FArray[AIndex]
    else
      raise ELKGenericCollectionsRangeException.CreateFmt('Index %d Out of Range.', [AIndex]);
  finally
    ReleaseReadLock;
  end;
end;

function TLKListBase<T>.IndexOf(const AItem: T): Integer;
var
  I: Integer;
begin
  Result := -1; // Pessimistic
  AcquireReadLock;
  try
    for I := 0 to FCount - 1 do
      if {$IFDEF FPC}CompareByte(FArray[I], AItem, SizeOf(AItem)) > 0{$ELSE}FComparer.Compare(FArray[I], AItem) = 0{$ENDIF FPC} then
      begin
        Result := I;
        Break; // No point going further if we've already found it
      end;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKListBase<T>.InitializeArray;
begin
  FCount := 0;
  SetLength(FArray, LKSL_LIST_CAPACITY_DEFAULT);
  FCapacityThreshold := LKSL_LIST_THRESHOLD_DEFAULT;
  FCapacityMultiplier := LKSL_LIST_MULTIPLIER_DEFAULT;
end;

function TLKListBase<T>.AddActual(const AItem: T): Integer;
begin
  AcquireWriteLock;
  try
    CheckCapacity(FCount + 1);
    Result := FCount;
    FArray[Result] := AItem;
    Inc(FCount);
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKListBase<T>.InsertActual(const AItem: T; const AIndex: Integer);
begin
  CheckCapacity(FCount + 1); // Ensure we have enough free slots in the Array to shift into
  Move(AIndex, AIndex + 1, FCount - AIndex); // Shift everything Right
  FArray[AIndex] := AItem; // Assign our Item to its Index
  Inc(FCount); // Incrememnt the Count
end;

procedure TLKListBase<T>.Iterate(const AIterateCallback: TLKListIterateCallbackUnbound<T>; const AIterateDirection: TLKListDirection);
var
  I: Integer;
begin
  AcquireReadLock;
  try
    case AIterateDirection of
      ldLeft: for I := FCount - 1 downto 0 do AIterateCallback(I, FArray[I]); // Reverse-order
      ldRight: for I := 0 to FCount - 1 do AIterateCallback(I, FArray[I]); // Forward-order
    end;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKListBase<T>.Iterate(const AIterateCallback: TLKListIterateCallbackUnbound<T>; const AFrom, ATo: Integer);
var
  I: Integer;
begin
  AcquireReadLock;
  try
    if ValidateRangeInArray(AFrom, ATo) then
    begin
      if AFrom < ATo then
      begin
        for I := AFrom to ATo do
          AIterateCallback(I, FArray[I]);
      end else
      begin
        for I := ATo downto AFrom do
          AIterateCallback(I, FArray[I]);
      end;
    end;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKListBase<T>.Iterate(const AIterateCallback: TLKListIterateCallbackOfObject<T>; const AIterateDirection: TLKListDirection);
var
  I: Integer;
begin
  AcquireReadLock;
  try
    case AIterateDirection of
      ldLeft: for I := FCount - 1 downto 0 do AIterateCallback(I, FArray[I]); // Reverse-order
      ldRight: for I := 0 to FCount - 1 do AIterateCallback(I, FArray[I]); // Forward-order
    end;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKListBase<T>.Iterate(const AIterateCallback: TLKListIterateCallbackOfObject<T>; const AFrom, ATo: Integer);
var
  I: Integer;
begin
  AcquireReadLock;
  try
    if ValidateRangeInArray(AFrom, ATo) then
    begin
      if AFrom < ATo then
      begin
        for I := AFrom to ATo do
          AIterateCallback(I, FArray[I]);
      end else
      begin
        for I := ATo downto AFrom do
          AIterateCallback(I, FArray[I]);
      end;
    end;
  finally
    ReleaseReadLock;
  end;
end;

{$IFDEF SUPPORTS_REFERENCETOMETHOD}
  procedure TLKListBase<T>.Iterate(const AIterateCallback: TLKListIterateCallbackAnon<T>; const AFrom, ATo: Integer);
  var
    I: Integer;
  begin
    AcquireReadLock;
    try
      if ValidateRangeInArray(AFrom, ATo) then
      begin
        if AFrom < ATo then
        begin
          for I := AFrom to ATo do
            AIterateCallback(I, FArray[I]);
        end else
        begin
          for I := ATo downto AFrom do
            AIterateCallback(I, FArray[I]);
        end;
      end;
    finally
      ReleaseReadLock;
    end;
  end;

  procedure TLKListBase<T>.Iterate(const AIterateCallback: TLKListIterateCallbackAnon<T>; const AIterateDirection: TLKListDirection = ldRight);
  var
    I: Integer;
  begin
    AcquireReadLock;
    try
      case AIterateDirection of
        ldLeft: for I := FCount - 1 downto 0 do AIterateCallback(I, FArray[I]); // Reverse-order
        ldRight: for I := 0 to FCount - 1 do AIterateCallback(I, FArray[I]); // Forward-order
      end;
    finally
      ReleaseReadLock;
    end;
  end;
{$ENDIF SUPPORTS_REFERENCETOMETHOD}

procedure TLKListBase<T>.Move(const AFromIndex, AToIndex, ACount: Integer);
begin
  AcquireWriteLock;
  try
    System.Move(FArray[AFromIndex], FArray[AToIndex], ACount * SizeOf(T));
  finally
    ReleaseWriteLock;
  end;
end;


procedure TLKListBase<T>.QuickSort(const AComparer: ILKComparer<T>; ALow, AHigh: Integer; const ASortOrder: TLKListSortOrder);
var
  I, J: Integer;
  LPivot, LTemp: T;
begin
  if (Length(FArray) = 0) or ((AHigh - ALow) <= 0) then
    Exit;

  repeat
    I := ALow;
    J := AHigh;
    LPivot := FArray[ALow + (AHigh - ALow) shr 1];
    repeat
      case ASortOrder of
        soAscending: begin
                       while AComparer.ALessThanB(FArray[I], LPivot) do
                         Inc(I);
                       while AComparer.AGreaterThanB(FArray[J], LPivot) do
                         Dec(J);
                     end;
        soDescending: begin
                       while AComparer.ALessThanB(LPivot, FArray[I]) do
                         Inc(I);
                       while AComparer.AGreaterThanB(LPivot, FArray[J]) do
                         Dec(J);
                      end;
      end;
      if I <= J then
      begin
        if I <> J then
        begin
          LTemp := FArray[I];
          FArray[I] := FArray[J];
          FArray[J] := LTemp;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if ALow < J then
      QuickSort(AComparer, ALow, J, ASortOrder);
    ALow := I;
  until I >= AHigh;
end;

procedure TLKListBase<T>.Remove(const AItems: Array of T);
var
  I: Integer;
begin
  for I := Low(AItems) to High(AItems) do
    RemoveActual(AItems[I]);
end;

function TLKListBase<T>.Remove(const AItem: T): Integer;
begin
  RemoveActual(AItem);
end;

function TLKListBase<T>.RemoveActual(const AItem: T): Integer;
begin
  Result := IndexOf(AItem);
  if Result >= 0 then
    DeleteActual(Result, fmFinalize, scmSmartCompact);
end;

procedure TLKListBase<T>.SetCapacityMultiplier(const AMultiplier: Single);
begin
  // Sanity Check on Multiplier
  if AMultiplier < LKSL_LIST_MULTIPLIER_MINIMUM then
    raise ELKGenericCollectionsLimitException.CreateFmt('Minimum Capacity Multiplier is %n', [LKSL_LIST_MULTIPLIER_MINIMUM])
  else if AMultiplier > LKSL_LIST_MULTIPLIER_MAXIMUM then
    raise ELKGenericCollectionsLimitException.CreateFmt('Maximum Capacity Multiplier is %n', [LKSL_LIST_MULTIPLIER_MAXIMUM]);
  // If we got this far, we're ready to change our Multiplier
  AcquireWriteLock;
  try
    FCapacityMultiplier := AMultiplier;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKListBase<T>.SetCapacityThreshold(const AThreshold: Integer);
begin
  // Sanity Check on Threshold
  if AThreshold < LKSL_LIST_THRESHOLD_MINIMUM then
    raise ELKGenericCollectionsLimitException.CreateFmt('Minimum Capacity Threshold is %d', [LKSL_LIST_THRESHOLD_MINIMUM]);
  // If we got this far, we're ready to change our Threshold
  AcquireWriteLock;
  try
    FCapacityThreshold := AThreshold;
    CheckCapacity; // Adjust the Array Capacity if necessary
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKListBase<T>.SetItemByIndex(const AIndex: Integer; const AItem: T);
begin
  AcquireWriteLock;
  try
    if (AIndex > -1) and (AIndex <= FCount) then
      FArray[AIndex] := AItem
    else
      raise ELKGenericCollectionsRangeException.CreateFmt('Index %d Out of Range.', [AIndex]);
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKListBase<T>.SmartCompact;
begin
  AcquireWriteLock;
  try
    if Length(FArray) > Round(FCount * FCapacityMultiplier) + FCapacityThreshold then
      CompactActual;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKListBase<T>.Sort(const AComparer: ILKComparer<T>; const ASortOrder: TLKListSortOrder = soAscending);
begin
  AcquireWriteLock;
  try
    QuickSort(AComparer, 0, FCount - 1, ASortOrder);
  finally
    ReleaseWriteLock;
  end;
end;

function TLKListBase<T>.ValidateIndexInRange(const AIndex: Integer): Boolean;
begin
  AcquireReadLock;
  try
    Result := (AIndex < FCount);
  finally
    ReleaseReadLock;
  end;
  if (not Result) then
    raise ELKGenericCollectionsRangeException.CreateFmt('Index Out of Bounds: %d', [AIndex]);
end;

function TLKListBase<T>.ValidateRangeInArray(const AFrom, ATo: Integer): Boolean;
begin
  Result := False;
  AcquireReadLock;
  try
    if (AFrom < 0) or (AFrom >= FCount) then
      raise ELKGenericCollectionsRangeException.CreateFmt('"From" Index Out of Range: %d', [AFrom]);
    if (ATo < 0) or (ATo >= FCount) then
      raise ELKGenericCollectionsRangeException.CreateFmt('"To" Index Out of Range: %d', [AFrom]);
  finally
    ReleaseReadLock;
  end;
  Result := True;
end;

{ TLKList<T> }

procedure TLKList<T>.Insert(const AItem: T; const AIndex: Integer);
begin
  AcquireWriteLock;
  try
    if ValidateIndexInRange(AIndex) then
      InsertActual(AItem, AIndex);
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKObjectList<T> }

procedure TLKObjectList<T>.Clear(const ACompact: Boolean);
var
  I: Integer;
begin
  AcquireWriteLock;
  try
    if FOwnsObjects then
    begin
      for I := 0 to FCount - 1 do
        FArray[I].{$IFDEF SUPPORTS_DISPOSEOF}DisposeOf{$ELSE}Free{$ENDIF SUPPORTS_DISPOSEOF};
    end;
    inherited;
  finally
    ReleaseWriteLock;
  end;
end;

constructor TLKObjectList<T>.Create(const AOwnsObjects: Boolean = True);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

{$IFNDEF FPC}
  constructor TLKObjectList<T>.Create(const AComparer: IComparer<T>; const AOwnsObjects: Boolean);
  begin
    inherited Create(AComparer);
    FOwnsObjects := True;
  end;
{$ENDIF FPC}

procedure TLKObjectList<T>.Delete(const AIndex: Integer);
begin
  AcquireWriteLock;
  try
    if FOwnsObjects then
      {$IFDEF FPC}
        FArray[AIndex].Free;
      {$ELSE}
        FArray[AIndex].{$IFDEF SUPPORTS_DISPOSEOF}DisposeOf{$ELSE}Free{$ENDIF SUPPORTS_DISPOSEOF};
      {$ENDIF FPC}
    inherited;
  finally
    ReleaseWriteLock;
  end;
end;

destructor TLKObjectList<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TLKObjectList<T>.GetOwnsObjects: Boolean;
begin
  AcquireReadLock;
  try
    Result := FOwnsObjects;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKObjectList<T>.SetOwnsObjects(const AOwnsObjects: Boolean);
begin
  AcquireWriteLock;
  try
    FOwnsObjects := AOwnsObjects;
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKSortedListBase<T> }

function TLKSortedListBase<T>.GetSortedPosition(const AItem: T): Integer;
var
  LIndex, LLow, LHigh: Integer;
begin
  Result := 0;
  AcquireReadLock;
  try
    LLow := 0;
    LHigh := FCount - 1;
    if LHigh = - 1 then
      Exit;
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if ALessThanOrEqualB(AItem, FArray[LIndex]) then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if ALessThanB(FArray[LHigh], AItem) then
      Result := LHigh + 1
    else if ALessThanB(FArray[LLow], AItem) then
      Result := LLow + 1
    else
      Result := LLow;
  finally
    ReleaseReadLock;
  end;
end;

{ TLKSortedList<T> }

function TLKSortedList<T>.AddActual(const AItem: T): Integer;
begin
  AcquireWriteLock;
  try
    CheckCapacity(FCount + 1);
    Result := GetSortedPosition(AItem);
    if Result = FCount then
      inherited AddActual(AItem)
    else
      InsertActual(AItem, Result);
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKSortedObjectList<T> }

procedure TLKSortedObjectList<T>.Clear(const ACompact: Boolean);
var
  I: Integer;
begin
  AcquireWriteLock;
  try
    if FOwnsObjects then
    begin
      for I := 0 to FCount - 1 do
        {$IFDEF FPC}
          FArray[I].Free;
        {$ELSE}
          FArray[I].{$IFDEF SUPPORTS_DISPOSEOF}DisposeOf{$ELSE}Free{$ENDIF SUPPORTS_DISPOSEOF};
        {$ENDIF FPC}
    end;
    inherited;
  finally
    ReleaseWriteLock;
  end;
end;

constructor TLKSortedObjectList<T>.Create(const AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

{$IFNDEF FPC}
  constructor TLKSortedObjectList<T>.Create(const AComparer: IComparer<T>; const AOwnsObjects: Boolean);
  begin
    inherited Create(AComparer);
    FOwnsObjects := AOwnsObjects;
  end;
{$ENDIF FPC}

procedure TLKSortedObjectList<T>.Delete(const AIndex: Integer);
begin
  AcquireWriteLock;
  try
    if FOwnsObjects then
      {$IFDEF FPC}
        FArray[AIndex].Free;
      {$ELSE}
        FArray[AIndex].{$IFDEF SUPPORTS_DISPOSEOF}DisposeOf{$ELSE}Free{$ENDIF SUPPORTS_DISPOSEOF};
      {$ENDIF FPC}
    inherited;
  finally
    ReleaseWriteLock;
  end;
end;

destructor TLKSortedObjectList<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TLKSortedObjectList<T>.GetOwnsObjects: Boolean;
begin
  AcquireReadLock;
  try
    Result := FOwnsObjects;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKSortedObjectList<T>.SetOwnsObjects(const AOwnsObjects: Boolean);
begin
  AcquireWriteLock;
  try
    FOwnsObjects := AOwnsObjects;
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKTreeNode<T> }
{$IFNDEF FPC}
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
  AcquireReadLock;
  try
    if IsRoot then
      Result := Self
    else
      Result := Parent.RootNode;
  finally
    ReleaseReadLock;
  end;
end;

function TLKTreeNode<T>.GetChildCount: Integer;
begin
  AcquireReadLock;
  try
    Result := FChildren.Count;
  finally
    ReleaseReadLock;
  end;
end;

function TLKTreeNode<T>.GetChildren(const AIndex: Integer): TLKTreeNode<T>;
begin
  AcquireReadLock;
  try
    Result := FChildren[AIndex];
  finally
    ReleaseReadLock;
  end;
end;

function TLKTreeNode<T>.GetIndexAsChild: Integer;
begin
  if Parent = nil then
    Result := -1
  else
    Result := Parent.IndexOf(Self);
end;

function TLKTreeNode<T>.GetIsRoot: Boolean;
begin
  AcquireReadLock;
  try
    Result := Parent = nil;
  finally
    ReleaseReadLock;
  end;
end;

function TLKTreeNode<T>.GetIsBranch: Boolean;
begin
  AcquireReadLock;
  try
    Result := (not GetIsRoot) and (not GetIsLeaf);
  finally
    ReleaseReadLock;
  end;
end;

function TLKTreeNode<T>.GetIsLeaf: Boolean;
begin
  AcquireReadLock;
  try
    Result := FChildren.Count = 0;
  finally
    ReleaseReadLock;
  end;
end;

function TLKTreeNode<T>.GetDepth: Integer;
var
  Ancestor: TLKTreeNode<T>;
begin
  AcquireReadLock;
  try
    Ancestor := Parent;
    Result := 0;

    while Ancestor <> nil do
    begin
      Inc(Result);
      Ancestor := Ancestor.Parent;
    end;
  finally
    ReleaseReadLock;
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
    FChildren.Insert(AChild, AIndex);
end;

procedure TLKTreeNode<T>.RemoveChild(const AChild: TLKTreeNode<T>);
begin
  FChildren.Remove(AChild);
end;

procedure TLKTreeNode<T>.SetValue(const AValue: T);
begin
  AcquireWriteLock;
  try
    FValue := AValue;
  finally
    ReleaseWriteLock;
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

  AcquireWriteLock;
  try
    if Parent = ANewParent then
    begin
      if AIndex <> IndexAsChild then
      begin
        Parent.FChildren.Remove(Self);
        if AIndex < 0 then
          Parent.FChildren.Add(Self)
        else
          Parent.FChildren.Insert(Self, AIndex);
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
    ReleaseWriteLock;
  end;
end;

procedure TLKTreeNode<T>.MoveTo(const AIndex: Integer);
begin
  MoveTo(Parent, AIndex);
end;

function TLKTreeNode<T>.IndexOf(const AChild: TLKTreeNode<T>): Integer;
begin
  Result := FChildren.IndexOf(AChild);
end;

{$IFDEF SUPPORTS_REFERENCETOMETHOD}
  procedure TLKTreeNode<T>.PreOrderWalk(const AAction: TLKTreeNodeValueCallbackAnon<TLKTreeNode<T>>);
  var
    LIndex: Integer;
  begin
    AAction(Self);
    for LIndex := 0 to ChildCount-1 do
      FChildren[LIndex].PreOrderWalk(AAction);
  end;

  procedure TLKTreeNode<T>.PreOrderWalk(const AAction: TLKTreeNodeValueCallbackAnon<T>);
  var
    LIndex: Integer;
  begin
    AAction(Self.Value);
    for LIndex := 0 to ChildCount-1 do
      FChildren[LIndex].PreOrderWalk(AAction);
  end;
{$ENDIF SUPPORTS_REFERENCETOMETHOD}

procedure TLKTreeNode<T>.PreOrderWalk(const AAction: TLKTreeNodeValueCallbackOfObject<TLKTreeNode<T>>);
var
  LIndex: Integer;
begin
  AAction(Self);
  for LIndex := 0 to ChildCount-1 do
    FChildren[LIndex].PreOrderWalk(AAction);
end;

procedure TLKTreeNode<T>.PreOrderWalk(const AAction: TLKTreeNodeValueCallbackOfObject<T>);
var
  LIndex: Integer;
begin
  AAction(Self.Value);
  for LIndex := 0 to ChildCount-1 do
    FChildren[LIndex].PreOrderWalk(AAction);
end;

procedure TLKTreeNode<T>.PreOrderWalk(const AAction: TLKTreeNodeValueCallbackUnbound<TLKTreeNode<T>>);
var
  LIndex: Integer;
begin
  AAction(Self);
  for LIndex := 0 to ChildCount-1 do
    FChildren[LIndex].PreOrderWalk(AAction);
end;

procedure TLKTreeNode<T>.PreOrderWalk(const AAction: TLKTreeNodeValueCallbackUnbound<T>);
var
  LIndex: Integer;
begin
  AAction(Self.Value);
  for LIndex := 0 to ChildCount-1 do
    FChildren[LIndex].PreOrderWalk(AAction);
end;

{$IFDEF SUPPORTS_REFERENCETOMETHOD}
  procedure TLKTreeNode<T>.PostOrderWalk(const AAction: TLKTreeNodeValueCallbackAnon<TLKTreeNode<T>>);
  var
    LIndex: Integer;
  begin
    for LIndex := 0 to ChildCount-1 do
      FChildren[LIndex].PostOrderWalk(AAction);
    AAction(Self);
  end;

  procedure TLKTreeNode<T>.PostOrderWalk(const AAction: TLKTreeNodeValueCallbackAnon<T>);
  var
    LIndex: Integer;
  begin
    for LIndex := 0 to ChildCount-1 do
      FChildren[LIndex].PostOrderWalk(AAction);
    AAction(Self.Value);
  end;
{$ENDIF SUPPORTS_REFERENCETOMETHOD}

procedure TLKTreeNode<T>.PostOrderWalk(const AAction: TLKTreeNodeValueCallbackOfObject<TLKTreeNode<T>>);
var
  LIndex: Integer;
begin
  for LIndex := 0 to ChildCount-1 do
    FChildren[LIndex].PostOrderWalk(AAction);
  AAction(Self);
end;

procedure TLKTreeNode<T>.PostOrderWalk(const AAction: TLKTreeNodeValueCallbackOfObject<T>);
var
  LIndex: Integer;
begin
  for LIndex := 0 to ChildCount-1 do
    FChildren[LIndex].PostOrderWalk(AAction);
  AAction(Self.Value);
end;

procedure TLKTreeNode<T>.PostOrderWalk(const AAction: TLKTreeNodeValueCallbackUnbound<TLKTreeNode<T>>);
var
  LIndex: Integer;
begin
  for LIndex := 0 to ChildCount-1 do
    FChildren[LIndex].PostOrderWalk(AAction);
  AAction(Self);
end;

procedure TLKTreeNode<T>.PostOrderWalk(const AAction: TLKTreeNodeValueCallbackUnbound<T>);
var
  LIndex: Integer;
begin
  for LIndex := 0 to ChildCount-1 do
    FChildren[LIndex].PostOrderWalk(AAction);
  AAction(Self.Value);
end;

{ TLKTreeObjectNode<T> }

constructor TLKTreeObjectNode<T>.Create;
begin
  inherited;
  FOwnsObject := True;
end;

constructor TLKTreeObjectNode<T>.Create(const AParent: TLKTreeNode<T>; const AValue: T);
begin
  inherited;
  FOwnsObject := True;
end;

constructor TLKTreeObjectNode<T>.Create(const AParent: TLKTreeNode<T>);
begin
  inherited;
  FOwnsObject := True;
end;

constructor TLKTreeObjectNode<T>.Create(const AValue: T);
begin
  inherited;
  FOwnsObject := True;
end;

destructor TLKTreeObjectNode<T>.Destroy;
begin
  if FOwnsObject then
    FValue.{$IFDEF SUPPORTS_DISPOSEOF}DisposeOf{$ELSE}Free{$ENDIF SUPPORTS_DISPOSEOF};
  inherited;
end;

function TLKTreeObjectNode<T>.GetOwnsObjects: Boolean;
begin
  AcquireReadLock;
  try
    Result := FOwnsObject;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKTreeObjectNode<T>.SetOwnsObjects(const AOwnsObjects: Boolean);
begin
  AcquireWriteLock;
  try
    FOwnsObject := AOwnsObjects;
  finally
    ReleaseWriteLock;
  end;
end;
{$ENDIF FPC}

end.

