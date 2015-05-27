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
    System.Classes, System.SysUtils,
  {$ELSE}
    Classes, SysUtils,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
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
    ILKListSorter<T> = interface;
    ILKListExpander<T> = interface;
    ILKListCompactor<T> = interface;
    ILKList<T> = interface;
    ILKObjectList<T: class> = interface;
    ILKLookupList<TKey, TValue> = interface;
    ILKObjectLookupList<TKey, TValue: class> = interface;
    ILKCircularList<T> = interface;
    ILKCircularObjectList<T: class> = interface;
    { Class Forward Declarations }
    TLKArray<T> = class;
    TLKArrayContainer<T> = class;
    TLKListSorter<T> = class;
    TLKListSorterDefault<T> = class;
    TLKListExpander<T> = class;
    TLKListExpanderDefault<T> = class;
    TLKListCompactor<T> = class;
    TLKListCompactorDefault<T> = class;
    TLKList<T> = class;
    TLKObjectList<T: class> = class;
    TLKLookupList<TKey, TValue> = class;
    TLKObjectLookupList<TKey, TValue: class> = class;
    TLKCircularList<T> = class;
    TLKCircularObjectList<T: class> = class;
  {$ENDIF FPC}

  { Exception Types }
  ELKGenericCollectionsException = class abstract(ELKException);
    ELKGenericCollectionsLimitException = class(ELKGenericCollectionsException);
    ELKGenericCollectionsRangeException = class(ELKGenericCollectionsException);
    ELKGenericCollectionsKeyAlreadyExists = class(ELKGenericCollectionsException);
    ELKGenericCollectionsKeyNotFound = class(ELKGenericCollectionsException);
    ELKGenericCollectionsCannotBeNil = class(ELKGenericCollectionsException);
      ELKGenericCollectionsNilCompactor = class(ELKGenericCollectionsCannotBeNil);
      ELKGenericCollectionsNilExpander = class(ELKGenericCollectionsCannotBeNil);
      ELKGenericCollectionsNilSorter = class(ELKGenericCollectionsCannotBeNil);      

{
  Interfaces Start Here
}

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

  ///  <summary><c>An Object containing an ILKArray instance.</c></summary>
  ///  <remarks><c>Exists merely to eliminate code replication.</c></remarks>
  ILKArrayContainer<T> = interface(ILKInterface)
  ['{9E6880CA-D55C-4166-9B72-45CCA6900488}']
    function GetArray: ILKArray<T>;
  end;

  ///  <summary><c>A Sorting Algorithm for Lists.</c></summary>
  ///  <remarks><c>Can do either Sorted Insertion or On-Demand Sorting.</c></remarks>
  ILKListSorter<T> = interface(ILKInterface)
  ['{2644E14D-A7C9-44BC-B8DD-109EC0C0A0D1}']
    // Getters
    function GetAutoSort: Boolean;
    function GetComparer: ILKComparer<T>;
    // Setters
    procedure SetAutoSort(const AAutoSort: Boolean);
    procedure SetComparer(const AComparer: ILKComparer<T>);
    // Management Methods
    function Add(const AItem: T; const AExistingCount: Integer): Boolean;
    function Sort: Boolean;
    // Properties
    property AutoSort: Boolean read GetAutoSort write SetAutoSort;
    property Comparer: ILKComparer<T> read GetComparer write SetComparer;
  end;

  ///  <summary><c>An Allocation Algorithm for Lists.</c></summary>
  ///  <remarks><c>Dictates how to grow an Array based on its current Capacity and the number of Items we're looking to Add/Insert.</c></remarks>
  ILKListExpander<T> = interface(ILKInterface)
  ['{9B4D9541-96E4-4767-81A7-5565AC24F4A9}']
    // Management Methods
    function CheckExpand(const AAmount: Integer; const AExistingCount: Integer): Integer;
  end;

  ///  <summary><c>A Deallocation Algorithm for Lists.</c></summary>
  ///  <remarks><c>Dictates how to shrink an Array based on its current Capacity and the number of Items we're looking to Delete.</c></remarks>
  ILKListCompactor<T> = interface(ILKInterface)
  ['{B72ECE0C-F629-4002-A84A-2F7FAEC122E0}']
    // Management Methods
    function CheckCompact(const AAmount: Integer; const AExistingCount: Integer): Integer;
  end;

  ///  <summary><c>Generic List Type.</c></summary>
  ///  <remarks>
  ///    <para><c>You can specify a </c>TLKListCompactor<c> to dynamically compact the List.</c></para>
  ///    <para><c>You can specify a </c>TLKListExpander<c> to dynamically expand the List.</c></para>
  ///    <para><c>You can specify a </c>TLKListSorter<c> to organize the List.</c></para>
  ///  </remarks>
  ILKList<T> = interface(ILKInterface)
  ['{FD2E0742-9079-4E03-BDA5-A39D5FAC80A0}']
    // Getters
    function GetCapacity: Integer;
    function GetCompactor: ILKListCompactor<T>;
    function GetCount: Integer;
    function GetExpander: ILKListExpander<T>;
    function GetItem(const AIndex: Integer): T;
    function GetSorter: ILKListSorter<T>;
    // Setters
    procedure SetCapacity(const ACapacity: Integer);
    procedure SetCompactor(const ACompactor: ILKListCompactor<T>);
    procedure SetExpander(const AExpander: ILKListExpander<T>);
    procedure SetItem(const AIndex: Integer; const AItem: T);
    procedure SetSorter(const ASorter: ILKListSorter<T>);
    // Management Methods
    procedure Add(const AItem: T); overload;
    procedure Add(const AList: ILKList<T>); overload;
    procedure AddItems(const AItems: Array of T);
    procedure Clear;
    procedure Delete(const AIndex: Integer);
    procedure DeleteRange(const AFirst, ACount: Integer);
    procedure Insert(const AItem: T; const AIndex: Integer);
    procedure InsertItems(const AItems: TArray<T>; const AIndex: Integer);
    // Properties
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Compactor: ILKListCompactor<T> read GetCompactor write SetCompactor;
    property Count: Integer read GetCount;
    property Expander: ILKListExpander<T> read GetExpander write SetExpander;
    property Items[const AIndex: Integer]: T read GetItem write SetItem; default;
    property Sorter: ILKListSorter<T> read GetSorter write SetSorter;
  end;

  ///  <summary><c>Specialized Generic List for Object Types</c></summary>
  ///  <remarks><c>Can take Ownership of the Objects, disposing of them for you.</c></remarks>
  ILKObjectList<T: class> = interface(ILKList<T>)
  ['{A6CBAAD7-0FAB-48D1-901B-83C70B555AA9}']
    // Getters
    function GetOwnsObjects: Boolean;
    // Setters
    procedure SetOwnsObjects(const AOwnsObjects: Boolean);
    // Properties
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  ///  <summary><c>Pairs a List of Values with a Sorted List of Keys</c></summary>
  ILKLookupList<TKey, TValue> = interface(ILKList<TValue>)
  ['{A425AFB5-E2CD-4842-BADD-5F91EC159A58}']

  end;

  ///  <summary><c>Pairs a List of Objects with a Sorted List of Keys</c></summary>
  ILKObjectLookupList<TKey, TValue: class> = interface(ILKLookupList<TKey, TValue>)
  ['{FA05DF5C-9C9B-410D-9758-6DA91671961D}']
    // Getters
    function GetOwnsObjects: Boolean;
    // Setters
    procedure SetOwnsObjects(const AOwnsObjects: Boolean);
    // Properties
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  ///  <summary><c>A Fixed-Capacity Revolving List</c></summary>
  ///  <remarks>
  ///    <para><c>When the current Index is equal to the Capacity, the Index resets to 0, and items are subsequently Replaced by new ones.</c></para>
  ///    <para><c>NOT an ancestor of ILKList.</c></para>
  ///  </remarks>
  ILKCircularList<T> = interface(ILKInterface)
  ['{229BD38F-FFFE-4CE1-89B2-4E9ED8B08E32}']
    // Getters
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): T;
    // Setters
    procedure SetItem(const AIndex: Integer; const AItem: T);
    // Management Methods
    function Add(const AItem: T): Integer;
    procedure AddItems(const AItems: Array of T);
    procedure Clear;
    procedure Delete(const AIndex: Integer);
    // Properties
    property Capacity: Integer read GetCapacity;
    property Count: Integer read GetCount;
    property Items[const AIndex: Integer]:  T read GetItem write SetItem;
  end;

  ///  <summary><c>Specialized Revolving List for Object Types</c></summary>
  ///  <remarks><c>Can take Ownership of the Objects, disposing of them for you.</c></remarks>
  ILKCircularObjectList<T: class> = interface(ILKCircularList<T>)
  ['{9BDA7DA2-F270-4AEA-BEAA-1513AC17C1E2}']
    // Getters
    function GetOwnsObjects: Boolean;
    // Setters
    procedure SetOwnsObjects(const AOwnsObjects: Boolean);
    // Properties
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

{
  Classes Start Here
}

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
    property Items[const AIndex: Integer]: T read GetItem write SetItem; default;
  end;

  ///  <summary><c>An Object containing an ILKArray instance.</c></summary>
  ///  <remarks><c>Exists merely to eliminate code replication.</c></remarks>
  TLKArrayContainer<T> = class(TLKInterfacedObject, ILKArrayContainer<T>)
  protected
    FArray: ILKArray<T>;
    function GetArray: ILKArray<T>;
  public
    constructor Create(const AArray: ILKArray<T>); reintroduce;
  end;

  ///  <summary><c>A Sorting Algorithm for Lists.</c></summary>
  ///  <remarks><c>Can do either Sorted Insertion or On-Demand Sorting.</c></remarks>
  TLKListSorter<T> = class abstract(TLKArrayContainer<T>, ILKListSorter<T>)
  private
    FComparer: ILKComparer<T>;
    FAutoSort: Boolean;
    // Getters
    function GetAutoSort: Boolean;
    function GetComparer: ILKComparer<T>;
    // Setters
    procedure SetAutoSort(const AAutoSort: Boolean);
    procedure SetComparer(const AComparer: ILKComparer<T>);
  public
    // Management Methods
    function Add(const AItem: T; const AExistingCount: Integer): Boolean; virtual; abstract;
    function Sort: Boolean; virtual; abstract;
    // Properties
    property AutoSort: Boolean read GetAutoSort write SetAutoSort;
    property Comparer: ILKComparer<T> read GetComparer write SetComparer;
  end;

  ///  <summary><c>The Default Sorting Algorithm for Lists.</c></summary>
  ///  <remarks><c>By default, Lists are entirely UNSORTED.</c></summary>
  TLKListSorterDefault<T> = class(TLKListSorter<T>)
  public
    // Management Methods
    function Add(const AItem: T; const AExistingCount: Integer): Boolean; override;
    function Sort: Boolean; override;
  end;

  ///  <summary><c>An Allocation Algorithm for Lists.</c></summary>
  ///  <remarks><c>Dictates how to grow an Array based on its current Capacity and the number of Items we're looking to Add/Insert.</c></remarks>
  TLKListExpander<T> = class abstract(TLKArrayContainer<T>, ILKListExpander<T>)
  protected
    ///  <summary><c>Override this to implement the actual Allocation Algorithm</c></summary>
    ///  <remarks><c>Must return the amount by which the Array has been Expanded.</c></remarks>
    function CheckExpandActual(const AAmount: Integer; const AExistingCount: Integer): Integer; virtual; abstract;
  public
    function CheckExpand(const AAmount: Integer; const AExistingCount: Integer): Integer; 
  end;

  ///  <summary><c>The Default Allocation Algorithm for Lists.</c></summary>
  ///  <remarks><c>By default, the Array will grow by 1 each time it becomes full</c></remarks>
  TLKListExpanderDefault<T> = class(TLKListExpander<T>)
  protected
    function CheckExpandActual(const AAmount: Integer; const AExistingCount: Integer): Integer; override;
  end;

  ///  <summary><c>A Deallocation Algorithm for Lists.</c></summary>
  ///  <remarks><c>Dictates how to shrink an Array based on its current Capacity and the number of Items we're looking to Delete.</c></remarks>
  TLKListCompactor<T> = class abstract(TLKArrayContainer<T>, ILKListCompactor<T>)
  protected
    function CheckCompactActual(const AAmount: Integer; const AExistingCount: Integer): Integer; virtual; abstract;
  public
    function CheckCompact(const AAmount: Integer; const AExistingCount: Integer): Integer;
  end;

  ///  <summary><c>The Default Deallocation Algorithm for Lists.</c></summary>
  ///  <remarks><c>By default, the Array will shrink by 1 each time an Item is removed.</c></remarks>
  TLKListCompactorDefault<T> = class(TLKListCompactor<T>)
  protected
    function CheckCompactActual(const AAmount: Integer; const AExistingCount: Integer): Integer; override;  
  end;

  ///  <summary><c>Generic List Type.</c></summary>
  ///  <remarks>
  ///    <para><c>You can specify a </c>TLKListCompactor<c> to dynamically compact the List.</c></para>
  ///    <para><c>You can specify a </c>TLKListExpander<c> to dynamically expand the List.</c></para>
  ///    <para><c>You can specify a </c>TLKListSorter<c> to organize the List.</c></para>
  ///  </remarks>
  TLKList<T> = class abstract(TLKInterfacedObject, ILKList<T>)
  private
    FArray: ILKArray<T>;
    FCount: Integer;
    FCompactor: ILKListCompactor<T>;
    FExpander: ILKListExpander<T>;
    FSorter: ILKListSorter<T>;
    // Getters
    function GetCapacity: Integer;
    function GetCompactor: ILKListCompactor<T>;
    function GetCount: Integer;
    function GetExpander: ILKListExpander<T>;
    function GetItem(const AIndex: Integer): T; inline;
    function GetSorter: ILKListSorter<T>;
    // Setters
    procedure SetCapacity(const ACapacity: Integer);
    procedure SetCompactor(const ACompactor: ILKListCompactor<T>);
    procedure SetExpander(const AExpander: ILKListExpander<T>);
    procedure SetItem(const AIndex: Integer; const AItem: T); inline;
    procedure SetSorter(const ASorter: ILKListSorter<T>);
  protected
    ///  <summary><c>Override if you need something special to occur before and/or after an Item has been added.</c></summary>
    procedure AddActual(const AItem: T);
    ///  <summary><c>Override if you need something special to occur before and/or after the list is emptied.</c></summary>
    procedure ClearActual; virtual;
    ///  <summary><c>Override if you want to use a custom Compactor</c></summary>
    ///  <remarks><c>By default, the List will be Compacted by One for each Removed Item (Default = </c>nil<c>).</c></remarks>
    function CreateDefaultCompactor: ILKListCompactor<T>; virtual;
    ///  <summary><c>Override if you want to use a custom Expander</c></summary>
    ///  <remarks>
    ///    <para><c>(Default = </c>TLKListExpanderDefault</para>
    ///  </remarks>
    function CreateDefaultExpander: ILKListExpander<T>; virtual;
    ///  <summary><c>Override if you want to use a custom Sorter</c></summary>
    ///  <remarks><c>Default = </c>TLKListSorterDefault</remarks>
    function CreateDefaultSorter: ILKListSorter<T>; virtual;
    procedure CheckCompact(const AAmount: Integer);
    procedure CheckExpand(const AAmount: Integer);
    procedure DeleteActual(const AIndex: Integer); virtual;
  public
    constructor Create(const ACapacity: Integer = 0); reintroduce;
    destructor Destroy; override;
    // Management Methods
    procedure Add(const AItem: T); overload;
    procedure Add(const AList: ILKList<T>); overload;
    procedure AddItems(const AItems: Array of T);
    procedure Clear;
    procedure Delete(const AIndex: Integer);
    procedure DeleteRange(const AFirst, ACount: Integer);
    procedure Insert(const AItem: T; const AIndex: Integer);
    procedure InsertItems(const AItems: TArray<T>; const AIndex: Integer);
    // Properties
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Compactor: ILKListCompactor<T> read GetCompactor write SetCompactor;
    property Expander: ILKListExpander<T> read GetExpander write SetExpander;
    property Items[const AIndex: Integer]: T read GetItem write SetItem; default;
    property Sorter: ILKListSorter<T> read GetSorter write SetSorter;
  end;

  ///  <summary><c>Specialized Generic List for Object Types</c></summary>
  ///  <remarks><c>Can take Ownership of the Objects, disposing of them for you.</c></remarks>
  TLKObjectList<T: class> = class(TLKList<T>, ILKObjectList<T>)
  private
    FOwnsObjects: Boolean;
    // Getters
    function GetOwnsObjects: Boolean;
    // Setters
    procedure SetOwnsObjects(const AOwnsObjects: Boolean);
  protected
    procedure DeleteActual(const AIndex: Integer); override;
  public
    constructor Create(const AOwnsObjects: Boolean = True; const ACapacity: Integer = 0); reintroduce;
    // Properties
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  ///  <summary><c>Pairs a List of Values with a Sorted List of Keys</c></summary>
  TLKLookupList<TKey, TValue> = class(TLKList<TValue>, ILKLookupList<TKey, TValue>)

  end;

  ///  <summary><c>Pairs a List of Objects with a Sorted List of Keys</c></summary>
  TLKObjectLookupList<TKey, TValue: class> = class(TLKLookupList<TKey, TValue>, ILKObjectLookupList<TKey, TValue>)
  private
    FOwnsObjects: Boolean;
    // Getters
    function GetOwnsObjects: Boolean;
    // Setters
    procedure SetOwnsObjects(const AOwnsObjects: Boolean);
  public
    constructor Create(const AOwnsObjects: Boolean = True; const ACapacity: Integer = 0); reintroduce;
    destructor Destroy; override;
    // Properties
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  ///  <summary><c>A Fixed-Capacity Revolving List</c></summary>
  ///  <remarks>
  ///    <para><c>When the current Index is equal to the Capacity, the Index resets to 0, and items are subsequently Replaced by new ones.</c></para>
  ///    <para><c>NOT an ancestor of TLKList.</c></para>
  ///  </remarks>
  TLKCircularList<T> = class(TLKInterfacedObject, ILKCircularList<T>)
  private
    FCount: Integer;
    FIndex: Integer;
    FItems: TLKArray<T>;
    // Getters
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): T;
    // Setters
    procedure SetItem(const AIndex: Integer; const AItem: T);
  protected
    ///  <summary><c>Adds or Replaces the Item in the Array and manages the Index for the NEXT call.</c></summary>
    ///  <remarks><c>This method is NOT thread-safe! Call </c>AcquireWriteLock<c> before calling it!</c></remarks>
    function AddActual(const AItem: T): Integer;
    ///  <summary><c>Manages the Finalization of Items within the Array.</c></summary>
    ///  <remarks><c>This method is NOT thread-safe! Call </c>AcquireWriteLock<c> before calling it!</c></remarks>
    procedure ClearActual;
    ///  <summary><c>Manages the Movement and Finalization of Items within the Array.</c></summary>
    ///  <remarks><c>This method is NOT thread-safe! Call </c>AcquireWriteLock<c> before calling it!</c></remarks>
    procedure DeleteActual(const AIndex: Integer);
    ///  <summary><c>Finalizes an Item at the given Index</c></summary>
    ///  <remarks><c>This method is NOT thread-safe! Call </c>AcquireWriteLock<c> before calling it!</c></remarks>
    procedure Remove(const AIndex: Integer); virtual;
  public
    constructor Create(const ACapacity: Integer); reintroduce;
    destructor Destroy; override;
    // Management Methods
    function Add(const AItem: T): Integer; virtual;
    procedure AddItems(const AItems: Array of T); virtual;
    procedure Clear; virtual;
    procedure Delete(const AIndex: Integer); virtual;
    // Properties
    property Capacity: Integer read GetCapacity;
    property Count: Integer read GetCount;
    property Items[const AIndex: Integer]:  T read GetItem write SetItem;
  end;

  ///  <summary><c>Specialized Revolving List for Object Types</c></summary>
  ///  <remarks><c>Can take Ownership of the Objects, disposing of them for you.</c></remarks>
  TLKCircularObjectList<T: class> = class(TLKCircularList<T>, ILKCircularObjectList<T>)
  private
    FOwnsObjects: Boolean;
    // Getters
    function GetOwnsObjects: Boolean;
    // Setters
    procedure SetOwnsObjects(const AOwnsObjects: Boolean);
  protected
    procedure Remove(const AIndex: Integer); override;
  public
    constructor Create(const ACapacity: Integer; const AOwnsObjects: Boolean = True); reintroduce;
    destructor Destroy; override;
    // Management Methods
    procedure Clear; override;
    procedure Delete(const AIndex: Integer); override;
    // Properties
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
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
    System.Finalize(FArray[AIndex], ACount);
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
    if (AIndex < Low(FArray)) or (AIndex > High(FArray)) then
      raise ELKGenericCollectionsRangeException.CreateFmt('Index [%d] Out Of Range', [AIndex]);
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

function TLKListSorter<T>.GetAutoSort: Boolean;
begin
  AcquireReadLock;
  try
    Result := FAutoSort;  
  finally
    ReleaseReadLock;
  end;
end;

function TLKListSorter<T>.GetComparer: ILKComparer<T>;
begin
  AcquireReadLock;
  try
    Result := FComparer;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKListSorter<T>.SetAutoSort(const AAutoSort: Boolean);
begin
  AcquireWriteLock;
  try
    FAutoSort := AAutoSort;
  finally
    ReleaseWriteLock;
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

{ TLKListSorterDefault<T> }

function TLKListSorterDefault<T>.Add(const AItem: T; const AExistingCount: Integer): Boolean;
begin
  FArray.Items[AExistingCount] := AItem;
  Result := True;
end;

function TLKListSorterDefault<T>.Sort: Boolean;
begin
  Result := False; // We aren't going to sort anything
end;

{ TLKListExpander<T> }

function TLKListExpander<T>.CheckExpand(const AAmount, AExistingCount: Integer): Integer;
begin
  FArray.AcquireWriteLock;
  try
    Result := CheckExpandActual(AAmount, AExistingCount);  
  finally
    FArray.ReleaseWriteLock;
  end;
end;

{ TLKListExpanderDefault<T> }

function TLKListExpanderDefault<T>.CheckExpandActual(const AAmount, AExistingCount: Integer): Integer;
begin
  Result := (AAmount - (FArray.Capacity - AExistingCount));
  if Result > 0 then
    FArray.Capacity := FArray.Capacity + Result;
end;

{ TLKListCompactor<T> }

function TLKListCompactor<T>.CheckCompact(const AAmount, AExistingCount: Integer): Integer;
begin
  FArray.AcquireWriteLock;
  try
    Result := CheckCompactActual(AAmount, AExistingCount);
  finally
    FArray.ReleaseWriteLock;
  end;
end;

{ TLKListCompactorDefault<T> }

function TLKListCompactorDefault<T>.CheckCompactActual(const AAmount, AExistingCount: Integer): Integer;
begin
  Result := FArray.Capacity - AExistingCount;
  if Result <= AAmount then
  begin
    FArray.Capacity := FArray.Capacity - AAmount;
    Result := AAmount;
  end else
    FArray.Capacity := FArray.Capacity - Result;
end;

{ TLKList<T> }

procedure TLKList<T>.Add(const AItem: T);
begin
  AcquireWriteLock;
  try
    CheckExpand(1);
    AddActual(AItem);
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKList<T>.Add(const AList: ILKList<T>);
var
  I: Integer;
begin
  AcquireWriteLock;
  try
    CheckExpand(AList.Count);    
    for I := 0 to AList.Count - 1 do
      AddActual(AList[I]);
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKList<T>.AddActual(const AItem: T);
begin
  if FSorter.Add(AItem, FCount) then
    Inc(FCount);
end;

procedure TLKList<T>.AddItems(const AItems: Array of T);
var
  I: Integer;
begin
  AcquireWriteLock;
  try
    CheckExpand(Length(AItems));
    for I := Low(AItems) to High(AItems) do
      AddActual(AItems[I]);    
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKList<T>.CheckCompact(const AAmount: Integer);
begin
  FCompactor.CheckCompact(AAmount, FCount);
end;

procedure TLKList<T>.CheckExpand(const AAmount: Integer);
begin
  FExpander.CheckExpand(AAmount, FCount);
end;

procedure TLKList<T>.Clear;
begin
  AcquireWriteLock;
  try
    ClearActual;  
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKList<T>.ClearActual;
begin
  DeleteRange(0, FCount);
end;

constructor TLKList<T>.Create(const ACapacity: Integer);
begin
  inherited Create;
  FArray := TLKArray<T>.Create;
  FArray.Capacity := ACapacity;
  FCount := 0;
  SetCompactor(CreateDefaultCompactor);
  SetExpander(CreateDefaultExpander);
  SetSorter(CreateDefaultSorter);
end;

function TLKList<T>.CreateDefaultCompactor: ILKListCompactor<T>;
begin
  Result := TLKListCompactorDefault<T>.Create(FArray);
end;

function TLKList<T>.CreateDefaultExpander: ILKListExpander<T>;
begin
  Result := TLKListExpanderDefault<T>.Create(FArray); 
end;

function TLKList<T>.CreateDefaultSorter: ILKListSorter<T>;
begin
  Result := TLKListSorterDefault<T>.Create(FArray); // By default, Lists are Unsorted.
end;

procedure TLKList<T>.Delete(const AIndex: Integer);
begin
  AcquireWriteLock;
  try
    DeleteActual(AIndex);
    CheckCompact(1);
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKList<T>.DeleteActual(const AIndex: Integer);
begin
  FArray.Finalize(AIndex, 1);
  if AIndex < FCount - 1 then
    FArray.Move(AIndex + 1, AIndex, FCount - AIndex); // Shift all subsequent items left by 1
  Dec(FCount); // Decrement the Count
end;

procedure TLKList<T>.DeleteRange(const AFirst, ACount: Integer);
var
  I: Integer;
begin
  AcquireWriteLock;
  try
    for I := AFirst + (ACount - 1) downto AFirst do
      DeleteActual(I);
    CheckCompact(ACount);
  finally
    ReleaseWriteLock;
  end;
end;

destructor TLKList<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TLKList<T>.GetCapacity: Integer;
begin
  Result := FArray.Capacity;
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

function TLKList<T>.GetCount: Integer;
begin
  AcquireReadLock;
  try
    Result := FCount;
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

function TLKList<T>.GetItem(const AIndex: Integer): T;
begin
  AcquireReadLock;
  try
    Result := FArray.Items[AIndex];
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

procedure TLKList<T>.Insert(const AItem: T; const AIndex: Integer);
begin
  CheckExpand(1);

end;

procedure TLKList<T>.InsertItems(const AItems: TArray<T>; const AIndex: Integer);
begin
  CheckExpand(Length(AItems));

end;

procedure TLKList<T>.SetCapacity(const ACapacity: Integer);
begin
  FArray.Capacity := ACapacity;
end;

procedure TLKList<T>.SetCompactor(const ACompactor: ILKListCompactor<T>);
begin
  if ACompactor = nil then
    raise ELKGenericCollectionsNilCompactor.Create('Cannot assign a Nil Compactor.');
  AcquireWriteLock;
  try
    FCompactor := ACompactor;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKList<T>.SetExpander(const AExpander: ILKListExpander<T>);
begin
  if AExpander = nil then
    raise ELKGenericCollectionsNilExpander.Create('Cannot assign a Nil Expander.');
  AcquireWriteLock;
  try
    FExpander := AExpander;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKList<T>.SetItem(const AIndex: Integer; const AItem: T);
begin
  AcquireWriteLock;
  try
    FArray.Items[AIndex] := AItem;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKList<T>.SetSorter(const ASorter: ILKListSorter<T>);
begin
  if ASorter = nil then
    raise ELKGenericCollectionsNilSorter.Create('Cannot assign a Nil Sorter.');    
  AcquireWriteLock;
  try
    FSorter := ASorter;
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKObjectList<T> }

constructor TLKObjectList<T>.Create(const AOwnsObjects: Boolean; const ACapacity: Integer);
begin
  inherited Create(ACapacity);
  FOwnsObjects := AOwnsObjects;
end;

procedure TLKObjectList<T>.DeleteActual(const AIndex: Integer);
begin
  if FOwnsObjects then  
    FArray.Items[AIndex].{$IFDEF SUPPORTS_DISPOSEOF}DisposeOf{$ELSE}Free{$ENDIF SUPPORTS_DISPOSEOF};
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

{ TLKObjectLookupList<T> }

constructor TLKObjectLookupList<TKey, TValue>.Create(const AOwnsObjects: Boolean; const ACapacity: Integer);
begin
  inherited Create(ACapacity);
  FOwnsObjects := AOwnsObjects;
end;

destructor TLKObjectLookupList<TKey, TValue>.Destroy;
begin

  inherited;
end;

function TLKObjectLookupList<TKey, TValue>.GetOwnsObjects: Boolean;
begin
  AcquireReadLock;
  try
    Result := FOwnsObjects;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKObjectLookupList<TKey, TValue>.SetOwnsObjects(const AOwnsObjects: Boolean);
begin
  AcquireWriteLock;
  try
    FOwnsObjects := AOwnsObjects;
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKCircularList<T> }

function TLKCircularList<T>.Add(const AItem: T): Integer;
begin
  AcquireWriteLock;
  try
    Result := AddActual(AItem);
  finally
    ReleaseWriteLock;
  end;
end;

function TLKCircularList<T>.AddActual(const AItem: T): Integer;
begin
  Result := FIndex;
  if FIndex <= FCount then
    Remove(FIndex);
  FItems[FIndex] := AItem;
  Inc(FIndex);
  if FIndex > FItems.Capacity - 1 then
    FIndex := 0;
  if FCount <= FItems.Capacity - 1 then
    Inc(FCount);
end;

procedure TLKCircularList<T>.AddItems(const AItems: array of T);
var
  I: Integer;
begin
  AcquireWriteLock;
  try
    for I := Low(AItems) to High(AItems) do
      AddActual(AItems[I]);
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKCircularList<T>.Clear;
begin
  AcquireWriteLock;
  try
    ClearActual;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKCircularList<T>.ClearActual;
begin
  FItems.Finalize(0, FCount - 1);
  FCount := 0;
  FIndex := 0;
end;

constructor TLKCircularList<T>.Create(const ACapacity: Integer);
begin
  FItems := TLKArray<T>.Create(ACapacity);
  inherited Create;
  FCount := 0;
  FIndex := 0;
end;

procedure TLKCircularList<T>.Delete(const AIndex: Integer);
begin
  AcquireWriteLock;
  try
    DeleteActual(AIndex);
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKCircularList<T>.DeleteActual(const AIndex: Integer);
begin
  FItems.Finalize(AIndex, 1); // Finalize the item at the specified Index
  if AIndex < FItems.Capacity then
    FItems.Move(AIndex + 1, AIndex, FCount - AIndex); // Shift all subsequent items left by 1
  Dec(FCount); // Decrement the Count
  if AIndex <= FIndex then
    Dec(FIndex); // Shift the Index back by 1
end;

destructor TLKCircularList<T>.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TLKCircularList<T>.GetCapacity: Integer;
begin
  AcquireReadLock;
  try
    Result := FItems.Capacity;
  finally
    ReleaseReadLock;
  end;
end;

function TLKCircularList<T>.GetCount: Integer;
begin
  AcquireReadLock;
  try
    Result := FCount;
  finally
    ReleaseReadLock;
  end;
end;

function TLKCircularList<T>.GetItem(const AIndex: Integer): T;
begin
  AcquireReadLock;
  try
    if (AIndex < 0) or (AIndex > FCount - 1) then
      raise ELKGenericCollectionsRangeException.CreateFmt('Index [%d] Out Of Range', [AIndex]);
    Result := FItems[AIndex];
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKCircularList<T>.Remove(const AIndex: Integer);
begin
  FItems.Finalize(AIndex, 1);
end;

procedure TLKCircularList<T>.SetItem(const AIndex: Integer; const AItem: T);
begin
  AcquireWriteLock;
  try
    // TODO -oSJS -cGenerics Redux: Index Validation here!
    FItems[AIndex] := AItem;
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKCircularObjectList<T> }

procedure TLKCircularObjectList<T>.Clear;
var
  I: Integer;
begin
  AcquireWriteLock;
  try
    if FOwnsObjects then
    begin
      for I := 0 to FCount - 1 do
        FItems[I].{$IFDEF SUPPORTS_DISPOSEOF}DisposeOf{$ELSE}Free{$ENDIF SUPPORTS_DISPOSEOF};
    end;
    ClearActual;
  finally
    ReleaseWriteLock;
  end;
end;

constructor TLKCircularObjectList<T>.Create(const ACapacity: Integer; const AOwnsObjects: Boolean);
begin
  inherited Create(ACapacity);
  FOwnsObjects := AOwnsObjects;
end;

procedure TLKCircularObjectList<T>.Delete(const AIndex: Integer);
begin
  AcquireWriteLock;
  try
    if FOwnsObjects then
      FItems[AIndex].{$IFDEF SUPPORTS_DISPOSEOF}DisposeOf{$ELSE}Free{$ENDIF SUPPORTS_DISPOSEOF};
    DeleteActual(AIndex);
  finally
    ReleaseWriteLock;
  end;
end;

destructor TLKCircularObjectList<T>.Destroy;
var
  I: Integer;
begin
  if FOwnsObjects then
  begin
    for I := 0 to FCount - 1 do
      FItems[I].{$IFDEF SUPPORTS_DISPOSEOF}DisposeOf{$ELSE}Free{$ENDIF SUPPORTS_DISPOSEOF};
  end;
  inherited;
end;

function TLKCircularObjectList<T>.GetOwnsObjects: Boolean;
begin
  AcquireReadLock;
  try
    Result := FOwnsObjects;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKCircularObjectList<T>.Remove(const AIndex: Integer);
begin
  if FOwnsObjects then
    FItems[AIndex].{$IFDEF SUPPORTS_DISPOSEOF}DisposeOf{$ELSE}Free{$ENDIF SUPPORTS_DISPOSEOF};
  inherited;
end;

procedure TLKCircularObjectList<T>.SetOwnsObjects(const AOwnsObjects: Boolean);
begin
  AcquireWriteLock;
  try
    FOwnsObjects := AOwnsObjects;
  finally
    ReleaseWriteLock;
  end;
end;

end.
