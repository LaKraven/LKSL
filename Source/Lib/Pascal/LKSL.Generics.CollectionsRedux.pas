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
    ILKListExpander = interface;
    ILKListCompactor = interface;
    ILKListBase<T> = interface;
    ILKList<T> = interface;
    ILKObjectList<T: class> = interface;
    ILKSortedList<T> = interface;
    ILKSortedObjectList<T> = interface;
    ILKLookupList<TKey, TValue> = interface;
    ILKLookupObjectList<TKey, TValue: class> = interface;
    ILKCircularList<T> = interface;
    ILKCircularObjectList<T: class> = interface;
    { Class Forward Declarations }
    TLKArray<T> = class;
    TLKArrayContainer<T> = class;
    TLKListSorter<T> = class;
    TLKListSorterDefault<T> = class;
    TLKListExpander = class;
    TLKListExpanderDefault = class;
    TLKListExpanderGeometric = class;
    TLKListCompactor = class;
    TLKListCompactorDefault = class;
    TLKListBase<T> = class;
    TLKList<T> = class;
    TLKObjectList<T: class> = class;
    TLKSortedList<T> = class;
    TLKSortedObjectList<T: class> = class;
    TLKLookupList<TKey, TValue> = class;
    TLKLookupObjectList<TKey, TValue: class> = class;
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
    property Items[const AIndex: Integer]: T read GetItem write SetItem; default;
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
    function Contains(const AItem: T; const AExistingCount: Integer): Boolean;
    function Sort: Boolean;
    // Properties
    property AutoSort: Boolean read GetAutoSort write SetAutoSort;
    property Comparer: ILKComparer<T> read GetComparer write SetComparer;
  end;

  ///  <summary><c>An Allocation Algorithm for Lists.</c></summary>
  ///  <remarks><c>Dictates how to grow an Array based on its current Capacity and the number of Items we're looking to Add/Insert.</c></remarks>
  ILKListExpander = interface(ILKInterface)
  ['{9B4D9541-96E4-4767-81A7-5565AC24F4A9}']
    // Management Methods
    function CheckExpand(const ACapacity, ACurrentCount, AAdditionalRequired: Integer): Integer;
  end;

  ILKListExpanderGeometric = interface(ILKListExpander)
  ['{670B52D1-F655-43CB-84F0-243E52D4E54F}']
    // Getters
    function GetCapacityMultiplier: Single;
    function GetCapacityThreshold: Integer;
    // Setters
    procedure SetCapacityMultiplier(const AMultiplier: Single);
    procedure SetCapacityThreshold(const AThreshold: Integer);
    // Properties
    property CapacityMultiplier: Single read GetCapacityMultiplier write SetCapacityMultiplier;
    property CapacityThreshold: Integer read GetCapacityThreshold write SetCapacityThreshold;
  end;

  ///  <summary><c>A Deallocation Algorithm for Lists.</c></summary>
  ///  <remarks><c>Dictates how to shrink an Array based on its current Capacity and the number of Items we're looking to Delete.</c></remarks>
  ILKListCompactor = interface(ILKInterface)
  ['{B72ECE0C-F629-4002-A84A-2F7FAEC122E0}']
    // Management Methods
    function CheckCompact(const ACapacity, ACurrentCount, AVacating: Integer): Integer;
  end;

  ///  <summary><c>Base Interface for Generic List Types.</c></summary>
  ///  <remarks>
  ///    <para><c>You can specify a </c>TLKListCompactor<c> to dynamically compact the List.</c></para>
  ///    <para><c>You can specify a </c>TLKListExpander<c> to dynamically expand the List.</c></para>
  ///  </remarks>
  ILKListBase<T> = interface(ILKInterface)
  ['{FD2E0742-9079-4E03-BDA5-A39D5FAC80A0}']
    // Getters
    function GetCapacity: Integer;
    function GetCompactor: ILKListCompactor;
    function GetCount: Integer;
    function GetExpander: ILKListExpander;
    function GetItem(const AIndex: Integer): T;
    // Setters
    procedure SetCapacity(const ACapacity: Integer);
    procedure SetCompactor(const ACompactor: ILKListCompactor);
    procedure SetExpander(const AExpander: ILKListExpander);
    procedure SetItem(const AIndex: Integer; const AItem: T);
    // Management Methods
    procedure Add(const AItem: T); overload;
    procedure Add(const AList: ILKListBase<T>); overload;
    procedure AddItems(const AItems: Array of T);
    procedure Clear;
    procedure Delete(const AIndex: Integer);
    procedure DeleteRange(const AFirst, ACount: Integer);
    procedure Insert(const AItem: T; const AIndex: Integer);
    procedure InsertItems(const AItems: TArray<T>; const AIndex: Integer);
    // Properties
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Compactor: ILKListCompactor read GetCompactor write SetCompactor;
    property Count: Integer read GetCount;
    property Expander: ILKListExpander read GetExpander write SetExpander;
    property Items[const AIndex: Integer]: T read GetItem write SetItem; default;
  end;

  ///  <summary><c>A Simple Generic List.</c></summary>
  ILKList<T> = interface(ILKListBase<T>)
  ['{62EFA497-D06C-42C4-B9D6-CD3FA1842D57}']

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

  ///  <summary><c>Sorted Generic List Type.</c></summary>
  ///  <remarks>
  ///    <para><c>You can specify a </c>TLKListSorter<c> to organize the List.</c></para>
  ///  </remarks>
  ILKSortedList<T> = interface(ILKListBase<T>)
  ['{C0FE590C-218B-46A2-890D-DE05422F9DE8}']
    // Getters
    function GetSorter: ILKListSorter<T>;
    // Setters
    procedure SetSorter(const ASorter: ILKListSorter<T>);
    // Properties
    property Sorter: ILKListSorter<T> read GetSorter write SetSorter;
  end;

  ILKSortedObjectList<T> = interface(ILKSortedList<T>)
  ['{0BB00F5C-19CC-4311-A566-1F012E058377}']
    // Getters
    function GetOwnsObjects: Boolean;
    // Setters
    procedure SetOwnsObjects(const AOwnsObjects: Boolean);
    // Properties
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  ///  <summary><c>Pairs a List of Values with a Sorted List of Keys</c></summary>
  ILKLookupList<TKey, TValue> = interface(ILKListBase<TValue>)
  ['{A425AFB5-E2CD-4842-BADD-5F91EC159A58}']

  end;

  ///  <summary><c>Pairs a List of Objects with a Sorted List of Keys</c></summary>
  ILKLookupObjectList<TKey, TValue: class> = interface(ILKLookupList<TKey, TValue>)
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
  protected
    function AddActual(const AItem: T; const AExistingCount: Integer): Boolean; virtual; abstract;
    function ContainsActual(const AItem: T; const AExistingCount: Integer): Boolean; virtual; abstract;
    function SortActual: Boolean; virtual; abstract;
  public
    // Management Methods
    function Add(const AItem: T; const AExistingCount: Integer): Boolean;
    function Contains(const AItem: T; const AExistingCount: Integer): Boolean;
    function Sort: Boolean;
    // Properties
    property AutoSort: Boolean read GetAutoSort write SetAutoSort;
    property Comparer: ILKComparer<T> read GetComparer write SetComparer;
  end;

  ///  <summary><c>The Default Sorting Algorithm for Lists.</c></summary>
  ///  <remarks><c>By default, Lists are entirely UNSORTED.</c></summary>
  TLKListSorterDefault<T> = class(TLKListSorter<T>)
  protected
    function AddActual(const AItem: T; const AExistingCount: Integer): Boolean; override;
    function ContainsActual(const AItem: T; const AExistingCount: Integer): Boolean; override;
    function SortActual: Boolean; override;
  end;

  ///  <summary><c>An Allocation Algorithm for Lists.</c></summary>
  ///  <remarks><c>Dictates how to grow an Array based on its current Capacity and the number of Items we're looking to Add/Insert.</c></remarks>
  TLKListExpander = class abstract(TLKInterfacedObject, ILKListExpander)
  public
    ///  <summary><c>Override this to implement the actual Allocation Algorithm</c></summary>
    ///  <remarks><c>Must return the amount by which the Array has been Expanded.</c></remarks>
    function CheckExpand(const ACapacity, ACurrentCount, AAdditionalRequired: Integer): Integer; virtual; abstract;
  end;

  ///  <summary><c>The Default Allocation Algorithm for Lists.</c></summary>
  ///  <remarks><c>By default, the Array will grow by 1 each time it becomes full</c></remarks>
  TLKListExpanderDefault = class(TLKListExpander)
  public
    function CheckExpand(const ACapacity, ACurrentCount, AAdditionalRequired: Integer): Integer; override;
  end;

  ///  <summary><c>A Geometric Allocation Algorithm for Lists.</c></summary>
  ///  <remarks><c>When the number of Vacant Slots falls below the Threshold, the number of Vacant Slots increases by the value of the current Capacity multiplied by the Mulitplier.</c></remarks>
  TLKListExpanderGeometric = class(TLKListExpander, ILKListExpanderGeometric)
  private
    FMultiplier: Single;
    FThreshold: Integer;
    // Getters
    function GetCapacityMultiplier: Single;
    function GetCapacityThreshold: Integer;
    // Setters
    procedure SetCapacityMultiplier(const AMultiplier: Single);
    procedure SetCapacityThreshold(const AThreshold: Integer);
  public
    function CheckExpand(const ACapacity, ACurrentCount, AAdditionalRequired: Integer): Integer; override;
  public

    // Properties
    property CapacityMultiplier: Single read GetCapacityMultiplier write SetCapacityMultiplier;
    property CapacityThreshold: Integer read GetCapacityThreshold write SetCapacityThreshold;
  end;

  ///  <summary><c>A Deallocation Algorithm for Lists.</c></summary>
  ///  <remarks><c>Dictates how to shrink an Array based on its current Capacity and the number of Items we're looking to Delete.</c></remarks>
  TLKListCompactor = class abstract(TLKInterfacedObject, ILKListCompactor)
  public
    function CheckCompact(const ACapacity, ACurrentCount, AVacating: Integer): Integer; virtual; abstract;
  end;

  ///  <summary><c>The Default Deallocation Algorithm for Lists.</c></summary>
  ///  <remarks><c>By default, the Array will shrink by 1 each time an Item is removed.</c></remarks>
  TLKListCompactorDefault = class(TLKListCompactor)
  public
    function CheckCompact(const ACapacity, ACurrentCount, AVacating: Integer): Integer; override;
  end;

  ///  <summary><c>Absolute Base Type for Generic Lists.</c></summary>
  ///  <remarks>
  ///    <para><c>You can specify a </c>TLKListCompactor<c> to dynamically compact the List.</c></para>
  ///    <para><c>You can specify a </c>TLKListExpander<c> to dynamically expand the List.</c></para>
  ///    <para><c>You can specify a </c>TLKListSorter<c> to organize the List.</c></para>
  ///  </remarks>
  TLKListBase<T> = class abstract(TLKInterfacedObject, ILKListBase<T>)
  private
    FArray: ILKArray<T>;
    FCount: Integer;
    FCompactor: ILKListCompactor;
    FExpander: ILKListExpander;
    // Getters
    function GetCapacity: Integer;
    function GetCompactor: ILKListCompactor;
    function GetCount: Integer;
    function GetExpander: ILKListExpander;
    function GetItem(const AIndex: Integer): T; inline;
    // Setters
    procedure SetCapacity(const ACapacity: Integer);
    procedure SetCompactor(const ACompactor: ILKListCompactor);
    procedure SetExpander(const AExpander: ILKListExpander);
    procedure SetItem(const AIndex: Integer; const AItem: T); inline;
  protected
    ///  <summary><c>Override if you need something special to occur before and/or after an Item has been added.</c></summary>
    function AddActual(const AItem: T): Boolean; virtual; abstract;
    ///  <summary><c>Compacts the Array according to the given Compactor Algorithm.</c></summary>
    procedure CheckCompact(const AAmount: Integer);
    ///  <summary><c>Expands the Array according to the given Expander Algorithm.</c></summary>
    procedure CheckExpand(const AAmount: Integer);
    ///  <summary><c>Override if you need something special to occur before and/or after the list is emptied.</c></summary>
    procedure ClearActual; virtual;
    ///  <summary><c>Override if you want to use a custom Compactor</c></summary>
    ///  <remarks><c>By default, the List will be Compacted by One for each Removed Item (Default = </c>nil<c>).</c></remarks>
    function CreateDefaultCompactor: ILKListCompactor; virtual;
    ///  <summary><c>Override if you want to use a custom Expander</c></summary>
    ///  <remarks><c>(Default = </c>TLKListExpanderDefault</remarks>
    function CreateDefaultExpander: ILKListExpander; virtual;
    ///  <summary><c>Override if you require some custom behavior when Deleting an Item.</c></summary>
    function DeleteActual(const AIndex: Integer): Boolean; virtual;
  public
    constructor Create(const ACapacity: Integer = 0); reintroduce; virtual;
    destructor Destroy; override;
    // Management Methods
    procedure Add(const AItem: T); overload;
    procedure Add(const AList: ILKListBase<T>); overload;
    procedure AddItems(const AItems: Array of T);
    procedure Clear;
    procedure Delete(const AIndex: Integer);
    procedure DeleteRange(const AFirst, ACount: Integer);
    procedure Insert(const AItem: T; const AIndex: Integer);
    procedure InsertItems(const AItems: TArray<T>; const AIndex: Integer);
    // Properties
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Compactor: ILKListCompactor read GetCompactor write SetCompactor;
    property Expander: ILKListExpander read GetExpander write SetExpander;
    property Items[const AIndex: Integer]: T read GetItem write SetItem; default;
  end;

  TLKList<T> = class abstract(TLKListBase<T>, ILKList<T>)
  protected
    function AddActual(const AItem: T): Boolean; override;
  end;

  ///  <summary><c>Specialized Generic List for Object Types</c></summary>
  ///  <remarks><c>Can take Ownership of the Objects, disposing of them for you.</c></remarks>
  TLKObjectList<T: class> = class abstract(TLKList<T>, ILKObjectList<T>)
  private
    FOwnsObjects: Boolean;
    // Getters
    function GetOwnsObjects: Boolean;
    // Setters
    procedure SetOwnsObjects(const AOwnsObjects: Boolean);
  protected
    function DeleteActual(const AIndex: Integer): Boolean; override;
  public
    constructor Create(const AOwnsObjects: Boolean = True; const ACapacity: Integer = 0); reintroduce;
    // Properties
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  ///  <summary><c>A Generic List Type with Sorting and Lookup capabilities.</c></summary>
  ///  <remarks>
  ///    <para><c>You can specify a </c>TLKListSorter<c> to organize the List.</c></para>
  ///  </remarks>
  TLKSortedList<T> = class abstract(TLKListBase<T>, ILKSortedList<T>)
  private
    FSorter: ILKListSorter<T>;
    // Getters
    function GetSorter: ILKListSorter<T>;
    // Setters
    procedure SetSorter(const ASorter: ILKListSorter<T>);
  protected
    ///  <summary><c>Override if you want to use a custom Sorter</c></summary>
    ///  <remarks><c>Default = </c>TLKListSorterDefault</remarks>
    function CreateDefaultSorter: ILKListSorter<T>; virtual;
  public
    constructor Create(const ACapacity: Integer = 0; const ASorter: ILKListSorter<T> = nil); reintroduce;
    // Properties
    property Sorter: ILKListSorter<T> read GetSorter write SetSorter;
  end;

  ///  <summary><c>Specialized Sorted Generic List for Object Types</c></summary>
  ///  <remarks><c>Can take Ownership of the Objects, disposing of them for you.</c></remarks>
  TLKSortedObjectList<T: class> = class abstract(TLKSortedList<T>, ILKSortedObjectList<T>)
  private
    FOwnsObjects: Boolean;
    // Getters
    function GetOwnsObjects: Boolean;
    // Setters
    procedure SetOwnsObjects(const AOwnsObjects: Boolean);
  public
    constructor Create(const AOwnsObjects: Boolean = True; const ACapacity: Integer = 0; const ASorter: ILKListSorter<T> = nil); reintroduce;
    // Properties
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  ///  <summary><c>Pairs a List of Values with a Sorted List of Keys</c></summary>
  TLKLookupList<TKey, TValue> = class(TLKListBase<TValue>, ILKLookupList<TKey, TValue>)

  end;

  ///  <summary><c>Pairs a List of Objects with a Sorted List of Keys</c></summary>
  TLKLookupObjectList<TKey, TValue: class> = class(TLKLookupList<TKey, TValue>, ILKLookupObjectList<TKey, TValue>)
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
    FItems: ILKArray<T>;
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

function TLKListSorter<T>.Add(const AItem: T; const AExistingCount: Integer): Boolean;
begin
  AcquireWriteLock;
  try
    Result := AddActual(AItem, AExistingCount);
  finally
    ReleaseWriteLock;
  end;
end;

function TLKListSorter<T>.Contains(const AItem: T; const AExistingCount: Integer): Boolean;
begin
  AcquireReadLock;
  try
    Result := ContainsActual(AItem, AExistingCount);
  finally
    ReleaseReadLock;
  end;
end;

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

function TLKListSorter<T>.Sort: Boolean;
begin
  AcquireWriteLock;
  try
    Result := SortActual;
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKListSorterDefault<T> }

function TLKListSorterDefault<T>.AddActual(const AItem: T; const AExistingCount: Integer): Boolean;
begin
  FArray[AExistingCount] := AItem;
  Result := True;
end;

function TLKListSorterDefault<T>.ContainsActual(const AItem: T; const AExistingCount: Integer): Boolean;
begin

end;

function TLKListSorterDefault<T>.SortActual: Boolean;
begin
  Result := False; // We aren't going to sort anything
end;

{ TLKListExpanderDefault }

function TLKListExpanderDefault.CheckExpand(const ACapacity, ACurrentCount, AAdditionalRequired: Integer): Integer;
begin
  if ACurrentCount + AAdditionalRequired > ACapacity then
    Result := (ACapacity - ACurrentCount) + AAdditionalRequired
  else
    Result := 0;
end;

{ TLKListExpanderGeometric }

function TLKListExpanderGeometric.CheckExpand(const ACapacity, ACurrentCount, AAdditionalRequired: Integer): Integer;
begin

end;

function TLKListExpanderGeometric.GetCapacityMultiplier: Single;
begin

end;

function TLKListExpanderGeometric.GetCapacityThreshold: Integer;
begin

end;

procedure TLKListExpanderGeometric.SetCapacityMultiplier(const AMultiplier: Single);
begin

end;

procedure TLKListExpanderGeometric.SetCapacityThreshold(const AThreshold: Integer);
begin

end;

{ TLKListCompactorDefault }

function TLKListCompactorDefault.CheckCompact(const ACapacity, ACurrentCount, AVacating: Integer): Integer;
begin
  Result := AVacating;
end;

{ TLKListBase<T> }

procedure TLKListBase<T>.Add(const AItem: T);
begin
  AcquireWriteLock;
  try
    CheckExpand(1);
    if AddActual(AItem) then
      Inc(FCount);
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKListBase<T>.Add(const AList: ILKListBase<T>);
var
  I: Integer;
begin
  AcquireWriteLock;
  try
    CheckExpand(AList.Count);    
    for I := 0 to AList.Count - 1 do
      if AddActual(AList[I]) then
        Inc(FCount);
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKListBase<T>.AddItems(const AItems: Array of T);
var
  I: Integer;
begin
  AcquireWriteLock;
  try
    CheckExpand(Length(AItems));
    for I := Low(AItems) to High(AItems) do
      if AddActual(AItems[I]) then
        Inc(FCount);
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKListBase<T>.CheckCompact(const AAmount: Integer);
var
  LShrinkBy: Integer;
begin
  LShrinkBy := FCompactor.CheckCompact(FArray.Capacity, FCount, AAmount);
  if LShrinkBy > 0 then
    FArray.Capacity := FArray.Capacity - LShrinkBy;
end;

procedure TLKListBase<T>.CheckExpand(const AAmount: Integer);
var
  LNewCapacity: Integer;
begin
  LNewCapacity := FExpander.CheckExpand(FArray.Capacity, FCount, AAmount);
  if LNewCapacity > 0 then
    FArray.Capacity := FArray.Capacity + LNewCapacity;
end;

procedure TLKListBase<T>.Clear;
var
  LCount: Integer;
begin
  AcquireWriteLock;
  try
    ClearActual;
  finally
    ReleaseWriteLock;
  end;
end;

procedure TLKListBase<T>.ClearActual;
begin
  DeleteRange(0, FCount);
end;

constructor TLKListBase<T>.Create(const ACapacity: Integer);
begin
  inherited Create;
  FArray := TLKArray<T>.Create;
  FArray.Capacity := ACapacity;
  FCount := 0;
  SetCompactor(CreateDefaultCompactor);
  SetExpander(CreateDefaultExpander);
end;

function TLKListBase<T>.CreateDefaultCompactor: ILKListCompactor;
begin
  Result := TLKListCompactorDefault.Create;
end;

function TLKListBase<T>.CreateDefaultExpander: ILKListExpander;
begin
  Result := TLKListExpanderDefault.Create;
end;

procedure TLKListBase<T>.Delete(const AIndex: Integer);
begin
  AcquireWriteLock;
  try
    if DeleteActual(AIndex) then
    begin
      Dec(FCount);
      CheckCompact(1);
    end;
  finally
    ReleaseWriteLock;
  end;
end;

function TLKListBase<T>.DeleteActual(const AIndex: Integer): Boolean;
begin
  FArray.Finalize(AIndex, 1);
  if AIndex < FCount - 1 then
    FArray.Move(AIndex + 1, AIndex, FCount - AIndex); // Shift all subsequent items left by 1
  Result := True;
end;

procedure TLKListBase<T>.DeleteRange(const AFirst, ACount: Integer);
var
  I: Integer;
begin
  AcquireWriteLock;
  try
    for I := AFirst + (ACount - 1) downto AFirst do
      if DeleteActual(I) then
        Dec(FCount);
    CheckCompact(ACount);
  finally
    ReleaseWriteLock;
  end;
end;

destructor TLKListBase<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TLKListBase<T>.GetCapacity: Integer;
begin
  Result := FArray.Capacity;
end;

function TLKListBase<T>.GetCompactor: ILKListCompactor;
begin
  AcquireReadLock;
  try
    Result := FCompactor;
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

function TLKListBase<T>.GetExpander: ILKListExpander;
begin
  AcquireReadLock;
  try
    Result := FExpander;
  finally
    ReleaseReadLock;
  end;
end;

function TLKListBase<T>.GetItem(const AIndex: Integer): T;
begin
  AcquireReadLock;
  try
    Result := FArray[AIndex];
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKListBase<T>.Insert(const AItem: T; const AIndex: Integer);
begin
  CheckExpand(1);

end;

procedure TLKListBase<T>.InsertItems(const AItems: TArray<T>; const AIndex: Integer);
begin
  CheckExpand(Length(AItems));

end;

procedure TLKListBase<T>.SetCapacity(const ACapacity: Integer);
begin
  FArray.Capacity := ACapacity;
end;

procedure TLKListBase<T>.SetCompactor(const ACompactor: ILKListCompactor);
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

procedure TLKListBase<T>.SetExpander(const AExpander: ILKListExpander);
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

procedure TLKListBase<T>.SetItem(const AIndex: Integer; const AItem: T);
begin
  AcquireWriteLock;
  try
    FArray[AIndex] := AItem;
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKList<T> }

function TLKList<T>.AddActual(const AItem: T): Boolean;
begin
  FArray[FCount] := AItem;
  Result := True;
end;

{ TLKObjectList<T> }

constructor TLKObjectList<T>.Create(const AOwnsObjects: Boolean; const ACapacity: Integer);
begin
  inherited Create(ACapacity);
  FOwnsObjects := AOwnsObjects;
end;

function TLKObjectList<T>.DeleteActual(const AIndex: Integer): Boolean;
begin
  if FOwnsObjects then  
    FArray[AIndex].{$IFDEF SUPPORTS_DISPOSEOF}DisposeOf{$ELSE}Free{$ENDIF SUPPORTS_DISPOSEOF};
  Result := inherited;
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

{ TLKSortedList<T> }

constructor TLKSortedList<T>.Create(const ACapacity: Integer; const ASorter: ILKListSorter<T>);
begin
  inherited Create(ACapacity);
  if ASorter <> nil then
    FSorter := ASorter
  else
    FSorter := CreateDefaultSorter;
end;

function TLKSortedList<T>.CreateDefaultSorter: ILKListSorter<T>;
begin
  Result := TLKListSorterDefault<T>.Create(FArray);
end;

function TLKSortedList<T>.GetSorter: ILKListSorter<T>;
begin
  AcquireReadLock;
  try
    Result := FSorter;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKSortedList<T>.SetSorter(const ASorter: ILKListSorter<T>);
begin
  if ASorter = nil then
    raise ELKGenericCollectionsNilSorter.Create('Sorter Cannot be Nil.');
  AcquireWriteLock;
  try
    FSorter := ASorter;
  finally
    ReleaseWriteLock;
  end;
end;

{ TLKSortedObjectList<T> }

constructor TLKSortedObjectList<T>.Create(const AOwnsObjects: Boolean; const ACapacity: Integer; const ASorter: ILKListSorter<T>);
begin
  inherited Create(ACapacity, ASorter);
  FOwnsObjects := AOwnsObjects;
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

{ TLKLookupObjectList<T> }

constructor TLKLookupObjectList<TKey, TValue>.Create(const AOwnsObjects: Boolean; const ACapacity: Integer);
begin
  inherited Create(ACapacity);
  FOwnsObjects := AOwnsObjects;
end;

destructor TLKLookupObjectList<TKey, TValue>.Destroy;
begin

  inherited;
end;

function TLKLookupObjectList<TKey, TValue>.GetOwnsObjects: Boolean;
begin
  AcquireReadLock;
  try
    Result := FOwnsObjects;
  finally
    ReleaseReadLock;
  end;
end;

procedure TLKLookupObjectList<TKey, TValue>.SetOwnsObjects(const AOwnsObjects: Boolean);
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
  Clear;
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
    FItems[AIndex] := AItem; // Index Validation is now performed by TLKArray<T>.GetItem
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
