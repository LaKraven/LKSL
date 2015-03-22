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
unit LKSL.Events.Main;

interface

{$I LKSL.inc}

{$REGION 'Unit About'}
  {
    About this unit:
      - This unit provides type declarations required for our "Event Engine"
      - This unit also provides the Abstract Base Implementation for the same.
  }
{$ENDREGION}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs,
  {$ELSE}
    Classes, SysUtils, SyncObjs,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types, LKSL.Common.Performance,
  LKSL.Threads.Main,
  LKSL.Generics.Collections,
  LKSL.Streamables.Main;

  {$I LKSL_RTTI.inc}

type
  { Forward Declarations }
  TLKEvent = class;
  TLKEventListener = class;
  TLKEventStreamable = class;
  TLKEventContainer = class;
  TLKEventPreProcessor = class;
  TLKEventStreamProcessor = class;
  TLKEventThread = class;
  TLKEventPool = class;

  { Class References }
  TLKEventClass = class of TLKEvent;
  TLKEventStreamableClass = class of TLKEventStreamable;
  TLKEventPreProcessorClass = class of TLKEventPreProcessor;
  TLKEventThreadClass = class of TLKEventThread;

  { Enum Types }
  ///  <summary><c>The condition(s) under which we should process the Cancellation of an Event.</c></summary>
  ///  <remarks>
  ///    <para>eccIfNotProcessing <c>= Only Cancel if the Event hasn't begun being processed yet.</c></para>
  ///    <para>eccRegardless <c>= Cancel even if the Event is currently being processed.</c></para>
  ///  </remarks>
  TLKEventCancelCondition = (eccIfNotProcessing, eccRegardless);
  ///  <summary><c>The Method by which the Event was Dispatched.</c></summary>
  TLKEventDispatchMethod = (edmNotDispatched, edmQueue, edmStack);
  ///  <summary><c>The means by which the Lifetime of a </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> Instance is managed.</c></summary>
  ///  <remarks>
  ///    <para>elcAutomatic <c>= Dispatched Events are Reference Counted and destroyed once processed.</c></para>
  ///    <para>elcManual <c>= Dispatched Events are NOT Reference Counted, and must be destroyed by the implementing developer's code (very carefully, of course).</c></para>
  ///  </remarks>
  TLKEventLifetimeControl = (elcAutomatic, elcManual);
  ///  <summary><c>Defined Origins for a </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> Instance.</c></summary>
  TLKEventOrigin = (eoInternal, eoReplay, eoRemote, eoUnknown);
  ///  <summary><c>Defined Target for a </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> Instance.</c></summary>
  TLKEventTarget = (edThreads, edPools, edRecorders, edRemotes, edUknown);
  TLKEventTargets = set of TLKEventTarget;
  ///  <summary><c>The current State of an Event.</c></summary>
  TLKEventState = (esNotDispatched, esScheduled, esDispatched, esProcessing, esProcessed, esCancelled);
  ///  <summary><c>Describes whether or not an Event-related Type should automatically Register itself on Construction.</c></summary>
  TLKEventRegistrationMode = (ermAutomatic, ermManual);
  ///  <summary><c>Used to define whether a </c><see DisplayName="TLKEventListener" cref="LKSL.Events.Main|TLKEventListener"/><c> should accept Descendants of the defined </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> Type.</c></summary>
  TLKEventTypeRestriction = (etrAllowDescendants, etrDefinedTypeOnly);

  { Exceptions }
  ELKEventEngineException = class(ELKException);
    ELKEventPreProcessorException = class(ELKEventEngineException);
      ELKEventPreProcessorMapperException = class(ELKEventPreProcessorException);
        ELKEventPreProcessorMapperGUIDExists = class(ELKEventPreProcessorMapperException);
        ELKEVentPreProcessorMapperGUIDNotExist = class(ELKEventPreProcessorMapperException);
        ELKEventPreProcessorMapperValueExists = class(ELKEventPreProcessorMapperException);
    ELKEventListenerException = class(ELKEventEngineException);
      ELKEventListenerNoEventThreadDefined = class(ELKEventListenerException);
    ELKEventPoolException = class(ELKEventEngineException);
      ELKEventPoolThreadTypeMismatch = class(ELKEventPoolException);

  { Generics Collections }
  TLKEventList = class(TLKObjectList<TLKEvent>);
  TLKEventListenerList = class(TLKList<TLKEventListener>);
  TLKEventStreamableClassList = class(TLKList<TLKEventStreamableClass>);
  TLKEventPreProcessorList = class(TLKList<TLKEventPreProcessor>);
  TLKEventThreadList = class(TLKList<TLKEventThread>);
  TLKEventPoolList = class(TLKList<TLKEventPool>);

  ///  <summary><c>Abstract Base Class for all Event Types</c></summary>
  ///  <remarks>
  ///    <para><c>Don't implement behaviour on your descendants. Events are intended to provide raw information (properties) only, not functionality.</c></para>
  ///    <para><c>Your Event's information (properties) should be read-only!</c></para>
  ///  </remarks
  TLKEvent = class abstract(TLKPersistent)
  private
    ///  <summary><c>The Time at which the Event was Created.</c></summary>
    FCreatedTime: LKFloat;
    ///  <summary><c>The Time (in Seconds) after which the Event should be Dispatched.</c></summary>
    ///  <remarks>
    ///    <para><c>Value represents an Offset (in Seconds) from </c><see DisplayName="FDispatchTime" cref="LKSL.Events.Main|TLKEvent.FDispatchTime"/></para>
    ///    <para><c>0 = Instant Dispatch (no Scheduling)</c></para>
    ///    <para><c>Default = </c>0</para>
    ///  </remarks>
    FDispatchAfter: LKFloat;
    ///  <summary><c>The Physical Reference Time at which the Event should be Dispatched by the Scheduler.</c></summary>
    FDispatchAt: LKFloat;
    ///  <summary><c>The Method by which the Event was Dispatched.</c></summary>
    ///  <remarks><c>We either Queue an Event, or Stack it!</c></remarks>
    FDispatchMethod: TLKEventDispatchMethod;
    ///  <summary><c>The Targets to which this Event is allowed to be Dispatched.</c></summary>
    ///  <remarks>
    ///    <para><c>Default = LKSL_EVENTENGINE_DEFAULT_TARGETS (meaning that there are no restrictions)</c></para>
    ///    <para><c>By default, we want to allow the Event to be processed by ALL available PreProcessors</c></para>
    ///  </remarks>
    FDispatchTargets: TLKEventTargets;
    ///  <summary><c>The Reference Time at which the Event was Dispatched.</c></summary>
    FDispatchTime: LKFloat;
    ///  <summary><c>The Duration of Time after which the Event will Expire once Dispatched.</c></summary>
    ///  <remarks><c>Default will be 0.00 (Never Expires)</c></remarks>
    FExpiresAfter: LKFloat;
    ///  <summary><c>Dictates whether the Event Engine should take control of this Event's Lifetime</c></summary>
    ///  <remarks><c>Default =</c> elcAutomatic</remarks>
    FLifetimeControl: TLKEventLifetimeControl;
    ///  <summary><c>Where this Event came from.</c></summary>
    FOrigin: TLKEventOrigin;
    ///  <summary><c>The Reference Time at which the Event was First Processed.</c></summary>
    FProcessedTime: LKFloat;
    ///  <summary><c>Reference Count for when Lifetime Control is owned by the Event Engine.</c></summary>
    FRefCount: Integer;
    ///  <summary><c>Current State of this Event.</c></summary>
    FState: TLKEventState;

    function GetDispatchAfter: LKFloat;
    function GetDispatchTargets: TLKEventTargets;
    function GetDispatchTime: LKFloat;
    function GetExpiresAfter: LKFloat;
    function GetHasExpired: Boolean;
    function GetProcessedTime: LKFloat;
    function GetState: TLKEventState;

    procedure SetDispatchAfter(const ADispatchAfter: LKFloat);
    procedure SetDispatchTargets(const ADispatchTargets: TLKEventTargets);
    procedure SetExpiresAfter(const AExpiresAfter: LKFloat);

    ///  <summary><c>Incrememnts (atomically) the Reference Count for the Event.</c></summary>
    procedure Ref;
    ///  <summary><c>Decrememnts (atomically) the Reference Count for the Event.</c></summary>
    ///  <remarks><c>If the resulting Count = 0, the Event is Freed.</c></remarks>
    procedure Unref;
  protected
    ///  <summary><c>Override if you want your Event Type to Schedule its Dispatch by default.</c></summary>
    ///  <remarks>
    ///    <para>0<c> = no Scheduling</c></para>
    ///    <para><c>Default = </c>0</para>
    ///  </remarks>
    function GetDefaultDispatchAfter: LKFloat; virtual;
    ///  <summary><c>Override if you want your Event Type to only dispatch to specific Targets.</c></summary>
    function GetDefaultDispatchTargets: TLKEventTargets; virtual;
    ///  <summary><c>Override if you want your Event Type to Expire after a specific amount of time.</c></summary>
    function GetDefaultExpiresAfter: LKFloat; virtual;
  public
    ///  <summary><c>A simple macro to return this Class Type as a TLKEventClass instead of TClass.</c></summary>
    class function GetEventType: TLKEventClass; inline;
    ///  <summary><c>Override this if you wish to associate your Event with a type-specific </c><see DisplayName="TLKEventStreamable" cref="LKSL.Events.Main|TLKEventStreamable"/><c> Class.</c></summary>
    ///  <remarks><c>Default = </c>nil</remarks>
    class function GetStreamableType: TLKEventStreamableClass; virtual;

    constructor Create(const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic); reintroduce;
    destructor Destroy; override;

    ///  <summary><c>Cancels the Event after Dispatch.</c></summary>
    ///  <param name="ACancelConditions">Defines whether the Event should be cancelled if processing has begun or regardless there-of.</param>
    procedure Cancel(const ACancelConditions: TLKEventCancelCondition = eccIfNotProcessing);

    ///  <summary><c>Dispatch the Event through the Queue.</c></summary>
    procedure Queue(const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic); overload;
    ///  <summary><c>Dispatch the Event through the Stack.</c></summary>
    procedure Stack(const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic); overload;

    ///  <summary><c>Schedule the Event to be Dispatched through the Queue</c></summary>
    procedure ScheduleQueue(const AScheduleFor: LKFloat; const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic);
    ///  <summary><c>Schedule the Event to be Dispatched through the Stack</c></summary>
    procedure ScheduleStack(const AScheduleFor: LKFloat; const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic);

    property CreatedTime: LKFloat read FCreatedTime; // SET ON CONSTRUCTION ONLY
    property DispatchAfter: LKFloat read GetDispatchAfter write SetDispatchAfter;
    property DispatchMethod: TLKEventDispatchMethod read FDispatchMethod; // ATOMIC OPERATION
    property DispatchTargets: TLKEventTargets read GetDispatchTargets write SetDispatchTargets;
    property DispatchTime: LKFloat read GetDispatchTime;
    property ExpiresAfter: LKFloat read GetExpiresAfter write SetExpiresAfter;
    property HasExpired: Boolean read GetHasExpired;
    property LifetimeControl: TLKEventLifetimeControl read FLifetimeControl; // SET ON CONSTRUCTION ONLY
    property Origin: TLKEventOrigin read FOrigin; // SET ON CONSTRUCTION ONLY
    property ProcessedTime: LKFloat read GetProcessedTime;
    property State: TLKEventState read GetState;
  end;

  ///  <summary><c>Listeners are invoked when a relevant </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> instance is being processed.</c></summary>
  ///  <remarks>
  ///    <para><c>Listeners are invoked in no particular order.</c></para>
  ///    <para><c>Listeners can either register an interest in a singular </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> Type, or any descendant there-of.</c></para>
  ///  </remarks>
  TLKEventListener = class abstract(TLKPersistent)
  private
    ///  <summary><c>The Event Thread on which this Listener is to be Registered/Unregistered.</c></summary>
    ///  <remarks><c>Also the Event Thread on which this Listener is Executed.</c></remarks>
    FEventThread: TLKEventThread;
    ///  <summary><c>The maximum Age (time from Dispatch) of a </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> instance before this Listener loses interest in it.</c></summary>
    FExpireAfter: LKFloat;
    ///  <summary><c>Dictates whether this Listener should be automatically Registered after Construction.</c></summary>
    FRegistrationMode: TLKEventRegistrationMode;
    ///  <summary><c></c></summary>
    FTypeRestriction: TLKEventTypeRestriction;

    function GetExpireAfter: LKFloat;
    function GetTypeRestriction: TLKEventTypeRestriction;

    procedure SetExpireAfter(const AExpireAfter: LKFloat);
    procedure SetTypeRestriction(const ATypeRestriction: TLKEventTypeRestriction);
  protected
    ///  <summary><c>Override if you want to make your Event Listener ignore by default any </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> instance dispatched further back in time than the given value.</c></summary>
    ///  <remarks><c>Default = 0.00</c></remarks>
    function GetDefaultExpireAfter: LKFloat; virtual;
    ///  <summary><c>Override if you want to make your Event Listener ignore Descendants of the defined Event Type.</c></summary>
    ///  <remarks><c>Default = </c>etrAllowDescendants</remarks>
    function GetDefaultTypeRestriction: TLKEventTypeRestriction; virtual;
    ///  <summary><c>Override if you wish to specify additional (custom) criteria to determine whether the given Event is relevant.</c></summary>
    ///  <param name="AEvent"><c>The Event to be tested for relevance.</c></param>
    ///  <returns><c>Default = </c>True</returns>
    function GetEventRelevant(const AEvent: TLKEvent): Boolean; virtual;
    ///  <summary><c>You MUST override "DoEvent" to define what action is to take place.</c></summary>
    procedure DoEvent(const AEvent: TLKEvent); virtual; abstract;
  public
    constructor Create(const AEventThread: TLKEventThread; const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce;

    procedure AfterConstruction; override;

    ///  <summary><c>You MUST override this and set the Result to the Top-Level </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> Type in which this Listener is interested.</c></summary>
    function GetEventClass: TLKEventClass; virtual; abstract;

    ///  <summary><c>Registers the Listener with its parent </c><see DisplayName="TLKEventThread" cref="LKSL.Events.Main|TLKEventThread"/><c> instance.</c></summary>
    procedure Register;
    ///  <summary><c>Unregisters the Listener with its parent </c><see DisplayName="TLKEventThread" cref="LKSL.Events.Main|TLKEventThread"/><c> instance.</c></summary>
    procedure Unregister;

    property ExpireAfter: LKFloat read GetExpireAfter write SetExpireAfter;
    property TypeRestriction: TLKEventTypeRestriction read GetTypeRestriction write SetTypeRestriction;
  end;

  ///  <summary><c>Generic Layer for </c><see DisplayName="TLKEventListener" cref="LKSL.Events.Main|TLKEventListener"/><c> enabling one-line implementation of a Listener for the given </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> Type.</c></summary>
  TLKEventListener<T: TLKEvent> = class abstract(TLKEventListener)
  private type
    TEventCallbackUnbound = procedure(const AEvent: T);
    TEventCallbackOfObject = procedure(const AEvent: T) of Object;
    {$IFDEF SUPPORTS_REFERENCETOMETHOD}TEventCallbackAnonymous = reference to procedure(const AEvent: T);{$ENDIF SUPPORTS_REFERENCETOMETHOD}
  private
    FOnEventUnbound: TEventCallbackUnbound;
    FOnEventOfObject: TEventCallbackOfObject;
    {$IFDEF SUPPORTS_REFERENCETOMETHOD}FOnEventAnonymous: TEventCallbackAnonymous;{$ENDIF SUPPORTS_REFERENCETOMETHOD}
  protected
    procedure DoEvent(const AEvent: TLKEvent); override; final;
  public
    constructor Create(const AEventThread: TLKEventThread; const AOnEventCallback: TEventCallbackUnbound; const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce; overload;
    constructor Create(const AEventThread: TLKEventThread; const AOnEventCallback: TEventCallbackOfObject; const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce; overload;
    {$IFDEF SUPPORTS_REFERENCETOMETHOD}constructor Create(const AEventThread: TLKEventThread; const AOnEventCallback: TEventCallbackAnonymous; const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce; overload;{$ENDIF SUPPORTS_REFERENCETOMETHOD}

    function GetEventClass: TLKEventClass; override; final;
  end;

  ///  <summary><c>Abstract Base Class for all Event Streamable Descriptors</c></summary>
  ///  <comments>
  ///    <c>Provides Streamable Handlers for a </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> Type.</c>
  ///  </comments>
  ///  <remarks>
  ///    <para><c>Override </c><see DisplayName="TLKEventStreamable.GetEventType" cref="LKSL.Events.Main|TLKEventStreamable.GetEventType"/><c> to provide the </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> Type Reference for which this Streamable Descriptor applies.</c></para>
  ///    <para><c>Don't forget to override </c><see DisplayName="TLKStreamable.GetTypeGUID" cref="LKSL.Streamables.Main|TLKStreamable.GetTypeGUID"/><c> from </c><see DisplayName="TLKStreamable" cref="LKSL.Streamables.Main|TLKStreamable"/><c> to provide a unique GUID.</c></para>
  ///    <para><c>Don't forget to Register your descendants with the </c><see DisplayName="Streamables" cref="LKSL.Streamables.Main|Streamables"/><c> Manager!</c></para>
  ///  </remarks>
  TLKEventStreamable = class abstract(TLKStreamable)
  private
    FEvent: TLKEvent;
  protected
    function GetEvent: TLKEvent; virtual;

    procedure ReadFromStream(const AStream: TStream); override; final;
    procedure InsertIntoStream(const AStream: TStream); override; final;
    procedure WriteToStream(const AStream: TStream); override; final;

    ///  <summary><c>You MUST overload this and provide instructions on how to populate your </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> descendant instance from a Stream.</c></summary>
    procedure ReadEventFromStream(const AStream: TStream); virtual; abstract;
    ///  <summary><c>You MUST overload this and provide instructions on how to Insert your </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> descendant instance into a Stream.</c></summary>
    procedure InsertEventIntoStream(const AStream: TStream); virtual; abstract;
    ///  <summary><c>You MUST overload this and provide instructions on how to Write your </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> descendant instance into a Stream.</c></summary>
    procedure WriteEventToStream(const AStream: TStream); virtual; abstract;

    ///  <summary><c>If you aren't using Generics, you can define "property Event;" in the Public section.</c></summary>
    property Event: TLKEvent read GetEvent;
  public
    class function GetEventType: TLKEventClass; virtual; abstract;
    class function GetTypeVersion: Double; override;
    constructor Create; overload; override;
    constructor Create(const AEvent: TLKEvent); reintroduce; overload;
    destructor Destroy; override;
  end;

  ///  <summary><c>Generic Abstract Base Class for all Event Streamable Descriptors</c></summary>
  ///  <comments>
  ///    <c>Provides Streamable Handlers for a </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> Type.</c>
  ///  </comments>
  ///  <remarks>
  ///    <para><c>Generic Parameter "T" associates this Streamable Handler Type with a </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> Type.</c></para>
  ///    <para><c>Don't forget to override </c><see DisplayName="TLKStreamable.GetTypeGUID" cref="LKSL.Streamables.Main|TLKStreamable.GetTypeGUID"/><c> from </c><see DisplayName="TLKStreamable" cref="LKSL.Streamables.Main|TLKStreamable"/><c> to provide a unique GUID.</c></para>
  ///    <para><c>Don't forget to Register your descendants with the </c><see DisplayName="Streamables" cref="LKSL.Streamables.Main|Streamables"/><c> Manager!</c></para>
  ///  </remarks>
  TLKEventStreamable<T: TLKEvent, constructor> = class abstract(TLKEventStreamable)
  protected
    function GetEvent: T; reintroduce;
    // Don't override the following methods anymore...
    procedure ReadEventFromStream(const AStream: TStream); overload; override; final;
    procedure InsertEventIntoStream(const AStream: TStream); overload; override; final;
    procedure WriteEventToStream(const AStream: TStream); overload; override; final;
    // ...  override these instead!
    procedure ReadEventFromStream(const AEvent: T; const AStream: TStream); reintroduce; overload; virtual; abstract;
    procedure InsertEventIntoStream(const AEvent: T; const AStream: TStream); reintroduce; overload; virtual; abstract;
    procedure WriteEventToStream(const AEvent: T; const AStream: TStream); reintroduce; overload; virtual; abstract;
  public
    class function GetEventType: TLKEventClass; override; final;
    property Event: T read GetEvent;
  end;

  ///  <summary><c>Abstract Base Type for all Thread Types containing an Event Queue and Stack</c></summary>
  ///  <remarks>
  ///    <para><c>This includes </c><see DisplayName="TLKEventPreProcessor" cref="LKSL.Events.Main|TLKEventPreProcessor"/><c> and </c><see DisplayName="TLKEventThread" cref="LKSL.Events.Main|TLKEventThread"/><c> Types.</c></para>
  ///    <para><c>You should not need to inherit from this Type in your own code!</c></para>
  ///  </remarks>
  TLKEventContainer = class abstract(TLKThread)
  private
    FEventQueue: TLKEventList;
    FEventRate: LKFloat;
    FEventRateAverage: LKFloat;
    FEventRateAverageOver: LKFloat;
    FEventStack: TLKEventList;
    FPauseAt: LKFloat;
    FPerformance: TLKPerformanceCounter;
    FRateLock: TLKCriticalSection;
    FWorking: Boolean;

    function GetEventCount: Integer;
    function GetEventCountQueue: Integer;
    function GetEventCountStack: Integer;
    function GetEventRate: LKFloat;
    function GetEventRateAverage: LKFloat;
    function GetEventRateAverageOver: LKFloat;
    function GetWorking: Boolean;

    procedure SetEventRate(const AEventRate: LKFloat);
    procedure SetEventRateAverageOver(const AEventRateAverageOver: LKFloat);
    procedure SetWorking(const AWorking: Boolean);

    procedure LockRate; inline;
    procedure UnlockRate; inline;

    procedure ProcessEventList(const AEventList: TLKEventList; const ADelta, AStartTime: LKFloat);
    procedure ProcessEvents(const ADelta, AStartTime: LKFloat);
  protected
    { TLKThread overrides }
    function GetInitialThreadState: TLKThreadState; override;
    procedure Tick(const ADelta, AStartTime: LKFloat); override;
    { Overrideables }
    ///  <summary><c>Override if you wish to change the default Event Rate Averaging Time.</c></summary>
    ///  <remarks>
    ///    <para><c>Value is in Seconds (</c>1<c> = 1 second)</c></para>
    ///    <para><c>Default = </c>2</para>
    ///  </remarks>
    function GetDefaultEventRateAverageOver: LKFloat; virtual;
    ///  <summary><c>How long should the Thread wait for new Events before self-Pausing?</c></summary>
    ///  <remarks>
    ///    <para><c>A small delay is good when performance is critical.</c></para>
    ///    <para><c>Value is presented in Seconds.</c></para>
    ///    <para><c>Default =</c> 0.25 <c>seconds</c></para>
    ///  </remarks>
    function GetDefaultPauseDelay: LKFloat; virtual;
    ///  <summary><c>You MUST override "ProcessEvent" to define what action is to take place when the Event Stack and Queue are being processed.</c></summary>
    procedure ProcessEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat); virtual; abstract;
    ///  <summary><c>Should this Thread self-Pause when there are no Events in the Stack or Queue?</c></summary>
    ///  <remarks><c>Default =</c> True</remarks>
    function GetPauseOnNoEvent: Boolean; virtual;
    ///  <summary><c>Should this Thread self-Wake when a new Event is placed into the Stack or Queue?</c></summary>
    ///  <remarks><c>Default =</c> True</remarks>
    function GetWakeOnEvent: Boolean; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    ///  <summary><c>Places the nominated Event into the Event Queue.</c></summary>
    procedure QueueEvent(const AEvent: TLKEvent);
    ///  <summary><c>Places the nominated Event into the Event Stack.</c></summary>
    procedure StackEvent(const AEvent: TLKEvent);

    ///  <summary><c>The combined number of Events waiting in both the Queue and the Stack.</c></summary>
    property EventCount: Integer read GetEventCount;
    ///  <summary><c>The number of Events waiting in the Queue</c></summary>
    property EventCountQueue: Integer read GetEventCountQueue;
    ///  <summary><c>The number of Events waiting in the Stack</c></summary>
    property EventCountStack: Integer read GetEventCountStack;
    ///  <summary><c>The number of Events Per Second being processed.</c></summary>
    ///  <remarks><c>Based solely on the time it took to process the last Event.</c></remarks>
    property EventRate: LKFloat read GetEventRate;
    ///  <summary><c>The Average number of Events Per Second being processed.</c></summary>
    ///  <remarks><c>Based on the sum of all Events processed over the number of seconds dictated by </c><see DisplayName="EventRateAverageOver" cref="LKSL.Events.Main|TLKEventContainer.EventRateAverageOver"/></remarks>
    property EventRateAverage: LKFloat read GetEventRateAverage;
    ///  <summary><c>How much time (in seconds) the Average Event Rate is to be calculated over.</c></summary>
    property EventRateAverageOver: LKFloat read GetEventRateAverageOver write SetEventRateAverageOver;
    ///  <summary><c>This flag is set to </c>True<c> when Events are being processed.</c></summary>
    ///  <remarks><c>It is mainly used to determine whether a </c><see DisplayName="TLKEventThread" cref="LKSL.Events.Main|TLKEventThread"/><c> is a viable candidate to receive an </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> from an </c><see DisplayName="TLKEventPool" cref="LKSL.Events.Main|TLKEventPool"/></remarks>
    property Working: Boolean read GetWorking write SetWorking;
  end;

  ///  <summary><c>An Event PreProcessor contains an Event Queue and Stack, wakes up when one or more Events are pending for processing, then processes the Events according to its purpose-specific implementation.</c></summary>
  TLKEventPreProcessor = class abstract(TLKEventContainer)
  private
    ///  <summary><c>Dictates whether this PreProcessor should be automatically Registered after Construction.</c></summary>
    FRegistrationMode: TLKEventRegistrationMode;
  protected
    procedure Tick(const ADelta, AStartTime: LKFloat); override;
  public
    class function GetPreProcessorClass: TLKEventPreProcessorClass;
    ///  <summary><c>You MUST override and provide a Target Flag</c></summary>
    class function GetTargetFlag: TLKEventTarget; virtual; abstract;
    constructor Create(const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce; virtual;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure Register;
    procedure Unregister;
  end;

  ///  <summary><c>A Specialized version of TLKEventPreProcessor designed to handle Events as Streams.</c></summary>
  ///  <remarks>
  ///    <para><c>TLKEventRecorder inherits from this class!</c></para>
  ///  </remarks>
  TLKEventStreamProcessor = class abstract(TLKEventPreProcessor)
  protected
    ///  <summary><c>Override this if you need your descendant to do something particular with Events for which there is no defined Streamable Handler.</c></summary>
    procedure CannotProcessEventStreamable(const AEvent: TLKEvent; const ADelta, AStartTime: LKFLoat); virtual;
    ///  <summary><c>Takes care of preparing a </c><see DisplayName="TLKEventStreamable" cref="LKSL.Events.Main|TLKEventStreamable"/><c> for each </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> instance.</c></summary>
    ///  <remarks>
    ///    <para><c>Calls </c><see DisplayName="TLKEventStreamProcessor.ProcessEventStreamable" cref="LKSL.Events.Main|TLKEventStreamProcessor.ProcessEventStreamable"/><c> when a suitable </c><see DisplayName="TLKEventStreamable" cref="LKSL.Events.Main|TLKEventStreamable"/><c> Type exists for a given Event.</c></para>
    ///    <para><c>Calls </c><see DisplayName="TLKEventStreamProcessor.CannotProcessEventStreamable" cref="LKSL.Events.Main|TLKEventStreamProcessor.CannotProcessEventStreamable"/><c> when a suitable </c><see DisplayName="TLKEventStreamable" cref="LKSL.Events.Main|TLKEventStreamable"/><c> Type does NOT exist for a given Event.</c></para>
    ///  </remarks>
    procedure ProcessEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat); override; final;
    ///  <summary><c>You MUST override this method to dictate what to do with the pre-prepared </c><see DisplayName="TLKEventStreamable" cref="LKSL.Events.Main|TLKEventStreamable"/><c> instance.</c></summary>
    procedure ProcessEventStreamable(const AEventStream: TLKEventStreamable; const ADelta, AStartTime: LKFloat); virtual; abstract;
    ///  <summary><c>Override if you need to define some custom criteria</c></summary>
    ///  <remarks>
    ///    <para><c>Default = </c>True</para>
    ///    <para><c>Only invoked if a valid </c><see DisplayName="TLKEventStreamable" cref="LKSL.Events.Main|TLKEventStreamable"/><c> Type exists for the given Event!</c></para>
    ///  </remarks>
    function ValidateEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat): Boolean; virtual;
  end;

  ///  <summary><c>A special kind of Thread, designed to operate using Events.</c></summary>
  TLKEventThread = class abstract(TLKEventContainer)
  private
    FListeners: TLKEventListenerList;
    FListenersPending: TLKEventListenerList;
    FListenersLeaving: TLKEventListenerList;
    ///  <summary><c>Dictates whether this Event Thread should be automatically Registered after Construction.</c></summary>
    FRegistrationMode: TLKEventRegistrationMode;
    ///  <summary><c>If this Thread belongs to a Pool, this is the reference to that Pool.</c></summary>
    ///  <remarks>nil<c> if the Thread does NOT belong to a Pool</c></remarks>
    FPool: TLKEventPool;
    constructor Create(const AEventPool: TLKEventPool; const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce; overload; virtual;
    procedure ProcessEnqueuedListeners;
  protected
    { TLKThread Overrides }
    procedure PreTick(const ADelta, AStartTime: LKFloat); override;

    { TLKEventContainer Overrides }
    procedure ProcessEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat); override;

    { Overrideables }
    procedure InitializeListeners; virtual;
    procedure FinalizeListeners; virtual;
  public
    constructor Create(const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce; overload; virtual;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure RegisterListener(const AEventListener: TLKEventListener);
    procedure UnregisterListener(const AEventListener: TLKEventListener);

    procedure Register;
    procedure Unregister;
  end;

  ///  <summary><c>Base Class for Event Pools</c></summary>
  ///  <remarks>
  ///    <para><c>Event Pools create multiple </c><see DisplayName="TLKEventThread" cref="LKSL.Events.Main|TLKEventThread"/><c> Descendant Instances (of the specified Type), and distribute Events between them.</c></para>
  ///    <para><c>You can adjust the number of Event Threads in the Pool on-the-fly.</c></para>
  ///  </remarks>
  TLKEventPool = class abstract(TLKEventContainer)
  private
    FEventThreads: TLKEventThreadList;
    FRegistrationMode: TLKEventRegistrationMode;
    FThreadCount: Integer;
    FThreadCountTarget: Integer;

    procedure AddEventThread(const AEventThread: TLKEventThread);
    procedure RemoveEventThread(const AEventThread: TLKEventThread);

    procedure CreateThreads;
    procedure DestroyThreads;

    function GetThreadCount: Integer;
    procedure SetThreadCount(const AThreadCount: Integer);
  protected
    ///  <summary><c>Override and return a reference to the </c><see DisplayName="TLKEventThread" cref="LKSL.Events.Main|TLKEventThread"/><c> Type you wish for this Pool to manage!</c></summary>
    function GetEventThreadType: TLKEventThreadClass; virtual; abstract;

    { TLKThread Overrides }
    procedure PreTick(const ADelta, AStartTime: LKFloat); override;

    { TLKEventContainer Overrides }
    procedure ProcessEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat); override;
    ///  <summary><c>Called only when there is at least one viable Event Thread in the Pool.</c></summary>
    procedure PoolEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat); virtual;
  public
    constructor Create(const AThreadCount: Integer; const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure Register;
    procedure Unregister;

    property ThreadCount: Integer read GetThreadCount write SetThreadCount;
  end;

  ///  <summary><c>A Generic Event Pool, enabling single-line Event Pool Type definitions.</c></summary>
  TLKEventPool<T: TLKEventThread> = class(TLKEventPool)
  protected
    function GetEventThreadType: TLKEventThreadClass; override; final;
  end;

const
  LKSL_EVENTENGINE_VERSION: Double = 4.00;
  LKSL_EVENTENGINE_DEFAULT_TARGETS: TLKEventTargets = [edThreads, edPools, edRecorders, edRemotes, edUknown];

implementation

uses
  LKSL.Streams.System, LKSL.Common.Streams, LKSL.Events.Streams;

type
  { Forward Declarations }
  TLKEventScheduleList = class;
  TLKEventScheduler = class;
  TLKEventEngine = class;

  ///  <summary><c>A list of </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> instances arranged by the time at which they will be Dispatched.</c></summary>
  TLKEventScheduleList = class(TLKSortedList<TLKEvent>)
  protected
    // Parity Checks
    function AEqualToB(const A, B: TLKEvent): Boolean; override;
    function AGreaterThanB(const A, B: TLKEvent): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: TLKEvent): Boolean; override;
    function ALessThanB(const A, B: TLKEvent): Boolean; override;
    function ALessThanOrEqualB(const A, B: TLKEvent): Boolean; override;
  public
    destructor Destroy; override;
  end;

  ///  <summary><c>The actual Event Scheduler</c></summary>
  ///  <remarks><c>Takes care of Dispatching </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> instances when the appropriate time comes.</c></remarks>
  TLKEventScheduler = class(TLKThread)
  private
    FEvents: TLKEventScheduleList;
    FNextEventTime: LKFloat;
  protected
    function GetInitialThreadState: TLKThreadState; override;
    procedure Tick(const ADelta, AStartTime: LKFloat); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ScheduleEvent(const AEvent: TLKEvent); inline;
  end;

  ///  <summary><c>Heart and soul of the Event Engine.</c></summary>
  TLKEventEngine = class(TLKPersistent)
  private
    FPreProcessors: TLKEventPreProcessorList;
    FScheduler: TLKEventScheduler;
    FEventThreads: TLKEventThreadList;
    FPools: TLKEventPoolList;
    FPoolsPending: TLKEventPoolList;
    FPoolsLeaving: TLKEventPoolList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure RegisterPool(const AEventPool: TLKEventPool);
    procedure UnregisterPool(const AEventPool: TLKEventPool);

    procedure RegisterPreProcessor(const APreProcessor: TLKEventPreProcessor);
    procedure UnregisterPreProcessor(const APreProcessor: TLKEventPreProcessor);

    procedure RegisterEventThread(const AEventThread: TLKEventThread);
    procedure UnregisterEventThread(const AEventThread: TLKEventThread);

    procedure QueueInPools(const AEvent: TLKEvent);
    procedure StackInPools(const AEvent: TLKEvent);

    procedure QueueInThreads(const AEvent: TLKEvent);
    procedure StackInThreads(const AEvent: TLKEvent);

    procedure QueueEvent(const AEvent: TLKEvent);
    procedure StackEvent(const AEvent: TLKEvent);
  end;

var
  EventEngine: TLKEventEngine = nil;

{ TLKEvent }

procedure TLKEvent.Cancel(const ACancelConditions: TLKEventCancelCondition);
begin
  Lock;
  try
    case ACancelConditions of
      eccIfNotProcessing: if FState <> esProcessing then FState := esCancelled;
      eccRegardless: FState := esCancelled;
    end;
  finally
    Unlock;
  end;
end;

constructor TLKEvent.Create(const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic);
begin
  inherited Create;
  FCreatedTime := GetReferenceTime; // We've just created it...
  FDispatchAfter := GetDefaultDispatchAfter; // To Schedule, or NOT To Schedule?...
  FDispatchMethod := edmNotDispatched; // It hasn't yet been dispatched...
  FDispatchTime := 0; // We haven't dispatched it yet...
  FDispatchTargets := GetDefaultDispatchTargets;
  FExpiresAfter := GetDefaultExpiresAfter; // We request the default expiration for its Type...
  FLifetimeControl := ALifetimeControl; // Define who is responsible for Lifetime Control...
  FOrigin := eoInternal; // We presume it originates internally...
  FProcessedTime := 0; // We haven't processed it yet (it hasn't even been dispatched)...
  FState := esNotDispatched; // We haven't dispatched it yet...
end;

destructor TLKEvent.Destroy;
begin
  inherited;
end;

function TLKEvent.GetDefaultDispatchAfter: LKFloat;
begin
  Result := 0;
end;

function TLKEvent.GetDefaultDispatchTargets: TLKEventTargets;
begin
  Result := LKSL_EVENTENGINE_DEFAULT_TARGETS;
end;

function TLKEvent.GetDefaultExpiresAfter: LKFloat;
begin
  Result := 0;
end;

function TLKEvent.GetDispatchTime: LKFloat;
begin
  Lock;
  try
    Result := FDispatchTime;
  finally
    Unlock;
  end;
end;

function TLKEvent.GetDispatchAfter: LKFloat;
begin
  Lock;
  try
    Result := FDispatchAfter;
  finally
    Unlock;
  end;
end;

function TLKEvent.GetDispatchTargets: TLKEventTargets;
begin
  Lock;
  try
    Result := FDispatchTargets;
  finally
    Unlock;
  end;
end;

class function TLKEvent.GetEventType: TLKEventClass;
begin
  Result := TLKEventClass(Self);
end;

function TLKEvent.GetExpiresAfter: LKFloat;
begin
  Lock;
  try
    Result := FExpiresAfter;
  finally
    Unlock;
  end;
end;

function TLKEvent.GetHasExpired: Boolean;
begin
  Lock;
  try
    Result := (
                (FExpiresAfter > 0) and
                (GetReferenceTime - FDispatchTime >= FExpiresAfter)
              );
  finally
    Unlock;
  end;
end;

function TLKEvent.GetProcessedTime: LKFloat;
begin
  Lock;
  try
    Result := FProcessedTime;
  finally
    Unlock;
  end;
end;

function TLKEvent.GetState: TLKEventState;
begin
  Lock;
  try
    Result := FState;
  finally
    Unlock;
  end;
end;

class function TLKEvent.GetStreamableType: TLKEventStreamableClass;
begin
  Result := nil;
end;

procedure TLKEvent.Queue(const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic);
begin
  if FDispatchMethod = edmNotDispatched then
  begin
    FDispatchTime := GetReferenceTime;
    FLifetimeControl := ALifetimeControl;
    FState := esDispatched;
    FDispatchMethod := edmQueue;
    EventEngine.QueueEvent(Self);
  end;
end;

procedure TLKEvent.Ref;
begin
  AtomicIncrement(FRefCount);
end;

procedure TLKEvent.ScheduleQueue(const AScheduleFor: LKFloat; const ALifetimeControl: TLKEventLifetimeControl);
begin
  FDispatchAfter := AScheduleFor;
  Queue(ALifetimeControl);
end;

procedure TLKEvent.ScheduleStack(const AScheduleFor: LKFloat; const ALifetimeControl: TLKEventLifetimeControl);
begin
  FDispatchAfter := AScheduleFor;
  Stack(ALifetimeControl);
end;

procedure TLKEvent.SetDispatchAfter(const ADispatchAfter: LKFloat);
begin
  Lock;
  try
    FDispatchAfter := ADispatchAfter;
  finally
    Unlock;
  end;
end;

procedure TLKEvent.SetDispatchTargets(const ADispatchTargets: TLKEventTargets);
begin
  Lock;
  try
    FDispatchTargets := ADispatchTargets;
  finally
    Unlock;
  end;
end;

procedure TLKEvent.SetExpiresAfter(const AExpiresAfter: LKFloat);
begin
  Lock;
  try
    FExpiresAfter := AExpiresAfter;
  finally
    Unlock;
  end;
end;

procedure TLKEvent.Stack(const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic);
begin
  if FDispatchMethod = edmNotDispatched then
  begin
    FDispatchTime := GetReferenceTime;
    FLifetimeControl := ALifetimeControl;
    FState := esDispatched;
    FDispatchMethod := edmStack;
    EventEngine.StackEvent(Self);
  end;
end;

procedure TLKEvent.Unref;
begin
  AtomicDecrement(FRefCount);
  if (FRefCount = 0) then
  begin
    if (FLifetimeControl = elcAutomatic) then
      Free
    else
      FState := esProcessed;
  end;
end;

{ TLKEventListener }

procedure TLKEventListener.AfterConstruction;
begin
  inherited;
  if FRegistrationMode = ermAutomatic then
    Register;
end;

constructor TLKEventListener.Create(const AEventThread: TLKEventThread; const ARegistrationMode: TLKEventRegistrationMode);
begin
  inherited Create;
  FExpireAfter := GetDefaultExpireAfter;
  FTypeRestriction := GetDefaultTypeRestriction;
  FEventThread := AEventThread;
  if FEventThread = nil then
    raise ELKEventListenerNoEventThreadDefined.Create('Event Listeners MUST declare a parent Event Thread.');
  FRegistrationMode := ARegistrationMode;
end;

function TLKEventListener.GetDefaultExpireAfter: LKFloat;
begin
  Result := 0;
end;

function TLKEventListener.GetDefaultTypeRestriction: TLKEventTypeRestriction;
begin
  Result := etrAllowDescendants;
end;

function TLKEventListener.GetEventRelevant(const AEvent: TLKEvent): Boolean;
begin
  Result := True;
end;

function TLKEventListener.GetExpireAfter: LKFloat;
begin
  Lock;
  try
    Result := FExpireAfter;
  finally
    Unlock;
  end;
end;

function TLKEventListener.GetTypeRestriction: TLKEventTypeRestriction;
begin
  Lock;
  try
    Result := FTypeRestriction;
  finally
    Unlock;
  end;
end;

procedure TLKEventListener.Register;
begin
  FEventThread.RegisterListener(Self);
end;

procedure TLKEventListener.SetExpireAfter(const AExpireAfter: LKFloat);
begin
  Lock;
  try
    FExpireAfter := AExpireAfter;
  finally
    Unlock;
  end;
end;

procedure TLKEventListener.SetTypeRestriction(const ATypeRestriction: TLKEventTypeRestriction);
begin
  Lock;
  try
    FTypeRestriction := ATypeRestriction;
  finally
    Unlock;
  end;
end;

procedure TLKEventListener.Unregister;
begin
  FEventThread.UnregisterListener(Self);
end;

{ TLKEventListener<T> }

constructor TLKEventListener<T>.Create(const AEventThread: TLKEventThread; const AOnEventCallback: TEventCallbackUnbound; const ARegistrationMode: TLKEventRegistrationMode);
begin
  inherited Create(AEventThread, ARegistrationMode);
  FOnEventUnbound := AOnEventCallback;
end;

constructor TLKEventListener<T>.Create(const AEventThread: TLKEventThread; const AOnEventCallback: TEventCallbackOfObject; const ARegistrationMode: TLKEventRegistrationMode);
begin
  inherited Create(AEventThread, ARegistrationMode);
  FOnEventOfObject := AOnEventCallback;
end;
{$IFDEF SUPPORTS_REFERENCETOMETHOD}
  constructor TLKEventListener<T>.Create(const AEventThread: TLKEventThread; const AOnEventCallback: TEventCallbackAnonymous; const ARegistrationMode: TLKEventRegistrationMode);
  begin
    inherited Create(AEventThread, ARegistrationMode);
    FOnEventAnonymous := AOnEventCallback;
  end;
{$ENDIF SUPPORTS_REFERENCETOMETHOD}
procedure TLKEventListener<T>.DoEvent(const AEvent: TLKEvent);
begin
  if Assigned(FOnEventUnbound) then
    FOnEventUnbound(AEvent)
  else if Assigned(FOnEventOfObject) then
    FOnEventOfObject(AEvent)
  else if Assigned(FOnEventAnonymous) then
    FOnEventAnonymous(AEvent);
end;

function TLKEventListener<T>.GetEventClass: TLKEventClass;
begin
  Result := T;
end;

{ TLKEventStreamable }

constructor TLKEventStreamable.Create;
begin
  inherited Create;
  FEvent := GetEventType.Create;
  FEvent.Ref;
end;

constructor TLKEventStreamable.Create(const AEvent: TLKEvent);
begin
  inherited Create;
  FEvent := AEvent;
  FEvent.Ref;
end;

destructor TLKEventStreamable.Destroy;
begin
  FEvent.Unref;
  inherited;
end;

function TLKEventStreamable.GetEvent: TLKEvent;
begin
  Result := FEvent;
end;

class function TLKEventStreamable.GetTypeVersion: Double;
begin
  Result := LKSL_EVENTENGINE_VERSION;
end;

procedure TLKEventStreamable.InsertIntoStream(const AStream: TStream);
var
  LCountPosition, LEndPosition: Int64;
  LTarget: TLKEventTarget;
  LTargetCount: Integer;
begin
  FEvent.Lock;
  try
    StreamInsertLKFloat(AStream, FEvent.FCreatedTime);
    StreamInsertLKFloat(AStream, FEvent.FDispatchAfter);
    StreamInsertTLKEventDispatchMethod(AStream, FEvent.FDispatchMethod);
    LTargetCount := 0;
    LCountPosition := AStream.Position;
    StreamInsertInteger(AStream, LTargetCount);
    for LTarget in FEvent.FDispatchTargets do
    begin
      Inc(LTargetCount);
      StreamInsertTLKEventTarget(AStream, LTarget);
    end;
    LEndPosition := AStream.Position;
    StreamWriteInteger(AStream, LTargetCount, LCountPosition);
    AStream.Position := LEndPosition;
    StreamInsertLKFloat(AStream, FEvent.FDispatchTime);
    StreamInsertLKFloat(AStream, FEvent.FExpiresAfter);
    StreamInsertTLKEventLifetimeControl(AStream, FEvent.FLifetimeControl);
    StreamInsertTLKEventOrigin(AStream, FEvent.FOrigin);
    StreamInsertLKFloat(AStream, FEvent.FProcessedTime);
    StreamInsertTLKEventState(AStream, FEvent.FState);

    InsertEventIntoStream(AStream);
  finally
    FEvent.Unlock;
  end;
end;

procedure TLKEventStreamable.ReadFromStream(const AStream: TStream);
var
  I, LCount: Integer;
begin
  FEvent.Lock;
  try
    FEvent.FCreatedTime := StreamReadLKFloat(AStream);
    FEvent.FDispatchAfter := StreamReadLKFloat(AStream);
    FEvent.FDispatchMethod := StreamReadTLKEventDispatchMethod(AStream);
    FEvent.FDispatchTargets := [];
    LCount := StreamReadInteger(AStream);
    for I := 0 to LCount - 1 do
      FEvent.FDispatchTargets := FEvent.FDispatchTargets + [StreamReadTLKEventTarget(AStream)];
    FEvent.FDispatchTime := StreamReadLKFloat(AStream);
    FEvent.FExpiresAfter := StreamReadLKFloat(AStream);
    FEvent.FLifetimeControl := StreamReadTLKEventLifetimeControl(AStream);
    FEvent.FOrigin := StreamReadTLKEventOrigin(AStream);
    FEvent.FProcessedTime := StreamReadLKFloat(AStream);
    FEvent.FState := StreamReadTLKEventState(AStream);
    ReadEventFromStream(AStream);
  finally
    FEvent.Unlock;
  end;
end;

procedure TLKEventStreamable.WriteToStream(const AStream: TStream);
var
  LCountPosition, LEndPosition: Int64;
  LTarget: TLKEventTarget;
  LTargetCount: Integer;
begin
  FEvent.Lock;
  try
    StreamWriteLKFloat(AStream, FEvent.FCreatedTime);
    StreamWriteLKFloat(AStream, FEvent.FDispatchAfter);
    StreamWriteTLKEventDispatchMethod(AStream, FEvent.FDispatchMethod);
    LTargetCount := 0;
    LCountPosition := AStream.Position;
    StreamWriteInteger(AStream, LTargetCount);
    for LTarget in FEvent.FDispatchTargets do
    begin
      Inc(LTargetCount);
      StreamWriteTLKEventTarget(AStream, LTarget);
    end;
    LEndPosition := AStream.Position;
    StreamWriteInteger(AStream, LTargetCount, LCountPosition);
    AStream.Position := LEndPosition;
    StreamWriteLKFloat(AStream, FEvent.FDispatchTime);
    StreamWriteLKFloat(AStream, FEvent.FExpiresAfter);
    StreamWriteTLKEventLifetimeControl(AStream, FEvent.FLifetimeControl);
    StreamWriteTLKEventOrigin(AStream, FEvent.FOrigin);
    StreamWriteLKFloat(AStream, FEvent.FProcessedTime);
    StreamWriteTLKEventState(AStream, FEvent.FState);
    WriteEventToStream(AStream);
  finally
    FEvent.Unlock;
  end;
end;

{ TLKEventStreamable<T> }

function TLKEventStreamable<T>.GetEvent: T;
begin
  Result := T(FEvent);
end;

class function TLKEventStreamable<T>.GetEventType: TLKEventClass;
begin
  Result := T;
end;

procedure TLKEventStreamable<T>.InsertEventIntoStream(const AStream: TStream);
begin
  InsertEventIntoStream(GetEvent, AStream);
end;

procedure TLKEventStreamable<T>.ReadEventFromStream(const AStream: TStream);
begin
  ReadEventFromStream(GetEvent, AStream);
end;

procedure TLKEventStreamable<T>.WriteEventToStream(const AStream: TStream);
begin
  WriteEventToStream(GetEvent, AStream);
end;

{ TLKEventContainer }

constructor TLKEventContainer.Create;
begin
  inherited;
  FPerformance := TLKPerformanceCounter.Create(10);
  FRateLock := TLKCriticalSection.Create;
  FEventRate := 0;
  FEventRateAverageOver := GetDefaultEventRateAverageOver;
  FEventQueue := TLKEventList.Create(False);
  FEventStack := TLKEventList.Create(False);
end;

destructor TLKEventContainer.Destroy;
begin
  FEventQueue.OwnsObjects := True;
  FEventStack.OwnsObjects := True;
  FEventQueue.Free;
  FEventStack.Free;
  FRateLock.Free;
  FPerformance.Free;
  inherited;
end;

procedure TLKEventContainer.ProcessEventList(const AEventList: TLKEventList; const ADelta, AStartTime: LKFloat);
var
  I, LEnd: Integer;
  LProcessStarted: LKFloat;
begin
  if AEventList.Count > 0 then
  begin
    LEnd := AEventList.Count - 1;
    for I := 0 to LEnd do
    begin
      if (not Terminated) then
        if (AEventList[I].State <> esCancelled) and (not AEventList[I].HasExpired) then // We don't want to bother processing Cancelled Events!
        begin
          LProcessStarted := GetReferenceTime;
          ProcessEvent(AEventList[I], ADelta, AStartTime);
          FPerformance.RecordSample(GetReferenceTime - LProcessStarted);
          SetEventRate(FPerformance.InstantRate);
        end;
        AEventList[I].Unref; // We're no longer referencing the Event
    end;
    SetWorking(True);
    AEventList.DeleteRange(0, LEnd); // Locking occurs automagically
    SetWorking(False);
  end;
end;

function TLKEventContainer.GetDefaultEventRateAverageOver: LKFloat;
begin
  Result := 2;
end;

function TLKEventContainer.GetDefaultPauseDelay: LKFloat;
begin
  Result := 0.25;
end;

function TLKEventContainer.GetEventCount: Integer;
begin
  Result := GetEventCountQueue + GetEventCountStack;
end;

function TLKEventContainer.GetEventCountQueue: Integer;
begin
  Result := FEventQueue.Count;
end;

function TLKEventContainer.GetEventCountStack: Integer;
begin
  Result := FEventStack.Count;
end;

function TLKEventContainer.GetEventRate: LKFloat;
begin
  LockRate;
  try
    Result := FEventRate;
  finally
    UnlockRate;
  end;
end;

function TLKEventContainer.GetEventRateAverage: LKFloat;
begin
  LockRate;
  try
    Result := FEventRateAverage;
  finally
    UnlockRate;
  end;
end;

function TLKEventContainer.GetEventRateAverageOver: LKFloat;
begin
  LockRate;
  try
    Result := FEventRateAverageOver;
  finally
    UnlockRate;
  end;
end;

function TLKEventContainer.GetInitialThreadState: TLKThreadState;
begin
  Result := tsPaused;
end;

function TLKEventContainer.GetPauseOnNoEvent: Boolean;
begin
  Result := True; // By default, we want to make the Thread sleep when there are no Events left to process
end;

function TLKEventContainer.GetWakeOnEvent: Boolean;
begin
  Result := True; // By default, we want to wake the Thread when an Event is added to the Queue/Stack
end;

function TLKEventContainer.GetWorking: Boolean;
begin
  Lock;
  try
    Result := FWorking;
  finally
    Unlock;
  end;
end;

procedure TLKEventContainer.LockRate;
begin
  FRateLock.Acquire;
end;

procedure TLKEventContainer.ProcessEvents(const ADelta, AStartTime: LKFloat);
begin
  ProcessEventList(FEventStack, ADelta, AStartTime); // Stack first
  ProcessEventList(FEventQueue, ADelta, AStartTime); // Queue second
  if (GetPauseOnNoEvent) and ((FEventStack.IsEmpty) and (FEventQueue.IsEmpty)) then
  begin
    if (FPauseAt > 0) and (GetReferenceTime >= FPauseAt) then
      Rest
    else if FPauseAt = 0 then
      FPauseAt := GetReferenceTime + GetDefaultPauseDelay;
  end;
end;

procedure TLKEventContainer.QueueEvent(const AEvent: TLKEvent);
begin
  AEvent.Ref; // Add a Reference to the Event
  FEventQueue.Add(AEvent);
  if GetWakeOnEvent then
  begin
    Wake; // Wake up this Thread
    FPauseAt := 0;
  end;
end;

procedure TLKEventContainer.SetEventRate(const AEventRate: LKFloat);
begin
  LockRate;
  try
    FEventRate := AEventRate;
  finally
    UnlockRate;
  end;
end;

procedure TLKEventContainer.SetEventRateAverageOver(const AEventRateAverageOver: LKFloat);
begin
  LockRate;
  try
    FEventRateAverageOver := AEventRateAverageOver;
  finally
    UnlockRate;
  end;
end;

procedure TLKEventContainer.SetWorking(const AWorking: Boolean);
begin
  Lock;
  try
    FWorking := AWorking;
  finally
    Unlock;
  end;
end;

procedure TLKEventContainer.StackEvent(const AEvent: TLKEvent);
begin
  AEvent.Ref; // Add a Reference to the Event
  FEventStack.Add(AEvent);
  if GetWakeOnEvent then
  begin
    Wake; // Wake up this Thread
    FPauseAt := 0;
  end;
end;

procedure TLKEventContainer.Tick(const ADelta, AStartTime: LKFloat);
begin
  // Do nothing
end;

procedure TLKEventContainer.UnlockRate;
begin
  FRateLock.Release;
end;

{ TLKEventPreProcessor }

procedure TLKEventPreProcessor.AfterConstruction;
begin
  inherited;
  if FRegistrationMode = ermAutomatic then
    Register;
end;

constructor TLKEventPreProcessor.Create(const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic);
begin
  inherited Create;
  FRegistrationMode := ARegistrationMode;
end;

destructor TLKEventPreProcessor.Destroy;
begin
  Unregister;
  inherited;
end;

class function TLKEventPreProcessor.GetPreProcessorClass: TLKEventPreProcessorClass;
begin
  Result := TLKEventPreProcessorClass(Self);
end;

procedure TLKEventPreProcessor.Register;
begin
  EventEngine.RegisterPreProcessor(Self);
end;

procedure TLKEventPreProcessor.Tick(const ADelta, AStartTime: LKFloat);
begin
  ProcessEvents(ADelta, AStartTime);
end;

procedure TLKEventPreProcessor.Unregister;
begin
  EventEngine.UnregisterPreProcessor(Self);
end;

{ TLKEventStreamProcessor }

procedure TLKEventStreamProcessor.CannotProcessEventStreamable(const AEvent: TLKEvent; const ADelta, AStartTime: LKFLoat);
begin
  // Do nothing
end;

procedure TLKEventStreamProcessor.ProcessEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat);
var
  LStreamableType: TLKEventStreamableClass;
  LStreamableEvent: TLKEventStreamable;
begin
  LStreamableType := AEvent.GetStreamableType;
  if LStreamableType <> nil then
  begin
    if LStreamableType.GetEventType = AEvent.GetEventType then
    begin
      LStreamableEvent := LStreamableType.Create(AEvent);
      try
        if ValidateEvent(AEvent, ADelta, AStartTime) then
          ProcessEventStreamable(LStreamableEvent, ADelta, AStartTime);
      finally
        LStreamableEvent.Free;
      end;
    end;
  end;
end;

function TLKEventStreamProcessor.ValidateEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat): Boolean;
begin
  Result := True;
end;

{ TLKEventThread }

procedure TLKEventThread.AfterConstruction;
begin
  inherited;
  if FRegistrationMode = ermAutomatic then
    Register;
end;

constructor TLKEventThread.Create(const ARegistrationMode: TLKEventRegistrationMode);
begin
  inherited Create;
  FPool := nil;
  FRegistrationMode := ARegistrationMode;
  FListeners := TLKEventListenerList.Create;
  FListenersPending := TLKEventListenerList.Create;
  FListenersLeaving := TLKEventListenerList.Create;
  InitializeListeners;
end;

constructor TLKEventThread.Create(const AEventPool: TLKEventPool; const ARegistrationMode: TLKEventRegistrationMode);
begin
  Create(ARegistrationMode);
  FPool := AEventPool;
end;

destructor TLKEventThread.Destroy;
begin
  Unregister;
  FinalizeListeners;
  FListeners.Free;
  FListenersPending.Free;
  FListenersLeaving.Free;
  inherited;
end;

procedure TLKEventThread.FinalizeListeners;
begin
  // Do nothing (yet)
end;

procedure TLKEventThread.InitializeListeners;
begin
  // Do nothing (yet)
end;

procedure TLKEventThread.PreTick(const ADelta, AStartTime: LKFloat);
begin
  ProcessEvents(ADelta, AStartTime);
  ProcessEnqueuedListeners;
end;

procedure TLKEventThread.ProcessEnqueuedListeners;
var
  I, LIndex: Integer;
begin
  FListeners.Lock;
  try
    // Process Pending
    FListenersPending.Lock;
    try
      for I := 0 to FListenersPending.Count - 1 do
        if (not FListeners.Contains(FListenersPending[I])) then
          FListeners.Add(FListenersPending[I]);
      FListenersPending.Clear(False);
    finally
      FListenersPending.Unlock;
    end;
    // Process Leaving
    FListenersLeaving.Lock;
    try
      for I := 0 to FListenersLeaving.Count - 1 do
      begin
        LIndex := FListeners.IndexOf(FListenersLeaving[I]);
        if LIndex > -1 then
          FListeners.Delete(LIndex);
      end;
      FListenersLeaving.Clear(False);
    finally
      FListenersLeaving.Unlock;
    end;
  finally
    FListeners.Unlock;
  end;
end;

procedure TLKEventThread.ProcessEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat);
var
  I: Integer;
begin
  FListeners.Lock;
  try
    for I := 0 to FListeners.Count - 1 do
      if (AEvent.State <> esCancelled) and (not AEvent.HasExpired) then // We don't want to bother actioning the Event if it has been Cancelled or has Expired
        if (((FListeners[I].GetTypeRestriction = etrAllowDescendants) and (AEvent is FListeners[I].GetEventClass)) or
            ((FListeners[I].GetTypeRestriction = etrDefinedTypeOnly) and (AEvent.ClassType = FListeners[I].GetEventClass))) and
           ((FListeners[I].ExpireAfter = 0) or (GetReferenceTime < (AEvent.DispatchTime + AEvent.ExpiresAfter))) and
           (FListeners[I].GetEventRelevant(AEvent)) then // We want to make sure that the Event is relevant to the Listener
          FListeners[I].DoEvent(AEvent);
  finally
    FListeners.Unlock;
  end;
end;

procedure TLKEventThread.Register;
begin
  if FPool <> nil then
    FPool.AddEventThread(Self)
  else
    EventEngine.RegisterEventThread(Self);
end;

procedure TLKEventThread.RegisterListener(const AEventListener: TLKEventListener);
begin
  if FListeners.LockIfAvailable then
  begin
    try
      if (not FListeners.Contains(AEventListener)) then
        FListeners.Add(AEventListener);
    finally
      FListeners.Unlock;
    end;
  end else
  begin
    FListenersPending.Lock;
    try
      if (not FListenersPending.Contains(AEventListener)) then
        FListenersPending.Add(AEventListener);
    finally
      FListenersPending.Unlock;
    end;
  end;
end;

procedure TLKEventThread.Unregister;
begin
  if FPool <> nil then
    FPool.RemoveEventThread(Self)
  else
    EventEngine.UnregisterEventThread(Self);
end;

procedure TLKEventThread.UnregisterListener(const AEventListener: TLKEventListener);
var
  LIndex: Integer;
begin
  if FListeners.LockIfAvailable then
  begin
    try
      LIndex := FListeners.IndexOf(AEventListener);
      if LIndex > -1 then
        FListeners.Delete(LIndex);
    finally
      FListeners.Unlock;
    end;
  end else
  begin
    FListenersLeaving.Lock;
    try
      if (not FListenersLeaving.Contains(AEventListener)) then
        FListenersLeaving.Add(AEventListener);
    finally
      FListenersLeaving.Unlock;
    end;
  end;
end;

{ TLKEventPool }

procedure TLKEventPool.AddEventThread(const AEventThread: TLKEventThread);
begin
  if not (AEventThread is GetEventThreadType) then
    raise ELKEventPoolThreadTypeMismatch.CreateFmt('Event Pool wants Event Threads of Type "%s", but attempted to register Event Thread of Type "%s"', [GetEventThreadType.ClassName, AEventThread.ClassName]);
  FEventThreads.Lock;
  try
    if (not FEventThreads.Contains(AEventThread)) then
      FEventThreads.Add(AEventThread);
  finally
    FEventThreads.Unlock;
  end;
end;

procedure TLKEventPool.AfterConstruction;
begin
  inherited;
  CreateThreads;
  if FRegistrationMode = ermAutomatic then
    Register;
end;

constructor TLKEventPool.Create(const AThreadCount: Integer; const ARegistrationMode: TLKEventRegistrationMode);
begin
  inherited Create;
  FThreadCount := AThreadCount;
  FThreadCountTarget := AThreadCount;
  FRegistrationMode := ARegistrationMode;
  FEventThreads := TLKEventThreadList.Create;
end;

procedure TLKEventPool.CreateThreads;
var
  I: Integer;
begin
  for I := 1 to FThreadCount do
    GetEventThreadType.Create(Self, ermAutomatic);
end;

destructor TLKEventPool.Destroy;
begin
  DestroyThreads;
  FEventThreads.Free;
  inherited;
end;

procedure TLKEventPool.DestroyThreads;
var
  I, LCount: Integer;
begin
  Lock;
  try
    FThreadCount := 0;
    FThreadCountTarget := 0;
    FEventThreads.Lock;
    try
      LCount := FEventThreads.Count;
      for I := LCount - 1 downto 0 do
        FEventThreads[I].Kill;
    finally
      FEventThreads.Unlock;
    end;
  finally
    Unlock;
  end;
end;

function TLKEventPool.GetThreadCount: Integer;
begin
  Lock;
  try
    Result := FThreadCount;
  finally
    Unlock;
  end;
end;

procedure TLKEventPool.PoolEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat);
var
  I: Integer;
  LBestThreadIndex, LBestThreadCount: Integer;
begin
  LBestThreadIndex := -1;
  LBestThreadCount := -1;
  AEvent.Ref;
  try
    repeat
      FEventThreads.Lock;
      try
        for I := 0 to FEventThreads.Count - 1 do
        begin
          if FEventThreads[I].EventCount = 0 then // First check if the Event Thread is Idle
          begin
            LBestThreadIndex := I;
            Break; // No point going any further!
          end else // If the Thread isn't idle, let's see if it's a good contender...
          begin
            if ((((not FEventThreads[I].Working)))) and (((LBestThreadIndex = -1)) or (FEventThreads[I].EventCount < LBestThreadCount)) then // We assume that the first Thread will be the best
            begin
              LBestThreadIndex := I;
              LBestThreadCount := FEventThreads[I].EventCount;
            end;
          end;
        end;
        // Dispatch to the best Thread
        if LBestThreadIndex > -1 then
        begin
          case AEvent.DispatchMethod of
            edmQueue: FEventThreads[LBestThreadIndex].QueueEvent(AEvent);
            edmStack: FEventThreads[LBestThreadIndex].StackEvent(AEvent);
          end;
        end;
      finally
        FEventThreads.Unlock;
      end;
    until LBestThreadIndex > -1;
  finally
    AEvent.Unref;
  end;
end;

procedure TLKEventPool.PreTick(const ADelta, AStartTime: LKFloat);
begin
  ProcessEvents(ADelta, AStartTime);
end;

procedure TLKEventPool.ProcessEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat);
begin
  if FEventThreads.Count > 0 then // If there are no Event Threads in this Pool, we can't do anything with the Event
    PoolEvent(AEvent, ADelta, AStartTime);
end;

procedure TLKEventPool.Register;
begin
  EventEngine.RegisterPool(Self);
end;

procedure TLKEventPool.RemoveEventThread(const AEventThread: TLKEventThread);
var
  LIndex: Integer;
begin
  FEventThreads.Lock;
  try
    LIndex := FEventThreads.IndexOf(AEventThread);
    if LIndex > -1 then
      FEventThreads.Delete(LIndex);
  finally
    FEventThreads.Unlock;
  end;
end;

procedure TLKEventPool.SetThreadCount(const AThreadCount: Integer);
begin
  Lock;
  try
    FThreadCountTarget := AThreadCount;
  finally
    Unlock;
  end;
end;

procedure TLKEventPool.Unregister;
begin
  EventEngine.UnregisterPool(Self);
end;

{ TLKEventPool<T> }

function TLKEventPool<T>.GetEventThreadType: TLKEventThreadClass;
begin
  Result := T;
end;

{ TLKEventScheduleList }

function TLKEventScheduleList.AEqualToB(const A, B: TLKEvent): Boolean;
begin
  Result := (A.FDispatchAt = B.FDispatchAt);
end;

function TLKEventScheduleList.AGreaterThanB(const A, B: TLKEvent): Boolean;
begin
  Result := (A.FDispatchAt > B.FDispatchAt);
end;

function TLKEventScheduleList.AGreaterThanOrEqualB(const A, B: TLKEvent): Boolean;
begin
  Result := (A.FDispatchAt >= B.FDispatchAt);
end;

function TLKEventScheduleList.ALessThanB(const A, B: TLKEvent): Boolean;
begin
  Result := (A.FDispatchAt < B.FDispatchAt);
end;

function TLKEventScheduleList.ALessThanOrEqualB(const A, B: TLKEvent): Boolean;
begin
  Result := (A.FDispatchAt <= B.FDispatchAt);
end;

destructor TLKEventScheduleList.Destroy;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    Items[I].Unref;
    Items[I].Free;
  end;
  Clear(False);
  inherited;
end;

{ TLKEventScheduler }

constructor TLKEventScheduler.Create;
begin
  inherited;
  FNextEventTime := 0;
  FEvents := TLKEventScheduleList.Create;
end;

destructor TLKEventScheduler.Destroy;
begin
  FEvents.Free;
  inherited;
end;

function TLKEventScheduler.GetInitialThreadState: TLKThreadState;
begin
  Result := tsPaused;
end;

procedure TLKEventScheduler.ScheduleEvent(const AEvent: TLKEvent);
begin
  AEvent.Ref;
  FEvents.Add(AEvent);
  Wake;
end;

procedure TLKEventScheduler.Tick(const ADelta, AStartTime: LKFloat);
var
  LThrottleInterval: Cardinal;
begin
  LThrottleInterval := ThrottleInterval; // Pull once to prevent unnecessary locking
  if FEvents.Count > 0 then
  begin
    if FEvents[0].FDispatchAt <= GetReferenceTime then // Is it time to dispatch this Event yet?
    begin
      FEvents[0].FDispatchAfter := 0;
      FEvents[0].FDispatchTime := GetReferenceTime;
      case FEvents[0].DispatchMethod of // If so...
        edmQueue: EventEngine.QueueEvent(FEvents[0]); // ... Dispatch to the queue...
        edmStack: EventEngine.StackEvent(FEvents[0]); // ... or the Stack...
      end;
      FEvents.Lock;
      try
        FEvents[0].Unref; // ... then remove the container...
        FEvents.Delete(0); // ... and delete the entry from the Schedule
      finally
        FEvents.Unlock;
      end;
    end else
      if (FEvents[0].FDispatchAt - GetReferenceTime >= LThrottleInterval / 1000) then
        TThread.Sleep(LThrottleInterval);
  end else
    Rest;
end;

{ TLKEventEngine }

constructor TLKEventEngine.Create;
begin
  inherited;
  FPreProcessors := TLKEventPreProcessorList.Create; // Create this FIRST
  FEventThreads := TLKEventThreadList.Create;
  FScheduler := TLKEventScheduler.Create;
  FPools := TLKEventPoolList.Create;
  FPoolsPending := TLKEventPoolList.Create;
  FPoolsLeaving := TLKEventPoolList.Create;
end;

destructor TLKEventEngine.Destroy;
begin
  FPoolsPending.Free;
  FPoolsLeaving.Free;
  FPools.Free;
  FScheduler.Kill;
  FEventThreads.Free;
  FPreProcessors.Free; // Free this LAST
  inherited;
end;

procedure TLKEventEngine.QueueEvent(const AEvent: TLKEvent);
var
  I: Integer;
begin
  AEvent.Ref;
  try
    if AEvent.DispatchAfter > 0 then
    begin
      AEvent.FDispatchAt := (AEvent.DispatchTime + AEvent.DispatchAfter);
      AEvent.FState := esScheduled;
      FScheduler.ScheduleEvent(AEvent);
    end else
    begin
      if TLKEventTarget.edThreads in AEvent.DispatchTargets then
        QueueInThreads(AEvent);
      if TLKEventTarget.edPools in AEvent.DispatchTargets then
        QueueInPools(AEvent);
      FPreProcessors.Lock;
      try
        for I := 0 to FPreProcessors.Count - 1 do
          if ((AEvent.State <> esCancelled) and (not AEvent.HasExpired)) and (FPreProcessors[I].GetTargetFlag in AEvent.DispatchTargets) then
            FPreProcessors[I].QueueEvent(AEvent)
      finally
        FPreProcessors.Unlock;
      end;
    end;
  finally
    AEvent.Unref;
  end;
end;

procedure TLKEventEngine.QueueInPools(const AEvent: TLKEvent);
var
  I: Integer;
begin
  AEvent.Ref;
  try
    FPools.Lock;
    try
      for I := 0 to FPools.Count - 1 do
        FPools[I].QueueEvent(AEvent);
    finally
      FPools.Unlock;
    end;
  finally
    AEvent.Unref;
  end;
end;

procedure TLKEventEngine.QueueInThreads(const AEvent: TLKEvent);
var
  I: Integer;
begin
  AEvent.Ref;
  try
    FEventThreads.Lock;
    try
      for I := 0 to FEventThreads.Count - 1 do
        FEventThreads[I].QueueEvent(AEvent);
    finally
      FEventThreads.Unlock;
    end;
  finally
    AEvent.Unref;
  end;
end;

procedure TLKEventEngine.RegisterEventThread(const AEventThread: TLKEventThread);
begin
  FEventThreads.Lock;
  try
    if (not FEventThreads.Contains(AEventThread)) then
      FEventThreads.Add(AEventThread);
  finally
    FEventThreads.Unlock;
  end;
end;

procedure TLKEventEngine.RegisterPool(const AEventPool: TLKEventPool);
begin
  if FPools.LockIfAvailable then
  begin
    try
      if (not FPools.Contains(AEventPool)) then
        FPools.Add(AEventPool);
    finally
      FPools.Unlock;
    end;
  end else
  begin
    FPoolsPending.Lock;
    try
      if (not FPoolsPending.Contains(AEventPool)) then
        FPoolsPending.Add(AEventPool);
    finally
      FPoolsPending.Unlock;
    end;
  end;
end;

procedure TLKEventEngine.RegisterPreProcessor(const APreProcessor: TLKEventPreProcessor);
begin
  FPreProcessors.Lock;
  try
    if (not FPreProcessors.Contains(APreProcessor)) then
      FPreProcessors.Add(APreProcessor);
  finally
    FPreProcessors.Unlock;
  end;
end;

procedure TLKEventEngine.StackEvent(const AEvent: TLKEvent);
var
  I: Integer;
begin
  AEvent.Ref;
  try
    if AEvent.DispatchAfter > 0 then
    begin
      AEvent.FDispatchAt := (AEvent.DispatchTime + AEvent.DispatchAfter);
      AEvent.FState := esScheduled;
      FScheduler.ScheduleEvent(AEvent);
    end else
    begin
      if TLKEventTarget.edThreads in AEvent.DispatchTargets then
        StackInThreads(AEvent);
      if TLKEventTarget.edPools in AEvent.DispatchTargets then
        StackInPools(AEvent);
      FPreProcessors.Lock;
      try
        for I := 0 to FPreProcessors.Count - 1 do
          if ((AEvent.State <> esCancelled) and (not AEvent.HasExpired)) and (FPreProcessors[I].GetTargetFlag in AEvent.DispatchTargets) then
            FPreProcessors[I].StackEvent(AEvent)
      finally
        FPreProcessors.Unlock;
      end;
    end;
  finally
    AEvent.Unref;
  end;
end;

procedure TLKEventEngine.StackInPools(const AEvent: TLKEvent);
var
  I: Integer;
begin
  AEvent.Ref;
  try
    FPools.Lock;
    try
      for I := 0 to FPools.Count - 1 do
        FPools[I].StackEvent(AEvent);
    finally
      FPools.Unlock;
    end;
  finally
    AEvent.Unref;
  end;
end;

procedure TLKEventEngine.StackInThreads(const AEvent: TLKEvent);
var
  I: Integer;
begin
  AEvent.Ref;
  try
    FEventThreads.Lock;
    try
      for I := 0 to FEventThreads.Count - 1 do
        FEventThreads[I].StackEvent(AEvent);
    finally
      FEventThreads.Unlock;
    end;
  finally
    AEvent.Unref;
  end;
end;

procedure TLKEventEngine.UnregisterEventThread(const AEventThread: TLKEventThread);
var
  LIndex: Integer;
begin
  FEventThreads.Lock;
  try
    LIndex := FEventThreads.IndexOf(AEventThread);
    if LIndex > -1 then
      FEventThreads.Delete(LIndex);
  finally
    FEventThreads.Unlock;
  end;
end;

procedure TLKEventEngine.UnregisterPool(const AEventPool: TLKEventPool);
var
  LIndex: Integer;
begin
  if FPools.LockIfAvailable then
  begin
    try
      LIndex := FPools.IndexOf(AEventPool);
      if LIndex > -1 then
        FPools.Delete(LIndex);
    finally
      FPools.Unlock;
    end;
  end else
  begin
    FPoolsLeaving.Lock;
    try
      if (not FPoolsLeaving.Contains(AEventPool)) then
        FPoolsLeaving.Add(AEventPool);
    finally
      FPoolsLeaving.Unlock;
    end;
  end;
end;

procedure TLKEventEngine.UnregisterPreProcessor(const APreProcessor: TLKEventPreProcessor);
var
  LIndex: Integer;
begin
  FPreProcessors.Lock;
  try
    LIndex := FPreProcessors.IndexOf(APreProcessor);
    if LIndex > -1 then
      FPreProcessors.Delete(LIndex);
  finally
    FPreProcessors.Unlock;
  end;
end;

initialization
  if EventEngine = nil then
    EventEngine := TLKEventEngine.Create; // Create this FIRST
finalization
  if EventEngine <> nil then
    EventEngine.Free; // Free this LAST

end.
