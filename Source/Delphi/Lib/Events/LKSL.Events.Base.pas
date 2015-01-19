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
unit LKSL.Events.Base;

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
  {$IFDEF POSIX}Posix.Unistd,{$ENDIF}
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs,
  {$ELSE}
    Classes, SysUtils, SyncObjs,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  Generics.Collections, LKSL.Generics.Collections,
  LKSL.Common.Types,
  LKSL.Threads.Base,
  LKSL.Streamables.Base;

  {$I LKSL_RTTI.inc}

type
  { Forward Declarations }
  TLKEvent = class;
  TLKEventListener = class;
  TLKEventListenerGroup = class;
  TLKEventStreamable = class;
  TLKEventThreadBase = class;
  TLKEventThreadBaseWithListeners = class;
  TLKEventThread = class;
  TLKEventThreadPooled = class;
  TLKEventThreadPool = class;
  TLKEventTransmitterBase = class;
  TLKEventReceiverBase = class;
  TLKEventRecorder = class;

  { Exception Types }
  ELKEventException = ELKException;
    ELEventListenerException = ELKEventException;
    ELEventListenerGroupException = ELKEventException;
    ELEventRecorderException = ELKEventException;
      ELEventRecorderSignatureMismatchException = ELEventRecorderException;
    ELKEventStreamableException = ELKEventException;
      ELKEventStreamableExistsForEventType = ELKEventStreamableException;
      ELKEventStreamableNotExists = ELKEventStreamableException;

  { Enum Types }
  ///  <summary><c>Values represent the system through which an Event was dispatched for processing.</c></summary>
  TLKEventDispatchMode = (edmQueue, edmStack, edmThreads);
  ///  <summary><c>Values represent the system through which an Event should be dispatched for processing.</c></summary>
  TLKEventDispatchMethod = (edQueue, edStack);

  { Set Types }
  TLKEventDispatchModes = set of TLKEventDispatchMode;

  { Class References }
  ///  <summary><c>Class Reference of <see DisplayName="TLKEvent" cref="LKSL.Events.Base|TLKEvent"/></c></summary>
  TLKEventType = class of TLKEvent;
  ///  <summary><c>Class Reference of <see DisplayName="TLKEventListener" cref="LKSL.Events.Base|TLKEventListener"/></c></summary>
  TLKEventListenerType = class of TLKEventListener;
  ///  <summary><c>Class Reference of <see DisplayName="TLKEventStreamable" cref="LKSL.Events.Base|TLKEventStreamable"/></c></summary>
  TLKEventStreamableType = class of TLKEventStreamable;
  TLKEventThreadPooledType = class of TLKEventThreadPooled;

  { Generics Lists Types }
  TLKEventTypeList = TLKList<TLKEventType>;
  ///  <summary><c>A List of </c><see DisplayName="TLKEvent" cref="LKSL.Events.Base|TLKEvent"/><c> instances.</c></summary>
  TLKEventList = TLKObjectList<TLKEvent>; // Used for in-order Lists of Events (such as in the Recorder)
  ///  <summary><c>A List of </c><see DisplayName="TLKEventListener" cref="LKSL.Events.Base|TLKEventListener"/><c> instances.</c></summary>
  TLKEventListenerList = TLKList<TLKEventListener>;
  ///  <summary><c>A List of </c><see DisplayName="TLKEventThread" cref="LKSL.Events.Base|TLKEventThread"/><c> instances.</c></summary>
  TLKEventThreadList = TLKList<TLKEventThread>;
  TLKEventThreadList = TLKList<TLKEventThread>;
  TLKEventThreadPooledList = TLKList<TLKEventThreadPooled>;
  TLKEventTransmitterList = TLKList<TLKEventTransmitterBase>;
  ///  <summary><c>A List of </c><see DisplayName="TLKEventRecorder" cref="LKSL.Events.Base|TLKEventRecorder"/><c> instances.</c></summary>
  TLKEventRecorderList = TLKList<TLKEventRecorder>;

  { Hashmap Types }
  ///  <summary><c>Associates an Event Type (</c><see DisplayName="TLKEventType" cref="LKSL.Events.Base|TLKEventType"/><c>) with a group of Listeners (</c><see DisplayName="TLKEventListenerGroup" cref="LKSL.Events.Base|TLKEventListenerGroup"/><c>).</c></summary>
  TLKEventListenerGroupDictionary =  TLKDictionary<TLKEventType, TLKEventListenerGroup>;
  ///  <summary><c>Associates an Event Type (</c><see DisplayName="TLKEventType" cref="LKSL.Events.Base|TLKEventType"/><c>) with a Streamable Descriptor (</c><see DisplayName="TLKEventStreamable" cref="LKSL.Events.Base|TLKEventStreamable"/><c>) Type Reference.</c></summary>
  TLKEventStreamableDictionary = class(TLKDictionary<TLKEventType, TLKEventStreamableType>);

  ///  <summary><c>Abstract Base Class for all Events</c></summary>
  ///  <comments>
  ///    <c>Provides parameter information for relevant Event Listeners to consume and operate against.</c>
  ///  </comments>
  ///  <remarks>
  ///    <c>Try to avoid defining functionality on a TLKEvent descendant.</c>
  ///    <c>Implement a </c><see DisplayName="TLKEventStreamable" cref="LKSL.Events.Base|TLKEventStreamable"/><c> for this Event Type if you want to be able to Transmit/Record them.</c>
  ///  </remarks>
  ///  <permission>Public</permission>
  TLKEvent = class abstract(TLKPersistent)
  private
    FAllowRecording: Boolean;
    FAllowTransmit: Boolean;
    FCreatedTime: LKFloat;
    FDelta: LKFloat;
    FDispatchModes: TLKEventDispatchModes;
    FDispatchMethod: TLKEventDispatchMethod;
    FDispatchTime: LKFloat;
    FExpiresAfter: LKFloat;
    FIsClone: Boolean;
    FIsReplay: Boolean;
    FProcessedTime: LKFloat;

    function GetAllowRecording: Boolean;
    function GetAllowTransmit: Boolean;
    function GetDelta: LKFloat;
    function GetDispatchMethod: TLKEventDispatchMethod;
    function GetDispatchTime: LKFloat;
    function GetExpiresAfter: LKFloat;
    function GetIsClone: Boolean;
    function GetIsReplay: Boolean;
    function GetProcessedTime: LKFloat;
    function GetTimeSinceCreated: LKFloat;
    function GetTimeSinceDispatched: LKFloat;

    procedure SetAllowRecording(const AAllowRecording: Boolean);
    procedure SetAllowTransmit(const AAllowTransmit: Boolean);
    procedure SetExpiresAfter(const AExpiresAfter: LKFloat);
    procedure SetIsReplay(const AIsReplay: Boolean);
  protected
    ///  <summary><c>Populates the referenced Instance with the values of </c>AFromEvent<c></c></summary>
    ///  <param name="AFromEvent"><c>A reference to the source Instance</c></param>
    ///  <comments><c>MUST be overriden in descendant classes</c></comments>
    ///  <permission>Protected - Override Only</permission>
    procedure Clone(const AFromEvent: TLKEvent); virtual; abstract;
    ///  <summary><c>Defines the default Recording Permission state.</c></summary>
    ///  <returns><c>True = Allow Recording</c> Default = True</returns>
    ///  <comments><c>Override if you wish to default to False</c></comments>
    ///  <remarks><c>Recording can only work if a TLKEventStreamHandler exists to describe an implementing TLKEvent Type</c></remarks>
    ///  <permission>Protected</permission>
    function GetDefaultAllowRecording: Boolean; virtual;
    ///  <summary><c>Defines the default Expiration Time.</c></summary>
    ///  <returns><c>Expiration Time in Seconds (Default = 0.00)</c></returns>
    ///  <comments><c>Override if you want the Event Type to expire after a period of time by default.</c></comments>
    ///  <permission>Protected</permission>
    function GetDefaultExpiresAfter: LKFloat; virtual;
    ///  <summary><c>Defines the default Transmit Permission state.</c></summary>
    ///  <returns><c>True = Allow Transmit</c> Default = False</returns>
    ///  <comments><c>Override if you wish to default to True</c></comments>
    ///  <remarks><c>Transmission can only work if a TLKEventStreamHandler exists to describe an implementing TLKEvent Type</c></remarks>    ///
    ///  <permission>Protected</permission>
    function GetDefaultAllowTransmit: Boolean; virtual;
    ///  <summary><c>Defines the default allowable Dispatch Modes for the Event.</c></summary>
    ///  <returns><c>A Set containing one or more of: edmQueue, edmStack, edmThreads</c> Default = [edmQueue, edmStack, edmThreads]</returns>
    ///  <comments><c></c></comments>
    ///  <permission>Protected</permission>
    function GetDispatchModes: TLKEventDispatchModes; virtual;
  public
    constructor Create; override;

    ///  <summary><c>Used to populate this instance from a nominated instance.</c></summary>
    ///  <remarks><c>Marked as </c>final<c>, override </c>Clone<c> instead!</c></remarks>
    ///  <permission>Public</permission>
    procedure Assign(AFromEvent: TPersistent); override; final;

    ///  <summary><c>Simply returns a Reference to this Event Type.</c></summary>
    ///  <remarks><c>Avoids the need to keep casting from "TClass" to "TLKEventType".</c></remarks>
    ///  <remarks><c>Inlined for performance.</c></remarks>
    ///  <permission>Public</permission>
    function GetEventType: TLKEventType; inline;

    ///  <summary><c>Dispatches the Event through the Event Queue.</c></summary>
    ///  <remarks><c>The Lifecycle of the Event is passed to the Event Engine!</c></remarks>
    ///  <permission>Public</permission>
    procedure Queue; overload; // Add this Event to the Event Queue
    ///  <summary><c>Schedules the Event through the Event Queue.</c></summary>
    ///  <param name="ASecondsFromNow"><c>How long (in seconds) from the point of dispatch before the Event is processed.</c></param>
    ///  <remarks><c>The Lifecycle of the Event is passed to the Event Engine!</c></remarks>
    ///  <permission>Public</permission>
    procedure Queue(const ASecondsFromNow: LKFloat); overload;
    ///  <summary><c>Dispatches the Event through the Event Stack.</c></summary>
    ///  <permission>Public</permission>
    procedure Stack; overload; // Add this Event to the Event Stack
    ///  <summary><c>Schedules the Event through the Event Stack.</c></summary>
    ///  <param name="ASecondsFromNow"><c>How long (in seconds) from the point of dispatch before the Event is processed.</c></param>
    ///  <remarks><c>The Lifecycle of the Event is passed to the Event Engine!</c></remarks>
    ///  <permission>Public</permission>
    procedure Stack(const ASecondsFromNow: LKFloat); overload;
    ///  <summary><c>Dispatches the Event ONLY through registered Transmitters.</c></summary>
    ///  <remarks><c>The Event will not be processed in the local process.</c></remarks>
    ///  <remarks><c>The Lifecycle of the Event is passed to the Event Engine!</c></remarks>
    ///  <permission>Public</permission>
    procedure TransmitOnly;

    ///  <summary><c>Do you want this Event to be Recorded by any registered Event Recorder?</c></summary>
    ///  <permission>Public - Read/Write</permission>
    property AllowRecording: Boolean read GetAllowRecording write SetAllowRecording;
    ///  <summary><c>Do you want this Event to be Transmitter to any registered Event Transmitter?</c></summary>
    ///  <permission>Public - Read/Write</permission>
    property AllowTransmit: Boolean read GetAllowTransmit write SetAllowTransmit;
    ///  <summary><c>The Reference Time at which this Event was Created.</c></summary>
    ///  <permission>Public - Read-Only</permission>
    property CreatedTime: LKFloat read FCreatedTime;
    ///  <summary><c>The Time Differential (Delta) between DispatchTime and the time of Processing.</c></summary>
    ///  <permission>Public - Read-Only</permission>
    property Delta: LKFloat read GetDelta;
    ///  <summary><c>Returns a value representing whether the Event was dispatched through the Queue or the Stack.</c></summary>
    ///  <remarks><c>Valid returns are</c> edQueue <c>or</c> edStack<c>.</c></remarks>
    ///  <permission>Public - Read-Only</permission>
    property DispatchMethod: TLKEventDispatchMethod read GetDispatchMethod;
    ///  <summary><c>The Time at which the Event was Dispatched.</c></summary>
    ///  <permission>Public - Read-Only</permission>
    property DispatchTime: LKFloat read GetDispatchTime;
    ///  <summary><c>The time (in seconds) after which you want this Event to Expire.</c></summary>
    ///  <remarks><c>A value of 0 means the Event will never Expire.</c></remarks>
    ///  <permission>Public - Read/Write</permission>
    property ExpiresAfter: LKFloat read GetExpiresAfter write SetExpiresAfter;
    ///  <summary><c>Whether this instance is the Original, or a Clone.</c></summary>
    ///  <permission>Public - Read-Only</permission>
    property IsClone: Boolean read GetIsClone;
    ///  <summary><c>Was the Event created as part of a Replay?</c></summary>
    ///  <permission>Public - Read/Write</permission>
    property IsReplay: Boolean read GetIsReplay write SetIsReplay;
    ///  <summary><c>The time at which the Event Queue/Stack began processing this Event.</c></summary>
    ///  <permission>Public - Read-Only</permission>
    property ProcessedTime: LKFloat read GetProcessedTime;
    ///  <summary><c>How much time has elapsed (in seconds) since this Event was Created.</c></summary>
    ///  <permission>Public - Read-Only</permission>
    property TimeSinceCreated: LKFloat read GetTimeSinceCreated;
    ///  <summary><c>How much time has elapsed (in seconds) since this Event was Dispatched.</c></summary>
    ///  <permission>Public - Read-Only</permission>
    property TimeSinceDispatched: LKFloat read GetTimeSinceDispatched;
  end;

  {
    TLKEventListener
      - An "Event Listener" performs a defined Action when it receives a relevant Event
      - Event Listeners can be owned by an Event-enabled Thread ("TLKEventThread")
  }
  TLKEventListener = class abstract(TLKPersistent)
  private
    FCallUIThread: Boolean;
    FEventThread: TLKEventThread;
    FExpireAfter: LKFloat;
    FIndex: Integer;
    FLastEventTime: LKFloat;
    FNewestEventOnly: Boolean;

    function GetCallUIThread: Boolean;
    function GetEventThread: TLKEventThread;
    function GetExpireAfter: LKFloat;
    function GetLastEventTime: LKFloat;
    function GetNewestEventOnly: Boolean;

    procedure SetCallUIThread(const ACallUIThread: Boolean);
    procedure SetExpireAfter(const AExpireAfter: LKFloat);
    procedure SetNewestEventOnly(const ANewestOnly: Boolean);
  protected
    // Override "GetDefaultCallUIThread" if you want your Listener to Synchronize its execution with the UI Thread
    // By default, Event Callbacks are NOT Synchronized (it returns "False")
    function GetDefaultCallUIThread: Boolean; virtual;
    // Override "GetDefaultExpireAfter" if you want your Listener to disregard Events that are older than the specified time (in Seconds)
    // By default, Listeners do not care how old an Event is ("0.00" = No Expiry)
    function GetDefaultExpireAfter: LKFloat; virtual;
    // Override "GetDefaultNewestEventOnly" if you want your Listener to only process Events that are NEWER than the last processed Event.
    // By default, Listeners do not care about the order in which Events are executed (it returns "False")
    function GetDefaultNewestEventOnly: Boolean; virtual;
  public
    constructor Create; overload; override; final;
    constructor Create(const AEventThread: TLKEventThread); reintroduce; overload; virtual;
    destructor Destroy; override;

    // You MUST override "GetEventType" and declare what Event Type (descendant of "TLKEvent") your Listener is interested in.
    function GetEventType: TLKEventType; virtual; abstract;
    // Override "GetConditionsMatch" if your Listener has a specific set of Criteria to determine whether it is interested in a particular Event.
    // By default, Listeners care about ALL Events so long as they match the Event Type specified as the return value of "GetEventType"
    function GetConditionsMatch(const AEvent: TLKEvent): Boolean; virtual;
    // You MUST override "EventCall" and define the action your Listener takes when it receives a relevant Event.
    // DON'T FORGET TO CALL "INHERITED;"
    procedure EventCall(const AEvent: TLKEvent); virtual;

    // Call "Subscribe" to register the Listener with the Event Handler
    procedure Subscribe;
    // Call "Unsubscribe" to UNregister the Listener with the Event Handler
    // You can temporarily Unsubscribe your Listeners at any time, and re-Subscribe it when you need to!
    procedure Unsubscribe;

    property CallUIThread: Boolean read GetCallUIThread write SetCallUIThread;
    property EventThread: TLKEventThread read GetEventThread;
    property ExpireAfter: LKFloat read GetExpireAfter write SetExpireAfter;
    property LastEventTime: LKFloat read GetLastEventTime;
    property NewestEventOnly: Boolean read GetNewestEventOnly write SetNewestEventOnly;
  end;

  {
    TLKEventListener<T: TLKEvent>
      - A Generic version of the original TLKEventListener
      - Eliminates some replication of boilerplate code.
      - Contributed by Uwe Raab
      - (see http://www.uweraabe.de/Blog/2014/11/09/a-generic-tlkeventlistener-for-lksl/)
      ---
      - Updated by Simon J Stuart (26th October 2011)
        - Now includes overloaded Constructors so you can assign "OnEvent" inline on creation
  }
  TLKEventListener<T: TLKEvent> = class(TLKEventListener)
  private type
    TEventCallback = procedure(const AEvent: T) of object;
  private
    FOnEvent: TEventCallback;
    function GetOnEvent: TEventCallback;
    procedure SetOnEvent(const AOnEvent: TEventCallback);
  public
    constructor Create(const AOnEvent: TEventCallback); reintroduce; overload;
    constructor Create(const AEventThread: TLKEventThread; const AOnEvent: TEventCallback); reintroduce; overload;
    procedure EventCall(const AEvent: TLKEvent); override;
    function GetEventType: TLKEventType; override;
    property OnEvent: TEventCallback read GetOnEvent write SetOnEvent;
  end;

  {
    TLKEventListenerGroup
      - Pairs multiple Listeners with a single Event Type
      - This is to speed up the Processing of Events by ensuring that an Event is relevant at the point
        of Queuing/Stacking
  }
  TLKEventListenerGroup = class(TLKPersistent)
  private
    FEventThread: TLKEventThreadBaseWithListeners;
    FEventType: TLKEventType;
    FListeners: TLKEventListenerList;
  public
    constructor Create(const AEventThread: TLKEventThreadBaseWithListeners; const AEventType: TLKEventType); reintroduce;
    destructor Destroy; override;

    procedure ProcessEvent(const AEvent: TLKEvent);
    procedure RegisterListener(const AListener: TLKEventListener);
    procedure UnregisterListener(const AListener: TLKEventListener);

    property EventType: TLKEventType read FEventType;
  end;

  ///  <summary><c>Abstract Base Class for all Event Streamable Descriptors</c></summary>
  ///  <comments>
  ///    <c>Provides Streamable Handlers for a </c><see DisplayName="TLKEvent" cref="LKSL.Events.Base|TLKEvent"/><c> Type.</c>
  ///  </comments>
  ///  <remarks>
  ///    <para><c>Override </c><see DisplayName="TLKEventStreamable.GetEventType" cref="LKSL.Events.Base|TLKEventStreamable.GetEventType"/><c> to provide the </c><see DisplayName="TLKEvent" cref="LKSL.Events.Base|TLKEvent"/><c> Type Reference for which this Streamable Descriptor applies.</c></para>
  ///    <para><c>Don't forget to override </c><see DisplayName="TLKStreamable.GetTypeGUID" cref="LKSL.Streamables.Base|TLKStreamable.GetTypeGUID"/><c> from </c><see DisplayName="TLKStreamable" cref="LKSL.Streamables.Base|TLKStreamable"/><c> to provide a unique GUID.</c></para>
  ///    <para><c>Don't forget to Register your descendants with the </c><see DisplayName="Streamables" cref="LKSL.Streamables.Base|Streamables"/><c> Manager!</c></para>
  ///  </remarks>
  ///  <permission>Public</permission>
  TLKEventStreamable = class abstract(TLKStreamable)
  protected
    FEvent: TLKEvent;
    function GetEvent: TLKEvent; virtual;
    /// <summary><c>Defines the TLKEvent descendant to which this Streamable Descriptor applies</c></summary>
    class function GetEventType: TLKEventType; virtual; abstract;
    class procedure OnRegistration; override;
    class procedure OnUnregistration; override;

    procedure ReadFromStream(const AStream: TStream); override;
    procedure InsertIntoStream(const AStream: TStream); override;
    procedure WriteToStream(const AStream: TStream); override;

    procedure ReadEventFromStream(const AStream: TStream); virtual; abstract;
    procedure InsertEventIntoStream(const AStream: TStream); virtual; abstract;
    procedure WriteEventToStream(const AStream: TStream); virtual; abstract;
  public
    constructor Create; overload; override;
    constructor Create(const AEvent: TLKEvent); reintroduce; overload;
    destructor Destroy; override;
    property Event: TLKEvent read GetEvent;
  end;

  ///  <summary><c>Generic Abstract Base Class for all Event Streamable Descriptors</c></summary>
  ///  <comments>
  ///    <c>Provides Streamable Handlers for a </c><see DisplayName="TLKEvent" cref="LKSL.Events.Base|TLKEvent"/><c> Type.</c>
  ///  </comments>
  ///  <remarks>
  ///    <para><c>Generic Parameter "T" associates this Streamable Handler Type with a </c><see DisplayName="TLKEvent" cref="LKSL.Events.Base|TLKEvent"/><c> Type.</c></para>
  ///    <para><c>Don't forget to override </c><see DisplayName="TLKStreamable.GetTypeGUID" cref="LKSL.Streamables.Base|TLKStreamable.GetTypeGUID"/><c> from </c><see DisplayName="TLKStreamable" cref="LKSL.Streamables.Base|TLKStreamable"/><c> to provide a unique GUID.</c></para>
  ///    <para><c>Don't forget to Register your descendants with the </c><see DisplayName="Streamables" cref="LKSL.Streamables.Base|Streamables"/><c> Manager!</c></para>
  ///  </remarks>
  ///  <permission>Public</permission>
  TLKEventStreamable<T: TLKEvent, constructor> = class abstract(TLKEventStreamable)
  protected
    function GetEvent: T; reintroduce;
    class function GetEventType: TLKEventType; override; final;
    // Don't override the following methods anymore...
    procedure ReadEventFromStream(const AStream: TStream); overload; override; final;
    procedure InsertEventIntoStream(const AStream: TStream); overload; override; final;
    procedure WriteEventToStream(const AStream: TStream); overload; override; final;
    // ...  override these instead!
    procedure ReadEventFromStream(const AEvent: T; const AStream: TStream); reintroduce; overload; virtual; abstract;
    procedure InsertEventIntoStream(const AEvent: T; const AStream: TStream); reintroduce; overload; virtual; abstract;
    procedure WriteEventToStream(const AEvent: T; const AStream: TStream); reintroduce; overload; virtual; abstract;
  public
    property Event: T read GetEvent;
  end;

  {
    TLKEventThreadBase
      - Abstract Base Type for Threads containing an Event Array
  }
  TLKEventThreadBase = class abstract(TLKThread)
  private
    FQueue: TLKEventList;
    FStack: TLKEventList;
  protected
    // "ProcessEvents" is overriden by TLKEventThread, TLKEventQueue and TLKEventStack,
    // which individually dictate how to process Events from the Events Array.
    procedure ProcessEvents(const ADelta, AStartTime: LKFloat); virtual; abstract;
    function GetInitialThreadState: TLKThreadState; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure QueueEvent(const AEvent: TLKEvent); virtual;
    procedure StackEvent(const AEvent: TLKEvent); virtual;
  end;

  {
    TLKEventThreadBaseWithListeners
      - Abstract Base Type for Threads containing Event Listeners
  }
  TLKEventThreadBaseWithListeners = class abstract(TLKEventThreadBase)
  private
    FEventListenerGroups: TLKEventListenerGroupDictionary;

    procedure AddEventListenerGroup(const AEventListenerGroup: TLKEventListenerGroup);
    procedure ClearEventListenerGroups;
    procedure DeleteEventListenerGroup(const AEventListenerGroup: TLKEventListenerGroup);
    function GetEventListenerGroup(const AEventType: TLKEventType): TLKEventListenerGroup;
    // "ProcessListeners" iterates every Listener paired with the Thread and (assuming
    // the parameters match) executes the Event Call on the Listeners (respectively)
    procedure ProcessListeners(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat);

    // Methods to register and unregister Event Listeners
    procedure RegisterListener(const AListener: TLKEventListener);
    function UnregisterListener(const AListener: TLKEventListener): Integer;
  protected
    procedure ProcessEvents(const ADelta, AStartTime: LKFloat); override; final;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure QueueEvent(const AEvent: TLKEvent); override;
    procedure StackEvent(const AEvent: TLKEvent); override;
  end;

  {
    TLKEventThread
      - A "High Precision Thread" with an added Event Queue.
      - Each cycle will process Events from the Queue before processing your Tick method.
  }
  TLKEventThread = class abstract(TLKEventThreadBaseWithListeners)
  private
    FIndex: Integer; // This Thread's position in the Event Handler's "EventThread" Array
  protected
    function GetDefaultYieldAccumulatedTime: Boolean; override; final;
    procedure PreTick(const ADelta, AStartTime: LKFloat); override;
    procedure Tick(const ADelta, AStartTime: LKFloat); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  {
    TLKEventThreadPooled
      - Essentially the same as TLKEventThread except it is created by a TLKEventThreadPool descendant
  }
  TLKEventThreadPooled = class abstract(TLKEventThreadBaseWithListeners)
  private
    FIndex: Integer; // This Thread's position in the Event Thread Pool's "EventThread" Array
  protected
    function GetDefaultYieldAccumulatedTime: Boolean; override; final;
    procedure PreTick(const ADelta, AStartTime: LKFloat); override;
    procedure Tick(const ADelta, AStartTime: LKFloat); override;
  public
    constructor Create(const AEventThreadPool: TLKEventThreadPool); reintroduce;
    destructor Destroy; override;
  end;

  {
    TLKEventThreadPool
      - Manages and maintains a collection of TLKEventThreadPooled descendants of the nominated Type
      - Balances the Event Load between the collection of Event Threads
      -
  }
  TLKEventThreadPool = class abstract(TLKEventThreadBase)
  private
    FEventThreadPooledType: TLKEventThreadPooledType;
    FPooledEventThreads: TLKEventThreadPooledList;
  protected
    function GetEventThreadPooledType: TLKEventThreadPooledType; virtual; abstract;
  public
    constructor Create; overload; override;
    constructor Create(const AInstanceLimit: Integer); reintroduce; overload;
    destructor Destroy; override;
  end;

  {
    TLKEventThreadPool<T: TLKEventThreadPooled>
      - A Generic version of TLKEventThreadPool
      - Eliminates the replication of boilerplate code.
  }
  TLKEventThreadPool<T: TLKEventThreadPooled> = class(TLKEventThreadPool)

  end;

  {
    TLKEventTransmitterBase
      - Abstract Base Class for Event Transmitters
      - Populate the virtual/abstract methods with the code necessary to send Events to whatever process
        you want.
  }
  TLKEventTransmitterBase = class abstract(TLKPersistent)
  private
    FValidEventTypes: TLKEventTypeList;
    FIndex: Integer; // This Thread's position in the Event Handler's "EventThread" Array
    FUseEventTypeList: Boolean;
    function GetUseEventTypeList: Boolean;
    procedure SetUseEventTypeList(const AValue: Boolean);
  public
    constructor Create; override;
    destructor Destroy; override;
    // Call "AddValidEventType" to add a Valid Event Type to the list
    procedure AddValidEventType(const AEventType: TLKEventType);
    // Call "RemoveValidEventType" to REMOVE a Valid Event Type from the list
    procedure RemoveValidEventType(const AEventType: TLKEventType);
    // Override "AcceptEvent" if you have a special criteria to determine whether or not THIS Transmitter
    // should actually bother transmitting the Event at all.
    // DON'T FORGET you can set "UseEventTypeList" to TRUE and add relevant Event Types using "AddEventType"
    // so that this Transmitter will only transmit Events if they are of a relevant Type.
    function AcceptEvent(const AEvent: TLKEvent): Boolean; virtual;
    // You MUST override "TransmitEvent" to instruct YOUR specific Transmission System what to do with
    // the Event.
    // NOTE: You should pass ONLY the "AEventStream" along, as this is a Streamable-packed copy of the
    //       Event containing all its Data.
    //       "AEvent" is provided merely for dealing with Type-specific or Value-specific Transmissions.
    procedure TransmitEvent(const AEvent: TLKEvent; const AEventStream: TMemoryStream); virtual; abstract;

    property UseEventTypeList: Boolean read GetUseEventTypeList write SetUseEventTypeList;
  end;

  {
    TLKEventReceiverBase
      - Abstract Base Class for Event Receivers
      - Populate the virtual/abstract methods with the code necessary to receive Events from other
        processes.
  }
  TLKEventReceiverBase = class abstract(TLKPersistent)

  end;

  {
    TLKEventRecorder
      - A thread for spooling ANY Event type (along with its parameter values) out into a Stream
  }
  TLKEventRecorder = class abstract(TLKEventThreadBase)
  private
    FIndex: Integer;
  protected
    function PrepareStreamable(const AEvent: TLKEvent): TLKEventStreamable;

    function GetInitialThreadState: TLKThreadState; override;
    procedure ProcessEvents(const ADelta, AStartTime: LKFloat); override; final;
    procedure Tick(const ADelta, AStartTime: LKFloat); override;

    procedure RecordEvent(const AEvent: TLKEvent); virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Subscribe;
    procedure Unsubscribe;

    procedure QueueEvent(const AEvent: TLKEvent); override;
    procedure StackEvent(const AEvent: TLKEvent); override;
  end;

implementation

uses
  LKSL.Common.Streams,
  LKSL.Streams.System,
  LKSL.Events.Streams;

  {$I LKSL_RTTI.inc}

type
  { Forward Declarations }
  TLKEventProcessor = class;
  TLKEventScheduled = class;
  TLKEventScheduleList = class;
  TLKEventScheduler = class;
  TLKEventEngine = class;

  {
    TLKEventProcessor
      - Provides both the Queue and Stack
  }
  TLKEventProcessor = class(TLKEventThreadBaseWithListeners)
  protected
    function GetInitialThreadState: TLKThreadState; override;
    procedure Tick(const ADelta, AStartTime: LKFloat); override;
  public
    procedure QueueEvent(const AEvent: TLKEvent); override;
    procedure StackEvent(const AEvent: TLKEvent); override;
  end;

  {
    TLKEventScheduled
      - Pairs a TLKEvent with its Scheduled Time
  }
  TLKEventScheduled = class(TLKPersistent)
  private
    FEvent: TLKEvent;
    FScheduledFor: LKFloat;
  public
    constructor Create(const AEvent: TLKEvent; const AScheduledFor: LKFloat); reintroduce;
    destructor Destroy; override;

    property Event: TLKEvent read FEvent;
    property ScheduledFor: LKFloat read FScheduledFor;
  end;

  {
    TLKEventScheduleList
      - A List of Events sorted by Scheduled Time
  }
  TLKEventScheduleList = class(TLKSortedList<TLKEventScheduled>)
  protected
    // Parity Checks
    function AEqualToB(const A, B: TLKEventScheduled): Boolean; override;
    function AGreaterThanB(const A, B: TLKEventScheduled): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: TLKEventScheduled): Boolean; override;
    function ALessThanB(const A, B: TLKEventScheduled): Boolean; override;
    function ALessThanOrEqualB(const A, B: TLKEventScheduled): Boolean; override;
  public
    destructor Destroy; override;
  end;

  {
    TLKEventScheduler
      -
  }
  TLKEventScheduler = class(TLKThread)
  private
    FEvents: TLKEventScheduleList;
    FNextEventTime: LKFloat;
  protected
    function GetInitialThreadState: TLKThreadState; override;
    procedure ScheduleEvent(const AEvent: TLKEvent; const AScheduleFor: LKFloat);
    procedure Tick(const ADelta, AStartTime: LKFloat); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure QueueEvent(const AEvent: TLKEvent; const AScheduleFor: LKFloat);
    procedure StackEvent(const AEvent: TLKEvent; const AScheduleFor: LKFloat);
  end;

  {
    TLKEventEngine
      - Heart and soul of the Event Engine.
  }
  TLKEventEngine = class(TLKPersistent)
  private
    FEventThreads: TLKEventThreadList;
    FRecorders: TLKEventRecorderList;
    FEventProcessor: TLKEventProcessor;
    FEventScheduler: TLKEventScheduler;
    FEventStreamables: TLKEventStreamableDictionary;

    // "QueueInThreads" iterates through all Event Threads and (if there's a relevant
    // Listener for the Event Type) adds the Event to the Thread's internal Event Queue
    procedure QueueInThreads(const AEvent: TLKEvent);
    // "QueueInRecorders" iterates through all Event Recorders and adds the Event to them
    procedure QueueInRecorders(const AEvent: TLKEvent);
    // "QueueEvent" adds an Event to the Processing Queue (first in, first out)
    procedure QueueEvent(const AEvent: TLKEvent);
    // "StackEvent" adds an Event to the Processing Stack (last in, first out)
    procedure StackEvent(const AEvent: TLKEvent);
    // "StackInThreads" iterates through all Event Threads and (if there's a relevant
    // Listener for the Event Type) adds the Event to the Thread's internal Event Stack
    procedure StackInThreads(const AEvent: TLKEvent);
    // "TransmitEvent" passes an Event along to the Transmitters WITHOUT broadcasting it internally
    procedure TransmitEvent(const AEvent: TLKEvent);
    // "GetEventStreamableType" returns a TLKEventStreamable type for a given TLKEvent type
    function GetEventStreamableType(const AEventType: TLKEventType): TLKEventStreamableType; overload;
    function GetEventStreamableType(const AEvent: TLKEvent): TLKEventStreamableType; overload;

    procedure RegisterEventStreamable(const AEventStreamableType: TLKEventStreamableType);
    procedure UnregisterEventStreamable(const AEventStreamableType: TLKEventStreamableType);

    procedure RegisterEventThread(const AEventThread: TLKEventThread);
    procedure UnregisterEventThread(const AEventThread: TLKEventThread);

    procedure RegisterRecorder(const ARecorder: TLKEventRecorder);
    procedure UnregisterRecorder(const ARecorder: TLKEventRecorder);

    procedure RegisterListener(const AListener: TLKEventListener);
    function UnregisterListener(const AListener: TLKEventListener): Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

const
  LKSL_EVENTS_SCHEDULER_MINLEADTIME = 0.0001;

var
  Events: TLKEventEngine = nil;

{ TLKEvent }

procedure TLKEvent.Assign(AFromEvent: TPersistent);
begin
  if AFromEvent is TLKEvent then
  begin
    TLKEvent(AFromEvent).Lock;
    Lock;
    try
      FIsClone := True;
      FAllowRecording := TLKEvent(AFromEvent).FAllowRecording;
      FAllowTransmit := TLKEvent(AFromEvent).FAllowTransmit;
      FCreatedTime := TLKEvent(AFromEvent).CreatedTime;
      FDelta := TLKEvent(AFromEvent).FDelta;
      FDispatchMethod := TLKEvent(AFromEvent).FDispatchMethod;
      FDispatchTime := TLKEvent(AFromEvent).FDispatchTime;
      FExpiresAfter := TLKEvent(AFromEvent).FExpiresAfter;
      FIsReplay := TLKEvent(AFromEvent).FIsReplay;
      FProcessedTime := TLKEvent(AFromEvent).FProcessedTime;
      Clone(TLKEvent(AFromEvent));
    finally
      Unlock;
      TLKEvent(AFromEvent).Unlock;
    end;
  end else
    inherited;
end;

constructor TLKEvent.Create;
begin
  inherited;
  FIsClone := False;
  FCreatedTime := GetReferenceTime;
  FAllowRecording := GetDefaultAllowRecording;
  FDelta := 0.00;
  FDispatchTime := 0.00;
  FExpiresAfter := GetDefaultExpiresAfter;
  FDispatchMethod := edQueue;
  FIsReplay := False;
  FProcessedTime := 0.00;
  FAllowTransmit := GetDefaultAllowTransmit;
  FDispatchModes := GetDispatchModes;
end;

function TLKEvent.GetDispatchModes: TLKEventDispatchModes;
begin
  Result := [edmQueue, edmStack, edmThreads];
end;

function TLKEvent.GetAllowRecording: Boolean;
begin
  Lock;
  try
    Result := FAllowRecording;
  finally
    Unlock;
  end;
end;

function TLKEvent.GetDefaultAllowRecording: Boolean;
begin
  Result := True;
end;

function TLKEvent.GetDefaultExpiresAfter: LKFloat;
begin
  Result := 0.00;
end;

function TLKEvent.GetDefaultAllowTransmit: Boolean;
begin
  Result := False; // We don't intend to transmit Event Types to other process by default
end;

function TLKEvent.GetDelta: LKFloat;
begin
  Lock;
  try
    Result := FDelta;
  finally
    Unlock;
  end;
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

function TLKEvent.GetEventType: TLKEventType;
begin
  Result := TLKEventType(Self.ClassType);
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

function TLKEvent.GetIsClone: Boolean;
begin
  Lock;
  try
    Result := FIsClone;
  finally
    Unlock;
  end;
end;

function TLKEvent.GetIsReplay: Boolean;
begin
  Lock;
  try
    Result := FIsReplay;
  finally
    Unlock;
  end;
end;

function TLKEvent.GetDispatchMethod: TLKEventDispatchMethod;
begin
  Lock;
  try
    Result := FDispatchMethod;
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

function TLKEvent.GetTimeSinceCreated: LKFloat;
begin
  Lock;
  try
    Result := GetReferenceTime - FCreatedTime;
  finally
    Unlock;
  end;
end;

function TLKEvent.GetTimeSinceDispatched: LKFloat;
begin
  Lock;
  try
    Result := GetReferenceTime - FDispatchTime;
  finally
    Unlock;
  end;
end;

function TLKEvent.GetAllowTransmit: Boolean;
begin
  Lock;
  try
    Result := FAllowTransmit;
  finally
    Unlock;
  end;
end;

procedure TLKEvent.Queue;
begin
  if (edmQueue in FDispatchModes) then
    Events.QueueEvent(Self)
  else if (edmStack in FDispatchModes) then
    Events.StackEvent(Self);
end;

procedure TLKEvent.Queue(const ASecondsFromNow: LKFloat);
begin
  if ASecondsFromNow > LKSL_EVENTS_SCHEDULER_MINLEADTIME then
    Events.FEventScheduler.QueueEvent(Self, GetReferenceTime + ASecondsFromNow)
  else
    Queue;
end;

procedure TLKEvent.SetAllowRecording(const AAllowRecording: Boolean);
begin
  Lock;
  try
    FAllowRecording := AAllowRecording;
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

procedure TLKEvent.SetIsReplay(const AIsReplay: Boolean);
begin
  Lock;
  try
    FIsReplay := AIsReplay;
  finally
    Unlock;
  end;
end;

procedure TLKEvent.SetAllowTransmit(const AAllowTransmit: Boolean);
begin
  Lock;
  try
    FAllowTransmit := AAllowTransmit;
  finally
    Unlock;
  end;
end;

procedure TLKEvent.Stack;
begin
  if (edmStack in FDispatchModes) then
    Events.StackEvent(Self)
  else if (edmQueue in FDispatchModes) then
    Events.QueueEvent(Self);
end;

procedure TLKEvent.Stack(const ASecondsFromNow: LKFloat);
begin
  if ASecondsFromNow > LKSL_EVENTS_SCHEDULER_MINLEADTIME then
    Events.FEventScheduler.StackEvent(Self, GetReferenceTime + ASecondsFromNow)
  else
    Stack;
end;

procedure TLKEvent.TransmitOnly;
begin
  Events.TransmitEvent(Self);
end;

{ TLKEventListener }

constructor TLKEventListener.Create;
begin
  Create(nil);
end;

constructor TLKEventListener.Create(const AEventThread: TLKEventThread);
begin
  inherited Create;
  FCallUIThread := GetDefaultCallUIThread;
  FExpireAfter := GetDefaultExpireAfter;
  FIndex := -1;
  FLastEventTime := 0.00;
  FNewestEventOnly := GetDefaultNewestEventOnly;
  FEventThread := AEventThread;
end;

destructor TLKEventListener.Destroy;
begin
  Unsubscribe;
  inherited;
end;

procedure TLKEventListener.EventCall(const AEvent: TLKEvent);
begin
  Lock;
  try
    FLastEventTime := AEvent.DispatchTime;
  finally
    Unlock;
  end;
end;

function TLKEventListener.GetCallUIThread: Boolean;
begin
  Lock;
  try
    Result := FCallUIThread;
  finally
    Unlock;
  end;
end;

function TLKEventListener.GetConditionsMatch(const AEvent: TLKEvent): Boolean;
begin
  Result := True; // We presume that there are no special conditions for Listeners
  // If you decide otherwise, override this method and set your conditions.
end;

function TLKEventListener.GetDefaultCallUIThread: Boolean;
begin
  Result := False;
end;

function TLKEventListener.GetDefaultExpireAfter: LKFloat;
begin
  Result := 0.00;
end;

function TLKEventListener.GetDefaultNewestEventOnly: Boolean;
begin
  Result := True;
end;

function TLKEventListener.GetEventThread: TLKEventThread;
begin
  Lock;
  try
    Result := FEventThread;
  finally
    Unlock;
  end;
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

function TLKEventListener.GetLastEventTime: LKFloat;
begin
  Lock;
  try
    Result := FLastEventTime;
  finally
    Unlock;
  end;
end;

function TLKEventListener.GetNewestEventOnly: Boolean;
begin
  Lock;
  try
    Result := FNewestEventOnly;
  finally
    Unlock;
  end;
end;

procedure TLKEventListener.SetCallUIThread(const ACallUIThread: Boolean);
begin
  Lock;
  try
    FCallUIThread := ACallUIThread;
  finally
    Unlock;
  end;
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

procedure TLKEventListener.SetNewestEventOnly(const ANewestOnly: Boolean);
begin
  Lock;
  try
    FNewestEventOnly := ANewestOnly;
  finally
    Unlock;
  end;
end;

procedure TLKEventListener.Subscribe;
begin
  if FIndex = -1 then
  begin
    if FEventThread = nil then
      Events.RegisterListener(Self)
    else
      FEventThread.RegisterListener(Self);
  end;
end;

procedure TLKEventListener.Unsubscribe;
begin
  if FIndex > -1 then
  begin
    if FEventThread = nil then
      FIndex := Events.UnregisterListener(Self)
    else
      FIndex := FEventThread.UnregisterListener(Self);
  end;
end;

{ TLKEventListener }
constructor TLKEventListener<T>.Create(const AOnEvent: TEventCallback);
begin
  inherited Create;
  FOnEvent := AOnEvent;
end;

constructor TLKEventListener<T>.Create(const AEventThread: TLKEventThread; const AOnEvent: TEventCallback);
begin
  inherited Create(AEventThread);
  FOnEvent := AOnEvent;
end;

procedure TLKEventListener<T>.EventCall(const AEvent: TLKEvent);
var
  LCallback: TEventCallback;
begin
  inherited;
  LCallback := OnEvent;
  if Assigned(LCallback) then
    LCallback(T(AEvent));
end;

function TLKEventListener<T>.GetEventType: TLKEventType;
begin
  Result := T;
end;

function TLKEventListener<T>.GetOnEvent: TEventCallback;
begin
  Lock;
  try
    Result := FOnEvent;
  finally
    Unlock;
  end;
end;

procedure TLKEventListener<T>.SetOnEvent(const AOnEvent: TEventCallback);
begin
  Lock;
  try
    FOnEvent := AOnEvent;
  finally
    Unlock;
  end;
end;

{ TLKEventListenerGroup }

constructor TLKEventListenerGroup.Create(const AEventThread: TLKEventThreadBaseWithListeners; const AEventType: TLKEventType);
begin
  inherited Create;
  FListeners := TLKEventListenerList.Create;
  FEventThread := AEventThread;
  FEventType := AEventType;
  FEventThread.AddEventListenerGroup(Self);
end;

destructor TLKEventListenerGroup.Destroy;
begin
  FEventThread.DeleteEventListenerGroup(Self);
  FListeners.Free;
  inherited;
end;

procedure TLKEventListenerGroup.ProcessEvent(const AEvent: TLKEvent);
var
  I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
  begin
    if AEvent.ClassType = FListeners[I].GetEventType then
    begin
      if (((FListeners[I].NewestEventOnly) and (AEvent.DispatchTime > FListeners[I].LastEventTime)) or
         (not FListeners[I].NewestEventOnly)) and (FListeners[I].GetConditionsMatch(AEvent)) and
         (((FListeners[I].ExpireAfter > 0.00) and (GetReferenceTime - AEvent.DispatchTime < FListeners[I].ExpireAfter)) or
         (FListeners[I].ExpireAfter <= 0.00)) then
      begin
        if FListeners[I].CallUIThread then
        begin
          FEventThread.Synchronize(procedure begin
                                     FListeners[I].EventCall(AEvent);
                                   end);
        end else
          FListeners[I].EventCall(AEvent);
      end;
    end;
  end;
end;

procedure TLKEventListenerGroup.RegisterListener(const AListener: TLKEventListener);
begin
  AListener.FIndex := FListeners.Add(AListener);
end;

procedure TLKEventListenerGroup.UnregisterListener(const AListener: TLKEventListener);
begin
  FListeners.Delete(AListener.FIndex);
  if FListeners.Count = 0 then
    Free;
end;

{ TLKEventStreamable }

constructor TLKEventStreamable.Create;
begin
  inherited Create;
  FEvent := GetEventType.Create;
end;

constructor TLKEventStreamable.Create(const AEvent: TLKEvent);
begin
  Create;
  FEvent.Assign(AEvent);
end;

destructor TLKEventStreamable.Destroy;
begin
  FEvent.Free;
  inherited;
end;

function TLKEventStreamable.GetEvent: TLKEvent;
begin
  Result := FEvent;
end;

procedure TLKEventStreamable.InsertIntoStream(const AStream: TStream);
begin
  FEvent.Lock;
  try
    StreamInsertBoolean(AStream, FEvent.FAllowRecording);
    StreamInsertBoolean(AStream, FEvent.FAllowTransmit);
    StreamInsertLKFloat(AStream, FEvent.FCreatedTime);
    StreamInsertLKFloat(AStream, FEvent.FDelta);
    StreamInsertTLKEventDispatchMethod(AStream, FEvent.FDispatchMethod);
    StreamInsertLKFloat(AStream, FEvent.FDispatchTime);
    StreamInsertLKFloat(AStream, FEvent.FExpiresAfter);
    StreamInsertBoolean(AStream, FEvent.FIsClone);
    StreamInsertBoolean(AStream, FEvent.FIsReplay);
    StreamInsertLKFloat(AStream, FEvent.FProcessedTime);
    InsertEventIntoStream(AStream);
  finally
    FEvent.Unlock;
  end;
end;

class procedure TLKEventStreamable.OnRegistration;
begin
  Events.RegisterEventStreamable(Self);
end;

class procedure TLKEventStreamable.OnUnregistration;
begin
  Events.UnregisterEventStreamable(Self);
end;

procedure TLKEventStreamable.ReadFromStream(const AStream: TStream);
begin
  FEvent.Lock;
  try
    FEvent.FAllowRecording := StreamReadBoolean(AStream);
    FEvent.FAllowTransmit := StreamReadBoolean(AStream);
    FEvent.FCreatedTime := StreamReadLKFloat(AStream);
    FEvent.FDelta := StreamReadLKFloat(AStream);
    FEvent.FDispatchMethod := StreamReadTLKEventDispatchMethod(AStream);
    FEvent.FDispatchTime := StreamReadLKFloat(AStream);
    FEvent.FExpiresAfter := StreamReadLKFloat(AStream);
    FEvent.FIsClone := StreamReadBoolean(AStream);
    FEvent.FIsReplay := StreamReadBoolean(AStream);
    FEvent.FProcessedTime := StreamReadLKFloat(AStream);
    ReadEventFromStream(AStream);
  finally
    FEvent.Unlock;
  end;
end;

procedure TLKEventStreamable.WriteToStream(const AStream: TStream);
begin
  FEvent.Lock;
  try
    StreamWriteBoolean(AStream, FEvent.FAllowRecording);
    StreamWriteBoolean(AStream, FEvent.FAllowTransmit);
    StreamWriteLKFloat(AStream, FEvent.FCreatedTime);
    StreamWriteLKFloat(AStream, FEvent.FDelta);
    StreamWriteTLKEventDispatchMethod(AStream, FEvent.FDispatchMethod);
    StreamWriteLKFloat(AStream, FEvent.FDispatchTime);
    StreamWriteLKFloat(AStream, FEvent.FExpiresAfter);
    StreamWriteBoolean(AStream, FEvent.FIsClone);
    StreamWriteBoolean(AStream, FEvent.FIsReplay);
    StreamWriteLKFloat(AStream, FEvent.FProcessedTime);
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

class function TLKEventStreamable<T>.GetEventType: TLKEventType;
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

{ TLKEventThreadBase }

constructor TLKEventThreadBase.Create;
begin
  inherited;
  FQueue := TLKEventList.Create(False);
  FStack := TLKEventList.Create(False);
end;

destructor TLKEventThreadBase.Destroy;
begin
  FQueue.OwnsObjects := True;
  FStack.OwnsObjects := True;
  FQueue.Free;
  FStack.Free;
  inherited;
end;

function TLKEventThreadBase.GetInitialThreadState: TLKThreadState;
begin
  Result := tsPaused;
end;

procedure TLKEventThreadBase.QueueEvent(const AEvent: TLKEvent);
begin
  AEvent.FDispatchTime := GetReferenceTime;
  FQueue.Add(AEvent);
end;

procedure TLKEventThreadBase.StackEvent(const AEvent: TLKEvent);
begin
  AEvent.FDispatchTime := GetReferenceTime;
  FStack.Add(AEvent);
end;

{ TLKEventThreadBaseWithListeners }

procedure TLKEventThreadBaseWithListeners.AddEventListenerGroup(const AEventListenerGroup: TLKEventListenerGroup);
begin
  FEventListenerGroups.Lock;
  try
    if not (FEventListenerGroups.ContainsKey(AEventListenerGroup.EventType)) then
      FEventListenerGroups.Add(AEventListenerGroup.EventType, AEventListenerGroup);
  finally
    FEventListenerGroups.Unlock;
  end;
end;

procedure TLKEventThreadBaseWithListeners.ClearEventListenerGroups;
var
  LEventListenerGroup: TLKEventListenerGroup;
begin
  FEventListenerGroups.Lock;
  try
    for LEventListenerGroup in FEventListenerGroups.Values do
      LEventListenerGroup.Free;
  finally
    FEventListenerGroups.Unlock;
  end;
end;

constructor TLKEventThreadBaseWithListeners.Create;
begin
  inherited;
  FEventListenerGroups := TLKEventListenerGroupDictionary.Create;
end;

procedure TLKEventThreadBaseWithListeners.DeleteEventListenerGroup(const AEventListenerGroup: TLKEventListenerGroup);
begin
  FEventListenerGroups.Lock;
  try
    FEventListenerGroups.Remove(AEventListenerGroup.EventType);
  finally
    FEventListenerGroups.Unlock;
  end;
end;

destructor TLKEventThreadBaseWithListeners.Destroy;
begin
  ClearEventListenerGroups;
  FEventListenerGroups.Free;
  inherited;
end;

function TLKEventThreadBaseWithListeners.GetEventListenerGroup(const AEventType: TLKEventType): TLKEventListenerGroup;
begin
  FEventListenerGroups.Lock;
  try
    FEventListenerGroups.TryGetValue(AEventType, Result);
  finally
    FEventListenerGroups.Unlock;
  end;
end;

procedure TLKEventThreadBaseWithListeners.ProcessEvents(const ADelta, AStartTime: LKFloat);
var
  I, LStart, LEnd: Integer;
begin
  // Process Stack
  if FStack.Count > 0 then
  begin
    LStart := 0;
    LEnd := FStack.Count - 1;
    for I := LStart to LEnd do
    begin
      ProcessListeners(FStack[I], ADelta, AStartTime);
      FStack[I].Free;
    end;
    FStack.DeleteRange(LStart, LEnd);
  end;
  // Process Queue
  if FQueue.Count > 0 then
  begin
    LStart := 0;
    LEnd := FQueue.Count - 1;
    for I := LStart to LEnd do
    begin
      ProcessListeners(FQueue[I], ADelta, AStartTime);
      FQueue[I].Free;
    end;
    FQueue.DeleteRange(LStart, LEnd);
  end;
end;

procedure TLKEventThreadBaseWithListeners.ProcessListeners(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat);
var
  LEventListenerGroup: TLKEventListenerGroup;
begin
  AEvent.FDelta := ADelta;
  AEvent.FProcessedTime := AStartTime;
  LEventListenerGroup := GetEventListenerGroup(TLKEventType(AEvent.ClassType));
  if LEventListenerGroup <> nil then
  begin
    if (((AEvent.ExpiresAfter > 0.00) and (GetReferenceTime - AEvent.DispatchTime < AEvent.ExpiresAfter)) or (AEvent.ExpiresAfter <= 0.00)) then
      LEventListenerGroup.ProcessEvent(AEvent);
  end;
end;

procedure TLKEventThreadBaseWithListeners.QueueEvent(const AEvent: TLKEvent);
begin
  if GetEventListenerGroup(TLKEventType(AEvent.ClassType)) = nil then
    AEvent.Free
  else
    inherited;
end;

procedure TLKEventThreadBaseWithListeners.RegisterListener(const AListener: TLKEventListener);
var
  LEventListenerGroup: TLKEventListenerGroup;
begin
  FEventListenerGroups.Lock;
  try
    LEventListenerGroup := GetEventListenerGroup(AListener.GetEventType);
    if LEventListenerGroup = nil then // If there's no Event Group for this Event Type...
    begin
      LEventListenerGroup := TLKEventListenerGroup.Create(Self, AListener.GetEventType); // Register it
    end;
    LEventListenerGroup.RegisterListener(AListener);
  finally
    FEventListenerGroups.Unlock;
  end;
end;

procedure TLKEventThreadBaseWithListeners.StackEvent(const AEvent: TLKEvent);
begin
  if GetEventListenerGroup(TLKEventType(AEvent.ClassType)) = nil then
    AEvent.Free
  else
    inherited;
end;

function TLKEventThreadBaseWithListeners.UnregisterListener(const AListener: TLKEventListener): Integer;
var
  LEventListenerGroup: TLKEventListenerGroup;
begin
  FEventListenerGroups.Lock;
  try
    LEventListenerGroup := GetEventListenerGroup(AListener.GetEventType);
    if LEventListenerGroup <> nil then
      LEventListenerGroup.UnregisterListener(AListener);
    FEventListenerGroups.Unlock;
  finally
    Result := -1;
  end;
end;

{ TLKEventThread }

constructor TLKEventThread.Create;
begin
  inherited;
  Events.RegisterEventThread(Self);
end;

destructor TLKEventThread.Destroy;
begin
  Events.UnregisterEventThread(Self);
  inherited;
end;

function TLKEventThread.GetDefaultYieldAccumulatedTime: Boolean;
begin
  // We must NOT yield all Accumulated Time on Event-enabled Threads.
  // Doing so would prevent the Event Queue being processed
  Result := False;
end;

procedure TLKEventThread.PreTick(const ADelta, AStartTime: LKFloat);
begin
  ProcessEvents(ADelta, AStartTime);
end;

procedure TLKEventThread.Tick(const ADelta, AStartTime: LKFloat);
begin
  // Do nothing (this just immutes the procedure, because you may not want a looping process in the Thread
end;

{ TLKEventThreadPooled }

constructor TLKEventThreadPooled.Create(const AEventThreadPool: TLKEventThreadPool);
begin
  inherited Create;
  { TODO -oSimon -cEvent Engine : Register with Event Thread Pool }
end;

destructor TLKEventThreadPooled.Destroy;
begin
{ TODO -oSimon -cEvent Engine : Unregister with Event Thread Pool }
  inherited;
end;

function TLKEventThreadPooled.GetDefaultYieldAccumulatedTime: Boolean;
begin
  // We must NOT yield all Accumulated Time on Event-enabled Threads.
  // Doing so would prevent the Event Queue being processed
  Result := False;
end;

procedure TLKEventThreadPooled.PreTick(const ADelta, AStartTime: LKFloat);
begin
  ProcessEvents(ADelta, AStartTime);
end;

procedure TLKEventThreadPooled.Tick(const ADelta, AStartTime: LKFloat);
begin
  // Do nothing (this just immutes the procedure, because you may not want a looping process in the Thread
end;

{ TLKEventTransmitterBase }

function TLKEventTransmitterBase.AcceptEvent(const AEvent: TLKEvent): Boolean;
begin
  Lock;
  try
    Result := ((FUseEventTypeList) and (FValidEventTypes.Contains(TLKEventType(AEvent)))) or (not FUseEventTypeList);
  finally
    Unlock;
  end;
end;

procedure TLKEventTransmitterBase.AddValidEventType(const AEventType: TLKEventType);
begin
  Lock;
  try
    if (not FValidEventTypes.Contains(AEventType)) then
      FValidEventTypes.Add(AEventType);
  finally
    Unlock;
  end;
end;

constructor TLKEventTransmitterBase.Create;
begin
  inherited;
  FValidEventTypes := TLKEventTypeList.Create;
  Events.FTransmitters.AddTransmitter(Self);
  FUseEventTypeList := False; // We don't want to limit outgoing Events by default
end;

destructor TLKEventTransmitterBase.Destroy;
begin
  Events.FTransmitters.DeleteTransmitter(Self);
  FValidEventTypes.Free;
  inherited;
end;

function TLKEventTransmitterBase.GetUseEventTypeList: Boolean;
begin
  Lock;
  try
    Result := FUseEventTypeList;
  finally
    Unlock;
  end;
end;

procedure TLKEventTransmitterBase.RemoveValidEventType(const AEventType: TLKEventType);
begin
  Lock;
  try
    FValidEventTypes.Remove(AEventType);
  finally
    Unlock;
  end;
end;

procedure TLKEventTransmitterBase.SetUseEventTypeList(const AValue: Boolean);
begin
  Lock;
  try
    FUseEventTypeList := AValue;
  finally
    Unlock;
  end;
end;

{ TLKEventTransmitterManager }

procedure TLKEventTransmitterManager.AddEvent(const AEvent: TLKEvent);
var
  LTransmitterExists: Boolean;
  LClone: TLKEvent;
begin
  // Check if there actually are any Transmitters first
  LTransmitterExists := FTransmitters.Count > 0;
  if LTransmitterExists then
  begin
    // Clone Event
    LClone := TLKEventType(AEvent.ClassType).Create;
    LClone.Assign(AEvent);
    // Add Clone to Event Queue
    FEvents.Add(AEvent);
  end;
end;

procedure TLKEventTransmitterManager.AddTransmitter(const ATransmitter: TLKEventTransmitterBase);
begin
  ATransmitter.FIndex := FTransmitters.Add(ATransmitter);
end;

constructor TLKEventTransmitterManager.Create;
begin
  inherited;
  FTransmitters := TLKEventTransmitterList.Create;
  FEvents := TLKEventList.Create(False);
end;

procedure TLKEventTransmitterManager.DeleteTransmitter(const ATransmitter: TLKEventTransmitterBase);
begin
  FTransmitters.Delete(ATransmitter.FIndex);
end;

destructor TLKEventTransmitterManager.Destroy;
begin
  FTransmitters.Free;
  FEvents.OwnsObjects := True;
  FEvents.Free;
  inherited;
end;

function TLKEventTransmitterManager.GetDefaultYieldAccumulatedTime: Boolean;
begin
  // We need to yield time in small chunks, rather than as a complete block
  Result := False;
end;

function TLKEventTransmitterManager.GetInitialThreadState: TLKThreadState;
begin
  Result := tsPaused;
end;

procedure TLKEventTransmitterManager.PreTick(const ADelta, AStartTime: LKFloat);
var
  I: Integer;
  LEventStream: TMemoryStream;
begin
  if FEvents.Count > 0 then
  begin
    LEventStream := TMemoryStream.Create;
    try
      // Since we're NOW Streaming the Event, we don't want an endless loop where Program A streams to
      // Program B, then Program B instantly streams back to Program A.... that would suck!
      FEvents[0].FAllowTransmit := False;
      FEvents[0].WriteToStream(LEventStream); // Serialize the Event into the Event Stream
      for I := 0 to FTransmitters.Count - 1 do
      begin
        if FTransmitters[I].AcceptEvent(FEvents[0]) then // Check if the Transmitter cares for this Event
          FTransmitters[I].TransmitEvent(FEvents[0], LEventStream); // If so, Transmit it!
      end;
      FEvents[0].Free; // Destroy the Event
      // Remove the processed Event from the Event Array
      FEvents.Delete(0);
    finally
      LEventStream.Free;
    end;
  end;
end;

procedure TLKEventTransmitterManager.Tick(const ADelta, AStartTime: LKFloat);
begin
  // Do Nothing (Yet, but probably never will either)
end;

{ TLKEventRecorder }

constructor TLKEventRecorder.Create;
begin
  inherited;
  FIndex := -1;
end;

destructor TLKEventRecorder.Destroy;
begin
  if FIndex <> -1 then
    Unsubscribe;
  inherited;
end;

function TLKEventRecorder.GetInitialThreadState: TLKThreadState;
begin
  Result := tsPaused;
end;

function TLKEventRecorder.PrepareStreamable(const AEvent: TLKEvent): TLKEventStreamable;
var
  LStreamableType: TLKEventStreamableType;
begin
  Result := nil;
  LStreamableType := Events.GetEventStreamableType(AEvent);
  if LStreamableType <> nil then
    Result := LStreamableType.Create(AEvent);
end;

procedure TLKEventRecorder.ProcessEvents(const ADelta, AStartTime: LKFloat);
var
  I, LStart, LEnd: Integer;
begin
  // We don't Lock the Event Array at this point, as doing so would prevent additional
  // events from being added to the Queue (and could cause a Thread to freeze)
  // Process Stack
  if FStack.Count > 0 then
  begin
    LStart := 0;
    LEnd := FStack.Count - 1;
    for I := LStart to LEnd do
    begin
      FStack[I].FDelta := ADelta;
      FStack[I].FProcessedTime := AStartTime;
      RecordEvent(FStack[I]);
      FStack[I].Free;
    end;
    FStack.DeleteRange(LStart, LEnd);
  end;
  // Process Queue
  if FQueue.Count > 0 then
  begin
    LStart := 0;
    LEnd := FQueue.Count - 1;
    for I := LStart to LEnd do
    begin
      FQueue[I].FDelta := ADelta;
      FQueue[I].FProcessedTime := AStartTime;
      RecordEvent(FQueue[I]);
      FQueue[I].Free;
    end;
    FQueue.DeleteRange(LStart, LEnd);
  end;
end;

procedure TLKEventRecorder.QueueEvent(const AEvent: TLKEvent);
begin
  inherited;
  ThreadState := tsRunning;
end;

procedure TLKEventRecorder.StackEvent(const AEvent: TLKEvent);
begin
  inherited;
  ThreadState := tsRunning;
end;

procedure TLKEventRecorder.Subscribe;
begin
  if FIndex = -1 then
    Events.RegisterRecorder(Self);
end;

procedure TLKEventRecorder.Tick(const ADelta, AStartTime: LKFloat);
begin
  ProcessEvents(ADelta, AStartTime);
  if (FStack.Count = 0) and (FQueue.Count = 0) then
    ThreadState := tsPaused;
end;

procedure TLKEventRecorder.Unsubscribe;
begin
  if FIndex <> -1 then
    Events.UnregisterRecorder(Self);
end;

{ TLKEventProcessor }

function TLKEventProcessor.GetInitialThreadState: TLKThreadState;
begin
  Result := tsPaused;
end;

procedure TLKEventProcessor.QueueEvent(const AEvent: TLKEvent);
begin
  inherited;
  ThreadState := tsRunning;
end;

procedure TLKEventProcessor.StackEvent(const AEvent: TLKEvent);
begin
  inherited;
  ThreadState := tsRunning;
end;

procedure TLKEventProcessor.Tick(const ADelta, AStartTime: LKFloat);
begin
  ProcessEvents(ADelta, AStartTime);
  if (FStack.Count = 0) and (FQueue.Count = 0) then
    ThreadState := tsPaused;
end;

{ TLKEventScheduled }

constructor TLKEventScheduled.Create(const AEvent: TLKEvent; const AScheduledFor: LKFloat);
begin
  inherited Create;
  FEvent := AEvent;
  FScheduledFor := AScheduledFor;
end;

destructor TLKEventScheduled.Destroy;
begin
  inherited;
end;

{ TLKEventScheduleList }

function TLKEventScheduleList.AEqualToB(const A, B: TLKEventScheduled): Boolean;
begin
  Result := A.ScheduledFor = B.ScheduledFor;
end;

function TLKEventScheduleList.AGreaterThanB(const A, B: TLKEventScheduled): Boolean;
begin
  Result := A.ScheduledFor > B.ScheduledFor;
end;

function TLKEventScheduleList.AGreaterThanOrEqualB(const A, B: TLKEventScheduled): Boolean;
begin
  Result := A.ScheduledFor >= B.ScheduledFor;
end;

function TLKEventScheduleList.ALessThanB(const A, B: TLKEventScheduled): Boolean;
begin
  Result := A.ScheduledFor < B.ScheduledFor;
end;

function TLKEventScheduleList.ALessThanOrEqualB(const A, B: TLKEventScheduled): Boolean;
begin
  Result := A.ScheduledFor <= B.ScheduledFor;
end;

destructor TLKEventScheduleList.Destroy;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    Items[I].FEvent.Free;
    Items[I].Free;
  end;

  Clear(True);
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

procedure TLKEventScheduler.QueueEvent(const AEvent: TLKEvent; const AScheduleFor: LKFloat);
begin
  AEvent.FDispatchMethod := edQueue;
  ScheduleEvent(AEvent, AScheduleFor);
end;

procedure TLKEventScheduler.ScheduleEvent(const AEvent: TLKEvent; const AScheduleFor: LKFloat);
var
  LSchedule: TLKEventScheduled;
begin
  LSchedule := TLKEventScheduled.Create(AEvent, AScheduleFor);
  FEvents.Add(LSchedule);
  ThreadState := tsRunning;
end;

procedure TLKEventScheduler.StackEvent(const AEvent: TLKEvent; const AScheduleFor: LKFloat);
begin
  AEvent.FDispatchMethod := edStack;
  ScheduleEvent(AEvent, AScheduleFor);
end;

procedure TLKEventScheduler.Tick(const ADelta, AStartTime: LKFloat);
begin
    if FEvents.Count > 0 then
    begin
      if FEvents[0].ScheduledFor <= GetReferenceTime then // Is it time to dispatch this Event yet?
      begin
        case FEvents[0].Event.DispatchMethod of // If so...
          edQueue: FEvents[0].Event.Queue; // ... send Dispatch to the queue...
          edStack: FEvents[0].Event.Stack; // ... or the Stack...
        end;
        FEvents.Lock;
        try
          FEvents[0].Free; // ... then remove the container...
          FEvents.Delete(0); // ... and delete the entry from the Schedule
        finally
          FEvents.Unlock;
        end;
      end else
        {$IFDEF POSIX}
          usleep(1);
        {$ELSE}
          Sleep(1);
        {$ENDIF POSIX}
    end else
      ThreadState := tsPaused;
end;

{ TLKEventEngine }

constructor TLKEventEngine.Create;
begin
  inherited;
  FEventThreads := TLKEventThreadList.Create;
  FRecorders := TLKEventRecorderList.Create;
  FEventProcessor := TLKEventProcessor.Create;
  FEventScheduler := TLKEventScheduler.Create;
  FEventStreamables := TLKEventStreamableDictionary.Create;
end;

destructor TLKEventEngine.Destroy;
var
  I: Integer;
begin
  FEventScheduler.Kill;
  for I := FEventThreads.Count - 1 downto 0 do
    UnregisterEventThread(FEventThreads[I]);
  for I := FRecorders.Count - 1 downto 0 do
    FRecorders[I].Unsubscribe;
  FEventProcessor.Kill;
  FEventThreads.Free;
  FRecorders.Free;
  FEventStreamables.Free;
  inherited;
end;

function TLKEventEngine.GetEventStreamableType(const AEventType: TLKEventType): TLKEventStreamableType;
begin
  if (not FEventStreamables.TryGetValue(AEventType, Result)) then
    Result := nil;
end;

function TLKEventEngine.GetEventStreamableType(const AEvent: TLKEvent): TLKEventStreamableType;
begin
  Result := GetEventStreamableType(TLKEventType(AEvent.ClassType));
end;

procedure TLKEventEngine.QueueEvent(const AEvent: TLKEvent);
begin
  AEvent.FDispatchMethod := edQueue;

  if (AEvent.AllowRecording) and (not AEvent.IsReplay) then
    QueueInRecorders(AEvent);

  QueueInThreads(AEvent);

  FEventProcessor.QueueEvent(AEvent);
end;

procedure TLKEventEngine.QueueInRecorders(const AEvent: TLKEvent);
var
  I: Integer;
  LClone: TLKEvent;
begin
  for I := 0 to FRecorders.Count - 1 do
  begin
    LClone := TLKEventType(AEvent.ClassType).Create; // Create a blank instance of the Event for the Clone
    LClone.Assign(AEvent); // Copy the original data into the Clone
    FRecorders[I].QueueEvent(LClone); // Send a CLONE of the Event to the Recorder!
  end;
end;

procedure TLKEventEngine.QueueInThreads(const AEvent: TLKEvent);
var
  I: Integer;
  LClone: TLKEvent;
begin
  for I := 0 to FEventThreads.Count - 1 do
  begin
    LClone := TLKEventType(AEvent.ClassType).Create; // Create a blank instance of the Event for the Clone
    LClone.Assign(AEvent); // Copy the original data into the Clone
    FEventThreads[I].QueueEvent(LClone); // Send a CLONE of the Event to the Thread!
  end;
end;

procedure TLKEventEngine.RegisterEventStreamable(const AEventStreamableType: TLKEventStreamableType);
var
  LStreamableType: TLKEventStreamableType;
begin
  if FEventStreamables.TryGetValue(AEventStreamableType.GetEventType, LStreamableType) then
    raise ELKEventStreamableExistsForEventType.CreateFmt('Event Type "%s" already has a registered Streamable Descriptor: "%s"', [AEventStreamableType.GetEventType.ClassName, LStreamableType.ClassName])
  else
    FEventStreamables.Add(AEventStreamableType.GetEventType, AEventStreamableType);
end;

procedure TLKEventEngine.RegisterEventThread(const AEventThread: TLKEventThread);
begin
  AEventThread.FIndex := FEventThreads.Add(AEventThread);
end;

procedure TLKEventEngine.RegisterListener(const AListener: TLKEventListener);
begin
  FEventProcessor.RegisterListener(AListener);
end;

procedure TLKEventEngine.RegisterRecorder(const ARecorder: TLKEventRecorder);
begin
  ARecorder.FIndex := FRecorders.Add(ARecorder);
end;

procedure TLKEventEngine.StackEvent(const AEvent: TLKEvent);
begin
  AEvent.FDispatchMethod := edStack;

  if (AEvent.AllowRecording) and (not AEvent.IsReplay) then
    QueueInRecorders(AEvent);

  StackInThreads(AEvent);

  FEventProcessor.StackEvent(AEvent);
end;

procedure TLKEventEngine.StackInThreads(const AEvent: TLKEvent);
var
  I: Integer;
  LClone: TLKEvent;
begin
  for I := 0 to FEventThreads.Count - 1 do
  begin
    LClone := TLKEventType(AEvent.ClassType).Create; // Create a blank instance of the Event for the Clone
    LClone.Assign(AEvent); // Copy the original data into the Clone
    FEventThreads[I].StackEvent(LClone); // Send a CLONE of the Event to the Thread!
  end;
end;

procedure TLKEventEngine.TransmitEvent(const AEvent: TLKEvent);
begin
//  FTransmitters.AddEvent(AEvent);
  AEvent.Free;
end;

procedure TLKEventEngine.UnregisterEventStreamable(const AEventStreamableType: TLKEventStreamableType);
begin
  if FEventStreamables.ContainsKey(AEventStreamableType.GetEventType) then
    FEventStreamables.Remove(AEventStreamableType.GetEventType)
  else
    raise ELKEventStreamableNotExists.CreateFmt('Event Streamable Descriptor "%s" not Registered!', [AEventStreamableType.ClassName])
end;

procedure TLKEventEngine.UnregisterEventThread(const AEventThread: TLKEventThread);
begin
  FEventThreads.Delete(AEventThread.FIndex);
end;

function TLKEventEngine.UnregisterListener(const AListener: TLKEventListener): Integer;
begin
  FEventProcessor.UnregisterListener(AListener);
  Result := -1;
end;

procedure TLKEventEngine.UnregisterRecorder(const ARecorder: TLKEventRecorder);
begin
  FRecorders.Delete(ARecorder.FIndex);
  ARecorder.FIndex := -1;
end;

{ TLKEventThreadPool }

constructor TLKEventThreadPool.Create;
begin
  Create(TThread.ProcessorCount);
end;

constructor TLKEventThreadPool.Create(const AInstanceLimit: Integer);
begin
  inherited Create;
  FPooledEventThreads := TLKEventThreadPooledList.Create;
end;

destructor TLKEventThreadPool.Destroy;
begin
  FPooledEventThreads.Free;
  inherited;
end;

initialization
  Events := TLKEventEngine.Create;
finalization
  Events.Free;

end.
