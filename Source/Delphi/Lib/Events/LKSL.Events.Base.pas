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
unit LKSL.Events.Base;

interface

{$I LKSL.inc}

{
  About this unit:
    - This unit provides type declarations required for our "Event Engine"
    - This unit also provides the Abstract Base Implementation for the same.

  Included Usage Demos:
    - "LKSL_Demo_EventEngine_Basic" in the "\Demos\Delphi\<version>\Event Engine\Basic" folder

  Changelog (latest changes first):
    1st December 2014:
      - Fixed issue where ALL events would be Recorded (regardless of their "AllowRecording" setting.
      - Fixed some other silly little bugs.
    30th November 2014:
      - Generics are now mandatory (support for non-Generics versions of Delphi is not being considered)
      - "TDictionary" references replaced by "TLKDictionary" (which integrates the thread-safe Lock)
      - Support for non-namespaced units added
      - Introduced the "Event Recorder" system
        - Events can be set to allow or disallow recording
        - Events that allow recording will be recorded regardless of whether or not there are
          any "Listeners" registered for that Event Type.
        - Event Recordings can be replayed either in the same application, or in any application you want
          so long as that application has all of the same Event Types registered.
        - TLKEvent.GetDefaultStreamEvent changed to "GetDefaultAllowTransmit"
        - TLKEvent.StreamEvent changed to "AllowTransmit"
      - TLKEvent additions:
        - (Protected) "GetDefaultAllowRecording"
          - Default = True
          - Override if you want your Event Type to NOT be Recordable by default
        - (Public, Property) "AllowRecording"
          - True = Allow the Event to be Recorded
          - False = Do NOT allow the Event to be Recorded.
        - (Public, Property) "IsReplay" [Read-only]
          - True = The Event Instance is part of a Replay
          - False = The Event Instance is NOT part of a Replay
    28th November 2014 (2nd update):
      - Reverted some of the "for in" loops because of performance implications!
    28th November 2014:
      - Added integration for Generic Containers (TDictionary) for Event Listener Groups
        in TLKEventThreadBase
      - Made some trivial syntactic changes to certain "for" loops because it looks cleaner.
    27th November 2014:
      - A known "ISSUE" remains in TLKEventThreadBase.ProcessListeners
        - Locking the Event Listener Groups at the point of a call represents a significant risk
          where the Event Call may attempt to register or unregister a Listener.
          This would cause a deadlock, because the Queue/Stack thread would hold the Lock, but
          the executing thread would try to demand it. This means the Queue/Stack thread won't
          be able to release its Lock (as the Event Call can't return until AFTER the Event Call returns).
          I'M OPEN TO SUGGESTIONS ON WAYS AROUND THIS ISSUE!
          You won't have any issues so long as you aren't Subscribing/Unsubscribing Event Listeners
          within an Event Call!
      - Wrapped all LOCKS with "Try/Finally" blocks to ensure that the lock is released if there's
        a failure (for whatever reason)
      - Updated TLKEvent to match changes made in TLKStreamable
          - (Public) "DeleteFromStream" is now (Protected) "RemoveEventFromStream"
          - (Public) "ReadFromStream" is now (Protected) "ReadEventFromStream"
          - (Public) "InsertIntoStream" is now (Protected) "InsertEventIntoStream"
          - (Public) "WriteToStream" is now (Protected) "WriteEventToStream"
          > This causes Compiler Warnings if you forget to implement these methods in your Event classes!
      - Added "GetDispatchMode" to TLKEvent... you can now set an Event to allow dispatch through:
        - The Queue OR the Stack
        - The Queue ONLY
        - The Stack ONLY
        > Default = Queue OR Stack
        > If you attempt to dispatch through a restricted handler, it simply refers the Event to the correct
          handler automatically. Once the "Event Debugger" is done, a warning will be introduced for these
          occurences.
      - "TLKPriority" became "TLKDispatchMethod"
        - "Priority" doesn't accurate describe what the value represents
        - Plans are afoot to introduce a "Priority" system to the Event Engine shortly, so this had to be
          changed to something more appropriate to avoid confusion down the line.
      - "TLKEventHandler" has become "LKEventEngine" and is no longer a public global (it doesn't need to be)
      - Deprecated global methods "QueueEvent" and "StackEvent" has been removed
        > Call "MyEvent.Queue" and "MyEvent.Stack" respectively to dispatch an Event
    26th November 2014:
      - Added overloaded Constructors to the GENERIC version of TLKEventListener.
        You can now pass an Event Callback as a parameter of the Constructor to streamline the
        initialization of the Listener (one line instead of 2)
    18th November 2014:
      - Added a GENERIC version of TLKEventListener contributed by Uwe Raab
        see http://www.uweraabe.de/Blog/2014/11/09/a-generic-tlkeventlistener-for-lksl/
        This eliminates the replication of boilerplate code for easier implementation.
      - Deprecated "QueueEvent" and "StackEvent" procedures. Will be removed after 1st December 2014
      - Added LKSL.INC include for configuration settings and version-specific configurations
    13th October 2014:
      - Added Enum "TLKEventProcessMode"
      - Added Property "ProcessMode" to TLKEventThreadBase to switch between full queue processing and
        one 'Single Event per Cycle' processing.
      - Set TLKEventThread to use the new full queue processing mode by default
        (set "ProcessMode := epmOneByOne;" against your Event Thread if you want the old processing
        mode instead of the new (more efficient) one.
    21st September 2014:
      - Immuted the "Tick" Procedure in TLKEventThread, as you may not wish to have a looped process
        taking place in an Event Thread (given that the Thread is Event-Driven).
    15th September 2014:
      - Removed unnecessary Callback Types (they weren't used anywhere).
      - Fixed several bugs (including one critical bug)
    9th Steptember 2014:
      - Removed a piece of fault-finding code I had forgotten about
      - Added Method "GetDefaultExpiresAfter" to "TLKEvent" so you can specify a default Expiry Time
        on your Event Types (where desired). It defaults to 0.00, so if you DON'T want the Event to
        expire, you don't need to override that function.
      - Added Method "GetDefaultStreamEvent" to "LKEvent" so you can specify whether or not that Event
        should be passed over to the Event Streaming system for transmission to other processes.
      - Added property "StreamEvent" to "TLKEvent". When "True", that Event will be passed to the
        Event Streaming system for transmission to other processes.
      - Added a Transmitter and Receiver system for Streaming Events to other processes. Cool, huh?
    8th September 2014:
      - Fixed a bug in "TLKEvent.Clone" method (Missing instruction to Lock the Original Event)
    6th September 2014:
      - Changed "TLKEvent" ancestor for "TLKPersistent" to "TLKStreamable"
      - Added interface Uses reference to "LKSL.Streamables.Base"
      - Added implementation Uses reference to "LKSL.Streams.System.pas"
    5th September 2014 (small change commit):
      - Changed "GetEventTypeGUID" to "GetTypeGUID" in TLKEvent. This is because its Parent Type
        needs to be changed to "TLKStreamable" once the new and improved Streamables Engine is
        released publicly.
    5th September 2014:
      - Prepared for Release
}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs,
  {$ELSE}
    Classes, SysUtils, SyncObjs,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  Generics.Collections, LKSL.Generics.Collections,
  LKSL.Common.Types,
  LKSL.Threads.Base,
  LKSL.Streamables.Base;

type
  { Forward Declarations }
  TLKEvent = class;
  TLKEventListener = class;
  TLKEventListenerGroup = class;
  TLKEventThreadBase = class;
  TLKEventThreadBaseWithListeners = class;
  TLKEventThread = class;
  TLKEventTransmitterBase = class;
  TLKEventTransmitterManager = class;
  TLKEventReceiverBase = class;
  TLKEventRecorder = class;

  { Exception Types }
  ELKEventException = ELKException;
    ELEventListenerException = ELKEventException;
    ELEventListenerGroupException = ELKEventException;
    ELEventRecorderException = ELKEventException;
      ELEventRecorderSignatureMismatchException = ELEventRecorderException;

  { Enum Types }
  TLKEventDispatchMode = (edmQueue, edmStack, edmThreads);
  TLKEventDispatchMethod = (edQueue, edStack);
  TLKEventProcessMode = (epmOneByOne, epmAll);

  { Set Types }
  TLKEventDispatchModes = set of TLKEventDispatchMode;

  { Class References }
  TLKEventType = class of TLKEvent;
  TLKEventListenerType = class of TLKEventListener;

  { Array Types }
  TLKEventArray = TArray<TLKEvent>;
  TLKEventTypeArray = TArray<TLKEventType>;
  TLKEventListenerArray = TArray<TLKEventListener>;
  TLKEventThreadArray = TArray<TLKEventThread>;
  TLKEventTransmitterArray = TArray<TLKEventTransmitterBase>;
  TLKEventReceiverArray = TArray<TLKEventReceiverBase>;
  TLKEventRecorderArray = TArray<TLKEventRecorder>;

  { Hashmap Types }
  TLKEventListenerGroupDictionary = TLKDictionary<TGUID, TLKEventListenerGroup>;

  {
    TLKEvent
      - An "Event" does nothing but provide "Parameter Data"
      - This "Parameter Data" is for use by your "Listeners"
      - Information provided by default relates to dispatch and process timing
      - Define an "ExpiresAfter" value of greater than 0.00 if you want the Event to expire
        if it is not processed after a certain amount of time. This is useful if Event Data
        is only valid for a limited time before being replaced by more up-to-date data.
  }
  TLKEvent = class abstract(TLKStreamable)
  private
    FAllowRecording: Boolean;
    FAllowTransmit: Boolean;
    FDelta: Double;
    FDispatchModes: TLKEventDispatchModes;
    FDispatchMethod: TLKEventDispatchMethod;
    FDispatchTime: Double;
    FExpiresAfter: Double;
    FIsReplay: Boolean;
    FProcessedTime: Double;

    function GetAllowRecording: Boolean;
    function GetAllowTransmit: Boolean;
    function GetDelta: Double;
    function GetDispatchMethod: TLKEventDispatchMethod;
    function GetDispatchTime: Double;
    function GetExpiresAfter: Double;
    function GetIsReplay: Boolean;
    function GetProcessedTime: Double;

    procedure SetAllowRecording(const AAllowRecording: Boolean);
    procedure SetAllowTransmit(const AAllowTransmit: Boolean);
    procedure SetExpiresAfter(const AExpiresAfter: Double);
  protected
    class procedure RemoveFromStream(const AStream: TStream); override; final;
    procedure ReadFromStream(const AStream: TStream); override; final;
    procedure InsertIntoStream(const AStream: TStream); override; final;
    procedure WriteToStream(const AStream: TStream); override; final;
    // You MUST override "Clone"
    // (Remembering to type-cast "AFromEvent" to your Event Type) populate your Event Type's properties.
    procedure Clone(const AFromEvent: TLKEvent); virtual; abstract;
    // You MUST override and implement "RemoveEventFromStream"
    // This removes an Instance of your Event Type from a Stream
    class procedure RemoveEventFromStream(const AStream: TStream); virtual; abstract;
    // You MUST override and implement "ReadEventFromStream"
    // This populates your Event Instance from a Stream
    procedure ReadEventFromStream(const AStream: TStream); virtual; abstract;
    // You MUST override and implement "InsertEventIntoStream"
    // This Inserts your Event Instance's data into a Stream
    procedure InsertEventIntoStream(const AStream: TStream); virtual; abstract;
    // You MUST override and implement "WriteEventToStream"
    // This APPENDS your Event Instance's data to the END of a Stream
    procedure WriteEventToStream(const AStream: TStream); virtual; abstract;
    // Override "GetDefaultAllowRecording" if you want to PREVENT your Event from being recorded for replay.
    // You can also set individual Event Instances to Allow or Disallow recording by setting the
    // "AllowRecording" property to True (will record) or False (won't record)
    // Default = True
    // CRITICAL: If your Event Type contains a Pointer or Reference, you must NOT let it be recorded!
    function GetDefaultAllowRecording: Boolean; virtual;
    // Override "GetDefaultExpiresAfter" if you want your Event to Expire after a certain amount of time
    // (measured in seconds). By default, it returns 0.00 to indiciate that the Event should NEVER expire.
    function GetDefaultExpiresAfter: Double; virtual;
    // Override "GetDefaultAllowTransmit" if you want the Event to be passed over to your Event Streaming
    // system. Your Event Transmission system should then decide where this Event should be Streamed to.
    // By default, it returns "False" indicating that this Event isn't intended to be Streamed to
    // another process.
    function GetDefaultAllowTransmit: Boolean; virtual;
    // Override "GetDispatchModes" if you want the Event to restrict the way your Event Type is dispatched.
    // Available mode switches are: edmQueue, edmStack, edmThreads.
    // Default = [edmQueue, edmStack, edmThreads]
    function GetDispatchModes: TLKEventDispatchModes; virtual;
  public
    constructor Create; override;

    procedure Assign(AFromEvent: TPersistent); override; final;

    procedure Queue; // Add this Event to the Event Queue
    procedure Stack; // Add this Event to the Event Stack
    procedure TransmitOnly; // DOESN'T Queue OR Stack the Event for internal processing, just Transmits it

    property AllowRecording: Boolean read GetAllowRecording write SetAllowRecording;
    property AllowTransmit: Boolean read GetAllowTransmit write SetAllowTransmit;
    property Delta: Double read GetDelta;
    property DispatchMethod: TLKEventDispatchMethod read GetDispatchMethod;
    property DispatchTime: Double read GetDispatchTime;
    property ExpiresAfter: Double read GetExpiresAfter write SetExpiresAfter;
    property IsReplay: Boolean read GetIsReplay;
    property ProcessedTime: Double read GetProcessedTime;
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
    FExpireAfter: Double;
    FIndex: Integer;
    FLastEventTime: Double;
    FNewestEventOnly: Boolean;

    function GetCallUIThread: Boolean;
    function GetEventThread: TLKEventThread;
    function GetExpireAfter: Double;
    function GetLastEventTime: Double;
    function GetNewestEventOnly: Boolean;

    procedure SetCallUIThread(const ACallUIThread: Boolean);
    procedure SetExpireAfter(const AExpireAfter: Double);
    procedure SetNewestEventOnly(const ANewestOnly: Boolean);
  protected
    // Override "GetDefaultCallUIThread" if you want your Listener to Synchronize its execution with the UI Thread
    // By default, Event Callbacks are NOT Synchronized (it returns "False")
    function GetDefaultCallUIThread: Boolean; virtual;
    // Override "GetDefaultExpireAfter" if you want your Listener to disregard Events that are older than the specified time (in Seconds)
    // By default, Listeners do not care how old an Event is ("0.00" = No Expiry)
    function GetDefaultExpireAfter: Double; virtual;
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
    property ExpireAfter: Double read GetExpireAfter write SetExpireAfter;
    property LastEventTime: Double read GetLastEventTime;
    property NewestEventOnly: Boolean read GetNewestEventOnly write SetNewestEventOnly;
  end;

  {
    TLKEventListener
      - A Generic version of the original TLKEventListener
      - Eliminates some replication of boilerplate code.
      - Contributed by Uwe Raab
      - (see http://www.uweraabe.de/Blog/2014/11/09/a-generic-tlkeventlistener-for-lksl/)
      ---
      - Updated by Simon J Stuart (26th October 2011)
        - Now includes overloaded Constructors so you can assign "OnEvent" inline on creation
  }
  TLKEventListener<T: TLKEvent> = class(TLKEventListener)
  type
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
    FListeners: TLKEventListenerArray;
  public
    constructor Create(const AEventThread: TLKEventThreadBaseWithListeners; const AEventType: TLKEventType); reintroduce;
    destructor Destroy; override;

    procedure ProcessEvent(const AEvent: TLKEvent);
    procedure RegisterListener(const AListener: TLKEventListener);
    procedure UnregisterListener(const AListener: TLKEventListener);

    property EventType: TLKEventType read FEventType;
  end;

  {
    TLKEventThreadBase
      - Abstract Base Type for Threads containing an Event Array
  }
  TLKEventThreadBase = class abstract(TLKThread)
  private
    FEventLock: TCriticalSection;
    FEvents: TLKEventArray;

    FProcessMode: TLKEventProcessMode;
    function GetProcessMode: TLKEventProcessMode;
    procedure SetProcessMode(const AProcessMode: TLKEventProcessMode);

    procedure ClearEvents;
    // Once we've executed an Event, "RemoveEvent" is called to manage the Event Array
    procedure RemoveEvent(const AIndex: Integer);
    // We also use a separate Lock for Events
    procedure LockEvents; inline;
    procedure UnlockEvents; inline;
  protected
    // "ProcessEvents" is overriden by TLKEventThread, TLKEventQueue and TLKEventStack,
    // which individually dictate how to process Events from the Events Array.
    procedure ProcessEvents(const ADelta, AStartTime: Double); virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;
    // "AddEvent" adds an Event to the Thread's internal Event Array for processing
    procedure AddEvent(const AEvent: TLKEvent); virtual;

    property ProcessMode: TLKEventProcessMode read GetProcessMode write SetProcessMode;
  end;

  {
    TLKEventThreadBase
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
    procedure ProcessListeners(const AEvent: TLKEvent; const ADelta, AStartTime: Double);

    // Methods to register and unregister Event Listeners
    procedure RegisterListener(const AListener: TLKEventListener);
    function UnregisterListener(const AListener: TLKEventListener): Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddEvent(const AEvent: TLKEvent); override;
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
    procedure PreTick(const ADelta, AStartTime: Double); override;
    procedure ProcessEvents(const ADelta, AStartTime: Double); override; final;
    procedure Tick(const ADelta, AStartTime: Double); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  {
    TLKEventTransmitterBase
      - Abstract Base Class for Event Transmitters
      - Populate the virtual/abstract methods with the code necessary to send Events to whatever process
        you want.
  }
  TLKEventTransmitterBase = class abstract(TLKPersistent)
  private
    FValidEventTypes: TLKEventTypeArray;
    FIndex: Integer; // This Thread's position in the Event Handler's "EventThread" Array
    FUseEventTypeList: Boolean;
    function GetUseEventTypeList: Boolean;
    procedure SetUseEventTypeList(const AValue: Boolean);
    function EventTypeValid(const AEventType: TLKEventType): Integer;
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
    TLKEventTransmitterManager
      - Manages one or more Event Transmitters
      - Events are Queued for Transmission (allows the internal Event Handler to continue processing
        Events)
  }
  TLKEventTransmitterManager = class(TLKThread)
  private
    FEvents: TLKEventArray;
    FEventLock: TCriticalSection;
    FTransmitterLock: TCriticalSection;
    FTransmitters: TLKEventTransmitterArray;

    procedure ClearEvents;
    procedure LockEvents;
    procedure UnlockEvents;

    procedure AddTransmitter(const ATransmitter: TLKEventTransmitterBase);
    procedure DeleteTransmitter(const ATransmitter: TLKEventTransmitterBase);
    procedure LockTransmitters;
    procedure UnlockTransmitters;
  protected
    function GetDefaultYieldAccumulatedTime: Boolean; override;
    function GetInitialThreadState: TLKThreadState; override;
    procedure PreTick(const ADelta, AStartTime: Double); override;
    procedure Tick(const ADelta, AStartTime: Double); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddEvent(const AEvent: TLKEvent);
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
    function GetInitialThreadState: TLKThreadState; override;
    procedure ProcessEvents(const ADelta, AStartTime: Double); override; final;
    procedure Tick(const ADelta, AStartTime: Double); override;
    //
    procedure RecordEvent(const AEvent: TLKEvent); virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Subscribe;
    procedure Unsubscribe;

    procedure AddEvent(const AEvent: TLKEvent); override;
  end;

implementation

uses
  LKSL.Streams.System,
  LKSL.Events.Streams;

type
  { Forward Declarations }
  TLKEventQueue = class;
  TLKEventStack = class;
  TLKEventEngine = class;

  {
    TLKEventQueue
      - Processes Events "first in, first out"
  }
  TLKEventQueue = class(TLKEventThreadBaseWithListeners)
  protected
    function GetInitialThreadState: TLKThreadState; override;
    procedure ProcessEvents(const ADelta, AStartTime: Double); override; final;
    procedure Tick(const ADelta, AStartTime: Double); override;
  public
    procedure AddEvent(const AEvent: TLKEvent); override;
  end;

  {
    TLKEventStack
      - Processes Events "last in, first out"
  }
  TLKEventStack = class(TLKEventThreadBaseWithListeners)
  protected
    function GetInitialThreadState: TLKThreadState; override;
    procedure ProcessEvents(const ADelta, AStartTime: Double); override; final;
    procedure Tick(const ADelta, AStartTime: Double); override;
  public
    procedure AddEvent(const AEvent: TLKEvent); override;
  end;

  {
    TLKEventEngine
      - Heart and soul of the Event Engine.
  }
  TLKEventEngine = class(TLKPersistent)
  private
    FEventThreadLock: TCriticalSection;
    FEventThreads: TLKEventThreadArray;
    FQueue: TLKEventThreadBaseWithListeners;
    FStack: TLKEventThreadBaseWithListeners;
    FTransmitters: TLKEventTransmitterManager;
    FRecorderLock: TCriticalSection;
    FRecorders: TLKEventRecorderArray;

    procedure AddEvent(const AEvent: TLKEvent; const AProcessingThread: TLKEventThreadBaseWithListeners);
    // "QueueInThreads" iterates through all Event Threads and (if there's a relevant
    // Listener for the Event Type) adds the Event to the Thread's internal Event Queue
    procedure QueueInThreads(const AEvent: TLKEvent);
    // "QueueInRecorders" iterates through all Event Recorders and adds the Event to them
    procedure QueueInRecorders(const AEvent: TLKEvent);
    // "QueueEvent" adds an Event to the Processing Queue (first in, first out)
    procedure QueueEvent(const AEvent: TLKEvent); inline;
    // "StackEvent" adds an Event to the Processing Stack (last in, first out)
    procedure StackEvent(const AEvent: TLKEvent); inline;
    // "TransmitEvent" passes an Event along to the Transmitters WITHOUT broadcasting it internally
    procedure TransmitEvent(const AEvent: TLKEvent);

    procedure LockThreads; inline;
    procedure UnlockThreads; inline;

    procedure RegisterEventThread(const AEventThread: TLKEventThread);
    procedure UnregisterEventThread(const AEventThread: TLKEventThread);

    procedure LockRecorders; inline;
    procedure UnlockRecorders; inline;

    procedure RegisterRecorder(const ARecorder: TLKEventRecorder);
    procedure UnregisterRecorder(const ARecorder: TLKEventRecorder);

    procedure RegisterListener(const AListener: TLKEventListener);
    function UnregisterListener(const AListener: TLKEventListener): Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  Events: TLKEventEngine;

{ TLKEvent }

procedure TLKEvent.Assign(AFromEvent: TPersistent);
begin
  if AFromEvent is TLKEvent then
  begin
    TLKEvent(AFromEvent).Lock;
    Lock;
    try
      FAllowRecording := TLKEvent(AFromEvent).FAllowRecording;
      FAllowTransmit := TLKEvent(AFromEvent).FAllowTransmit;
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

class procedure TLKEvent.RemoveFromStream(const AStream: TStream);
begin
  inherited;
  StreamDeleteBoolean(AStream); // Delete FAllowRecording
  StreamDeleteBoolean(AStream); // Delete FIsReplay
  StreamDeleteDouble(AStream); // Delete FDelta
  StreamDeleteDouble(AStream); // Delete FDispatchTime
  StreamDeleteDouble(AStream); // Delete FExpiresAfter
  StreamDeleteTLKEventDispatchMethod(AStream); // Delete FDispatchMethod
  StreamDeleteDouble(AStream); // Delete FProcessedTime
  RemoveEventFromStream(AStream);
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

function TLKEvent.GetDefaultExpiresAfter: Double;
begin
  Result := 0.00;
end;

function TLKEvent.GetDefaultAllowTransmit: Boolean;
begin
  Result := False; // We don't intend to transmit Event Types to other process by default
end;

function TLKEvent.GetDelta: Double;
begin
  Lock;
  try
    Result := FDelta;
  finally
    Unlock;
  end;
end;

function TLKEvent.GetDispatchTime: Double;
begin
  Lock;
  try
    Result := FDispatchTime;
  finally
    Unlock;
  end;
end;

function TLKEvent.GetExpiresAfter: Double;
begin
  Lock;
  try
    Result := FExpiresAfter;
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

function TLKEvent.GetProcessedTime: Double;
begin
  Lock;
  try
    Result := FProcessedTime;
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

procedure TLKEvent.InsertIntoStream(const AStream: TStream);
begin
  inherited;
  StreamInsertBoolean(AStream, FAllowRecording); // Insert FAllowRecording
  StreamInsertBoolean(AStream, FIsReplay); // Insert FIsReplay
  StreamInsertDouble(AStream, FDelta); // Insert FDelta
  StreamInsertDouble(AStream, FDispatchTime); // Insert FDispatchTime
  StreamInsertDouble(AStream, FExpiresAfter); // Insert FExpiresAfter
  StreamInsertTLKEventDispatchMethod(AStream, FDispatchMethod); // Insert FDispatchMethod
  StreamInsertDouble(AStream, FProcessedTime); // Insert FProcessedTime
  InsertEventIntoStream(AStream);
end;

procedure TLKEvent.Queue;
begin
  if (edmQueue in FDispatchModes) then
    Events.QueueEvent(Self)
  else if (edmStack in FDispatchModes) then
    Events.StackEvent(Self);
end;

procedure TLKEvent.ReadFromStream(const AStream: TStream);
begin
  inherited;
  FAllowRecording := StreamReadBoolean(AStream); // read FAllowRecording
  FIsReplay := StreamReadBoolean(AStream); // read FIsReplay
  FDelta := StreamReadDouble(AStream); // Read FDelta
  FDispatchTime := StreamReadDouble(AStream); // Read FDispatchTime
  FExpiresAfter := StreamReadDouble(AStream); // Read FExpiresAfter
  FDispatchMethod := StreamReadTLKEventDispatchMethod(AStream); // Read FDispatchMethod
  FProcessedTime := StreamReadDouble(AStream); // Read FProcessedTime
  ReadEventFromStream(AStream);
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

procedure TLKEvent.SetExpiresAfter(const AExpiresAfter: Double);
begin
  Lock;
  try
    FExpiresAfter := AExpiresAfter;
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

procedure TLKEvent.TransmitOnly;
begin
  Events.TransmitEvent(Self);
end;

procedure TLKEvent.WriteToStream(const AStream: TStream);
begin
  inherited;
  StreamWriteBoolean(AStream, FAllowRecording); // Append FAllowRecording
  StreamWriteBoolean(AStream, FIsReplay); // Append FIsReplay
  StreamWriteDouble(AStream, FDelta); // Append FDelta
  StreamWriteDouble(AStream, FDispatchTime); // Append FDispatchTime
  StreamWriteDouble(AStream, FExpiresAfter); // Append FExpiresAfter
  StreamInsertTLKEventDispatchMethod(AStream, FDispatchMethod); // Append FDispatchMethod
  StreamWriteDouble(AStream, FProcessedTime); // Append FProcessedTime
  WriteEventToStream(AStream);
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

function TLKEventListener.GetDefaultExpireAfter: Double;
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

function TLKEventListener.GetExpireAfter: Double;
begin
  Lock;
  try
    Result := FExpireAfter;
  finally
    Unlock;
  end;
end;

function TLKEventListener.GetLastEventTime: Double;
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

procedure TLKEventListener.SetExpireAfter(const AExpireAfter: Double);
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
  FEventThread := AEventThread;
  FEventType := AEventType;
  FEventThread.AddEventListenerGroup(Self);
end;

destructor TLKEventListenerGroup.Destroy;
begin
  FEventThread.DeleteEventListenerGroup(Self);
  inherited;
end;

procedure TLKEventListenerGroup.ProcessEvent(const AEvent: TLKEvent);
var
  I: Integer;
begin
  Lock;
  try
    for I := Low(FListeners) to High(FListeners) do
    begin
      if AEvent.GetTypeGUID = FListeners[I].GetEventType.GetTypeGUID then
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
  finally
    Unlock;
  end;
end;

procedure TLKEventListenerGroup.RegisterListener(const AListener: TLKEventListener);
var
  LIndex: Integer;
begin
  Lock;
  try
    LIndex := Length(FListeners);
    SetLength(FListeners, LIndex + 1);
    FListeners[LIndex] := AListener;
    FListeners[LIndex].FIndex := LIndex;
  finally
    Unlock;
  end;
end;

procedure TLKEventListenerGroup.UnregisterListener(const AListener: TLKEventListener);
var
  I: Integer;
begin
  Lock;
  try
    for I := AListener.FIndex to Length(FListeners) - 2 do
    begin
      FListeners[I] := FListeners[I + 1];
      FListeners[I].FIndex := I;
    end;
    SetLength(FListeners, Length(FListeners) - 1);
    if Length(FListeners) = 0 then
      Free;
  finally
    Unlock;
  end;
end;

{ TLKEventThreadBase }

procedure TLKEventThreadBase.AddEvent(const AEvent: TLKEvent);
var
  LIndex: Integer;
begin
  LockEvents;
  try
    AEvent.FDispatchTime := GetReferenceTime;
    LIndex := Length(FEvents);
    SetLength(FEvents, LIndex + 1);
    FEvents[LIndex] := AEvent;
  finally
    UnlockEvents;
  end;
end;

procedure TLKEventThreadBase.ClearEvents;
var
  I: Integer;
begin
  LockEvents;
  try
    for I := High(FEvents) downto Low(FEvents) do
      FEvents[I].Free;
  finally
    UnlockEvents;
  end;
end;

constructor TLKEventThreadBase.Create;
begin
  inherited;
  FEventLock := TCriticalSection.Create;
  FProcessMode := epmAll;
end;

destructor TLKEventThreadBase.Destroy;
begin
  ClearEvents;
  FEventLock.Free;
  inherited;
end;

function TLKEventThreadBase.GetProcessMode: TLKEventProcessMode;
begin
  Lock;
  try
    Result := FProcessMode;
  finally
    Unlock;
  end;
end;

procedure TLKEventThreadBase.LockEvents;
begin
  FEventLock.Acquire;
end;

procedure TLKEventThreadBase.RemoveEvent(const AIndex: Integer);
var
  I: Integer;
begin
  LockEvents;
  try
    for I := AIndex to High(FEvents) - 1 do
      FEvents[I] := FEvents[I + 1];
    SetLength(FEvents, Length(FEvents) - 1);
  finally
    UnlockEvents;
  end;
end;

procedure TLKEventThreadBase.SetProcessMode(const AProcessMode: TLKEventProcessMode);
begin
  Lock;
  try
    FProcessMode := AProcessMode;
  finally
    Unlock;
  end;
end;

procedure TLKEventThreadBase.UnlockEvents;
begin
  FEventLock.Release;
end;

{ TLKEventThreadBaseWithListeners }

procedure TLKEventThreadBaseWithListeners.AddEvent(const AEvent: TLKEvent);
begin
  if GetEventListenerGroup(TLKEventType(AEvent.ClassType)) = nil then
    AEvent.Free
  else
    inherited;
end;

procedure TLKEventThreadBaseWithListeners.AddEventListenerGroup(const AEventListenerGroup: TLKEventListenerGroup);
begin
  FEventListenerGroups.Lock;
  try
    if not (FEventListenerGroups.ContainsKey(AEventListenerGroup.EventType.GetTypeGUID)) then
      FEventListenerGroups.Add(AEventListenerGroup.EventType.GetTypeGUID, AEventListenerGroup);
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
  FEventListenerGroups := TLKDictionary<TGUID, TLKEventListenerGroup>.Create;
end;

procedure TLKEventThreadBaseWithListeners.DeleteEventListenerGroup(const AEventListenerGroup: TLKEventListenerGroup);
begin
  FEventListenerGroups.Lock;
  try
    FEventListenerGroups.Remove(AEventListenerGroup.EventType.GetTypeGUID);
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
    if not (FEventListenerGroups.TryGetValue(AEventType.GetTypeGUID, Result)) then
      Result := nil
  finally
    FEventListenerGroups.Unlock;
  end;
end;

procedure TLKEventThreadBaseWithListeners.ProcessListeners(const AEvent: TLKEvent; const ADelta, AStartTime: Double);
var
  LEventListenerGroup: TLKEventListenerGroup;
begin
  AEvent.FDelta := ADelta;
  AEvent.FProcessedTime := AStartTime;
//  FEventListenerGroups.Lock;
//  try
  LEventListenerGroup := GetEventListenerGroup(TLKEventType(AEvent.ClassType));
  if LEventListenerGroup <> nil then
  begin
    if (((AEvent.ExpiresAfter > 0.00) and (GetReferenceTime - AEvent.DispatchTime < AEvent.ExpiresAfter)) or (AEvent.ExpiresAfter <= 0.00)) then
      LEventListenerGroup.ProcessEvent(AEvent);
  end;
//  finally
//    FEventListenerGroups.Unlock;
//  end;
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

procedure TLKEventThread.PreTick(const ADelta, AStartTime: Double);
begin
  ProcessEvents(ADelta, AStartTime);
end;

procedure TLKEventThread.ProcessEvents(const ADelta, AStartTime: Double);
var
  I, LEnd: Integer;
begin
  // We don't Lock the Event Array at this point, as doing so would prevent additional
  // events from being added to the Queue (and could cause a Thread to freeze)
  if Length(FEvents) > 0 then
  begin
    case FProcessMode of
      epmOneByOne: begin
                     ProcessListeners(FEvents[0], ADelta, AStartTime); // Process the first Event in the Queue
                     FEvents[0].Free; // Destroy the Event
                     RemoveEvent(0); // Rebalance the Event Array
                   end;
      epmAll: begin
                // Set the High Range to the number of Events in  the Array right NOW.
                LEnd := High(FEvents);
                // Iterate the Events up to the High Range and Process
                for I := 0 to LEnd do
                begin
                  ProcessListeners(FEvents[I], ADelta, AStartTime);
                  FEvents[I].Free;
                end;
                // Shift the positions of any new Events in the Array
                LockEvents;
                try
                  for I := 0 to High(FEvents) - LEnd do
                    FEvents[I] := FEvents[I + 1 + LEnd];
                  SetLength(FEvents, Length(FEvents) - (LEnd + 1));
                finally
                  UnlockEvents;
                end;
              end;
    end;
  end;
end;

procedure TLKEventThread.Tick(const ADelta, AStartTime: Double);
begin
  // Do nothing (this just immutes the procedure, because you may not want a looping process in the Thread
end;

{ TLKEventTransmitterBase }

function TLKEventTransmitterBase.AcceptEvent(const AEvent: TLKEvent): Boolean;
begin
  Lock;
  try
    Result := ((FUseEventTypeList) and (EventTypeValid(TLKEventType(AEvent)) > -1)) or (not FUseEventTypeList);
  finally
    Unlock;
  end;
end;

procedure TLKEventTransmitterBase.AddValidEventType(const AEventType: TLKEventType);
var
  LIndex: Integer;
begin
  Lock;
  try
    if EventTypeValid(AEventType) = -1 then
    begin
      LIndex := Length(FValidEventTypes);
      SetLength(FValidEventTypes, LIndex + 1);
      FValidEventTypes[LIndex] := AEventType;
    end;
  finally
    Unlock;
  end;
end;

constructor TLKEventTransmitterBase.Create;
begin
  inherited;
  Events.FTransmitters.AddTransmitter(Self);
  FUseEventTypeList := False; // We don't want to limit outgoing Events by default
end;

destructor TLKEventTransmitterBase.Destroy;
begin
  Events.FTransmitters.DeleteTransmitter(Self);
  SetLength(FValidEventTypes, 0);
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
var
  I, LIndex: Integer;
begin
  Lock;
  try
    LIndex := EventTypeValid(AEventType);
    if LIndex > -1 then
    begin
      for I := LIndex to High(FValidEventTypes) - 1 do
        FValidEventTypes[I] := FValidEventTypes[I + 1];
      SetLength(FValidEventTypes, Length(FValidEventTypes) - 1);
    end;
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

function TLKEventTransmitterBase.EventTypeValid(const AEventType: TLKEventType): Integer;
var
  I: Integer;
begin
  Result := -1;
  Lock;
  try
    for I := Low(FValidEventTypes) to High(FValidEventTypes) do
      if FValidEventTypes[I] = AEventType then
      begin
        Result := I;
        Break;
      end;
  finally
    Unlock;
  end;
end;

{ TLKEventTransmitterManager }

procedure TLKEventTransmitterManager.AddEvent(const AEvent: TLKEvent);
var
  LTransmitterExists: Boolean;
  LIndex: Integer;
  LClone: TLKEvent;
begin
  // Check if there actually are any Transmitters first
  LockTransmitters;
  try
    LTransmitterExists := Length(FTransmitters) > 0;
  finally
    UnlockTransmitters;
  end;
  if LTransmitterExists then
  begin
    // Clone Event
    LClone := TLKEventType(AEvent.ClassType).Create;
    LClone.Assign(AEvent);
    // Add Clone to Event Queue
    LockEvents;
    try
      LIndex := Length(FEvents);
      SetLength(FEvents, LIndex + 1);
      FEvents[LIndex] := LClone;
    finally
      UnlockEvents;
    end;
  end;
end;

procedure TLKEventTransmitterManager.AddTransmitter(const ATransmitter: TLKEventTransmitterBase);
var
  LIndex: Integer;
begin
  LockTransmitters;
  try
    LIndex := Length(FTransmitters);
    SetLength(FTransmitters, LIndex + 1);
    FTransmitters[LIndex] := ATransmitter;
  finally
    UnlockTransmitters;
  end;
end;

procedure TLKEventTransmitterManager.ClearEvents;
var
  LEvent: TLKEvent;
begin
  LockEvents;
  try
    for LEvent in FEvents do
      LEvent.Free;

    SetLength(FEvents, 0);
  finally
    UnlockEvents;
  end;
end;

constructor TLKEventTransmitterManager.Create;
begin
  inherited;
  FTransmitterLock := TCriticalSection.Create;
  FEventLock := TCriticalSection.Create;
end;

procedure TLKEventTransmitterManager.DeleteTransmitter(const ATransmitter: TLKEventTransmitterBase);
var
  I: Integer;
begin
  LockTransmitters;
  try
    for I := ATransmitter.FIndex to High(FTransmitters) - 1 do
    begin
      FTransmitters[I] := FTransmitters[I + 1];
      FTransmitters[I].FIndex := I;
    end;
    SetLength(FTransmitters, Length(FTransmitters) - 1);
  finally
    UnlockTransmitters;
  end;
end;

destructor TLKEventTransmitterManager.Destroy;
begin
  SetLength(FTransmitters, 0);
  ClearEvents;
  FTransmitterLock.Free;
  FEventLock.Free;
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

procedure TLKEventTransmitterManager.LockEvents;
begin
  FEventLock.Acquire;
end;

procedure TLKEventTransmitterManager.LockTransmitters;
begin
  FTransmitterLock.Acquire;
end;

procedure TLKEventTransmitterManager.PreTick(const ADelta, AStartTime: Double);
var
  I: Integer;
  LEventStream: TMemoryStream;
begin
  if Length(FEvents) > 0 then
  begin
    LEventStream := TMemoryStream.Create;
    try
      // Since we're NOW Streaming the Event, we don't want an endless loop where Program A streams to
      // Program B, then Program B instantly streams back to Program A.... that would suck!
      FEvents[0].FAllowTransmit := False;
      FEvents[0].WriteToStream(LEventStream); // Serialize the Event into the Event Stream
      LockTransmitters;
      try
        for I := Low(FTransmitters) to High(FTransmitters) do // Iterate the Transmitters
        begin
          if FTransmitters[I].AcceptEvent(FEvents[0]) then // Check if the Transmitter cares for this Event
            FTransmitters[I].TransmitEvent(FEvents[0], LEventStream); // If so, Transmit it!
        end;
      finally
        UnlockTransmitters;
      end;
      FEvents[0].Free; // Destroy the Event
      // Remove the processed Event from the Event Array
      LockEvents;
      try
        for I := Low(FEvents) to High(FEvents) - 1 do
          FEvents[I] := FEvents[I + 1];
        SetLength(FEvents, Length(FEvents) - 1);
      finally
        UnlockEvents;
      end;
    finally
      LEventStream.Free;
    end;
  end;
end;

procedure TLKEventTransmitterManager.Tick(const ADelta, AStartTime: Double);
begin
  // Do Nothing (Yet, but probably never will either)
end;

procedure TLKEventTransmitterManager.UnlockEvents;
begin
  FEventLock.Release;
end;

procedure TLKEventTransmitterManager.UnlockTransmitters;
begin
  FTransmitterLock.Release;
end;

{ TLKEventRecorder }

procedure TLKEventRecorder.AddEvent(const AEvent: TLKEvent);
begin
  inherited;
  ThreadState := tsRunning;
end;

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

procedure TLKEventRecorder.ProcessEvents(const ADelta, AStartTime: Double);
var
  I, LEnd: Integer;
begin
  // We don't Lock the Event Array at this point, as doing so would prevent additional
  // events from being added to the Queue (and could cause a Thread to freeze)
  if Length(FEvents) > 0 then
  begin
    case FProcessMode of
      epmOneByOne: begin
                     FEvents[0].FDelta := ADelta;
                     FEvents[0].FProcessedTime := AStartTime;
                     RecordEvent(FEvents[0]); // Process the first Event in the Queue
                     FEvents[0].Free; // Destroy the Event
                     RemoveEvent(0); // Rebalance the Event Array
                   end;
      epmAll: begin
                // Set the High Range to the number of Events in  the Array right NOW.
                LEnd := High(FEvents);
                // Iterate the Events up to the High Range and Process
                for I := 0 to LEnd do
                begin
                  FEvents[I].FDelta := ADelta;
                  FEvents[I].FProcessedTime := AStartTime;
                  RecordEvent(FEvents[I]);
                  FEvents[I].Free;
                end;
                // Shift the positions of any new Events in the Array
                LockEvents;
                try
                  for I := 0 to High(FEvents) - LEnd do
                    FEvents[I] := FEvents[I + 1 + LEnd];
                  SetLength(FEvents, Length(FEvents) - (LEnd + 1));
                finally
                  UnlockEvents;
                end;
              end;
    end;
  end;
end;

procedure TLKEventRecorder.Subscribe;
begin
  if FIndex = -1 then
    Events.RegisterRecorder(Self);
end;

procedure TLKEventRecorder.Tick(const ADelta, AStartTime: Double);
begin
  ProcessEvents(ADelta, AStartTime);
  LockEvents;
  try
    if Length(FEvents) = 0 then
      ThreadState := tsPaused;
  finally
    UnlockEvents;
  end;
end;

procedure TLKEventRecorder.Unsubscribe;
begin
  if FIndex <> -1 then
    Events.UnregisterRecorder(Self);
end;

{ TLKEventQueue }

procedure TLKEventQueue.AddEvent(const AEvent: TLKEvent);
begin
  inherited;
  ThreadState := tsRunning;
end;

function TLKEventQueue.GetInitialThreadState: TLKThreadState;
begin
  Result := tsPaused;
end;

procedure TLKEventQueue.ProcessEvents(const ADelta, AStartTime: Double);
var
  I, LEnd: Integer;
begin
  // We don't Lock the Event Array at this point, as doing so would prevent additional
  // events from being added to the Queue (and could cause a Thread to freeze)
  if Length(FEvents) > 0 then
  begin
    case FProcessMode of
      epmOneByOne: begin
                     ProcessListeners(FEvents[0], ADelta, AStartTime); // Process the first Event in the Queue
                     FEvents[0].Free; // Destroy the Event
                     RemoveEvent(0); // Rebalance the Event Array
                   end;
      epmAll: begin
                // Set the High Range to the number of Events in  the Array right NOW.
                LEnd := High(FEvents);
                // Iterate the Events up to the High Range and Process
                for I := 0 to LEnd do
                begin
                  ProcessListeners(FEvents[I], ADelta, AStartTime);
                  FEvents[I].Free;
                end;
                // Shift the positions of any new Events in the Array
                LockEvents;
                try
                  for I := 0 to High(FEvents) - LEnd do
                    FEvents[I] := FEvents[I + 1 + LEnd];
                  SetLength(FEvents, Length(FEvents) - (LEnd + 1));
                finally
                  UnlockEvents;
                end;
              end;
    end;
  end;
end;

procedure TLKEventQueue.Tick(const ADelta, AStartTime: Double);
begin
  ProcessEvents(ADelta, AStartTime);
  LockEvents;
  try
    if Length(FEvents) = 0 then
      ThreadState := tsPaused;
  finally
    UnlockEvents;
  end;
end;

{ TLKEventStack }

procedure TLKEventStack.AddEvent(const AEvent: TLKEvent);
begin
  inherited;
  ThreadState := tsRunning;
end;

function TLKEventStack.GetInitialThreadState: TLKThreadState;
begin
  Result := tsPaused;
end;

procedure TLKEventStack.ProcessEvents(const ADelta, AStartTime: Double);
//var
//  LEvent: Integer;
var
  I, LEnd: Integer;
begin
  // We don't Lock the Event Array at this point, as doing so would prevent additional
  // events from being added to the Queue (and could cause a Thread to freeze)
  if Length(FEvents) > 0 then
  begin
    case FProcessMode of
      epmOneByOne: begin
                     LEnd := High(FEvents);
                     ProcessListeners(FEvents[LEnd], ADelta, AStartTime); // Process the first Event in the Queue
                     FEvents[LEnd].Free; // Destroy the Event
                     RemoveEvent(LEnd); // Rebalance the Event Array
                   end;
      epmAll: begin
                // Set the High Range to the number of Events in  the Array right NOW.
                LEnd := High(FEvents);
                // Iterate the Events up to the High Range and Process
                for I := LEnd downto 0 do
                begin
                  ProcessListeners(FEvents[I], ADelta, AStartTime);
                  FEvents[I].Free;
                end;
                // Shift the positions of any new Events in the Array
                LockEvents;
                try
                  for I := 0 to High(FEvents) - LEnd do
                    FEvents[I] := FEvents[I + 1 + LEnd];
                  SetLength(FEvents, Length(FEvents) - (LEnd + 1));
                finally
                  UnlockEvents;
                end;
              end;
    end;
  end;
end;

procedure TLKEventStack.Tick(const ADelta, AStartTime: Double);
begin
  ProcessEvents(ADelta, AStartTime);
  LockEvents;
  try
    if Length(FEvents) = 0 then
      ThreadState := tsPaused;
  finally
    UnlockEvents;
  end;
end;

{ TLKEventEngine }

procedure TLKEventEngine.AddEvent(const AEvent: TLKEvent; const AProcessingThread: TLKEventThreadBaseWithListeners);
begin
  if (AEvent.AllowRecording) and (not AEvent.IsReplay) then
    QueueInRecorders(AEvent);
  QueueInThreads(AEvent);
  if AEvent.AllowTransmit then
    FTransmitters.AddEvent(AEvent);
  AProcessingThread.AddEvent(AEvent);
end;

constructor TLKEventEngine.Create;
begin
  inherited;
  FEventThreadLock := TCriticalSection.Create;
  FQueue := TLKEventQueue.Create;
  FStack := TLKEventStack.Create;
  FTransmitters := TLKEventTransmitterManager.Create;
  FRecorderLock := TCriticalSection.Create;
end;

destructor TLKEventEngine.Destroy;
var
  I: Integer;
begin
  FTransmitters.Kill;
  for I := High(FEventThreads) downto Low(FEventThreads) do
    UnregisterEventThread(FEventThreads[I]);
  FEventThreadLock.Free;
  for I := High(FRecorders) downto Low(FRecorders) do
    FRecorders[I].Unsubscribe;
  FRecorderLock.Free;
  FQueue.Kill;
  FStack.Kill;
  inherited;
end;

procedure TLKEventEngine.LockRecorders;
begin
  FRecorderLock.Acquire;
end;

procedure TLKEventEngine.LockThreads;
begin
  FEventThreadLock.Acquire;
end;

procedure TLKEventEngine.QueueEvent(const AEvent: TLKEvent);
begin
  AEvent.FDispatchMethod := edQueue;
  AddEvent(AEvent, FQueue);
end;

procedure TLKEventEngine.QueueInRecorders(const AEvent: TLKEvent);
var
  I: Integer;
  LClone: TLKEvent;
begin
  LockRecorders;
  try
    for I := Low(FRecorders) to High(FRecorders) do
    begin
      LClone := TLKEventType(AEvent.ClassType).Create; // Create a blank instance of the Event for the Clone
      LClone.Assign(AEvent); // Copy the original data into the Clone
      FRecorders[I].AddEvent(LClone); // Send a CLONE of the Event to the Recorder!
    end;
  finally
    UnlockRecorders;
  end;
end;

procedure TLKEventEngine.QueueInThreads(const AEvent: TLKEvent);
var
  I: Integer;
  LClone: TLKEvent;
begin
  LockThreads;
  try
    for I := Low(FEventThreads) to High(FEventThreads) do
    begin
      LClone := TLKEventType(AEvent.ClassType).Create; // Create a blank instance of the Event for the Clone
      LClone.Assign(AEvent); // Copy the original data into the Clone
      FEventThreads[I].AddEvent(LClone); // Send a CLONE of the Event to the Thread!
    end;
  finally
    UnlockThreads;
  end;
end;

procedure TLKEventEngine.RegisterEventThread(const AEventThread: TLKEventThread);
var
  LIndex: Integer;
begin
  LockThreads;
  try
    LIndex := Length(FEventThreads);
    SetLength(FEventThreads, LIndex + 1);
    FEventThreads[LIndex] := AEventThread;
    AEventThread.FIndex := LIndex;
  finally
    UnlockThreads;
  end;
end;

procedure TLKEventEngine.RegisterListener(const AListener: TLKEventListener);
begin
  FStack.RegisterListener(AListener);
  FQueue.RegisterListener(AListener);
end;

procedure TLKEventEngine.RegisterRecorder(const ARecorder: TLKEventRecorder);
var
  LIndex: Integer;
begin
  LockRecorders;
  try
    LIndex := Length(FRecorders);
    SetLength(FRecorders, LIndex + 1);
    FRecorders[LIndex] := ARecorder;
    ARecorder.FIndex := LIndex;
  finally
    UnlockRecorders;
  end;
end;

procedure TLKEventEngine.StackEvent(const AEvent: TLKEvent);
begin
  AEvent.FDispatchMethod := edStack;
  AddEvent(AEvent, FStack);
end;

procedure TLKEventEngine.TransmitEvent(const AEvent: TLKEvent);
begin
  FTransmitters.AddEvent(AEvent);
  AEvent.Free;
end;

procedure TLKEventEngine.UnlockRecorders;
begin
  FRecorderLock.Release;
end;

procedure TLKEventEngine.UnlockThreads;
begin
  FEventThreadLock.Release;
end;

procedure TLKEventEngine.UnregisterEventThread(const AEventThread: TLKEventThread);
var
  I: Integer;
begin
  LockThreads;
  try
    for I := AEventThread.FIndex to High(FEventThreads) - 1 do
    begin
      FEventThreads[I] := FEventThreads[I + 1];
      FEventThreads[I].FIndex := I;
    end;
    SetLength(FEventThreads, Length(FEventThreads) - 1);
  finally
    UnlockThreads;
  end;
end;

function TLKEventEngine.UnregisterListener(const AListener: TLKEventListener): Integer;
begin
  FStack.UnregisterListener(AListener);
  FQueue.UnregisterListener(AListener);
  Result := -1;
end;

procedure TLKEventEngine.UnregisterRecorder(const ARecorder: TLKEventRecorder);
begin

end;

initialization
  Events := TLKEventEngine.Create;
finalization
  Events.Free;

end.
