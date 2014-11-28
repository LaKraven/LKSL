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
  System.Classes, System.SysUtils, System.SyncObjs,
  {$IFDEF LKSL_USE_GENERICS}Generics.Collections,{$ENDIF LKSL_USE_GENERICS}
  LKSL.Common.Types,
  LKSL.Threads.Base,
  LKSL.Streamables.Base;

type
  { Forward Declarations }
  TLKEvent = class;
  TLKEventListener = class;
  TLKEventListenerGroup = class;
  TLKEventThreadBase = class;
  TLKEventThread = class;
  TLKEventTransmitterBase = class;
  TLKEventTransmitterManager = class;
  TLKEventReceiverBase = class;

  { Enum Types }
  TLKEventDispatchMode = (edmQueueOrStack, edmQueueOnly, edmStackOnly);
  TLKEventDispatchMethod = (edQueue, edStack);
  TLKEventProcessMode = (epmOneByOne, epmAll);

  { Class References }
  TLKEventType = class of TLKEvent;
  TLKEventListenerType = class of TLKEventListener;

  { Array Types }
  TLKEventArray = Array of TLKEvent;
  TLKEventTypeArray = Array of TLKEventType;
  TLKEventListenerArray = Array of TLKEventListener;
  TLKEventListenerGroupArray = Array of TLKEventListenerGroup;
  TLKEventThreadArray = Array of TLKEventThread;
  TLKEventTransmitterArray = Array of TLKEventTransmitterBase;
  TLKEventReceiverArray = Array of TLKEventReceiverBase;

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
    FDelta: Double;
    FDispatchMode: TLKEventDispatchMode;
    FDispatchMethod: TLKEventDispatchMethod;
    FDispatchTime: Double;
    FExpiresAfter: Double;
    FProcessedTime: Double;
    FStreamEvent: Boolean;

    function GetDelta: Double;
    function GetDispatchMethod: TLKEventDispatchMethod;
    function GetDispatchTime: Double;
    function GetExpiresAfter: Double;
    function GetProcessedTime: Double;
    function GetStreamEvent: Boolean;

    procedure SetExpiresAfter(const AExpiresAfter: Double);
    procedure SetStreamEvent(const AStreamEvent: Boolean);
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
    // Override "GetDefaultExpiresAfter" if you want your Event to Expire after a certain amount of time
    // (measured in seconds). By default, it returns 0.00 to indiciate that the Event should NEVER expire.
    function GetDefaultExpiresAfter: Double; virtual;
    // Override "GetDefaultStreamEvent" if you want the Event to be passed over to your Event Streaming
    // system. Your Event Streaming system should then decide where this Event should be Streamed to.
    // By default, it returns "False" indicating that this Event isn't intended to be Streamed to
    // another process.
    function GetDefaultStreamEvent: Boolean; virtual;
    // Override "GetDispatchMode" if you want the Event to restrict the way your Event Type is dispatched.
    // Available modes are: edmQueueOrStack, edmQueueOnly, edmStackOnly.
    // Default = edmQueueOrStack
    function GetDispatchMode: TLKEventDispatchMode; virtual;
  public
    constructor Create; override;

    procedure Assign(AFromEvent: TPersistent); override;

    procedure Queue; // Add this Event to the Event Queue
    procedure Stack; // Add this Event to the Event Stack
    procedure TransmitOnly; // DOESN'T Queue OR Stack the Event for internal processing, just Transmits it

    property Delta: Double read GetDelta;
    property DispatchMethod: TLKEventDispatchMethod read GetDispatchMethod;
    property DispatchTime: Double read GetDispatchTime;
    property ExpiresAfter: Double read GetExpiresAfter write SetExpiresAfter;
    property ProcessedTime: Double read GetProcessedTime;
    property StreamEvent: Boolean read GetStreamEvent write SetStreamEvent;
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

  {$IFDEF LKSL_USE_GENERICS}
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
  {$ENDIF LKSL_USE_GENERICS}

  {
    TLKEventListenerGroup
      - Pairs multiple Listeners with a single Event Type
      - This is to speed up the Processing of Events by ensuring that an Event is relevant at the point
        of Queuing/Stacking
  }
  TLKEventListenerGroup = class(TLKPersistent)
  private
    FEventThread: TLKEventThreadBase;
    FEventType: TLKEventType;
    {$IFNDEF LKSL_USE_GENERICS}
      FIndex: Integer;
    {$ENDIF LKSL_USE_GENERICS}
    FListeners: TLKEventListenerArray;
  public
    constructor Create(const AEventThread: TLKEventThreadBase; const AEventType: TLKEventType); reintroduce;
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
    FEventListenerGroupLock: TCriticalSection;
    {$IFDEF LKSL_USE_GENERICS}
      FEventListenerGroups: TDictionary<TGUID, TLKEventListenerGroup>;
    {$ELSE}
      FEventListenerGroups: TLKEventListenerGroupArray;
    {$ENDIF LKSL_USE_GENERICS}

    FEventLock: TCriticalSection;
    FEvents: TLKEventArray;

    FProcessMode: TLKEventProcessMode;
    function GetProcessMode: TLKEventProcessMode;
    procedure SetProcessMode(const AProcessMode: TLKEventProcessMode);

    procedure AddEventListenerGroup(const AEventListenerGroup: TLKEventListenerGroup);
    procedure ClearEvents;
    procedure ClearEventListenerGroups;
    procedure DeleteEventListenerGroup(const AEventListenerGroup: TLKEventListenerGroup);
    function GetEventListenerGroup(const AEventType: TLKEventType): TLKEventListenerGroup;
    // "ProcessListeners" iterates every Listener paired with the Thread and (assuming
    // the parameters match) executes the Event Call on the Listeners (respectively)
    procedure ProcessListeners(const AEvent: TLKEvent; const ADelta, AStartTime: Double);
    // Once we've executed an Event, "RemoveEvent" is called to manage the Event Array
    procedure RemoveEvent(const AIndex: Integer);
    // We use a separate Lock for Event Listener Groups
    procedure LockEventListenerGroups;
    procedure UnlockEventListenerGroups;
    // We also use a separate Lock for Events
    procedure LockEvents;
    procedure UnlockEvents;
    // Methods to register and unregister Event Listeners
    procedure RegisterListener(const AListener: TLKEventListener);
    function UnregisterListener(const AListener: TLKEventListener): Integer;
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
    TLKEventThread
      - A "High Precision Thread" with an added Event Queue.
      - Each cycle will process Events from the Queue before processing your Tick method.
  }
  TLKEventThread = class abstract(TLKEventThreadBase)
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

implementation

uses
  LKSL.Streams.System,
  LKSL.Events.Streams;

type
  { Forward Declarations }
  TLKEventEngine = class;

  {
    TLKEventQueue
      - Processes Events "first in, first out"
  }
  TLKEventQueue = class(TLKEventThreadBase)
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
  TLKEventStack = class(TLKEventThreadBase)
  protected
    function GetInitialThreadState: TLKThreadState; override;
    procedure ProcessEvents(const ADelta, AStartTime: Double); override; final;
    procedure Tick(const ADelta, AStartTime: Double); override;
  public
    procedure AddEvent(const AEvent: TLKEvent); override;
  end;


  {
    TLKEventHandler
      - Heart and soul of the Event Engine.
  }
  TLKEventEngine = class(TLKPersistent)
  private
    FEventThreadLock: TCriticalSection;
    FEventThreads: TLKEventThreadArray;
    FQueue: TLKEventThreadBase;
    FStack: TLKEventThreadBase;
    FTransmitters: TLKEventTransmitterManager;

    procedure AddEvent(const AEvent: TLKEvent; const AProcessingThread: TLKEventThreadBase);
    // "QueueInThreads" iterates through all Event Threads and (if there's a relevant
    // Listener for the Event Type) adds the Event to the Thread's internal Event Queue
    procedure QueueInThreads(const AEvent: TLKEvent);

    procedure RegisterEventThread(const AEventThread: TLKEventThread);
    procedure UnregisterEventThread(const AEventThread: TLKEventThread);

    procedure RegisterListener(const AListener: TLKEventListener);
    function UnregisterListener(const AListener: TLKEventListener): Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    // "QueueEvent" adds an Event to the Processing Queue (first in, first out)
    procedure QueueEvent(const AEvent: TLKEvent);
    // "StackEvent" adds an Event to the Processing Stack (last in, first out)
    procedure StackEvent(const AEvent: TLKEvent);
    // "TransmitEvent" passes an Event along to the Transmitters WITHOUT broadcasting it internally
    procedure TransmitEvent(const AEvent: TLKEvent);

    procedure LockThreads;
    procedure UnlockThreads;
  end;

var
  Events: TLKEventEngine;

{ TLKEvent }

procedure TLKEvent.Assign(AFromEvent: TPersistent);
begin
  if AFromEvent is TLKEvent then
  begin
    TLKEvent(AFromEvent).Lock;
    try
      FDelta := TLKEvent(AFromEvent).FDelta;
      FDispatchTime := TLKEvent(AFromEvent).FDispatchTime;
      FExpiresAfter := TLKEvent(AFromEvent).FExpiresAfter;
      FDispatchMethod := TLKEvent(AFromEvent).FDispatchMethod;
      FProcessedTime := TLKEvent(AFromEvent).FProcessedTime;
      FStreamEvent := TLKEvent(AFromEvent).FStreamEvent;
      Clone(TLKEvent(AFromEvent));
    finally
      TLKEvent(AFromEvent).Unlock;
    end;
  end else
    inherited;
end;

constructor TLKEvent.Create;
begin
  inherited;
  FDelta := 0.00;
  FDispatchTime := 0.00;
  FExpiresAfter := GetDefaultExpiresAfter;
  FDispatchMethod := edQueue;
  FProcessedTime := 0.00;
  FStreamEvent := GetDefaultStreamEvent;
  FDispatchMode := GetDispatchMode;
end;

class procedure TLKEvent.RemoveFromStream(const AStream: TStream);
begin
  inherited;
  StreamDeleteDouble(AStream); // Delete FDelta
  StreamDeleteDouble(AStream); // Delete FDispatchTime
  StreamDeleteDouble(AStream); // Delete FExpiresAfter
  StreamDeleteTLKEventDispatchMethod(AStream);// Delete FDispatchMethod
  StreamDeleteDouble(AStream); // Delete FProcessedTime
end;

function TLKEvent.GetDispatchMode: TLKEventDispatchMode;
begin
  Result := edmQueueOrStack;
end;

function TLKEvent.GetDefaultExpiresAfter: Double;
begin
  Result := 0.00;
end;

function TLKEvent.GetDefaultStreamEvent: Boolean;
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

function TLKEvent.GetStreamEvent: Boolean;
begin
  Lock;
  try
    Result := FStreamEvent;
  finally
    Unlock;
  end;
end;

procedure TLKEvent.InsertIntoStream(const AStream: TStream);
begin
  inherited;
  StreamInsertDouble(AStream, FDelta); // Insert FDelta
  StreamInsertDouble(AStream, FDispatchTime); // Insert FDispatchTime
  StreamInsertDouble(AStream, FExpiresAfter); // Insert FExpiresAfter
  StreamInsertTLKEventDispatchMethod(AStream, FDispatchMethod);// Delete FDispatchMethod
  StreamInsertDouble(AStream, FProcessedTime); // Insert FProcessedTime
end;

procedure TLKEvent.Queue;
begin
  case FDispatchMode of
    edmQueueOrStack: Events.QueueEvent(Self);
    edmQueueOnly: Events.QueueEvent(Self);
    edmStackOnly: Events.StackEvent(Self);
  end;
end;

procedure TLKEvent.ReadFromStream(const AStream: TStream);
begin
  inherited;
  FDelta := StreamReadDouble(AStream); // Read FDelta
  FDispatchTime := StreamReadDouble(AStream); // Read FDispatchTime
  FExpiresAfter := StreamReadDouble(AStream); // Read FExpiresAfter
  FDispatchMethod := StreamReadTLKEventDispatchMethod(AStream);// Read FDispatchMethod
  FProcessedTime := StreamReadDouble(AStream); // Read FProcessedTime
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

procedure TLKEvent.SetStreamEvent(const AStreamEvent: Boolean);
begin
  Lock;
  try
    FStreamEvent := AStreamEvent;
  finally
    Unlock;
  end;
end;

procedure TLKEvent.Stack;
begin
  case FDispatchMode of
    edmQueueOrStack: Events.StackEvent(Self);
    edmStackOnly: Events.StackEvent(Self);
    edmQueueOnly: Events.QueueEvent(Self);
  end;
end;

procedure TLKEvent.TransmitOnly;
begin
  Events.TransmitEvent(Self);
end;

procedure TLKEvent.WriteToStream(const AStream: TStream);
begin
  inherited;
  StreamWriteDouble(AStream, FDelta); // Insert FDelta
  StreamWriteDouble(AStream, FDispatchTime); // Insert FDispatchTime
  StreamWriteDouble(AStream, FExpiresAfter); // Insert FExpiresAfter
  StreamInsertTLKEventDispatchMethod(AStream, FDispatchMethod);// Delete FDispatchMethod
  StreamWriteDouble(AStream, FProcessedTime); // Insert FProcessedTime
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
{$IFDEF LKSL_USE_GENERICS}
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
{$ENDIF LKSL_USE_GENERICS}
{ TLKEventListenerGroup }

constructor TLKEventListenerGroup.Create(const AEventThread: TLKEventThreadBase; const AEventType: TLKEventType);
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
  LListener: TLKEventListener;
begin
  Lock;
  try
    for LListener in FListeners do
    begin
      if AEvent.GetTypeGUID = LListener.GetEventType.GetTypeGUID then
      begin
        if (((LListener.NewestEventOnly) and (AEvent.DispatchTime > LListener.LastEventTime)) or
           (not LListener.NewestEventOnly)) and (LListener.GetConditionsMatch(AEvent)) and
           (((LListener.ExpireAfter > 0.00) and (GetReferenceTime - AEvent.DispatchTime < LListener.ExpireAfter)) or
           (LListener.ExpireAfter <= 0.00)) then
        begin
          if LListener.CallUIThread then
          begin
            FEventThread.Synchronize(procedure begin
                                        LListener.EventCall(AEvent);
                                      end);
          end else
            LListener.EventCall(AEvent);
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
  if GetEventListenerGroup(TLKEventType(AEvent.ClassType)) = nil then
  begin
    AEvent.Free;
    Exit;
  end;
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

procedure TLKEventThreadBase.AddEventListenerGroup(const AEventListenerGroup: TLKEventListenerGroup);
{$IFNDEF LKSL_USE_GENERICS}
    function GetSortedPosition: Integer;
    var
      LIndex, LLow, LHigh: Integer;
    begin
      Result := 0;
      LLow := 0;
      LHigh := Length(FEventListenerGroups) - 1;
      if LHigh = - 1 then
        Exit;
      if LLow < LHigh then
      begin
        while (LHigh - LLow > 1) do
        begin
          LIndex := (LHigh + LLow) div 2;
          if GUIDToString(AEventListenerGroup.EventType.GetTypeGUID) <= GUIDToString(FEventListenerGroups[LIndex].EventType.GetTypeGUID) then
            LHigh := LIndex
          else
            LLow := LIndex;
        end;
      end;
      if (GUIDToString(FEventListenerGroups[LHigh].EventType.GetTypeGUID) < GUIDToString(AEventListenerGroup.EventType.GetTypeGUID)) then
        Result := LHigh + 1
      else if (GUIDToString(FEventListenerGroups[LLow].EventType.GetTypeGUID) < GUIDToString(AEventListenerGroup.EventType.GetTypeGUID)) then
        Result := LLow + 1
      else
        Result := LLow;
    end;
  var
    LIndex, I: Integer;
{$ENDIF LKSL_USE_GENERICS}
begin
  LockEventListenerGroups;
  try
    {$IFDEF LKSL_USE_GENERICS}
      if not (FEventListenerGroups.ContainsKey(AEventListenerGroup.EventType.GetTypeGUID)) then
        FEventListenerGroups.Add(AEventListenerGroup.EventType.GetTypeGUID, AEventListenerGroup);
    {$ELSE}
      LIndex := GetEventListenerGroup(AEventListenerGroup.EventType).FIndex;
      if LIndex = -1 then
      begin
        LIndex := GetSortedPosition;
        SetLength(FEventListenerGroups, Length(FEventListenerGroups) + 1);
        // Shift items to the RIGHT
        if LIndex < Length(FEventListenerGroups) - 1 then
          for I := Length(FEventListenerGroups) - 1 downto (LIndex + 1) do
          begin
            FEventListenerGroups[I] := FEventListenerGroups[I - 1];
            FEventListenerGroups[I].FIndex := I;
          end;
        // Insert new Event Group
        FEventListenerGroups[LIndex] := AEventListenerGroup;
        AEventListenerGroup.FIndex := LIndex;
      end;
    {$ENDIF LKSL_USE_GENERICS}
  finally
    UnlockEventListenerGroups;
  end;
end;

procedure TLKEventThreadBase.ClearEventListenerGroups;
var
  {$IFDEF LKSL_USE_GENERICS}
    LEventListenerGroup: TLKEventListenerGroup;
  {$ELSE}
    I: Integer;
  {$ENDIF LKSL_USE_GENERICS}
begin
  LockEventListenerGroups;
  try
    {$IFDEF LKSL_USE_GENERICS}
      for LEventListenerGroup in FEventListenerGroups.Values do
        LEventListenerGroup.Free;
    {$ELSE}
      for I := High(FEventListenerGroups) downto Low(FEventListenerGroups) do
        FEventListenerGroups[I].Free;
    {$ENDIF LKSL_USE_GENERICS}
  finally
    UnlockEventListenerGroups;
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
  FEventListenerGroupLock := TCriticalSection.Create;
  FEventLock := TCriticalSection.Create;
  FProcessMode := epmAll;
  {$IFDEF LKSL_USE_GENERICS}
    FEventListenerGroups := TDictionary<TGUID, TLKEventListenerGroup>.Create;
  {$ENDIF LKSL_USE_GENERICS}
end;

procedure TLKEventThreadBase.DeleteEventListenerGroup(const AEventListenerGroup: TLKEventListenerGroup);
{$IFNDEF LKSL_USE_GENERICS}
  var
    LCount, I: Integer;
{$ENDIF LKSL_USE_GENERICS}
begin
  LockEventListenerGroups;
  try
    {$IFDEF LKSL_USE_GENERICS}
      FEventListenerGroups.Remove(AEventListenerGroup.EventType.GetTypeGUID);
    {$ELSE}
      LCount := Length(FEventListenerGroups);
      if (AEventListenerGroup.FIndex < 0) or (AEventListenerGroup.FIndex > LCount - 1) then
        Exit;
      if (AEventListenerGroup.FIndex < (LCount - 1)) then
        for I := AEventListenerGroup.FIndex to LCount - 2 do
        begin
          FEventListenerGroups[I] := FEventListenerGroups[I + 1];
          FEventListenerGroups[I].FIndex := I;
        end;
      SetLength(FEventListenerGroups, LCount - 1);
    {$ENDIF LKSL_USE_GENERICS}
  finally
    UnlockEventListenerGroups;
  end;
end;

destructor TLKEventThreadBase.Destroy;
begin
  ClearEvents;
  ClearEventListenerGroups;
  {$IFDEF LKSL_USE_GENERICS}
    FEventListenerGroups.Free;
  {$ENDIF LKSL_USE_GENERICS}
  FEventListenerGroupLock.Free;
  FEventLock.Free;
  inherited;
end;

function TLKEventThreadBase.GetEventListenerGroup(const AEventType: TLKEventType): TLKEventListenerGroup;
{$IFNDEF LKSL_USE_GENERICS}
  var
    LIndex, LLow, LHigh: Integer;
{$ENDIF LKSL_USE_GENERICS}
begin
  LockEventListenerGroups;
  try
    {$IFDEF LKSL_USE_GENERICS}
      if not (FEventListenerGroups.TryGetValue(AEventType.GetTypeGUID, Result)) then
        Result := nil
    {$ELSE}
      Result := nil;
      LLow := 0;
      LHigh := Length(FEventListenerGroups) - 1;
      if LHigh > -1 then
      begin
        if LLow < LHigh then
        begin
          while (LHigh - LLow > 1) do
          begin
            LIndex := (LHigh + LLow) div 2;
            if GUIDToString(AEventType.GetTypeGUID) <= GUIDToString(FEventListenerGroups[LIndex].EventType.GetTypeGUID) then
              LHigh := LIndex
            else
              LLow := LIndex;
          end;
        end;
        if (GUIDToString(FEventListenerGroups[LHigh].EventType.GetTypeGUID) = GUIDToString(AEventType.GetTypeGUID)) then
          Result := FEventListenerGroups[LHigh]
        else if (GUIDToString(FEventListenerGroups[LLow].EventType.GetTypeGUID) = GUIDToString(AEventType.GetTypeGUID)) then
          Result := FEventListenerGroups[LLow];
      end;
    {$ENDIF LKSL_USE_GENERICS}
  finally
    UnlockEventListenerGroups;
  end;
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

procedure TLKEventThreadBase.LockEventListenerGroups;
begin
  FEventListenerGroupLock.Acquire;
end;

procedure TLKEventThreadBase.LockEvents;
begin
  FEventLock.Acquire;
end;

procedure TLKEventThreadBase.ProcessListeners(const AEvent: TLKEvent; const ADelta, AStartTime: Double);
var
  LEventListenerGroup: TLKEventListenerGroup;
begin
  AEvent.FDelta := ADelta;
  AEvent.FProcessedTime := AStartTime;
//  LockEventListenerGroups;
//  try
  LEventListenerGroup := GetEventListenerGroup(TLKEventType(AEvent.ClassType));
  if LEventListenerGroup <> nil then
  begin
    if (((AEvent.ExpiresAfter > 0.00) and (GetReferenceTime - AEvent.DispatchTime < AEvent.ExpiresAfter)) or (AEvent.ExpiresAfter <= 0.00)) then
      LEventListenerGroup.ProcessEvent(AEvent);
  end;
//  finally
//    UnlockEventListenerGroups;
//  end;
end;

procedure TLKEventThreadBase.RegisterListener(const AListener: TLKEventListener);
var
  LEventListenerGroup: TLKEventListenerGroup;
begin
  LockEventListenerGroups;
  try
    LEventListenerGroup := GetEventListenerGroup(AListener.GetEventType);
    if LEventListenerGroup = nil then // If there's no Event Group for this Event Type...
    begin
      LEventListenerGroup := TLKEventListenerGroup.Create(Self, AListener.GetEventType); // Register it
    end;
    LEventListenerGroup.RegisterListener(AListener);
  finally
    UnlockEventListenerGroups;
  end;
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

procedure TLKEventThreadBase.UnlockEventListenerGroups;
begin
  FEventListenerGroupLock.Release;
end;

procedure TLKEventThreadBase.UnlockEvents;
begin
  FEventLock.Release;
end;

function TLKEventThreadBase.UnregisterListener(const AListener: TLKEventListener): Integer;
var
  LEventListenerGroup: TLKEventListenerGroup;
begin
  LockEventListenerGroups;
  try
    LEventListenerGroup := GetEventListenerGroup(AListener.GetEventType);
    if LEventListenerGroup <> nil then
      LEventListenerGroup.UnregisterListener(AListener);
    UnlockEventListenerGroups;
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
      FEvents[0].FStreamEvent := False;
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

{ TLKEventEngine }

procedure TLKEventEngine.AddEvent(const AEvent: TLKEvent; const AProcessingThread: TLKEventThreadBase);
begin
  QueueInThreads(AEvent);
  if AEvent.StreamEvent then
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
end;

destructor TLKEventEngine.Destroy;
begin
  FTransmitters.Kill;
  SetLength(FEventThreads, 0);
  FEventThreadLock.Free;
  FQueue.Kill;
  FStack.Kill;
  inherited;
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

procedure TLKEventEngine.QueueInThreads(const AEvent: TLKEvent);
var
  LEventThread: TLKEventThread;
  LClone: TLKEvent;
begin
  LockThreads;
  try
    for LEventThread in FEventThreads do
    begin
      LClone := TLKEventType(AEvent.ClassType).Create; // Create a blank instance of the Event for the Clone
      LClone.Assign(AEvent); // Copy the original data into the Clone
      LEventThread.AddEvent(LClone); // Send a CLONE of the Event to the Thread!
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

initialization
  Events := TLKEventEngine.Create;
finalization
  Events.Free;

end.
