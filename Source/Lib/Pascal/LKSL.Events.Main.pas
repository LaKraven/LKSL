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
  {$IFDEF POSIX}Posix.Unistd,{$ENDIF POSIX}
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs,
  {$ELSE}
    Classes, SysUtils, SyncObjs,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types,
  LKSL.Threads.Base,
  LKSL.Generics.Collections,
  LKSL.Streamables.Base;

  {$I LKSL_RTTI.inc}

type
  { Forward Declarations }
  TLKEvent = class;
  TLKEventListener = class;
  TLKEventContainer = class;
  TLKEventPreProcessor = class;
  TLKEventThread = class;

  { Class References }
  TLKEventClass = class of TLKEvent;
  TLKEventPreProcessorClass = class of TLKEventPreProcessor;

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
  ///  <summary><c>The current State of an Event.</c></summary>
  TLKEventState = (esNotDispatched, esDispatched, esProcessing, esProcessed, esCancelled);
  ///  <summary><c>Describes whether or not an Event-related Type should automatically Register itself on Construction.</c></summary>
  TLKEventRegistrationMode = (ermAutomatic, ermManual);

  { Exceptions }
  ELKEventEngineException = class(ELKException);
    ELKEventListenerException = class(ELKEventEngineException);
      ELKEventListenerNoEventThreadDefined = class(ELKEventListenerException);

  { Generics Collections }
  TLKEventList = class(TLKObjectList<TLKEvent>);
  TLKEventListenerList = class(TLKObjectList<TLKEventListener>);
  TLKEventPreProcessorClassArray = TArray<TLKEventPreProcessorClass>;
  TLKEventPreProcessorClassList = class(TLKList<TLKEventPreProcessorClass>);
  TLKEventPreProcessorList = class(TLKObjectList<TLKEventPreProcessor>);
  TLKEventThreadList = class(TLKObjectList<TLKEventThread>);

  ///  <summary><c>Abstract Base Class for all Event Types</c></summary>
  ///  <remarks>
  ///    <para><c>Don't implement behaviour on your descendants. Events are intended to provide raw information (properties) only, not functionality.</c></para>
  ///    <para><c>Your Event's information (properties) should be read-only!</c></para>
  ///  </remarks
  TLKEvent = class abstract(TLKPersistent)
  private
    ///  <summary><c>The Time at which the Event was Created.</c></summary>
    FCreatedTime: LKFloat;
    ///  <summary><c>The Method by which the Event was Dispatched.</c></summary>
    ///  <remarks><c>We either Queue an Event, or Stack it!</c></remarks>
    FDispatchMethod: TLKEventDispatchMethod;
    ///  <summary><c>The Targets to which this Event is allowed to be Dispatched.</c></summary>
    ///  <remarks>
    ///    <para><c>Default = [] (meaning that there are no restrictions)</c></para>
    ///    <para><c>By default, we want to allow the Event to be processed by ALL available PreProcessors</c></para>
    ///  </remarks>
    FDispatchTargets: TLKEventPreProcessorClassList;
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

    function GetDispatchTime: LKFloat;
    function GetExpiresAfter: LKFloat;
    function GetHasExpired: Boolean;
    function GetProcessedTime: LKFloat;
    function GetState: TLKEventState;

    procedure SetExpiresAfter(const AExpiresAfter: LKFloat);

    ///  <summary><c>Incrememnts (atomically) the Reference Count for the Event.</c></summary>
    procedure Ref;
    ///  <summary><c>Decrememnts (atomically) the Reference Count for the Event.</c></summary>
    ///  <remarks><c>If the resulting Count = 0, the Event is Freed.</c></remarks>
    procedure Unref;
  protected
    ///  <summary><c>Override if you want your Event Type to only dispatch to specific Targets.</c></summary>
    function GetDefaultDispatchTargets: TLKEventPreProcessorClassArray; virtual;
    ///  <summary><c>Override if you want your Event Type to Expire after a specific amount of time.</c></summary>
    function GetDefaultExpiresAfter: LKFloat; virtual;
  public
    constructor Create(const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic); reintroduce;
    destructor Destroy; override;

    class function GetEventType: TLKEventClass;

    ///  <summary><c>Cancels the Event after Dispatch.</c></summary>
    ///  <param name="ACancelConditions">Defines whether the Event should be cancelled if processing has begun or regardless there-of.</param>
    procedure Cancel(const ACancelConditions: TLKEventCancelCondition = eccIfNotProcessing);

    ///  <summary><c>Dispatch the Event through the Queue.</c></summary>
    procedure Queue(const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic); overload;
    ///  <summary><c>Dispatch the Event through the Queue with an Expiry time (T + AExpiresAfter).</c></summary>
    procedure Queue(const AExpiresAfter: LKFloat; const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic); overload;
    ///  <summary><c>Dispatch the Event through the Stack.</c></summary>
    procedure Stack(const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic); overload;
    ///  <summary><c>Dispatch the Event through the Stack with an Expiry time (T + AExpiresAfter).</c></summary>
    procedure Stack(const AExpiresAfter: LKFloat; const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic); overload;

    ///  <summary><c>Override the Type-defined default Dispatch Targets for a speciifc Instance.</c></summary>
    procedure SetDispatchTargets(const ADispatchTargets: TLKEventPreProcessorClassArray);

    property CreatedTime: LKFloat read FCreatedTime; // SET ON CONSTRUCTION ONLY
    property DispatchMethod: TLKEventDispatchMethod read FDispatchMethod; // ATOMIC OPERATION
    property DispatchTargets: TLKEventPreProcessorClassList read FDispatchTargets;
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

    function GetExpireAfter: LKFloat;

    procedure SetExpireAfter(const AExpireAfter: LKFloat);
  protected
    ///  <summary><c>Override if you want to make your Event Listener ignore by default any </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> instance dispatched further back in time than the given value.</c></summary>
    ///  <remarks><c>Default = 0.00</c></remarks>
    function GetDefaultExpireAfter: LKFloat; virtual;
    procedure DoEvent(const AEvent: TLKEvent); virtual; abstract;
  public
    constructor Create(const AEventThread: TLKEventThread; const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce;

    procedure AfterConstruction; override;

    ///  <summary><c>You MUST override this and set the Result to the Top-Level </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> Type in which this Listener is interested.</c></summary>
    function GetEventClass: TLKEventClass; virtual; abstract;

    procedure Register;
    procedure Unregister;

    property ExpireAfter: LKFloat read GetExpireAfter write SetExpireAfter;
  end;

  ///  <summary><c></c></summary>
  TLKEventListener<T: TLKEvent> = class abstract(TLKEventListener)
  private type
    TEventCallbackUnbound = procedure(const AEvent: T);
    TEventCallbackOfObject = procedure(const AEvent: T) of Object;
    TEventCallbackAnonymous = reference to procedure(const AEvent: T);
  private
    FOnEventUnbound: TEventCallbackUnbound;
    FOnEventOfObject: TEventCallbackOfObject;
    FOnEventAnonymous: TEventCallbackAnonymous;
  protected
    procedure DoEvent(const AEvent: TLKEvent); override; final;
  public
    constructor Create(const AEventThread: TLKEventThread; const AOnEventCallback: TEventCallbackUnbound; const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce; overload;
    constructor Create(const AEventThread: TLKEventThread; const AOnEventCallback: TEventCallbackOfObject; const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce; overload;
    constructor Create(const AEventThread: TLKEventThread; const AOnEventCallback: TEventCallbackAnonymous; const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce; overload;

    function GetEventClass: TLKEventClass; override; final;
  end;

  ///  <summary><c>Abstract Base Type for all Thread Types containing an Event Queue and Stack</c></summary>
  ///  <remarks>
  ///    <para><c>This includes </c><see DisplayName="TLKEventPreProcessor" cref="LKSL.Events.Main|TLKEventPreProcessor"/><c> and </c><see DisplayName="TLKEventThread" cref="LKSL.Events.Main|TLKEventThread"/><c> Types.</c></para>
  ///  </remarks>
  TLKEventContainer = class abstract(TLKThread)
  private
    FEventQueue: TLKEventList;
    FEventStack: TLKEventList;

    function GetEventCount: Integer;
    function GetEventCountQueue: Integer;
    function GetEventCountStack: Integer;
    procedure ProcessEventList(const AEventList: TLKEventList; const ADelta, AStartTime: LKFloat);
  protected
    { TLKThread overrides }
    function GetDefaultYieldAccumulatedTime: Boolean; override; final;
    function GetInitialThreadState: TLKThreadState; override;
    procedure Tick(const ADelta, AStartTime: LKFloat); override;
    { Overrideables }
    procedure ProcessEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat); virtual; abstract;
    function GetPauseOnNoEvent: Boolean; virtual;
    function GetWakeOnEvent: Boolean; virtual;
    { Callables }
    procedure ProcessEvents(const ADelta, AStartTime: LKFloat);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure QueueEvent(const AEvent: TLKEvent);
    procedure StackEvent(const AEvent: TLKEvent);

    property EventCount: Integer read GetEventCount;
    property EventCountQueue: Integer read GetEventCountQueue;
    property EventCountStack: Integer read GetEventCountStack;
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
    constructor Create(const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce; virtual;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure Register;
    procedure Unregister;
  end;

  ///  <summary><c>A special kind of Thread, designed to operate using Events.</c></summary>
  TLKEventThread = class(TLKEventContainer)
  private
    FListeners: TLKEventListenerList;
    ///  <summary><c>Dictates whether this Event Thread should be automatically Registered after Construction.</c></summary>
    FRegistrationMode: TLKEventRegistrationMode;
  protected
    { TLKThread Overrides }
    procedure PreTick(const ADelta, AStartTime: LKFloat); override;

    { TLKEventContainer Overrides }
    function GetPauseOnNoEvent: Boolean; override;
    function GetWakeOnEvent: Boolean; override;
    procedure ProcessEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat); override;

    { Overrideables }
    procedure InitializeListeners; virtual;
    procedure FinalizeListeners; virtual;
  public
    constructor Create(const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce; virtual;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure RegisterListener(const AEventListener: TLKEventListener);
    procedure UnregisterListener(const AEventListener: TLKEventListener);

    procedure Register;
    procedure Unregister;
  end;

implementation

type
  { Forward Declarations }
  TLKEventThreadPreProcessor = class;
  TLKEventEngine = class;

  ///  <summary><c>Specific Event PreProcessor for </c><see DisplayName="TLKEventThread" cref="LKSL.Events.Main|TLKEventThread"/><c> descendants.</c></summary>
  TLKEventThreadPreProcessor = class(TLKEventPreProcessor)
  private
    FEventThreads: TLKEventThreadList;
  protected
    procedure ProcessEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat); override;
  public
    constructor Create(const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); override;
    destructor Destroy; override;

    procedure RegisterEventThread(const AEventThread: TLKEventThread);
    procedure UnregisterEventThread(const AEventThread: TLKEventThread);
  end;

  ///  <summary><c>Heart and soul of the Event Engine.</c></summary>
  TLKEventEngine = class(TLKEventContainer)
  private
    FPreProcessors: TLKEventPreProcessorList;
  protected
    { TLKThread Overrides }
    procedure Tick(const ADelta, AStartTime: LKFloat); override;
    { TLKEventContainer Overrides }
    procedure ProcessEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure RegisterPreProcessor(const APreProcessor: TLKEventPreProcessor);
    procedure UnregisterPreProcessor(const APreProcessor: TLKEventPreProcessor);
  end;

var
  EventEngine: TLKEventEngine;
  ThreadPreProcessor: TLKEventThreadPreProcessor;

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
  FDispatchTargets := TLKEventPreProcessorClassList.Create;
  FCreatedTime := GetReferenceTime; // We've just created it...
  FDispatchMethod := edmNotDispatched; // It hasn't yet been dispatched...
  FDispatchTime := 0; // We haven't dispatched it yet...
  FDispatchTargets.Add(GetDefaultDispatchTargets); // We request the default defined Targets for its Type...
  FExpiresAfter := GetDefaultExpiresAfter; // We request the default expiration for its Type...
  FLifetimeControl := ALifetimeControl; // Define who is responsible for Lifetime Control...
  FOrigin := eoInternal; // We presume it originates internally...
  FProcessedTime := 0; // We haven't processed it yet (it hasn't even been dispatched)...
  FState := esNotDispatched; // We haven't dispatched it yet...
end;

destructor TLKEvent.Destroy;
begin
  FDispatchTargets.Free;
  inherited;
end;

function TLKEvent.GetDefaultDispatchTargets: TLKEventPreProcessorClassArray;
begin
  Result := [];
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

procedure TLKEvent.Queue(const AExpiresAfter: LKFloat; const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic);
begin
  Lock;
  try
    FExpiresAfter := AExpiresAfter;
  finally
    Unlock;
  end;
  Queue(ALifetimeControl);
end;

procedure TLKEvent.Queue(const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic);
begin
  FLifetimeControl := ALifetimeControl;
  FState := esDispatched;
  FDispatchMethod := edmQueue;
  EventEngine.QueueEvent(Self);
end;

procedure TLKEvent.Ref;
begin
  AtomicIncrement(FRefCount);
end;

procedure TLKEvent.SetDispatchTargets(const ADispatchTargets: TLKEventPreProcessorClassArray);
begin
  FDispatchTargets.Clear;
  FDispatchTargets.Add(ADispatchTargets)
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

procedure TLKEvent.Stack(const AExpiresAfter: LKFloat; const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic);
begin
  Lock;
  try
    FExpiresAfter := AExpiresAfter;
  finally
    Unlock;
  end;
  Stack(ALifetimeControl);
end;

procedure TLKEvent.Stack(const ALifetimeControl: TLKEventLifetimeControl = elcAutomatic);
begin
  FLifetimeControl := ALifetimeControl;
  FState := esDispatched;
  FDispatchMethod := edmStack;
  EventEngine.StackEvent(Self);
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
  FEventThread := AEventThread;
  if FEventThread = nil then
    raise ELKEventListenerNoEventThreadDefined.Create('Event Listeners MUST declare a parent Event Thread.');
  FRegistrationMode := ARegistrationMode;
end;

function TLKEventListener.GetDefaultExpireAfter: LKFloat;
begin
  Result := 0;
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

constructor TLKEventListener<T>.Create(const AEventThread: TLKEventThread; const AOnEventCallback: TEventCallbackAnonymous; const ARegistrationMode: TLKEventRegistrationMode);
begin
  inherited Create(AEventThread, ARegistrationMode);
  FOnEventAnonymous := AOnEventCallback;
end;

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

{ TLKEventContainer }

constructor TLKEventContainer.Create;
begin
  inherited;
  FEventQueue := TLKEventList.Create(False);
  FEventStack := TLKEventList.Create(False);
end;

destructor TLKEventContainer.Destroy;
begin
  FEventQueue.OwnsObjects := True;
  FEventStack.OwnsObjects := True;
  FEventQueue.Free;
  FEventStack.Free;
  inherited;
end;

procedure TLKEventContainer.ProcessEventList(const AEventList: TLKEventList; const ADelta, AStartTime: LKFloat);
var
  I, LStart, LEnd: Integer;
begin
  if AEventList.Count > 0 then
  begin
    LStart := 0;
    LEnd := AEventList.Count - 1;
    for I := LStart to LEnd do
    begin
      if (not Terminated) then
        if (AEventList[I].State <> esCancelled) and (not AEventList[I].HasExpired) then // We don't want to bother processing Cancelled Events!
          ProcessEvent(AEventList[I], ADelta, AStartTime);
        AEventList[I].Unref; // We're no longer referencing the Event
    end;
    AEventList.DeleteRange(LStart, LEnd); // Locking occurs automagically
  end;
end;

function TLKEventContainer.GetDefaultYieldAccumulatedTime: Boolean;
begin
  // We must NOT yield all Accumulated Time on Event-enabled Threads.
  // Doing so would prevent the Event Queue being processed
  Result := False;
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

procedure TLKEventContainer.ProcessEvents(const ADelta, AStartTime: LKFloat);
begin
  ProcessEventList(FEventStack, ADelta, AStartTime); // Stack first
  ProcessEventList(FEventQueue, ADelta, AStartTime); // Queue second
  if GetPauseOnNoEvent then
  begin
    if (FEventStack.IsEmpty) and (FEventQueue.IsEmpty) then
      ThreadState := tsPaused;
  end;
end;

procedure TLKEventContainer.QueueEvent(const AEvent: TLKEvent);
begin
  AEvent.Ref; // Add a Reference to the Event
  FEventQueue.Add(AEvent);
  if GetWakeOnEvent then
    ThreadState := tsRunning; // Wake up this Thread
end;

procedure TLKEventContainer.StackEvent(const AEvent: TLKEvent);
begin
  AEvent.Ref; // Add a Reference to the Event
  FEventStack.Add(AEvent);
  if GetWakeOnEvent then
    ThreadState := tsRunning; // Wake up this Thread
end;

procedure TLKEventContainer.Tick(const ADelta, AStartTime: LKFloat);
begin
  // Do nothing!
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
  FRegistrationMode := ARegistrationMode;
  FListeners := TLKEventListenerList.Create(False);
  InitializeListeners;
end;

destructor TLKEventThread.Destroy;
begin
  Unregister;
  FinalizeListeners;
  FListeners.Free;
  inherited;
end;

procedure TLKEventThread.FinalizeListeners;
begin
  // Do nothing (yet)
end;

function TLKEventThread.GetPauseOnNoEvent: Boolean;
begin
  // We process Events on PreTick in Event Threads, so the Thread's Running State is irrelevant here!
  Result := False;
end;

function TLKEventThread.GetWakeOnEvent: Boolean;
begin
  // We process Events on PreTick in Event Threads, so the Thread's Running State is irrelevant here!
  Result := False;
end;

procedure TLKEventThread.InitializeListeners;
begin
  // Do nothing (yet)
end;

procedure TLKEventThread.PreTick(const ADelta, AStartTime: LKFloat);
begin
  ProcessEvents(ADelta, AStartTime);
end;

procedure TLKEventThread.ProcessEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat);
var
  I: Integer;
begin
  FListeners.Lock; { TODO -oSJS -cEvent Engine (Redux) : Switch to LockIfAvailable and add failover list! }
  try
    for I := 0 to FListeners.Count - 1 do
      if (AEvent.State <> esCancelled) and (not AEvent.HasExpired) then // We don't want to bother actioning the Event if it has been Cancelled or has Expired
        if (AEvent is FListeners[I].GetEventClass) then // We want to make sure that the Event is relevant to the Listener
          FListeners[I].DoEvent(AEvent);
  finally
    FListeners.Unlock;
  end;
end;

procedure TLKEventThread.Register;
begin
  ThreadPreProcessor.RegisterEventThread(Self);
end;

procedure TLKEventThread.RegisterListener(const AEventListener: TLKEventListener);
begin
  FListeners.Lock; { TODO -oSJS -cEvent Engine (Redux) : Switch to LockIfAvailable and add failover list! }
  try
    if (not FListeners.Contains(AEventListener)) then
      FListeners.Add(AEventListener);
  finally
    FListeners.Unlock;
  end;
end;

procedure TLKEventThread.Unregister;
begin
  ThreadPreProcessor.UnregisterEventThread(Self);
end;

procedure TLKEventThread.UnregisterListener(const AEventListener: TLKEventListener);
var
  LIndex: Integer;
begin
  FListeners.Lock; { TODO -oSJS -cEvent Engine (Redux) : Switch to LockIfAvailable and add failover list! }
  try
    LIndex := FListeners.IndexOf(AEventListener);
    if LIndex > -1 then
      FListeners.Delete(LIndex);
  finally
    FListeners.Unlock;
  end;
end;

{ TLKEventThreadPreProcessor }

constructor TLKEventThreadPreProcessor.Create(const ARegistrationMode: TLKEventRegistrationMode);
begin
  inherited;
  FEventThreads := TLKEventThreadList.Create(False);
end;

destructor TLKEventThreadPreProcessor.Destroy;
begin
  FEventThreads.Free;
  inherited;
end;

procedure TLKEventThreadPreProcessor.ProcessEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat);
var
  I: Integer;
begin
  FEventThreads.Lock; { TODO -oSJS -cEvent Engine (Redux) : Switch to LockIfAvailable and add failover list! }
  try
    for I := 0 to FEventThreads.Count - 1 do
      if (AEvent.State <> esCancelled) and (not AEvent.HasExpired) then // No point passing it along if it's been cancelled!
        case AEvent.FDispatchMethod of
          edmQueue: FEventThreads[I].QueueEvent(AEvent);
          edmStack: FEventThreads[I].StackEvent(AEvent);
        end;
  finally
    FEventThreads.Unlock;
  end;
end;

procedure TLKEventThreadPreProcessor.RegisterEventThread(const AEventThread: TLKEventThread);
begin
  FEventThreads.Lock; { TODO -oSJS -cEvent Engine (Redux) : Switch to LockIfAvailable and add failover list! }
  try
    if (not FEventThreads.Contains(AEventThread)) then
      FEventThreads.Add(AEventThread);
  finally
    FEventThreads.Unlock;
  end;
end;

procedure TLKEventThreadPreProcessor.UnregisterEventThread(const AEventThread: TLKEventThread);
var
  LIndex: Integer;
begin
  FEventThreads.Lock; { TODO -oSJS -cEvent Engine (Redux) : Switch to LockIfAvailable and add failover list! }
  try
    LIndex := FEventThreads.IndexOf(AEventThread);
    if LIndex > -1 then
      FEventThreads.Delete(LIndex);
  finally
    FEventThreads.Unlock;
  end;
end;

{ TLKEventEngine }

procedure TLKEventEngine.AfterConstruction;
begin
  inherited;
end;

constructor TLKEventEngine.Create;
begin
  inherited;
  FPreProcessors := TLKEventPreProcessorList.Create(False); // Create this FIRST
end;

destructor TLKEventEngine.Destroy;
begin
  FPreProcessors.Free; // Free this LAST
  inherited;
end;

procedure TLKEventEngine.ProcessEvent(const AEvent: TLKEvent; const ADelta, AStartTime: LKFloat);
var
  I: Integer;
begin
  FPreProcessors.Lock; { TODO -oSJS -cEvent Engine (Redux) : Switch to LockIfAvailable and add failover list! }
  try
    for I := 0 to FPreProcessors.Count - 1 do
      if (AEvent.State <> esCancelled) and (not AEvent.HasExpired) then // No point passing it along if it's been cancelled!
        if (AEvent.DispatchTargets.IsEmpty) or (AEvent.DispatchTargets.Contains(FPreProcessors[I].GetPreProcessorClass)) then
          case AEvent.FDispatchMethod of
            edmQueue: FPreProcessors[I].QueueEvent(AEvent);
            edmStack: FPreProcessors[I].StackEvent(AEvent);
          end;
  finally
    FPreProcessors.Unlock;
  end;
end;

procedure TLKEventEngine.RegisterPreProcessor(const APreProcessor: TLKEventPreProcessor);
begin
  FPreProcessors.Lock; { TODO -oSJS -cEvent Engine (Redux) : Switch to LockIfAvailable and add failover list! }
  try
    if (not FPreProcessors.Contains(APreProcessor)) then
      FPreProcessors.Add(APreProcessor);
  finally
    FPreProcessors.Unlock;
  end;
end;

procedure TLKEventEngine.Tick(const ADelta, AStartTime: LKFloat);
begin
  ProcessEvents(ADelta, AStartTime);
end;

procedure TLKEventEngine.UnregisterPreProcessor(const APreProcessor: TLKEventPreProcessor);
var
  LIndex: Integer;
begin
  FPreProcessors.Lock; { TODO -oSJS -cEvent Engine (Redux) : Switch to LockIfAvailable and add failover list! }
  try
    LIndex := FPreProcessors.IndexOf(APreProcessor);
    if LIndex > -1 then
      FPreProcessors.Delete(LIndex);
  finally
    FPreProcessors.Unlock;
  end;
end;

initialization
  EventEngine := TLKEventEngine.Create; // Create this FIRST
  ThreadPreProcessor := TLKEventThreadPreProcessor.Create;
finalization
  ThreadPreProcessor.Kill;
  EventEngine.Kill; // Free this LAST

end.
