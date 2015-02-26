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

  { Generics Collections }
  TLKEventList = class(TLKObjectList<TLKEvent>);
  TLKEventListenerList = class(TLKObjectList<TLKEventListener>);
  TLKEventPreProcessorClassArray = TArray<TLKEventPreProcessorClass>;
  TLKEventPreProcessorList = TLKObjectList<TLKEventPreProcessor>;

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
    FDispatchTargets: TLKEventPreProcessorClassArray;
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

    function GetDispatchTargets: TLKEventPreProcessorClassArray;
    function GetDispatchTime: LKFloat;
    function GetExpiresAfter: LKFloat;
    function GetProcessedTime: LKFloat;
    function GetState: TLKEventState;

    procedure SetDispatchTargets(const ADispatchTargets: TLKEventPreProcessorClassArray);
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
    procedure Queue; overload;
    ///  <summary><c>Dispatch the Event through the Queue with an Expiry time (T + AExpiresAfter).</c></summary>
    procedure Queue(const AExpiresAfter: LKFloat); overload;
    ///  <summary><c>Dispatch the Event through the Stack.</c></summary>
    procedure Stack; overload;
    ///  <summary><c>Dispatch the Event through the Stack with an Expiry time (T + AExpiresAfter).</c></summary>
    procedure Stack(const AExpiresAfter: LKFloat); overload;

    property CreatedTime: LKFloat read FCreatedTime; // SET ON CONSTRUCTION ONLY
    property DispatchMethod: TLKEventDispatchMethod read FDispatchMethod; // ATOMIC OPERATION
    property DispatchTargets: TLKEventPreProcessorClassArray read GetDispatchTargets write SetDispatchTargets;
    property DispatchTime: LKFloat read GetDispatchTime;
    property ExpiresAfter: LKFloat read GetExpiresAfter write SetExpiresAfter;
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
  public
    constructor Create(const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce; overload;
    constructor Create(const AEventThread: TLKEventThread; const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce; overload;

    procedure AfterConstruction; override;

    ///  <summary><c>You MUST override this and set the Result to the Top-Level </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> Type in which this Listener is interested.</c></summary>
    function GetEventClass: TLKEventClass; virtual; abstract;

    procedure Register;
    procedure Unregister;
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
  protected
    function GetDefaultYieldAccumulatedTime: Boolean; override; final;
    function GetInitialThreadState: TLKThreadState; override;
    procedure Tick(const ADelta, AStartTime: LKFloat); override;
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
  public
    constructor Create(const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce;
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
  public
    constructor Create(const ARegistrationMode: TLKEventRegistrationMode = ermAutomatic); reintroduce;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure Register;
    procedure Unregister;
  end;

implementation

type
  { Forward Declarations }
  TLKEventEngine = class;

  TLKEventEngine = class(TLKEventContainer)
  private
    FPreProcessors: TLKEventPreProcessorList;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  EventEngine: TLKEventEngine;

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
  FDispatchMethod := edmNotDispatched; // It hasn't yet been dispatched...
  FDispatchTime := 0; // We haven't dispatched it yet...
  FDispatchTargets := GetDefaultDispatchTargets; // We request the default defined Targets for its Type...
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

function TLKEvent.GetDefaultDispatchTargets: TLKEventPreProcessorClassArray;
begin
  Result := [];
end;

function TLKEvent.GetDefaultExpiresAfter: LKFloat;
begin
  Result := 0;
end;

function TLKEvent.GetDispatchTargets: TLKEventPreProcessorClassArray;
begin
  Lock;
  try
    Result := FDispatchTargets;
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

procedure TLKEvent.Queue(const AExpiresAfter: LKFloat);
begin
  Lock;
  try
    FExpiresAfter := AExpiresAfter;
  finally
    Unlock;
  end;
  Queue;
end;

procedure TLKEvent.Queue;
begin
  { todo -cEvent Engine (Redux) -oSJS: Implement Queue }
end;

procedure TLKEvent.Ref;
begin
  AtomicIncrement(FRefCount);

end;

procedure TLKEvent.SetDispatchTargets(const ADispatchTargets: TLKEventPreProcessorClassArray);
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

procedure TLKEvent.Stack(const AExpiresAfter: LKFloat);
begin
  Lock;
  try
    FExpiresAfter := AExpiresAfter;
  finally
    Unlock;
  end;
  Stack;
end;

procedure TLKEvent.Stack;
begin
  { todo -cEvent Engine (Redux) -oSJS: Implement Stack }
end;

procedure TLKEvent.Unref;
begin
  AtomicDecrement(FRefCount);
  if (FLifetimeControl = elcAutomatic) and (FRefCount = 0) then
    Free;
end;

{ TLKEventListener }

procedure TLKEventListener.AfterConstruction;
begin
  inherited;
  if FRegistrationMode = ermAutomatic then
    Register;
end;

constructor TLKEventListener.Create(const ARegistrationMode: TLKEventRegistrationMode);
begin

end;

constructor TLKEventListener.Create(const AEventThread: TLKEventThread; const ARegistrationMode: TLKEventRegistrationMode);
begin

end;

procedure TLKEventListener.Register;
begin
  { todo -cEvent Engine (Redux) -oSJS: Implement Listener Register }
end;

procedure TLKEventListener.Unregister;
begin
  { todo -cEvent Engine (Redux) -oSJS: Implement Listener Unregister }
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

procedure TLKEventContainer.QueueEvent(const AEvent: TLKEvent);
begin
  FEventQueue.Add(AEvent);
end;

procedure TLKEventContainer.StackEvent(const AEvent: TLKEvent);
begin
  FEventStack.Add(AEvent);
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

procedure TLKEventPreProcessor.Register;
begin
  { todo -cEvent Engine (Redux) -oSJS: Implement PreProcessor Register }
end;

procedure TLKEventPreProcessor.Unregister;
begin
  { todo -cEvent Engine (Redux) -oSJS: Implement PreProcessor Unregister }
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
end;

destructor TLKEventThread.Destroy;
begin
  Unregister;
  inherited;
end;

procedure TLKEventThread.Register;
begin
  { todo -cEvent Engine (Redux) -oSJS: Implement Event Thread Register }
end;

procedure TLKEventThread.Unregister;
begin
  { todo -cEvent Engine (Redux) -oSJS: Implement Event Thread Unregister }
end;

{ TLKEventEngine }

constructor TLKEventEngine.Create;
begin
  inherited;
  FPreProcessors := TLKEventPreProcessorList.Create(False);
end;

destructor TLKEventEngine.Destroy;
begin
  FPreProcessors.Free;
  inherited;
end;

initialization
  EventEngine := TLKEventEngine.Create;
finalization
  EventEngine.Kill;

end.
