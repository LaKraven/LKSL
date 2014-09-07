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

{
  About this unit:
    - This unit provides type declarations required for our "Event Engine"
    - This unit also provides the Abstract Base Implementation for the same.

  Included Usage Demos:
    - "LKSL_Demo_EventEngine_Basic" in the "\Demos\Delphi\<version>\Event Engine - Basic" folder

  Changelog (latest changes first):
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
  TLKEventHandler = class;

  { Enum Types }
  TLKEventPriority = (epQueue, epStack);

  { Class References }
  TLKEventType = class of TLKEvent;
  TLKEventListenerType = class of TLKEventListener;

  { Method Types }
  TLKEventClassCallback = procedure(const AEvent: TLKEvent) of object;
  TLKEventUnboundCallback = procedure(const AEvent: TLKEvent) of object;

  { Array Types }
  TLKEventArray = Array of TLKEvent;
  TLKEventListenerArray = Array of TLKEventListener;
  TLKEventListenerGroupArray = Array of TLKEventListenerGroup;
  TLKEventThreadArray = Array of TLKEventThread;

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
    FDispatchTime: Double;
    FExpiresAfter: Double;
    FPriority: TLKEventPriority;
    FProcessedTime: Double;

    function GetDelta: Double;
    function GetDispatchTime: Double;
    function GetExpiresAfter: Double;
    function GetPriority: TLKEventPriority;
    function GetProcessedTime: Double;

    procedure SetExpiresAfter(const AExpiresAfter: Double);
  protected
    // You MUST override "Clone"
    // Start with "inherited;" then (remembering to type-cast "AFromEvent" to your Event Type)
    // populate your Event Type's properties.
    procedure Clone(const AFromEvent: TLKEvent); virtual;
  public
    constructor Create; override;

    procedure Queue;
    procedure Stack;

    { TLKStreamable Overrides Begin }
    // DON'T FORGET TO OVERRIDE THESE AND PERFORM THE APPROPRIATE ACTIONS TO DELETE/READ/INSERT/WRITE
    // THE VALUES PROVIDED BY >>YOUR<< EVENT TYPES!
    class procedure DeleteFromStream(const AStream: TStream); override;
    procedure ReadFromStream(const AStream: TStream); override;
    procedure InsertIntoStream(const AStream: TStream); override;
    procedure WriteToStream(const AStream: TStream); override;
    { TLKStreamable Overrides End }

    property Delta: Double read GetDelta;
    property DispatchTime: Double read GetDispatchTime;
    property ExpiresAfter: Double read GetExpiresAfter write SetExpiresAfter;
    property Priority: TLKEventPriority read GetPriority;
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
    TLKEventListenerGroup
      - Pairs multiple Listeners with a single Event Type
      - This is to speed up the Processing of Events by ensuring that an Event is relevant at the point
        of Queuing/Stacking
  }
  TLKEventListenerGroup = class(TLKPersistent)
  private
    FEventThread: TLKEventThreadBase;
    FEventType: TLKEventType;
    FIndex: Integer;
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
    FEventListenerGroups: TLKEventListenerGroupArray;

    FEventLock: TCriticalSection;
    FEvents: TLKEventArray;

    procedure AddEventListenerGroup(const AEventListenerGroup: TLKEventListenerGroup);
    procedure ClearEvents;
    procedure ClearEventListenerGroups;
    procedure DeleteEventListenerGroup(const AEventListenerGroup: TLKEventListenerGroup);
    function GetEventListenerGroup(const AEventType: TLKEventType): Integer;
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
    procedure UnregisterListener(const AListener: TLKEventListener);
    // "ProcessEvents" is overriden by TLKEventThread, TLKEventQueue and TLKEventStack,
    // which individually dictate how to process Events from the Events Array.
    procedure ProcessEvents(const ADelta, AStartTime: Double); virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;
    // "QueueEvent" adds an Event to the Thread's internal Event Array for processing
    procedure AddEvent(const AEvent: TLKEvent); virtual;
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
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  {
    TLKEventHandler
      -
  }
  TLKEventHandler = class(TLKPersistent)
  private
    FEventThreadLock: TCriticalSection;
    FEventThreads: TLKEventThreadArray;
    FQueue: TLKEventThreadBase;
    FStack: TLKEventThreadBase;

    // "QueueInThreads" iterates through all Event Threads and (if there's a relevant
    // Listener for the Event Type) adds the Event to the Thread's internal Event Queue
    procedure QueueInThreads(const AEvent: TLKEvent);

    procedure RegisterEventThread(const AEventThread: TLKEventThread);
    procedure UnregisterEventThread(const AEventThread: TLKEventThread);

    procedure RegisterListener(const AListener: TLKEventListener);
    procedure UnregisterListener(const AListener: TLKEventListener);
  public
    constructor Create; override;
    destructor Destroy; override;

    // "QueueEvent" adds an Event to the Processing Queue (first in, first out)
    procedure QueueEvent(const AEvent: TLKEvent);
    // "StackEvent" adds an Event to the Processing Stack (last in, first out)
    procedure StackEvent(const AEvent: TLKEvent);

    procedure LockThreads;
    procedure UnlockThreads;
  end;

var
  Events: TLKEventHandler;

procedure QueueEvent(const AEvent: TLKEvent);
procedure StackEvent(const AEvent: TLKEvent);

implementation

uses
  LKSL.Streams.System,
  LKSL.Events.Streams;

type
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

procedure QueueEvent(const AEvent: TLKEvent);
begin
  Events.QueueEvent(AEvent);
end;

procedure StackEvent(const AEvent: TLKEvent);
begin
  Events.StackEvent(AEvent);
end;

{ TLKEvent }

procedure TLKEvent.Clone(const AFromEvent: TLKEvent);
begin
  AFromEvent.Lock;
  FDelta := AFromEvent.FDelta;
  FDispatchTime := AFromEvent.FDispatchTime;
  FExpiresAfter := AFromEvent.FExpiresAfter;
  FPriority := AFromEvent.FPriority;
  FProcessedTime := AFromEvent.FProcessedTime;
  AFromEvent.Unlock;
end;

constructor TLKEvent.Create;
begin
  inherited;
  FDelta := 0.00;
  FDispatchTime := 0.00;
  FExpiresAfter := 0.00;
  FPriority := epQueue;
  FProcessedTime := 0.00;
end;

class procedure TLKEvent.DeleteFromStream(const AStream: TStream);
begin
  inherited;
  StreamDeleteDouble(AStream); // Delete FDelta
  StreamDeleteDouble(AStream); // Delete FDispatchTime
  StreamDeleteDouble(AStream); // Delete FExpiresAfter
  StreamDeleteTLKEventPriority(AStream);// Delete FPriority
  StreamDeleteDouble(AStream); // Delete FProcessedTime
end;

function TLKEvent.GetDelta: Double;
begin
  Lock;
  Result := FDelta;
  Unlock;
end;

function TLKEvent.GetDispatchTime: Double;
begin
  Lock;
  Result := FDispatchTime;
  Unlock;
end;

function TLKEvent.GetExpiresAfter: Double;
begin
  Lock;
  Result := FExpiresAfter;
  Unlock;
end;

function TLKEvent.GetPriority: TLKEventPriority;
begin
  Lock;
  Result := FPriority;
  Unlock;
end;

function TLKEvent.GetProcessedTime: Double;
begin
  Lock;
  Result := FProcessedTime;
  Unlock;
end;

procedure TLKEvent.InsertIntoStream(const AStream: TStream);
begin
  Lock;
  inherited;
  StreamInsertDouble(AStream, FDelta); // Insert FDelta
  StreamInsertDouble(AStream, FDispatchTime); // Insert FDispatchTime
  StreamInsertDouble(AStream, FExpiresAfter); // Insert FExpiresAfter
  StreamInsertTLKEventPriority(AStream, FPriority);// Delete FPriority
  StreamInsertDouble(AStream, FProcessedTime); // Insert FProcessedTime
  Unlock;
end;

procedure TLKEvent.Queue;
begin
  QueueEvent(Self);
end;

procedure TLKEvent.ReadFromStream(const AStream: TStream);
begin
  Lock;
  inherited;
  FDelta := StreamReadDouble(AStream); // Read FDelta
  FDispatchTime := StreamReadDouble(AStream); // Read FDispatchTime
  FExpiresAfter := StreamReadDouble(AStream); // Read FExpiresAfter
  FPriority := StreamReadTLKEventPriority(AStream);// Read FPriority
  FProcessedTime := StreamReadDouble(AStream); // Read FProcessedTime
  Unlock;
end;

procedure TLKEvent.SetExpiresAfter(const AExpiresAfter: Double);
begin
  Lock;
  FExpiresAfter := AExpiresAfter;
  Unlock;
end;

procedure TLKEvent.Stack;
begin
  StackEvent(Self);
end;

procedure TLKEvent.WriteToStream(const AStream: TStream);
begin
  Lock;
  inherited;
  StreamWriteDouble(AStream, FDelta); // Insert FDelta
  StreamWriteDouble(AStream, FDispatchTime); // Insert FDispatchTime
  StreamWriteDouble(AStream, FExpiresAfter); // Insert FExpiresAfter
  StreamInsertTLKEventPriority(AStream, FPriority);// Delete FPriority
  StreamWriteDouble(AStream, FProcessedTime); // Insert FProcessedTime
  Unlock;
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
  FLastEventTime := AEvent.DispatchTime;
  Unlock;
end;

function TLKEventListener.GetCallUIThread: Boolean;
begin
  Lock;
  Result := FCallUIThread;
  Unlock;
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
  Result := FEventThread;
  Unlock;
end;

function TLKEventListener.GetExpireAfter: Double;
begin
  Lock;
  Result := FExpireAfter;
  Unlock;
end;

function TLKEventListener.GetLastEventTime: Double;
begin
  Lock;
  Result := FLastEventTime;
  Unlock;
end;

function TLKEventListener.GetNewestEventOnly: Boolean;
begin
  Lock;
  Result := FNewestEventOnly;
  Unlock;
end;

procedure TLKEventListener.SetCallUIThread(const ACallUIThread: Boolean);
begin
  Lock;
  FCallUIThread := ACallUIThread;
  Unlock;
end;

procedure TLKEventListener.SetExpireAfter(const AExpireAfter: Double);
begin
  Lock;
  FExpireAfter := AExpireAfter;
  Unlock;
end;

procedure TLKEventListener.SetNewestEventOnly(const ANewestOnly: Boolean);
begin
  Lock;
  FNewestEventOnly := ANewestOnly;
  Unlock;
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
      Events.UnregisterListener(Self)
    else
      FEventThread.UnregisterListener(Self);
  end;
end;

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
  I: Integer;
begin
  Lock;
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
  Unlock;
end;

procedure TLKEventListenerGroup.RegisterListener(const AListener: TLKEventListener);
var
  LIndex: Integer;
begin
  Lock;
  LIndex := Length(FListeners);
  SetLength(FListeners, LIndex + 1);
  FListeners[LIndex] := AListener;
  FListeners[LIndex].FIndex := LIndex;
  Unlock;
end;

procedure TLKEventListenerGroup.UnregisterListener(const AListener: TLKEventListener);
var
  I: Integer;
begin
  Lock;
  AListener.FIndex := -1;
  for I := AListener.FIndex to High(FListeners) - 1 do
  begin
    FListeners[I] := FListeners[I + 1];
    FListeners[I].FIndex := I;
  end;
  if Length(FListeners) = 0 then
    Free;
  Unlock;
end;

{ TLKEventThreadBase }

procedure TLKEventThreadBase.AddEvent(const AEvent: TLKEvent);
var
  LIndex: Integer;
begin
  if GetEventListenerGroup(TLKEventType(AEvent.ClassType)) = -1 then
  begin
    AEvent.Free;
    Exit;
  end;
  LockEvents;
  AEvent.FDispatchTime := GetReferenceTime;
  LIndex := Length(FEvents);
  SetLength(FEvents, LIndex + 1);
  FEvents[LIndex] := AEvent;
  UnlockEvents;
end;

procedure TLKEventThreadBase.AddEventListenerGroup(const AEventListenerGroup: TLKEventListenerGroup);
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
        if AEventListenerGroup.EventType.GetTypeGUID <= FEventListenerGroups[LIndex].EventType.GetTypeGUID then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (FEventListenerGroups[LHigh].EventType.GetTypeGUID < AEventListenerGroup.EventType.GetTypeGUID) then
      Result := LHigh + 1
    else if (FEventListenerGroups[LLow].EventType.GetTypeGUID < AEventListenerGroup.EventType.GetTypeGUID) then
      Result := LLow + 1
    else
      Result := LLow;
  end;
var
  LIndex, I: Integer;
begin
  LIndex := GetEventListenerGroup(AEventListenerGroup.EventType);
  LockEventListenerGroups;
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
  UnlockEventListenerGroups;
end;

procedure TLKEventThreadBase.ClearEventListenerGroups;
var
  I: Integer;
begin
  LockEventListenerGroups;
  for I := High(FEventListenerGroups) downto Low(FEventListenerGroups) do
    FEventListenerGroups[I].Free;
  UnlockEventListenerGroups;
end;

procedure TLKEventThreadBase.ClearEvents;
var
  I: Integer;
begin
  LockEvents;
  for I := High(FEvents) downto Low(FEvents) do
    FEvents[I].Free;
  UnlockEvents;
end;

constructor TLKEventThreadBase.Create;
begin
  inherited;
  FEventListenerGroupLock := TCriticalSection.Create;
  FEventLock := TCriticalSection.Create;
end;

procedure TLKEventThreadBase.DeleteEventListenerGroup(const AEventListenerGroup: TLKEventListenerGroup);
var
  LCount, I: Integer;
begin
  LockEventListenerGroups;
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
  UnlockEventListenerGroups;
end;

destructor TLKEventThreadBase.Destroy;
begin
  ClearEvents;
  ClearEventListenerGroups;
  FEventListenerGroupLock.Free;
  FEventLock.Free;
  inherited;
end;

function TLKEventThreadBase.GetEventListenerGroup(const AEventType: TLKEventType): Integer;
var
  LIndex, LLow, LHigh: Integer;
begin
  LockEventListenerGroups;
  Result := -1;
  LLow := 0;
  LHigh := Length(FEventListenerGroups) - 1;
  if LHigh > -1 then
  begin
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AEventType.GetTypeGUID <= FEventListenerGroups[LIndex].EventType.GetTypeGUID then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (FEventListenerGroups[LHigh].EventType.GetTypeGUID = AEventType.GetTypeGUID) then
      Result := LHigh
    else if (FEventListenerGroups[LLow].EventType.GetTypeGUID = AEventType.GetTypeGUID) then
      Result := LLow;
  end;
  UnlockEventListenerGroups;
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
  LEventListenerGroup: Integer;
begin
  AEvent.FDelta := ADelta;
  AEvent.FProcessedTime := AStartTime;
  LockEventListenerGroups;
  LEventListenerGroup := GetEventListenerGroup(TLKEventType(AEvent.ClassType));
  if LEventListenerGroup > - 1 then
  begin
    if (((AEvent.ExpiresAfter > 0.00) and (GetReferenceTime - AEvent.DispatchTime < AEvent.ExpiresAfter)) or (AEvent.ExpiresAfter <= 0.00)) then
      FEventListenerGroups[LEventListenerGroup].ProcessEvent(AEvent);
  end;
  UnlockEventListenerGroups;
end;

procedure TLKEventThreadBase.RegisterListener(const AListener: TLKEventListener);
var
  LIndex: Integer;
begin
  LockEventListenerGroups;
  LIndex := GetEventListenerGroup(AListener.GetEventType);
  if LIndex = -1 then // If there's no Event Group for this Event Type...
  begin
    LIndex := TLKEventListenerGroup.Create(Self, AListener.GetEventType).FIndex; // Register it
//    LIndex := GetEventListenerGroup(AListener.GetEventType);
  end;
  FEventListenerGroups[LIndex].RegisterListener(AListener);
  UnlockEventListenerGroups;
end;

procedure TLKEventThreadBase.RemoveEvent(const AIndex: Integer);
var
  I: Integer;
begin
  LockEvents;
  for I := AIndex to High(FEvents) - 1 do
    FEvents[I] := FEvents[I + 1];
  SetLength(FEvents, Length(FEvents) - 1);
  UnlockEvents;
end;

procedure TLKEventThreadBase.UnlockEventListenerGroups;
begin
  FEventListenerGroupLock.Release;
end;

procedure TLKEventThreadBase.UnlockEvents;
begin
  FEventLock.Release;
end;

procedure TLKEventThreadBase.UnregisterListener(const AListener: TLKEventListener);
var
  LIndex: Integer;
begin
  LockEventListenerGroups;
  LIndex := GetEventListenerGroup(AListener.GetEventType);
  if LIndex > -1 then
    FEventListenerGroups[LIndex].UnregisterListener(AListener);
  UnlockEventListenerGroups;
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
begin
  // We don't Lock the Event Array at this point, as doing so would prevent additional
  // events from being added to the Queue (and could cause a Thread to freeze)
  if Length(FEvents) > 0 then
  begin
    ProcessListeners(FEvents[0], ADelta, AStartTime); // Process the first Event in the Queue
    FEvents[0].Free; // Destroy the Event
    RemoveEvent(0); // Rebalance the Event Array
  end;
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
begin
  if Length(FEvents) > 0 then
  begin
    ProcessListeners(FEvents[0], ADelta, AStartTime);
    FEvents[0].Free;
    RemoveEvent(0);
  end;;
end;

procedure TLKEventQueue.Tick(const ADelta, AStartTime: Double);
begin
  ProcessEvents(ADelta, AStartTime);
  LockEvents;
  if Length(FEvents) = 0 then
    ThreadState := tsPaused;
  UnlockEvents
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
var
  LEvent: Integer;
begin
  LEvent := High(FEvents);
  if LEvent > -1 then
  begin
    ProcessListeners(FEvents[LEvent], ADelta, AStartTime);
    FEvents[LEvent].Free;
    RemoveEvent(LEvent);
  end;
end;

procedure TLKEventStack.Tick(const ADelta, AStartTime: Double);
begin
  ProcessEvents(ADelta, AStartTime);
  LockEvents;
  if Length(FEvents) = 0 then
    ThreadState := tsPaused;
  UnlockEvents;
end;

{ TLKEventHandler }

constructor TLKEventHandler.Create;
begin
  inherited;
  FEventThreadLock := TCriticalSection.Create;
  FQueue := TLKEventQueue.Create;
  FStack := TLKEventStack.Create;
end;

destructor TLKEventHandler.Destroy;
begin
  SetLength(FEventThreads, 0);
  FEventThreadLock.Free;
  FQueue.Kill;
  FStack.Kill;
  inherited;
end;

procedure TLKEventHandler.LockThreads;
begin
  FEventThreadLock.Acquire;
end;

procedure TLKEventHandler.QueueEvent(const AEvent: TLKEvent);
begin
  AEvent.FPriority := epQueue;
  QueueInThreads(AEvent);
  FQueue.AddEvent(AEvent);
end;
{$O-}
procedure TLKEventHandler.QueueInThreads(const AEvent: TLKEvent);
var
  I: Integer;
  LClone: TLKEvent;
begin
  LockThreads;
  for I := Low(FEventThreads) to High(FEventThreads) do
  begin
    LClone := TLKEventType(AEvent.ClassType).Create;
    LClone.Clone(AEvent);
    FEventThreads[I].AddEvent(LClone); // NOTE: We send a CLONE of the Event to each Thread!
  end;
  UnlockThreads;
end;
{$O+}
procedure TLKEventHandler.RegisterEventThread(const AEventThread: TLKEventThread);
var
  LIndex: Integer;
begin
  LockThreads;
  LIndex := Length(FEventThreads);
  SetLength(FEventThreads, LIndex + 1);
  FEventThreads[LIndex] := AEventThread;
  AEventThread.FIndex := LIndex;
  UnlockThreads;
end;

procedure TLKEventHandler.RegisterListener(const AListener: TLKEventListener);
begin
  FStack.RegisterListener(AListener);
  FQueue.RegisterListener(AListener);
end;

procedure TLKEventHandler.StackEvent(const AEvent: TLKEvent);
begin
  AEvent.FPriority := epStack;
  QueueInThreads(AEvent);
  FStack.AddEvent(AEvent);
end;

procedure TLKEventHandler.UnlockThreads;
begin
  FEventThreadLock.Release;
end;

procedure TLKEventHandler.UnregisterEventThread(const AEventThread: TLKEventThread);
var
  I: Integer;
begin
  LockThreads;
  for I := AEventThread.FIndex to High(FEventThreads) - 1 do
  begin
    FEventThreads[I] := FEventThreads[I + 1];
    FEventThreads[I].FIndex := I;
  end;
  SetLength(FEventThreads, Length(FEventThreads) - 1);
  UnlockThreads;
end;

procedure TLKEventHandler.UnregisterListener(const AListener: TLKEventListener);
begin
  FStack.UnregisterListener(AListener);
  FQueue.UnregisterListener(AListener);
end;

initialization
  Events := TLKEventHandler.Create;
finalization
  Events.Free;

end.
