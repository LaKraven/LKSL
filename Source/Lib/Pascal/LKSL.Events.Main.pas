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

  { Enum Types }
  ///  <summary><c>The Method by which the Event was Dispatched.</c></summary>
  TLKEventDispatchMethod = (edmNotDispatched, edmQueue, edmStack);
  ///  <summary><c>Defined Dispatch Targets.</c></summary>
  TLKEventDispatchTarget = (edtQueue, edtStack, edtThreads, edtRecorders, edtStreaming);
  ///  <summary><c>The means by which the Lifetime of a </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> Instance is managed.</c></summary>
  ///  <remarks>
  ///    <para>elcAutomatic <c>= Dispatched Events are Reference Counted and destroyed once processed.</c></para>
  ///    <para>elcManual <c>= Dispatched Events are NOT Reference Counted, and must be destroyed by the implementing developer's code.</c></para>
  ///  </remarks>
  TLKEventLifetimeControl = (elcAutomatic, elcManual);
  ///  <summary><c>Defined Origins for a </c><see DisplayName="TLKEvent" cref="LKSL.Events.Main|TLKEvent"/><c> Instance.</c></summary>
  TLKEventOrigin = (eoInternal, eoReplay, eoRemote, eoUnknown);

  { Set Types }
  TLKEventDispatchTargets = set of TLKEventDispatchTarget;

  ///  <summary><c>Abstract Base Class for all Event Types</c></summary>
  TLKEvent = class abstract(TLKPersistent)
  private
    ///  <summary><c>The Time at which the Event was Created.</c></summary>
    FCreatedTime: LKFloat;
    ///  <summary><c>The Method by which the Event was Dispatched.</c></summary>
    ///  <remarks><c>We either Queue an Event, or Stack it!</c></remarks>
    FDispatchMethod: TLKEventDispatchMethod;
    ///  <summary><c>The Targets to which this Event is allowed to be Dispatched.</c></summary>
    ///  <remarks><c>Default = </c><see DisplayName="LKEVENT_DISPATCH_TARGETS_ALL" cref="LKSL.Events.Main|LKEVENT_DISPATCH_TARGETS_ALL"/></remarks>
    FDispatchTargets: TLKEventDispatchTargets;
    ///  <summary><c>The Reference Time at which the Event was Dispatched.</c></summary>
    FDispatchTime: LKFloat;
    ///  <summary><c>The Duration of Time after which the Event will Expire once Dispatched.</c></summary>
    ///  <remarks><c>Default will be 0.00 (Never Expires)</c></remarks>
    FExpiresAfter: LKFloat;
    ///  <summary><c>Where this Event came from.</c></summary>
    FOrigin: TLKEventOrigin;
    ///  <summary><c>The Reference Time at which the Event was First Processed.</c></summary>
    FProcessedTime: LKFloat;
    ///  <summary><c>Reference Count for when Lifetime Control is owned by the Event Engine.</c></summary>
    FRefCount: Integer;

    function GetDispatchTargets: TLKEventDispatchTargets;
    function GetDispatchTime: LKFloat;
    function GetExpiresAfter: LKFloat;
    function GetProcessedTime: LKFloat;

    procedure SetDispatchTargets(const ADispatchTargets: TLKEventDispatchTargets);
    procedure SetExpiresAfter(const AExpiresAfter: LKFloat);

    ///  <summary><c>Incrememnts (atomically) the Reference Count for the Event.</c></summary>
    procedure Ref;
    ///  <summary><c>Decrememnts (atomically) the Reference Count for the Event.</c></summary>
    ///  <remarks><c>If the resulting Count = 0, the Event is Freed.</c></remarks>
    procedure Unref;
  protected
    function GetDefaultDispatchTargets: TLKEventDispatchTargets; virtual;
    function GetDefaultExpiresAfter: LKFloat; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    property CreatedTime: LKFloat read FCreatedTime; // SET ON CONSTRUCTION ONLY
    property DispatchMethod: TLKEventDispatchMethod read FDispatchMethod; // ATOMIC OPERATION
    property DispatchTargets: TLKEventDispatchTargets read GetDispatchTargets write SetDispatchTargets;
    property DispatchTime: LKFloat read GetDispatchTime;
    property ExpiresAfter: LKFloat read GetExpiresAfter write SetExpiresAfter;
    property Origin: TLKEventOrigin read FOrigin; // SET ON CONSTRUCTION ONLY
    property ProcessedTime: LKFloat read GetProcessedTime;
  end;

const
  ///  <summary><c>All known Dispatch Targets.</c></summary>
  LKEVENT_DISPATCH_TARGETS_ALL: TLKEventDispatchTargets = [edtQueue, edtStack, edtThreads, edtRecorders, edtStreaming];

implementation

{ TLKEvent }

constructor TLKEvent.Create;
begin
  inherited Create;
  FCreatedTime := GetReferenceTime; // We've just created it...
  FDispatchMethod := edmNotDispatched; // It hasn't yet been dispatched...
  FDispatchTime := 0; // We haven't dispatched it yet...
  FDispatchTargets := GetDefaultDispatchTargets; // We request the default defined Targets for its Type...
  FExpiresAfter := GetDefaultExpiresAfter; // We request the default expiration for its Type...
  FOrigin := eoInternal; // We presume it originates internally...
  FProcessedTime := 0; // We haven't processed it yet (it hasn't even been dispatched)...
end;

destructor TLKEvent.Destroy;
begin

  inherited;
end;

function TLKEvent.GetDefaultDispatchTargets: TLKEventDispatchTargets;
begin
  Result := LKEVENT_DISPATCH_TARGETS_ALL;
end;

function TLKEvent.GetDefaultExpiresAfter: LKFloat;
begin
  Result := 0;
end;

function TLKEvent.GetDispatchTargets: TLKEventDispatchTargets;
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

procedure TLKEvent.Ref;
begin
  AtomicIncrement(FRefCount);
end;

procedure TLKEvent.SetDispatchTargets(const ADispatchTargets: TLKEventDispatchTargets);
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

procedure TLKEvent.Unref;
begin
  AtomicDecrement(FRefCount);
end;

end.
