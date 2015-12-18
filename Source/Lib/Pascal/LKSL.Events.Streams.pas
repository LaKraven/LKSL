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
unit LKSL.Events.Streams;

{$I LKSL.inc}

{
  About this unit:
    - This unit is used internally by the "Event Engine"
    - This unit provides methods for Deleting, Inserting, Reading and Writing Event-specific Types
      to/from Streams.
}

interface

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils,
  {$ELSE}
    Classes, SysUtils,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types,
  LKSL.Streams.Main,
  LKSL.Events.Main;

  {$I LKSL_RTTI.inc}

// Delete Methods
procedure StreamDeleteTLKEventDispatchMethod(const AStream: TStream); overload;
procedure StreamDeleteTLKEventDispatchMethod(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteTLKEventOrigin(const AStream: TStream); overload;
procedure StreamDeleteTLKEventOrigin(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteTLKEventState(const AStream: TStream); overload;
procedure StreamDeleteTLKEventState(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteTLKEventTarget(const AStream: TStream); overload;
procedure StreamDeleteTLKEventTarget(const AStream: TStream; const APosition: Int64); overload;
// Insert Methods
procedure StreamInsertTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod); overload;
procedure StreamInsertTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod; const APosition: Int64); overload;
procedure StreamInsertTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin); overload;
procedure StreamInsertTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin; const APosition: Int64); overload;
procedure StreamInsertTLKEventState(const AStream: TStream; const AValue: TLKEventState); overload;
procedure StreamInsertTLKEventState(const AStream: TStream; const AValue: TLKEventState; const APosition: Int64); overload;
procedure StreamInsertTLKEventTarget(const AStream: TStream; const AValue: TLKEventTarget); overload;
procedure StreamInsertTLKEventTarget(const AStream: TStream; const AValue: TLKEventTarget; const APosition: Int64); overload;
// Read Methods
function StreamReadTLKEventDispatchMethod(const AStream: TStream): TLKEventDispatchMethod; overload;
function StreamReadTLKEventDispatchMethod(const AStream: TStream; const APosition: Int64): TLKEventDispatchMethod; overload;
function StreamReadTLKEventOrigin(const AStream: TStream): TLKEventOrigin; overload;
function StreamReadTLKEventOrigin(const AStream: TStream; const APosition: Int64): TLKEventOrigin; overload;
function StreamReadTLKEventState(const AStream: TStream): TLKEventState; overload;
function StreamReadTLKEventState(const AStream: TStream; const APosition: Int64): TLKEventState; overload;
function StreamReadTLKEventTarget(const AStream: TStream): TLKEventTarget; overload;
function StreamReadTLKEventTarget(const AStream: TStream; const APosition: Int64): TLKEventTarget; overload;
// Write Methods
procedure StreamWriteTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod); overload;
procedure StreamWriteTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod; const APosition: Int64); overload;
procedure StreamWriteTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin); overload;
procedure StreamWriteTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin; const APosition: Int64); overload;
procedure StreamWriteTLKEventState(const AStream: TStream; const AValue: TLKEventState); overload;
procedure StreamWriteTLKEventState(const AStream: TStream; const AValue: TLKEventState; const APosition: Int64); overload;
procedure StreamWriteTLKEventTarget(const AStream: TStream; const AValue: TLKEventTarget); overload;
procedure StreamWriteTLKEventTarget(const AStream: TStream; const AValue: TLKEventTarget; const APosition: Int64); overload;

procedure StreamDeleteTLKEventDispatchMethod(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteTLKEventDispatchMethod(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteTLKEventOrigin(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteTLKEventOrigin(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteTLKEventState(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteTLKEventState(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteTLKEventTarget(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteTLKEventTarget(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
// Insert Methods
procedure StreamInsertTLKEventDispatchMethod(const ACaret: ILKStreamCaret; const AValue: TLKEventDispatchMethod); overload;
procedure StreamInsertTLKEventDispatchMethod(const ACaret: ILKStreamCaret; const AValue: TLKEventDispatchMethod; const APosition: Int64); overload;
procedure StreamInsertTLKEventOrigin(const ACaret: ILKStreamCaret; const AValue: TLKEventOrigin); overload;
procedure StreamInsertTLKEventOrigin(const ACaret: ILKStreamCaret; const AValue: TLKEventOrigin; const APosition: Int64); overload;
procedure StreamInsertTLKEventState(const ACaret: ILKStreamCaret; const AValue: TLKEventState); overload;
procedure StreamInsertTLKEventState(const ACaret: ILKStreamCaret; const AValue: TLKEventState; const APosition: Int64); overload;
procedure StreamInsertTLKEventTarget(const ACaret: ILKStreamCaret; const AValue: TLKEventTarget); overload;
procedure StreamInsertTLKEventTarget(const ACaret: ILKStreamCaret; const AValue: TLKEventTarget; const APosition: Int64); overload;
// Read Methods
function StreamReadTLKEventDispatchMethod(const ACaret: ILKStreamCaret): TLKEventDispatchMethod; overload;
function StreamReadTLKEventDispatchMethod(const ACaret: ILKStreamCaret; const APosition: Int64): TLKEventDispatchMethod; overload;
function StreamReadTLKEventOrigin(const ACaret: ILKStreamCaret): TLKEventOrigin; overload;
function StreamReadTLKEventOrigin(const ACaret: ILKStreamCaret; const APosition: Int64): TLKEventOrigin; overload;
function StreamReadTLKEventState(const ACaret: ILKStreamCaret): TLKEventState; overload;
function StreamReadTLKEventState(const ACaret: ILKStreamCaret; const APosition: Int64): TLKEventState; overload;
function StreamReadTLKEventTarget(const ACaret: ILKStreamCaret): TLKEventTarget; overload;
function StreamReadTLKEventTarget(const ACaret: ILKStreamCaret; const APosition: Int64): TLKEventTarget; overload;
// Write Methods
procedure StreamWriteTLKEventDispatchMethod(const ACaret: ILKStreamCaret; const AValue: TLKEventDispatchMethod); overload;
procedure StreamWriteTLKEventDispatchMethod(const ACaret: ILKStreamCaret; const AValue: TLKEventDispatchMethod; const APosition: Int64); overload;
procedure StreamWriteTLKEventOrigin(const ACaret: ILKStreamCaret; const AValue: TLKEventOrigin); overload;
procedure StreamWriteTLKEventOrigin(const ACaret: ILKStreamCaret; const AValue: TLKEventOrigin; const APosition: Int64); overload;
procedure StreamWriteTLKEventState(const ACaret: ILKStreamCaret; const AValue: TLKEventState); overload;
procedure StreamWriteTLKEventState(const ACaret: ILKStreamCaret; const AValue: TLKEventState; const APosition: Int64); overload;
procedure StreamWriteTLKEventTarget(const ACaret: ILKStreamCaret; const AValue: TLKEventTarget); overload;
procedure StreamWriteTLKEventTarget(const ACaret: ILKStreamCaret; const AValue: TLKEventTarget; const APosition: Int64); overload;

implementation

uses
  LKSL.Streams.System;

  {$I LKSL_RTTI.inc}

// Delete Methods

procedure StreamDeleteTLKEventDispatchMethod(const AStream: TStream);
begin
  StreamDeleteTLKEventDispatchMethod(AStream, AStream.Position);
end;

procedure StreamDeleteTLKEventDispatchMethod(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(Byte));
end;

procedure StreamDeleteTLKEventOrigin(const AStream: TStream); overload;
begin
  StreamDeleteTLKEventOrigin(AStream, AStream.Position);
end;

procedure StreamDeleteTLKEventOrigin(const AStream: TStream; const APosition: Int64); overload;
begin
  StreamClearSpace(AStream, APosition, SizeOf(Byte));
end;

procedure StreamDeleteTLKEventState(const AStream: TStream);
begin
  StreamDeleteTLKEventState(AStream, AStream.Position);
end;

procedure StreamDeleteTLKEventState(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(Byte));
end;

procedure StreamDeleteTLKEventTarget(const AStream: TStream);
begin
  StreamDeleteTLKEventTarget(AStream, AStream.Position);
end;

procedure StreamDeleteTLKEventTarget(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(Byte));
end;

// Insert Methods

procedure StreamInsertTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod);
begin
  StreamInsertTLKEventDispatchMethod(AStream, AValue, AStream.Position);
end;

procedure StreamInsertTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod; const APosition: Int64);
const
  DISPATCH_METHODS: Array[TLKEventDispatchMethod] of Byte = (0, 1, 2);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Byte));
  AStream.Write(DISPATCH_METHODS[AValue], SizeOf(Byte));
end;

procedure StreamInsertTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin);
begin
  StreamInsertTLKEventOrigin(AStream, AValue, AStream.Position);
end;

procedure StreamInsertTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin; const APosition: Int64);
const
  ORIGINS: Array[TLKEventOrigin] of Byte = (0, 1, 2, 3);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Byte));
  AStream.Write(ORIGINS[AValue], SizeOf(Byte));
end;

procedure StreamInsertTLKEventState(const AStream: TStream; const AValue: TLKEventState);
begin
  StreamInsertTLKEventState(AStream, AValue, AStream.Position);
end;

procedure StreamInsertTLKEventState(const AStream: TStream; const AValue: TLKEventState; const APosition: Int64);
const
  STATES: Array[TLKEventState] of Byte = (0, 1, 2, 3, 4, 5);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Byte));
  AStream.Write(STATES[AValue], SizeOf(Byte));
end;

procedure StreamInsertTLKEventTarget(const AStream: TStream; const AValue: TLKEventTarget);
begin
  StreamInsertTLKEventTarget(AStream, AValue, AStream.Position);
end;

procedure StreamInsertTLKEventTarget(const AStream: TStream; const AValue: TLKEventTarget; const APosition: Int64);
const
  TARGETS: Array[TLKEventTarget] of Byte = (0, 1, 2, 3, 4);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Byte));
  AStream.Write(TARGETS[AValue], SizeOf(Byte));
end;

// Read Methods

function StreamReadTLKEventDispatchMethod(const AStream: TStream): TLKEventDispatchMethod;
begin
  Result := StreamReadTLKEventDispatchMethod(AStream, AStream.Position);
end;

function StreamReadTLKEventDispatchMethod(const AStream: TStream; const APosition: Int64): TLKEventDispatchMethod;
const
  DISPATCH_METHODS: Array[0..2] of TLKEventDispatchMethod = (edmNotDispatched, edmQueue, edmStack);
var
  LDispatchMethod: Byte;
begin
  AStream.Position := APosition;
  AStream.Read(LDispatchMethod, SizeOf(Byte));
  Result := DISPATCH_METHODS[LDispatchMethod];
end;

function StreamReadTLKEventOrigin(const AStream: TStream): TLKEventOrigin;
begin
  Result := StreamReadTLKEventOrigin(AStream, AStream.Position);
end;

function StreamReadTLKEventOrigin(const AStream: TStream; const APosition: Int64): TLKEventOrigin;
const
  ORIGINS: Array[0..3] of TLKEventOrigin = (eoInternal, eoReplay, eoRemote, eoUnknown);
var
  LOrigin: Byte;
begin
  AStream.Position := APosition;
  AStream.Read(LOrigin, SizeOf(Byte));
  Result := ORIGINS[LOrigin];
end;

function StreamReadTLKEventState(const AStream: TStream): TLKEventState;
begin
  Result := StreamReadTLKEventState(AStream, AStream.Position);
end;

function StreamReadTLKEventState(const AStream: TStream; const APosition: Int64): TLKEventState;
const
  STATES: Array[0..5] of TLKEventState = (esNotDispatched, esScheduled, esDispatched, esProcessing, esProcessed, esCancelled);
var
  LState: Byte;
begin
  AStream.Position := APosition;
  AStream.Read(LState, SizeOf(Byte));
  Result := STATES[LState];
end;

function StreamReadTLKEventTarget(const AStream: TStream): TLKEventTarget;
begin
  Result := StreamReadTLKEventTarget(AStream, AStream.Position);
end;

function StreamReadTLKEventTarget(const AStream: TStream; const APosition: Int64): TLKEventTarget;
const
  TARGETS: Array[0..4] of TLKEventTarget = (edThreads, edPools, edRecorders, edRemotes, edUknown);
var
  LState: Byte;
begin
  AStream.Position := APosition;
  AStream.Read(LState, SizeOf(Byte));
  Result := TARGETS[LState];
end;

// Write Methods

procedure StreamWriteTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod);
begin
  StreamWriteTLKEventDispatchMethod(AStream, AValue, AStream.Size);
end;

procedure StreamWriteTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod; const APosition: Int64);
const
  DISPATCH_METHODS: Array[TLKEventDispatchMethod] of Byte = (0, 1, 2);
begin
  AStream.Position := APosition;
  AStream.Write(DISPATCH_METHODS[AValue], SizeOf(Byte));
end;

procedure StreamWriteTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin); overload;
begin
  StreamWriteTLKEventOrigin(AStream, AValue, AStream.Size);
end;

procedure StreamWriteTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin; const APosition: Int64); overload;
const
  ORIGINS: Array[TLKEventOrigin] of Byte = (0, 1, 2, 3);
begin
  AStream.Position := APosition;
  AStream.Write(ORIGINS[AValue], SizeOf(Byte));
end;

procedure StreamWriteTLKEventState(const AStream: TStream; const AValue: TLKEventState);
begin
  StreamWriteTLKEventState(AStream, AValue, AStream.Size);
end;

procedure StreamWriteTLKEventState(const AStream: TStream; const AValue: TLKEventState; const APosition: Int64);
const
  STATES: Array[TLKEventState] of Byte = (0, 1, 2, 3, 4, 5);
begin
  AStream.Position := APosition;
  AStream.Write(STATES[AValue], SizeOf(Byte));
end;

procedure StreamWriteTLKEventTarget(const AStream: TStream; const AValue: TLKEventTarget);
begin
  StreamWriteTLKEventTarget(AStream, AValue, AStream.Size);
end;

procedure StreamWriteTLKEventTarget(const AStream: TStream; const AValue: TLKEventTarget; const APosition: Int64);
const
  TARGETS: Array[TLKEventTarget] of Byte = (0, 1, 2, 3, 4);
begin
  AStream.Position := APosition;
  AStream.Write(TARGETS[AValue], SizeOf(Byte));
end;

// Delete Methods

procedure StreamDeleteTLKEventDispatchMethod(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(Byte));
end;

procedure StreamDeleteTLKEventDispatchMethod(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteTLKEventDispatchMethod(ACaret);
end;

procedure StreamDeleteTLKEventOrigin(const ACaret: ILKStreamCaret); overload;
begin
  ACaret.Delete(SizeOf(Byte));
end;

procedure StreamDeleteTLKEventOrigin(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
begin
  ACaret.Position := APosition;
  StreamDeleteTLKEventOrigin(ACaret);
end;

procedure StreamDeleteTLKEventState(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(Byte));
end;

procedure StreamDeleteTLKEventState(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteTLKEventState(ACaret);
end;

procedure StreamDeleteTLKEventTarget(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(Byte));
end;

procedure StreamDeleteTLKEventTarget(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteTLKEventTarget(ACaret);
end;

// Insert Methods

procedure StreamInsertTLKEventDispatchMethod(const ACaret: ILKStreamCaret; const AValue: TLKEventDispatchMethod);
const
  DISPATCH_METHODS: Array[TLKEventDispatchMethod] of Byte = (0, 1, 2);
begin
  ACaret.Insert(DISPATCH_METHODS[AValue], SizeOf(Byte));
end;

procedure StreamInsertTLKEventDispatchMethod(const ACaret: ILKStreamCaret; const AValue: TLKEventDispatchMethod; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertTLKEventDispatchMethod(ACaret, AValue);
end;

procedure StreamInsertTLKEventOrigin(const ACaret: ILKStreamCaret; const AValue: TLKEventOrigin);
const
  ORIGINS: Array[TLKEventOrigin] of Byte = (0, 1, 2, 3);
begin
  ACaret.Insert(ORIGINS[AValue], SizeOf(Byte));
end;

procedure StreamInsertTLKEventOrigin(const ACaret: ILKStreamCaret; const AValue: TLKEventOrigin; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertTLKEventOrigin(ACaret, AValue);
end;

procedure StreamInsertTLKEventState(const ACaret: ILKStreamCaret; const AValue: TLKEventState);
const
  STATES: Array[TLKEventState] of Byte = (0, 1, 2, 3, 4, 5);
begin
  ACaret.Insert(STATES[AValue], SizeOf(Byte));
end;

procedure StreamInsertTLKEventState(const ACaret: ILKStreamCaret; const AValue: TLKEventState; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertTLKEventState(ACaret, AValue);
end;

procedure StreamInsertTLKEventTarget(const ACaret: ILKStreamCaret; const AValue: TLKEventTarget);
const
  TARGETS: Array[TLKEventTarget] of Byte = (0, 1, 2, 3, 4);
begin
  ACaret.Insert(TARGETS[AValue], SizeOf(Byte));
end;

procedure StreamInsertTLKEventTarget(const ACaret: ILKStreamCaret; const AValue: TLKEventTarget; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertTLKEventTarget(ACaret, AValue);
end;

// Read Methods

function StreamReadTLKEventDispatchMethod(const ACaret: ILKStreamCaret): TLKEventDispatchMethod;
const
  DISPATCH_METHODS: Array[0..2] of TLKEventDispatchMethod = (edmNotDispatched, edmQueue, edmStack);
var
  LDispatchMethod: Byte;
begin
  ACaret.Read(LDispatchMethod, SizeOf(Byte));
  Result := DISPATCH_METHODS[LDispatchMethod];
end;

function StreamReadTLKEventDispatchMethod(const ACaret: ILKStreamCaret; const APosition: Int64): TLKEventDispatchMethod;
begin
  ACaret.Position := APosition;
  Result := StreamReadTLKEventDispatchMethod(ACaret);
end;

function StreamReadTLKEventOrigin(const ACaret: ILKStreamCaret): TLKEventOrigin;
const
  ORIGINS: Array[0..3] of TLKEventOrigin = (eoInternal, eoReplay, eoRemote, eoUnknown);
var
  LOrigin: Byte;
begin
  ACaret.Read(LOrigin, SizeOf(Byte));
  Result := ORIGINS[LOrigin];
end;

function StreamReadTLKEventOrigin(const ACaret: ILKStreamCaret; const APosition: Int64): TLKEventOrigin;
begin
  ACaret.Position := APosition;
  Result := StreamReadTLKEventOrigin(ACaret);
end;

function StreamReadTLKEventState(const ACaret: ILKStreamCaret): TLKEventState;
const
  STATES: Array[0..5] of TLKEventState = (esNotDispatched, esScheduled, esDispatched, esProcessing, esProcessed, esCancelled);
var
  LState: Byte;
begin
  ACaret.Read(LState, SizeOf(Byte));
  Result := STATES[LState];
end;

function StreamReadTLKEventState(const ACaret: ILKStreamCaret; const APosition: Int64): TLKEventState;
begin
  ACaret.Position := APosition;
  Result := StreamReadTLKEventState(ACaret);
end;

function StreamReadTLKEventTarget(const ACaret: ILKStreamCaret): TLKEventTarget;
const
  TARGETS: Array[0..4] of TLKEventTarget = (edThreads, edPools, edRecorders, edRemotes, edUknown);
var
  LState: Byte;
begin
  ACaret.Read(LState, SizeOf(Byte));
  Result := TARGETS[LState];
end;

function StreamReadTLKEventTarget(const ACaret: ILKStreamCaret; const APosition: Int64): TLKEventTarget;
begin
  ACaret.Position := APosition;
  Result := StreamReadTLKEventTarget(ACaret);
end;

// Write Methods

procedure StreamWriteTLKEventDispatchMethod(const ACaret: ILKStreamCaret; const AValue: TLKEventDispatchMethod);
const
  DISPATCH_METHODS: Array[TLKEventDispatchMethod] of Byte = (0, 1, 2);
begin
  ACaret.Write(DISPATCH_METHODS[AValue], SizeOf(Byte));
end;

procedure StreamWriteTLKEventDispatchMethod(const ACaret: ILKStreamCaret; const AValue: TLKEventDispatchMethod; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteTLKEventDispatchMethod(ACaret, AValue);
end;

procedure StreamWriteTLKEventOrigin(const ACaret: ILKStreamCaret; const AValue: TLKEventOrigin);
const
  ORIGINS: Array[TLKEventOrigin] of Byte = (0, 1, 2, 3);
begin
  ACaret.Write(ORIGINS[AValue], SizeOf(Byte));
end;

procedure StreamWriteTLKEventOrigin(const ACaret: ILKStreamCaret; const AValue: TLKEventOrigin; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteTLKEventOrigin(ACaret, AValue);
end;

procedure StreamWriteTLKEventState(const ACaret: ILKStreamCaret; const AValue: TLKEventState);
const
  STATES: Array[TLKEventState] of Byte = (0, 1, 2, 3, 4, 5);
begin
  ACaret.Write(STATES[AValue], SizeOf(Byte));
end;

procedure StreamWriteTLKEventState(const ACaret: ILKStreamCaret; const AValue: TLKEventState; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteTLKEventState(ACaret, AValue);
end;

procedure StreamWriteTLKEventTarget(const ACaret: ILKStreamCaret; const AValue: TLKEventTarget);
const
  TARGETS: Array[TLKEventTarget] of Byte = (0, 1, 2, 3, 4);
begin
  ACaret.Write(TARGETS[AValue], SizeOf(Byte));
end;

procedure StreamWriteTLKEventTarget(const ACaret: ILKStreamCaret; const AValue: TLKEventTarget; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteTLKEventTarget(ACaret, AValue);
end;

end.
