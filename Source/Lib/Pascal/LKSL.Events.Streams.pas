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
  LKSL.Events.Main;

  {$I LKSL_RTTI.inc}

// Delete Methods
procedure StreamDeleteTLKEventDispatchMethod(const AStream: TStream); overload;
procedure StreamDeleteTLKEventDispatchMethod(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteTLKEventLifetimeControl(const AStream: TStream); overload;
procedure StreamDeleteTLKEventLifetimeControl(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteTLKEventOrigin(const AStream: TStream); overload;
procedure StreamDeleteTLKEventOrigin(const AStream: TStream; const APosition: Int64); overload;
// Insert Methods
procedure StreamInsertTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod); overload;
procedure StreamInsertTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod; const APosition: Int64); overload;
procedure StreamInsertTLKEventLifetimeControl(const AStream: TStream; const AValue: TLKEventLifetimeControl); overload;
procedure StreamInsertTLKEventLifetimeControl(const AStream: TStream; const AValue: TLKEventLifetimeControl; const APosition: Int64); overload;
procedure StreamInsertTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin); overload;
procedure StreamInsertTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin; const APosition: Int64); overload;
// Read Methods
function StreamReadTLKEventDispatchMethod(const AStream: TStream): TLKEventDispatchMethod; overload;
function StreamReadTLKEventDispatchMethod(const AStream: TStream; const APosition: Int64): TLKEventDispatchMethod; overload;
function StreamReadTLKEventLifetimeControl(const AStream: TStream): TLKEventLifetimeControl; overload;
function StreamReadTLKEventLifetimeControl(const AStream: TStream; const APosition: Int64): TLKEventLifetimeControl; overload;
function StreamReadTLKEventOrigin(const AStream: TStream): TLKEventOrigin; overload;
function StreamReadTLKEventOrigin(const AStream: TStream; const APosition: Int64): TLKEventOrigin; overload;
// Write Methods
procedure StreamWriteTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod); overload;
procedure StreamWriteTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod; const APosition: Int64); overload;
procedure StreamWriteTLKEventLifetimeControl(const AStream: TStream; const AValue: TLKEventLifetimeControl); overload;
procedure StreamWriteTLKEventLifetimeControl(const AStream: TStream; const AValue: TLKEventLifetimeControl; const APosition: Int64); overload;
procedure StreamWriteTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin); overload;
procedure StreamWriteTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin; const APosition: Int64); overload;

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
  StreamClearSpace(AStream, APosition, SizeOf(Integer));
end;

procedure StreamDeleteTLKEventLifetimeControl(const AStream: TStream);
begin
  StreamDeleteTLKEventLifetimeControl(AStream, AStream.Position);
end;

procedure StreamDeleteTLKEventLifetimeControl(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(Integer));
end;

procedure StreamDeleteTLKEventOrigin(const AStream: TStream); overload;
begin
  StreamDeleteTLKEventOrigin(AStream, AStream.Position);
end;

procedure StreamDeleteTLKEventOrigin(const AStream: TStream; const APosition: Int64); overload;
begin
  StreamClearSpace(AStream, APosition, SizeOf(Integer));
end;

// Insert Methods

procedure StreamInsertTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod);
begin
  StreamInsertTLKEventDispatchMethod(AStream, AValue, AStream.Position);
end;

procedure StreamInsertTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod; const APosition: Int64);
const
  DISPATCH_METHODS: Array[TLKEventDispatchMethod] of Integer = (0, 1, 2);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Integer));
  AStream.Write(DISPATCH_METHODS[AValue], SizeOf(Integer));
end;

procedure StreamInsertTLKEventLifetimeControl(const AStream: TStream; const AValue: TLKEventLifetimeControl);
begin
  StreamInsertTLKEventLifetimeControl(AStream, AValue, AStream.Position);
end;

procedure StreamInsertTLKEventLifetimeControl(const AStream: TStream; const AValue: TLKEventLifetimeControl; const APosition: Int64);
const
  LIFETIME_CONTROL: Array[TLKEventLifetimeControl] of Integer = (0, 1);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Integer));
  AStream.Write(LIFETIME_CONTROL[AValue], SizeOf(Integer));
end;

procedure StreamInsertTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin);
begin
  StreamInsertTLKEventOrigin(AStream, AValue, AStream.Position);
end;

procedure StreamInsertTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin; const APosition: Int64);
const
  ORIGINS: Array[TLKEventOrigin] of Integer = (0, 1, 2, 3);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Integer));
  AStream.Write(ORIGINS[AValue], SizeOf(Integer));
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
  LDispatchMethod: Integer;
begin
  AStream.Position := APosition;
  AStream.Read(LDispatchMethod, SizeOf(Integer));
  Result := DISPATCH_METHODS[LDispatchMethod];
end;

function StreamReadTLKEventLifetimeControl(const AStream: TStream): TLKEventLifetimeControl;
begin
  Result := StreamReadTLKEventLifetimeControl(AStream, AStream.Position);
end;

function StreamReadTLKEventLifetimeControl(const AStream: TStream; const APosition: Int64): TLKEventLifetimeControl;
const
  LIFETIME_CONTROL: Array[0..1] of TLKEventLifetimeControl = (elcAutomatic, elcManual);
var
  LLifetimeControl: Integer;
begin
  AStream.Position := APosition;
  AStream.Read(LLifetimeControl, SizeOf(Integer));
  Result := LIFETIME_CONTROL[LLifetimeControl];
end;

function StreamReadTLKEventOrigin(const AStream: TStream): TLKEventOrigin;
begin
  Result := StreamReadTLKEventOrigin(AStream, AStream.Position);
end;

function StreamReadTLKEventOrigin(const AStream: TStream; const APosition: Int64): TLKEventOrigin;
const
  ORIGINS: Array[0..3] of TLKEventOrigin = (eoInternal, eoReplay, eoRemote, eoUnknown);
var
  LOrigin: Integer;
begin
  AStream.Position := APosition;
  AStream.Read(LOrigin, SizeOf(Integer));
  Result := ORIGINS[LOrigin];
end;

// Write Methods

procedure StreamWriteTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod);
begin
  StreamWriteTLKEventDispatchMethod(AStream, AValue, AStream.Size);
end;

procedure StreamWriteTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod; const APosition: Int64);
const
  DISPATCH_METHODS: Array[TLKEventDispatchMethod] of Integer = (0, 1, 2);
begin
  AStream.Position := APosition;
  AStream.Write(DISPATCH_METHODS[AValue], SizeOf(Integer));
end;

procedure StreamWriteTLKEventLifetimeControl(const AStream: TStream; const AValue: TLKEventLifetimeControl);
begin
  StreamWriteTLKEventLifetimeControl(AStream, AValue, AStream.Size);
end;

procedure StreamWriteTLKEventLifetimeControl(const AStream: TStream; const AValue: TLKEventLifetimeControl; const APosition: Int64);
const
  LIFETIME_CONTROL: Array[TLKEventLifetimeControl] of Integer = (0, 1);
begin
  AStream.Position := APosition;
  AStream.Write(LIFETIME_CONTROL[AValue], SizeOf(Integer));
end;

procedure StreamWriteTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin); overload;
begin
  StreamWriteTLKEventOrigin(AStream, AValue, AStream.Size);
end;

procedure StreamWriteTLKEventOrigin(const AStream: TStream; const AValue: TLKEventOrigin; const APosition: Int64); overload;
const
  ORIGINS: Array[TLKEventOrigin] of Integer = (0, 1, 2, 3);
begin
  AStream.Position := APosition;
  AStream.Write(ORIGINS[AValue], SizeOf(Integer));
end;

end.
