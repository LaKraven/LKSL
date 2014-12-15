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
  LKSL.Events.Base;

  {$I LKSL_RTTI.inc}

// Delete Methods
procedure StreamDeleteTLKEventDispatchMethod(const AStream: TStream); overload;
procedure StreamDeleteTLKEventDispatchMethod(const AStream: TStream; const APosition: Int64); overload;
// Insert Methods
procedure StreamInsertTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod); overload;
procedure StreamInsertTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod; const APosition: Int64); overload;
// Read Methods
function StreamReadTLKEventDispatchMethod(const AStream: TStream): TLKEventDispatchMethod; overload;
function StreamReadTLKEventDispatchMethod(const AStream: TStream; const APosition: Int64): TLKEventDispatchMethod; overload;
// Write Methods
procedure StreamWriteTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod); overload;
procedure StreamWriteTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod; const APosition: Int64); overload;

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

// Insert Methods

procedure StreamInsertTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod);
begin
  StreamInsertTLKEventDispatchMethod(AStream, AValue, AStream.Position);
end;

procedure StreamInsertTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod; const APosition: Int64);
const
  DISPATCH_METHODS: Array[TLKEventDispatchMethod] of Integer = (0, 1);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(TLKEventDispatchMethod));
    AStream.Write(DISPATCH_METHODS[AValue], SizeOf(Integer));
end;

// Read Methods

function StreamReadTLKEventDispatchMethod(const AStream: TStream): TLKEventDispatchMethod;
begin
  Result := StreamReadTLKEventDispatchMethod(AStream, AStream.Position);
end;

function StreamReadTLKEventDispatchMethod(const AStream: TStream; const APosition: Int64): TLKEventDispatchMethod;
const
  DISPATCH_METHODS: Array[0..1] of TLKEventDispatchMethod = (edQueue, edStack);
var
  LDispatchMethod: Integer;
begin
  AStream.Position := APosition;
  AStream.Read(LDispatchMethod, SizeOf(Integer));
  Result := DISPATCH_METHODS[LDispatchMethod];
end;

// Write Methods

procedure StreamWriteTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod);
begin
  StreamWriteTLKEventDispatchMethod(AStream, AValue, AStream.Size);
end;

procedure StreamWriteTLKEventDispatchMethod(const AStream: TStream; const AValue: TLKEventDispatchMethod; const APosition: Int64);
const
  DISPATCH_METHODS: Array[TLKEventDispatchMethod] of Integer = (0, 1);
begin
  AStream.Position := APosition;
  AStream.Write(DISPATCH_METHODS[AValue], SizeOf(Integer));
end;

end.
