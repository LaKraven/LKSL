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
unit LKSL.Common.Streams;

interface

{$I LKSL.inc}

{$IFDEF FPC}
  {$IFDEF LKSL_MODE_FPC}
    {$mode objfpc}{$H+}
  {$ELSE}
    {$mode delphi}
  {$ENDIF LKSL_MODE_FPC}
{$ENDIF FPC}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils,
  {$ELSE}
    Classes, SysUtils,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types;

  {$I LKSL_RTTI.inc}

// Delete Methods
procedure StreamDeleteLKFloat(const AStream: TStream); overload;
procedure StreamDeleteLKFloat(const AStream: TStream; const APosition: Int64); overload;
// Insert Methods
procedure StreamInsertLKFloat(const AStream: TStream; const AValue: LKFloat); overload;
procedure StreamInsertLKFloat(const AStream: TStream; const AValue: LKFloat; const APosition: Int64); overload;
// Read Methods
function StreamReadLKFloat(const AStream: TStream): LKFloat; overload;
function StreamReadLKFloat(const AStream: TStream; const APosition: Int64): LKFloat; overload;
// Write Methods
procedure StreamWriteLKFloat(const AStream: TStream; const AValue: LKFloat); overload;
procedure StreamWriteLKFloat(const AStream: TStream; const AValue: LKFloat; const APosition: Int64); overload;

implementation

uses
  LKSL.Streams.Main, LKSL.Streams.System;

procedure StreamDeleteLKFloat(const AStream: TStream);
begin
  StreamDeleteLKFloat(AStream, AStream.Position);
end;

procedure StreamDeleteLKFloat(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(LKFloat));
end;

procedure StreamInsertLKFloat(const AStream: TStream; const AValue: LKFloat);
begin
  StreamInsertLKFloat(AStream, AValue, AStream.Position);
end;

procedure StreamInsertLKFloat(const AStream: TStream; const AValue: LKFloat; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(LKFloat));
  AStream.Write(AValue, SizeOf(LKFloat));
end;

function StreamReadLKFloat(const AStream: TStream): LKFloat;
begin
  Result := StreamReadLKFloat(AStream, AStream.Position);
end;

function StreamReadLKFloat(const AStream: TStream; const APosition: Int64): LKFloat;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(LKFloat));
end;

procedure StreamWriteLKFloat(const AStream: TStream; const AValue: LKFloat);
begin
  StreamWriteLKFloat(AStream, AValue, AStream.Size);
end;

procedure StreamWriteLKFloat(const AStream: TStream; const AValue: LKFloat; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(LKFloat));
end;

end.
