{
  LaKraven Studios Standard Library [LKSL]
  Copyright (c) 2014-2016, Simon J Stuart, All Rights Reserved

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
unit LKSL.Streams.System;

interface

{$I LKSL.inc}

{
  About this unit:
    - This unit provides a series of methods to Delete, Insert, Read and Write values to/from Streams
    - The Types covered are those defined in "System.pas"
}

uses
  {$IFDEF FPC}
    lazutf8,
  {$ENDIF FPC}
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils,
  {$ELSE}
    Classes, SysUtils,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Streams.Main;

  {$I LKSL_RTTI.inc}

// Delete Methods
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamDeleteAnsiChar(const AStream: TStream); overload; platform;
  procedure StreamDeleteAnsiChar(const AStream: TStream; const APosition: Int64); overload; platform;
  procedure StreamDeleteAnsiString(const AStream: TStream); overload; platform;
  procedure StreamDeleteAnsiString(const AStream: TStream; const APosition: Int64); overload; platform;
{$ENDIF}
procedure StreamDeleteBoolean(const AStream: TStream); overload;
procedure StreamDeleteBoolean(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteByte(const AStream: TStream); overload;
procedure StreamDeleteByte(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteCardinal(const AStream: TStream); overload;
procedure StreamDeleteCardinal(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteChar(const AStream: TStream); overload;
procedure StreamDeleteChar(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteCurrency(const AStream: TStream); overload;
procedure StreamDeleteCurrency(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteDate(const AStream: TStream); overload;
procedure StreamDeleteDate(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteDateTime(const AStream: TStream); overload;
procedure StreamDeleteDateTime(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteDouble(const AStream: TStream); overload;
procedure StreamDeleteDouble(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteExtended(const AStream: TStream); overload;
procedure StreamDeleteExtended(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteGUID(const AStream: TStream); overload;
procedure StreamDeleteGUID(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteInt64(const AStream: TStream); overload;
procedure StreamDeleteInt64(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteInteger(const AStream: TStream); overload;
procedure StreamDeleteInteger(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteLongInt(const AStream: TStream); overload;
procedure StreamDeleteLongInt(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteShortInt(const AStream: TStream); overload;
procedure StreamDeleteShortInt(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteShortString(const AStream: TStream); overload;
procedure StreamDeleteShortString(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteSingle(const AStream: TStream); overload;
procedure StreamDeleteSingle(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteSmallInt(const AStream: TStream); overload;
procedure StreamDeleteSmallInt(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteString(const AStream: TStream); overload;
procedure StreamDeleteString(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteStream(const AStream: TStream; const ADeleteStream: TStream); overload;
procedure StreamDeleteStream(const AStream: TStream; const ADeleteStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteStream(const AStream: TStream); overload;
procedure StreamDeleteStream(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteTime(const AStream: TStream); overload;
procedure StreamDeleteTime(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteWideString(const AStream: TStream); overload;
procedure StreamDeleteWideString(const AStream: TStream; const APosition: Int64); overload;
procedure StreamDeleteWord(const AStream: TStream); overload;
procedure StreamDeleteWord(const AStream: TStream; const APosition: Int64); overload;
// Insert Methods
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamInsertAnsiChar(const AStream: TStream; const AValue: AnsiChar); overload; platform;
  procedure StreamInsertAnsiChar(const AStream: TStream; const AValue: AnsiChar; const APosition: Int64); overload; platform;
  procedure StreamInsertAnsiString(const AStream: TStream; const AValue: AnsiString); overload; platform;
  procedure StreamInsertAnsiString(const AStream: TStream; const AValue: AnsiString; const APosition: Int64); overload; platform;
{$ENDIF}
procedure StreamInsertBoolean(const AStream: TStream; const AValue: Boolean); overload;
procedure StreamInsertBoolean(const AStream: TStream; const AValue: Boolean; const APosition: Int64); overload;
procedure StreamInsertByte(const AStream: TStream; const AValue: Byte); overload;
procedure StreamInsertByte(const AStream: TStream; const AValue: Byte; const APosition: Int64); overload;
procedure StreamInsertCardinal(const AStream: TStream; const AValue: Cardinal); overload;
procedure StreamInsertCardinal(const AStream: TStream; const AValue: Cardinal; const APosition: Int64); overload;
procedure StreamInsertChar(const AStream: TStream; const AValue: Char); overload;
procedure StreamInsertChar(const AStream: TStream; const AValue: Char; const APosition: Int64); overload;
procedure StreamInsertCurrency(const AStream: TStream; const AValue: Currency); overload;
procedure StreamInsertCurrency(const AStream: TStream; const AValue: Currency; const APosition: Int64); overload;
procedure StreamInsertDate(const AStream: TStream; const AValue: TDate); overload;
procedure StreamInsertDate(const AStream: TStream; const AValue: TDate; const APosition: Int64); overload;
procedure StreamInsertDateTime(const AStream: TStream; const AValue: TDateTime); overload;
procedure StreamInsertDateTime(const AStream: TStream; const AValue: TDateTime; const APosition: Int64); overload;
procedure StreamInsertDouble(const AStream: TStream; const AValue: Double); overload;
procedure StreamInsertDouble(const AStream: TStream; const AValue: Double; const APosition: Int64); overload;
procedure StreamInsertExtended(const AStream: TStream; const AValue: Extended); overload;
procedure StreamInsertExtended(const AStream: TStream; const AValue: Extended; const APosition: Int64); overload;
procedure StreamInsertGUID(const AStream: TStream; const AValue: TGUID); overload;
procedure StreamInsertGUID(const AStream: TStream; const AValue: TGUID; const APosition: Int64); overload;
procedure StreamInsertInteger(const AStream: TStream; const AValue: Integer); overload;
procedure StreamInsertInteger(const AStream: TStream; const AValue: Integer; const APosition: Int64); overload;
procedure StreamInsertInt64(const AStream: TStream; const AValue: Int64); overload;
procedure StreamInsertInt64(const AStream: TStream; const AValue, APosition: Int64); overload;
procedure StreamInsertLongInt(const AStream: TStream; const AValue: Integer); overload;
procedure StreamInsertLongInt(const AStream: TStream; const AValue: Integer; const APosition: Int64); overload;
procedure StreamInsertSingle(const AStream: TStream; const AValue: Single); overload;
procedure StreamInsertSingle(const AStream: TStream; const AValue: Single; const APosition: Int64); overload;
procedure StreamInsertShortInt(const AStream: TStream; const AValue: ShortInt); overload;
procedure StreamInsertShortInt(const AStream: TStream; const AValue: ShortInt; const APosition: Int64); overload;
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamInsertShortString(const AStream: TStream; const AValue: ShortString); overload; platform;
  procedure StreamInsertShortString(const AStream: TStream; const AValue: ShortString; const APosition: Int64); overload; platform;
{$ENDIF}
procedure StreamInsertSmallInt(const AStream: TStream; const AValue: SmallInt); overload;
procedure StreamInsertSmallInt(const AStream: TStream; const AValue: SmallInt; const APosition: Int64); overload;
procedure StreamInsertString(const AStream: TStream; const AValue: String); overload;
procedure StreamInsertString(const AStream: TStream; const AValue: String; const APosition: Int64); overload;
procedure StreamInsertStream(const AStream: TStream; const AValue: TStream); overload;
procedure StreamInsertStream(const AStream: TStream; const AValue: TStream; const APosition: Int64); overload;
procedure StreamInsertTime(const AStream: TStream; const AValue: TTime); overload;
procedure StreamInsertTime(const AStream: TStream; const AValue: TTime; const APosition: Int64); overload;
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamInsertWideString(const AStream: TStream; const AValue: WideString); overload; platform;
  procedure StreamInsertWideString(const AStream: TStream; const AValue: WideString; const APosition: Int64); overload; platform;
{$ENDIF}
procedure StreamInsertWord(const AStream: TStream; const AValue: Word); overload;
procedure StreamInsertWord(const AStream: TStream; const AValue: Word; const APosition: Int64); overload;
// Read Methods
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  function StreamReadAnsiChar(const AStream: TStream): AnsiChar; overload; platform;
  function StreamReadAnsiChar(const AStream: TStream; const APosition: Int64): AnsiChar; overload; platform;
  function StreamReadAnsiString(const AStream: TStream): AnsiString; overload; platform;
  function StreamReadAnsiString(const AStream: TStream; const APosition: Int64): AnsiString; overload; platform;
{$ENDIF}
function StreamReadBoolean(const AStream: TStream): Boolean; overload;
function StreamReadBoolean(const AStream: TStream; const APosition: Int64): Boolean; overload;
function StreamReadByte(const AStream: TStream): Byte; overload;
function StreamReadByte(const AStream: TStream; const APosition: Int64): Byte; overload;
function StreamReadCardinal(const AStream: TStream): Cardinal; overload;
function StreamReadCardinal(const AStream: TStream; const APosition: Int64): Cardinal; overload;
function StreamReadChar(const AStream: TStream): Char; overload;
function StreamReadChar(const AStream: TStream; const APosition: Int64): Char; overload;
function StreamReadCurrency(const AStream: TStream): Currency; overload;
function StreamReadCurrency(const AStream: TStream; const APosition: Int64): Currency; overload;
function StreamReadDate(const AStream: TStream): TDate; overload;
function StreamReadDate(const AStream: TStream; const APosition: Int64): TDate; overload;
function StreamReadDateTime(const AStream: TStream): TDateTime; overload;
function StreamReadDateTime(const AStream: TStream; const APosition: Int64): TDateTime; overload;
function StreamReadDouble(const AStream: TStream): Double; overload;
function StreamReadDouble(const AStream: TStream; const APosition: Int64): Double; overload;
function StreamReadExtended(const AStream: TStream): Extended; overload;
function StreamReadExtended(const AStream: TStream; const APosition: Int64): Extended; overload;
function StreamReadGUID(const AStream: TStream): TGUID; overload;
function StreamReadGUID(const AStream: TStream; const APosition: Int64): TGUID; overload;
function StreamReadInteger(const AStream: TStream): Integer; overload;
function StreamReadInteger(const AStream: TStream; const APosition: Int64): Integer; overload;
function StreamReadInt64(const AStream: TStream): Int64; overload;
function StreamReadInt64(const AStream: TStream; const APosition: Int64): Int64; overload;
function StreamReadLongInt(const AStream: TStream): LongInt; overload;
function StreamReadLongInt(const AStream: TStream; const APosition: Int64): LongInt; overload;
function StreamReadSingle(const AStream: TStream): Single; overload;
function StreamReadSingle(const AStream: TStream; const APosition: Int64): Single; overload;
function StreamReadShortInt(const AStream: TStream): ShortInt; overload;
function StreamReadShortInt(const AStream: TStream; const APosition: Int64): ShortInt; overload;
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  function StreamReadShortString(const AStream: TStream): ShortString; overload; platform;
  function StreamReadShortString(const AStream: TStream; const APosition: Int64): ShortString; overload; platform;
{$ENDIF}
function StreamReadSmallInt(const AStream: TStream): SmallInt; overload;
function StreamReadSmallInt(const AStream: TStream; const APosition: Int64): SmallInt; overload;
function StreamReadString(const AStream: TStream): String; overload;
function StreamReadString(const AStream: TStream; const APosition: Int64): String; overload;
procedure StreamReadStream(const AStream: TStream; const AOutStream: TStream); overload;
procedure StreamReadStream(const AStream: TStream; const AOutStream: TStream; const APosition: Int64); overload;
function StreamReadTime(const AStream: TStream): TTime; overload;
function StreamReadTime(const AStream: TStream; const APosition: Int64): TTime; overload;
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  function StreamReadWideString(const AStream: TStream): WideString; overload; platform;
  function StreamReadWideString(const AStream: TStream; const APosition: Int64): WideString; overload; platform;
{$ENDIF}
function StreamReadWord(const AStream: TStream): Word; overload;
function StreamReadWord(const AStream: TStream; const APosition: Int64): Word; overload;
// Write Methods
{$IFDEF MSWINDOWS}
  procedure StreamWriteAnsiChar(const AStream: TStream; const AValue: AnsiChar); overload; platform;
  procedure StreamWriteAnsiChar(const AStream: TStream; const AValue: AnsiChar; const APosition: Int64); overload; platform;
  procedure StreamWriteAnsiString(const AStream: TStream; const AValue: AnsiString); overload; platform;
  procedure StreamWriteAnsiString(const AStream: TStream; const AValue: AnsiString; const APosition: Int64); overload; platform;
{$ENDIF}
procedure StreamWriteBoolean(const AStream: TStream; const AValue: Boolean); overload;
procedure StreamWriteBoolean(const AStream: TStream; const AValue: Boolean; const APosition: Int64); overload;
procedure StreamWriteByte(const AStream: TStream; const AValue: Byte); overload;
procedure StreamWriteByte(const AStream: TStream; const AValue: Byte; const APosition: Int64); overload;
procedure StreamWriteCardinal(const AStream: TStream; const AValue: Cardinal); overload;
procedure StreamWriteCardinal(const AStream: TStream; const AValue: Cardinal; const APosition: Int64); overload;
procedure StreamWriteChar(const AStream: TStream; const AValue: Char); overload;
procedure StreamWriteChar(const AStream: TStream; const AValue: Char; const APosition: Int64); overload;
procedure StreamWriteCurrency(const AStream: TStream; const AValue: Currency); overload;
procedure StreamWriteCurrency(const AStream: TStream; const AValue: Currency; const APosition: Int64); overload;
procedure StreamWriteDate(const AStream: TStream; const AValue: TDate); overload;
procedure StreamWriteDate(const AStream: TStream; const AValue: TDate; const APosition: Int64); overload;
procedure StreamWriteDateTime(const AStream: TStream; const AValue: TDateTime); overload;
procedure StreamWriteDateTime(const AStream: TStream; const AValue: TDateTime; const APosition: Int64); overload;
procedure StreamWriteDouble(const AStream: TStream; const AValue: Double); overload;
procedure StreamWriteDouble(const AStream: TStream; const AValue: Double; const APosition: Int64); overload;
procedure StreamWriteExtended(const AStream: TStream; const AValue: Extended); overload;
procedure StreamWriteExtended(const AStream: TStream; const AValue: Extended; const APosition: Int64); overload;
procedure StreamWriteGUID(const AStream: TStream; const AValue: TGUID); overload;
procedure StreamWriteGUID(const AStream: TStream; const AValue: TGUID; const APosition: Int64); overload;
procedure StreamWriteInteger(const AStream: TStream; const AValue: Integer); overload;
procedure StreamWriteInteger(const AStream: TStream; const AValue: Integer; const APosition: Int64); overload;
procedure StreamWriteInt64(const AStream: TStream; const AValue: Int64); overload;
procedure StreamWriteInt64(const AStream: TStream; const AValue, APosition: Int64); overload;
procedure StreamWriteLongInt(const AStream: TStream; const AValue: Integer); overload;
procedure StreamWriteLongInt(const AStream: TStream; const AValue: Integer; const APosition: Int64); overload;
procedure StreamWriteSingle(const AStream: TStream; const AValue: Single); overload;
procedure StreamWriteSingle(const AStream: TStream; const AValue: Single; const APosition: Int64); overload;
procedure StreamWriteShortInt(const AStream: TStream; const AValue: ShortInt); overload;
procedure StreamWriteShortInt(const AStream: TStream; const AValue: ShortInt; const APosition: Int64); overload;
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamWriteShortString(const AStream: TStream; const AValue: ShortString); overload; platform;
  procedure StreamWriteShortString(const AStream: TStream; const AValue: ShortString; const APosition: Int64); overload; platform;
{$ENDIF}
procedure StreamWriteSmallInt(const AStream: TStream; const AValue: SmallInt); overload;
procedure StreamWriteSmallInt(const AStream: TStream; const AValue: SmallInt; const APosition: Int64); overload;
procedure StreamWriteString(const AStream: TStream; const AValue: String); overload;
procedure StreamWriteString(const AStream: TStream; const AValue: String; const APosition: Int64); overload;
procedure StreamWriteStream(const AStream: TStream; const AValue: TStream); overload;
procedure StreamWriteStream(const AStream: TStream; const AValue: TStream; const APosition: Int64); overload;
procedure StreamWriteTime(const AStream: TStream; const AValue: TTime); overload;
procedure StreamWriteTime(const AStream: TStream; const AValue: TTime; const APosition: Int64); overload;
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamWriteWideString(const AStream: TStream; const AValue: WideString); overload; platform;
  procedure StreamWriteWideString(const AStream: TStream; const AValue: WideString; const APosition: Int64); overload; platform;
{$ENDIF}
procedure StreamWriteWord(const AStream: TStream; const AValue: Word); overload;
procedure StreamWriteWord(const AStream: TStream; const AValue: Word; const APosition: Int64); overload;

// Delete Methods
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamDeleteAnsiChar(const ACaret: ILKStreamCaret); overload; platform;
  procedure StreamDeleteAnsiChar(const ACaret: ILKStreamCaret; const APosition: Int64); overload; platform;
  procedure StreamDeleteAnsiString(const ACaret: ILKStreamCaret); overload; platform;
  procedure StreamDeleteAnsiString(const ACaret: ILKStreamCaret; const APosition: Int64); overload; platform;
{$ENDIF}
procedure StreamDeleteBoolean(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteBoolean(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteByte(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteByte(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteCardinal(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteCardinal(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteChar(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteChar(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteCurrency(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteCurrency(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteDate(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteDate(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteDateTime(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteDateTime(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteDouble(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteDouble(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteExtended(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteExtended(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteGUID(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteGUID(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteInt64(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteInt64(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteInteger(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteInteger(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteLongInt(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteLongInt(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteShortInt(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteShortInt(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteShortString(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteShortString(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteSingle(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteSingle(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteSmallInt(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteSmallInt(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteString(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteString(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteStream(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteStream(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteTime(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteTime(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteWideString(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteWideString(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
procedure StreamDeleteWord(const ACaret: ILKStreamCaret); overload;
procedure StreamDeleteWord(const ACaret: ILKStreamCaret; const APosition: Int64); overload;
// Insert Methods
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamInsertAnsiChar(const ACaret: ILKStreamCaret; const AValue: AnsiChar); overload; platform;
  procedure StreamInsertAnsiChar(const ACaret: ILKStreamCaret; const AValue: AnsiChar; const APosition: Int64); overload; platform;
  procedure StreamInsertAnsiString(const ACaret: ILKStreamCaret; const AValue: AnsiString); overload; platform;
  procedure StreamInsertAnsiString(const ACaret: ILKStreamCaret; const AValue: AnsiString; const APosition: Int64); overload; platform;
{$ENDIF}
procedure StreamInsertBoolean(const ACaret: ILKStreamCaret; const AValue: Boolean); overload;
procedure StreamInsertBoolean(const ACaret: ILKStreamCaret; const AValue: Boolean; const APosition: Int64); overload;
procedure StreamInsertByte(const ACaret: ILKStreamCaret; const AValue: Byte); overload;
procedure StreamInsertByte(const ACaret: ILKStreamCaret; const AValue: Byte; const APosition: Int64); overload;
procedure StreamInsertCardinal(const ACaret: ILKStreamCaret; const AValue: Cardinal); overload;
procedure StreamInsertCardinal(const ACaret: ILKStreamCaret; const AValue: Cardinal; const APosition: Int64); overload;
procedure StreamInsertChar(const ACaret: ILKStreamCaret; const AValue: Char); overload;
procedure StreamInsertChar(const ACaret: ILKStreamCaret; const AValue: Char; const APosition: Int64); overload;
procedure StreamInsertCurrency(const ACaret: ILKStreamCaret; const AValue: Currency); overload;
procedure StreamInsertCurrency(const ACaret: ILKStreamCaret; const AValue: Currency; const APosition: Int64); overload;
procedure StreamInsertDate(const ACaret: ILKStreamCaret; const AValue: TDate); overload;
procedure StreamInsertDate(const ACaret: ILKStreamCaret; const AValue: TDate; const APosition: Int64); overload;
procedure StreamInsertDateTime(const ACaret: ILKStreamCaret; const AValue: TDateTime); overload;
procedure StreamInsertDateTime(const ACaret: ILKStreamCaret; const AValue: TDateTime; const APosition: Int64); overload;
procedure StreamInsertDouble(const ACaret: ILKStreamCaret; const AValue: Double); overload;
procedure StreamInsertDouble(const ACaret: ILKStreamCaret; const AValue: Double; const APosition: Int64); overload;
procedure StreamInsertExtended(const ACaret: ILKStreamCaret; const AValue: Extended); overload;
procedure StreamInsertExtended(const ACaret: ILKStreamCaret; const AValue: Extended; const APosition: Int64); overload;
procedure StreamInsertGUID(const ACaret: ILKStreamCaret; const AValue: TGUID); overload;
procedure StreamInsertGUID(const ACaret: ILKStreamCaret; const AValue: TGUID; const APosition: Int64); overload;
procedure StreamInsertInteger(const ACaret: ILKStreamCaret; const AValue: Integer); overload;
procedure StreamInsertInteger(const ACaret: ILKStreamCaret; const AValue: Integer; const APosition: Int64); overload;
procedure StreamInsertInt64(const ACaret: ILKStreamCaret; const AValue: Int64); overload;
procedure StreamInsertInt64(const ACaret: ILKStreamCaret; const AValue, APosition: Int64); overload;
procedure StreamInsertLongInt(const ACaret: ILKStreamCaret; const AValue: Integer); overload;
procedure StreamInsertLongInt(const ACaret: ILKStreamCaret; const AValue: Integer; const APosition: Int64); overload;
procedure StreamInsertSingle(const ACaret: ILKStreamCaret; const AValue: Single); overload;
procedure StreamInsertSingle(const ACaret: ILKStreamCaret; const AValue: Single; const APosition: Int64); overload;
procedure StreamInsertShortInt(const ACaret: ILKStreamCaret; const AValue: ShortInt); overload;
procedure StreamInsertShortInt(const ACaret: ILKStreamCaret; const AValue: ShortInt; const APosition: Int64); overload;
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamInsertShortString(const ACaret: ILKStreamCaret; const AValue: ShortString); overload; platform;
  procedure StreamInsertShortString(const ACaret: ILKStreamCaret; const AValue: ShortString; const APosition: Int64); overload; platform;
{$ENDIF}
procedure StreamInsertSmallInt(const ACaret: ILKStreamCaret; const AValue: SmallInt); overload;
procedure StreamInsertSmallInt(const ACaret: ILKStreamCaret; const AValue: SmallInt; const APosition: Int64); overload;
procedure StreamInsertString(const ACaret: ILKStreamCaret; const AValue: String); overload;
procedure StreamInsertString(const ACaret: ILKStreamCaret; const AValue: String; const APosition: Int64); overload;
procedure StreamInsertStream(const ACaret: ILKStreamCaret; const AValue: TStream); overload;
procedure StreamInsertStream(const ACaret: ILKStreamCaret; const AValue: TStream; const APosition: Int64); overload;
procedure StreamInsertTime(const ACaret: ILKStreamCaret; const AValue: TTime); overload;
procedure StreamInsertTime(const ACaret: ILKStreamCaret; const AValue: TTime; const APosition: Int64); overload;
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamInsertWideString(const ACaret: ILKStreamCaret; const AValue: WideString); overload; platform;
  procedure StreamInsertWideString(const ACaret: ILKStreamCaret; const AValue: WideString; const APosition: Int64); overload; platform;
{$ENDIF}
procedure StreamInsertWord(const ACaret: ILKStreamCaret; const AValue: Word); overload;
procedure StreamInsertWord(const ACaret: ILKStreamCaret; const AValue: Word; const APosition: Int64); overload;
// Read Methods
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  function StreamReadAnsiChar(const ACaret: ILKStreamCaret): AnsiChar; overload; platform;
  function StreamReadAnsiChar(const ACaret: ILKStreamCaret; const APosition: Int64): AnsiChar; overload; platform;
  function StreamReadAnsiString(const ACaret: ILKStreamCaret): AnsiString; overload; platform;
  function StreamReadAnsiString(const ACaret: ILKStreamCaret; const APosition: Int64): AnsiString; overload; platform;
{$ENDIF}
function StreamReadBoolean(const ACaret: ILKStreamCaret): Boolean; overload;
function StreamReadBoolean(const ACaret: ILKStreamCaret; const APosition: Int64): Boolean; overload;
function StreamReadByte(const ACaret: ILKStreamCaret): Byte; overload;
function StreamReadByte(const ACaret: ILKStreamCaret; const APosition: Int64): Byte; overload;
function StreamReadCardinal(const ACaret: ILKStreamCaret): Cardinal; overload;
function StreamReadCardinal(const ACaret: ILKStreamCaret; const APosition: Int64): Cardinal; overload;
function StreamReadChar(const ACaret: ILKStreamCaret): Char; overload;
function StreamReadChar(const ACaret: ILKStreamCaret; const APosition: Int64): Char; overload;
function StreamReadCurrency(const ACaret: ILKStreamCaret): Currency; overload;
function StreamReadCurrency(const ACaret: ILKStreamCaret; const APosition: Int64): Currency; overload;
function StreamReadDate(const ACaret: ILKStreamCaret): TDate; overload;
function StreamReadDate(const ACaret: ILKStreamCaret; const APosition: Int64): TDate; overload;
function StreamReadDateTime(const ACaret: ILKStreamCaret): TDateTime; overload;
function StreamReadDateTime(const ACaret: ILKStreamCaret; const APosition: Int64): TDateTime; overload;
function StreamReadDouble(const ACaret: ILKStreamCaret): Double; overload;
function StreamReadDouble(const ACaret: ILKStreamCaret; const APosition: Int64): Double; overload;
function StreamReadExtended(const ACaret: ILKStreamCaret): Extended; overload;
function StreamReadExtended(const ACaret: ILKStreamCaret; const APosition: Int64): Extended; overload;
function StreamReadGUID(const ACaret: ILKStreamCaret): TGUID; overload;
function StreamReadGUID(const ACaret: ILKStreamCaret; const APosition: Int64): TGUID; overload;
function StreamReadInteger(const ACaret: ILKStreamCaret): Integer; overload;
function StreamReadInteger(const ACaret: ILKStreamCaret; const APosition: Int64): Integer; overload;
function StreamReadInt64(const ACaret: ILKStreamCaret): Int64; overload;
function StreamReadInt64(const ACaret: ILKStreamCaret; const APosition: Int64): Int64; overload;
function StreamReadLongInt(const ACaret: ILKStreamCaret): LongInt; overload;
function StreamReadLongInt(const ACaret: ILKStreamCaret; const APosition: Int64): LongInt; overload;
function StreamReadSingle(const ACaret: ILKStreamCaret): Single; overload;
function StreamReadSingle(const ACaret: ILKStreamCaret; const APosition: Int64): Single; overload;
function StreamReadShortInt(const ACaret: ILKStreamCaret): ShortInt; overload;
function StreamReadShortInt(const ACaret: ILKStreamCaret; const APosition: Int64): ShortInt; overload;
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  function StreamReadShortString(const ACaret: ILKStreamCaret): ShortString; overload; platform;
  function StreamReadShortString(const ACaret: ILKStreamCaret; const APosition: Int64): ShortString; overload; platform;
{$ENDIF}
function StreamReadSmallInt(const ACaret: ILKStreamCaret): SmallInt; overload;
function StreamReadSmallInt(const ACaret: ILKStreamCaret; const APosition: Int64): SmallInt; overload;
function StreamReadString(const ACaret: ILKStreamCaret): String; overload;
function StreamReadString(const ACaret: ILKStreamCaret; const APosition: Int64): String; overload;
procedure StreamReadStream(const ACaret: ILKStreamCaret; const AOutStream: ILKStreamCaret); overload;
procedure StreamReadStream(const ACaret: ILKStreamCaret; const AOutStream: ILKStreamCaret; const APosition: Int64); overload;
function StreamReadTime(const ACaret: ILKStreamCaret): TTime; overload;
function StreamReadTime(const ACaret: ILKStreamCaret; const APosition: Int64): TTime; overload;
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  function StreamReadWideString(const ACaret: ILKStreamCaret): WideString; overload; platform;
  function StreamReadWideString(const ACaret: ILKStreamCaret; const APosition: Int64): WideString; overload; platform;
{$ENDIF}
function StreamReadWord(const ACaret: ILKStreamCaret): Word; overload;
function StreamReadWord(const ACaret: ILKStreamCaret; const APosition: Int64): Word; overload;
// Write Methods
{$IFDEF MSWINDOWS}
  procedure StreamWriteAnsiChar(const ACaret: ILKStreamCaret; const AValue: AnsiChar); overload; platform;
  procedure StreamWriteAnsiChar(const ACaret: ILKStreamCaret; const AValue: AnsiChar; const APosition: Int64); overload; platform;
  procedure StreamWriteAnsiString(const ACaret: ILKStreamCaret; const AValue: AnsiString); overload; platform;
  procedure StreamWriteAnsiString(const ACaret: ILKStreamCaret; const AValue: AnsiString; const APosition: Int64); overload; platform;
{$ENDIF}
procedure StreamWriteBoolean(const ACaret: ILKStreamCaret; const AValue: Boolean); overload;
procedure StreamWriteBoolean(const ACaret: ILKStreamCaret; const AValue: Boolean; const APosition: Int64); overload;
procedure StreamWriteByte(const ACaret: ILKStreamCaret; const AValue: Byte); overload;
procedure StreamWriteByte(const ACaret: ILKStreamCaret; const AValue: Byte; const APosition: Int64); overload;
procedure StreamWriteCardinal(const ACaret: ILKStreamCaret; const AValue: Cardinal); overload;
procedure StreamWriteCardinal(const ACaret: ILKStreamCaret; const AValue: Cardinal; const APosition: Int64); overload;
procedure StreamWriteChar(const ACaret: ILKStreamCaret; const AValue: Char); overload;
procedure StreamWriteChar(const ACaret: ILKStreamCaret; const AValue: Char; const APosition: Int64); overload;
procedure StreamWriteCurrency(const ACaret: ILKStreamCaret; const AValue: Currency); overload;
procedure StreamWriteCurrency(const ACaret: ILKStreamCaret; const AValue: Currency; const APosition: Int64); overload;
procedure StreamWriteDate(const ACaret: ILKStreamCaret; const AValue: TDate); overload;
procedure StreamWriteDate(const ACaret: ILKStreamCaret; const AValue: TDate; const APosition: Int64); overload;
procedure StreamWriteDateTime(const ACaret: ILKStreamCaret; const AValue: TDateTime); overload;
procedure StreamWriteDateTime(const ACaret: ILKStreamCaret; const AValue: TDateTime; const APosition: Int64); overload;
procedure StreamWriteDouble(const ACaret: ILKStreamCaret; const AValue: Double); overload;
procedure StreamWriteDouble(const ACaret: ILKStreamCaret; const AValue: Double; const APosition: Int64); overload;
procedure StreamWriteExtended(const ACaret: ILKStreamCaret; const AValue: Extended); overload;
procedure StreamWriteExtended(const ACaret: ILKStreamCaret; const AValue: Extended; const APosition: Int64); overload;
procedure StreamWriteGUID(const ACaret: ILKStreamCaret; const AValue: TGUID); overload;
procedure StreamWriteGUID(const ACaret: ILKStreamCaret; const AValue: TGUID; const APosition: Int64); overload;
procedure StreamWriteInteger(const ACaret: ILKStreamCaret; const AValue: Integer); overload;
procedure StreamWriteInteger(const ACaret: ILKStreamCaret; const AValue: Integer; const APosition: Int64); overload;
procedure StreamWriteInt64(const ACaret: ILKStreamCaret; const AValue: Int64); overload;
procedure StreamWriteInt64(const ACaret: ILKStreamCaret; const AValue, APosition: Int64); overload;
procedure StreamWriteLongInt(const ACaret: ILKStreamCaret; const AValue: Integer); overload;
procedure StreamWriteLongInt(const ACaret: ILKStreamCaret; const AValue: Integer; const APosition: Int64); overload;
procedure StreamWriteSingle(const ACaret: ILKStreamCaret; const AValue: Single); overload;
procedure StreamWriteSingle(const ACaret: ILKStreamCaret; const AValue: Single; const APosition: Int64); overload;
procedure StreamWriteShortInt(const ACaret: ILKStreamCaret; const AValue: ShortInt); overload;
procedure StreamWriteShortInt(const ACaret: ILKStreamCaret; const AValue: ShortInt; const APosition: Int64); overload;
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamWriteShortString(const ACaret: ILKStreamCaret; const AValue: ShortString); overload; platform;
  procedure StreamWriteShortString(const ACaret: ILKStreamCaret; const AValue: ShortString; const APosition: Int64); overload; platform;
{$ENDIF}
procedure StreamWriteSmallInt(const ACaret: ILKStreamCaret; const AValue: SmallInt); overload;
procedure StreamWriteSmallInt(const ACaret: ILKStreamCaret; const AValue: SmallInt; const APosition: Int64); overload;
procedure StreamWriteString(const ACaret: ILKStreamCaret; const AValue: String); overload;
procedure StreamWriteString(const ACaret: ILKStreamCaret; const AValue: String; const APosition: Int64); overload;
procedure StreamWriteStream(const ACaret: ILKStreamCaret; const AValue: TStream); overload;
procedure StreamWriteStream(const ACaret: ILKStreamCaret; const AValue: TStream; const APosition: Int64); overload;
procedure StreamWriteTime(const ACaret: ILKStreamCaret; const AValue: TTime); overload;
procedure StreamWriteTime(const ACaret: ILKStreamCaret; const AValue: TTime; const APosition: Int64); overload;
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamWriteWideString(const ACaret: ILKStreamCaret; const AValue: WideString); overload; platform;
  procedure StreamWriteWideString(const ACaret: ILKStreamCaret; const AValue: WideString; const APosition: Int64); overload; platform;
{$ENDIF}
procedure StreamWriteWord(const ACaret: ILKStreamCaret; const AValue: Word); overload;
procedure StreamWriteWord(const ACaret: ILKStreamCaret; const AValue: Word; const APosition: Int64); overload;

implementation

// Utility Methods

{$IFDEF FPC}
  function ByteLength(const AString: String): Int64;
  begin
    Result := Length(AString) * SizeOf(Char);
  end;
{$ENDIF FPC}

// Delete Methods

{$IF Defined(MSWINDOWS) OR Defined(FPC)}
  procedure StreamDeleteAnsiChar(const AStream: TStream);
  begin
    StreamDeleteAnsiChar(AStream, AStream.Position);
  end;

  procedure StreamDeleteAnsiChar(const AStream: TStream; const APosition: Int64);
  begin
    StreamClearSpace(AStream, APosition, SizeOf(AnsiChar));
  end;

  procedure StreamDeleteAnsiString(const AStream: TStream);
  begin
    StreamDeleteAnsiString(AStream, AStream.Position);
  end;

  procedure StreamDeleteAnsiString(const AStream: TStream; const APosition: Int64);
  var
    LStringLength: Int64;
  begin
    AStream.Position := APosition;
    AStream.Read(LStringLength, SizeOf(Int64));
    StreamClearSpace(AStream, APosition, LStringLength + SizeOf(Int64));
  end;
{$ENDIF}

procedure StreamDeleteBoolean(const AStream: TStream);
begin
  StreamDeleteBoolean(AStream, AStream.Position);
end;

procedure StreamDeleteBoolean(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(Boolean));
end;

procedure StreamDeleteByte(const AStream: TStream);
begin
  StreamDeleteByte(AStream, AStream.Position);
end;

procedure StreamDeleteByte(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(Byte));
end;

procedure StreamDeleteCardinal(const AStream: TStream);
begin
  StreamDeleteCardinal(AStream, AStream.Position);
end;

procedure StreamDeleteCardinal(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(Cardinal));
end;

procedure StreamDeleteChar(const AStream: TStream);
begin
  StreamDeleteChar(AStream, AStream.Position);
end;

procedure StreamDeleteChar(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(Char));
end;

procedure StreamDeleteCurrency(const AStream: TStream);
begin
  StreamDeleteCurrency(AStream, AStream.Position);
end;

procedure StreamDeleteCurrency(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(Currency));
end;

procedure StreamDeleteDate(const AStream: TStream);
begin
  StreamDeleteDate(AStream, AStream.Position);
end;

procedure StreamDeleteDate(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(TDate));
end;

procedure StreamDeleteDateTime(const AStream: TStream);
begin
  StreamDeleteDateTime(AStream, AStream.Position);
end;

procedure StreamDeleteDateTime(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(TDateTime));
end;

procedure StreamDeleteDouble(const AStream: TStream);
begin
  StreamDeleteDouble(AStream, AStream.Position);
end;

procedure StreamDeleteDouble(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(Double));
end;

procedure StreamDeleteExtended(const AStream: TStream);
begin
  StreamDeleteExtended(AStream, AStream.Position);
end;

procedure StreamDeleteExtended(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(Extended));
end;

procedure StreamDeleteGUID(const AStream: TStream);
begin
  StreamDeleteGUID(AStream, AStream.Position);
end;

procedure StreamDeleteGUID(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(TGUID));
end;

procedure StreamDeleteInt64(const AStream: TStream);
begin
  StreamDeleteInt64(AStream, AStream.Position);
end;

procedure StreamDeleteInt64(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(Int64));
end;

procedure StreamDeleteInteger(const AStream: TStream);
begin
  StreamDeleteInteger(AStream, AStream.Position);
end;

procedure StreamDeleteInteger(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(Integer));
end;

procedure StreamDeleteLongInt(const AStream: TStream);
begin
  StreamDeleteLongInt(AStream, AStream.Position);
end;

procedure StreamDeleteLongInt(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(LongInt));
end;

procedure StreamDeleteShortInt(const AStream: TStream);
begin
  StreamDeleteShortInt(AStream, AStream.Position);
end;

procedure StreamDeleteShortInt(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(ShortInt));
end;

procedure StreamDeleteShortString(const AStream: TStream);
begin
  StreamDeleteShortString(AStream, AStream.Position);
end;

procedure StreamDeleteShortString(const AStream: TStream; const APosition: Int64);
var
  LStringLength: Int64;
begin
  AStream.Position := APosition;
  AStream.Read(LStringLength, SizeOf(Int64));
  StreamClearSpace(AStream, APosition, LStringLength + SizeOf(Int64));
end;

procedure StreamDeleteSingle(const AStream: TStream);
begin
  StreamDeleteSingle(AStream, AStream.Position);
end;

procedure StreamDeleteSingle(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(Single));
end;

procedure StreamDeleteSmallInt(const AStream: TStream);
begin
  StreamDeleteSmallInt(AStream, AStream.Position);
end;

procedure StreamDeleteSmallInt(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(SmallInt));
end;

procedure StreamDeleteString(const AStream: TStream);
begin
  StreamDeleteString(AStream, AStream.Position);
end;

procedure StreamDeleteString(const AStream: TStream; const APosition: Int64);
var
  LStringLength: Int64;
begin
  AStream.Position := APosition;
  AStream.Read(LStringLength, SizeOf(Int64));
  StreamClearSpace(AStream, APosition, LStringLength + SizeOf(Int64));
end;

procedure StreamDeleteStream(const AStream: TStream; const ADeleteStream: TStream);
begin
  StreamDeleteStream(AStream, ADeleteStream, AStream.Position);
end;

procedure StreamDeleteStream(const AStream: TStream; const ADeleteStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, ADeleteStream.Size + SizeOf(Int64));
end;

procedure StreamDeleteStream(const AStream: TStream);
begin
  StreamDeleteStream(AStream, AStream.Position);
end;

procedure StreamDeleteStream(const AStream: TStream; const APosition: Int64);
var
  LSize: Int64;
begin
  AStream.Position := APosition;
  LSize := StreamReadInt64(AStream);
  StreamDeleteInt64(AStream, APosition);
  StreamClearSpace(AStream, APosition, LSize);
end;

procedure StreamDeleteTime(const AStream: TStream);
begin
  StreamDeleteTime(AStream, AStream.Position);
end;

procedure StreamDeleteTime(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(TTime));
end;

procedure StreamDeleteWideString(const AStream: TStream);
begin
  StreamDeleteWideString(AStream, AStream.Position);
end;

procedure StreamDeleteWideString(const AStream: TStream; const APosition: Int64);
var
  LStringLength: Int64;
begin
  AStream.Position := APosition;
  AStream.Read(LStringLength, SizeOf(Int64));
  StreamClearSpace(AStream, APosition, LStringLength + SizeOf(Int64));
end;

procedure StreamDeleteWord(const AStream: TStream);
begin
  StreamDeleteWord(AStream, AStream.Position);
end;

procedure StreamDeleteWord(const AStream: TStream; const APosition: Int64);
begin
  StreamClearSpace(AStream, APosition, SizeOf(Word));
end;

// Insert Methods
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamInsertAnsiChar(const AStream: TStream; const AValue: AnsiChar);
  begin
    StreamInsertAnsiChar(AStream, AValue, AStream.Position);
  end;

  procedure StreamInsertAnsiChar(const AStream: TStream; const AValue: AnsiChar; const APosition: Int64);
  begin
    StreamMakeSpace(AStream, APosition, SizeOf(AnsiChar));
    AStream.Write(AValue, SizeOf(AnsiChar));
  end;

  procedure StreamInsertAnsiString(const AStream: TStream; const AValue: AnsiString);
  begin
    StreamInsertAnsiString(AStream, AValue, AStream.Position);
  end;

  procedure StreamInsertAnsiString(const AStream: TStream; const AValue: AnsiString; const APosition: Int64);
  var
    LStringLength: Int64;
  begin
    LStringLength := Length(AValue) * SizeOf(AnsiChar);
    StreamMakeSpace(AStream, APosition, LStringLength);
    AStream.Write(LStringLength, SizeOf(Int64));
    AStream.Write(AValue[1], LStringLength);
  end;
{$ENDIF}
procedure StreamInsertBoolean(const AStream: TStream; const AValue: Boolean);
begin
  StreamInsertBoolean(AStream, AValue, AStream.Position);
end;

procedure StreamInsertBoolean(const AStream: TStream; const AValue: Boolean; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Boolean));
  AStream.Write(AValue, SizeOf(Boolean));
end;

procedure StreamInsertByte(const AStream: TStream; const AValue: Byte);
begin
  StreamInsertByte(AStream, AValue, AStream.Position);
end;

procedure StreamInsertByte(const AStream: TStream; const AValue: Byte; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Byte));
  AStream.Write(AValue, SizeOf(Byte));
end;

procedure StreamInsertCardinal(const AStream: TStream; const AValue: Cardinal);
begin
  StreamInsertCardinal(AStream, AValue, AStream.Position);
end;

procedure StreamInsertCardinal(const AStream: TStream; const AValue: Cardinal; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Cardinal));
  AStream.Write(AValue, SizeOf(Cardinal));
end;

procedure StreamInsertChar(const AStream: TStream; const AValue: Char);
begin
  StreamInsertChar(AStream, AValue, AStream.Position);
end;

procedure StreamInsertChar(const AStream: TStream; const AValue: Char; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Char));
  AStream.Write(AValue, SizeOf(Char));
end;

procedure StreamInsertCurrency(const AStream: TStream; const AValue: Currency);
begin
  StreamInsertCurrency(AStream, AValue, AStream.Position);
end;

procedure StreamInsertCurrency(const AStream: TStream; const AValue: Currency; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Currency));
  AStream.Write(AValue, SizeOf(Currency));
end;

procedure StreamInsertDate(const AStream: TStream; const AValue: TDate);
begin
  StreamInsertDate(AStream, AValue, AStream.Position);
end;

procedure StreamInsertDate(const AStream: TStream; const AValue: TDate; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Date));
  AStream.Write(AValue, SizeOf(TDate));
end;

procedure StreamInsertDateTime(const AStream: TStream; const AValue: TDateTime);
begin
  StreamInsertDateTime(AStream, AValue, AStream.Position);
end;

procedure StreamInsertDateTime(const AStream: TStream; const AValue: TDateTime; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(TDateTime));
  AStream.Write(AValue, SizeOf(TDateTime));
end;

procedure StreamInsertDouble(const AStream: TStream; const AValue: Double);
begin
  StreamInsertDouble(AStream, AValue, AStream.Position);
end;

procedure StreamInsertDouble(const AStream: TStream; const AValue: Double; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Double));
  AStream.Write(AValue, SizeOf(Double));
end;

procedure StreamInsertExtended(const AStream: TStream; const AValue: Extended);
begin
  StreamInsertExtended(AStream, AValue, AStream.Position);
end;

procedure StreamInsertExtended(const AStream: TStream; const AValue: Extended; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Extended));
  AStream.Write(AValue, SizeOf(Extended));
end;

procedure StreamInsertGUID(const AStream: TStream; const AValue: TGUID);
begin
  StreamInsertGUID(AStream, AValue, AStream.Position);
end;

procedure StreamInsertGUID(const AStream: TStream; const AValue: TGUID; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(AValue));
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure StreamInsertInteger(const AStream: TStream; const AValue: Integer);
begin
  StreamInsertInteger(AStream, AValue, AStream.Position);
end;

procedure StreamInsertInteger(const AStream: TStream; const AValue: Integer; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Integer));
  AStream.Write(AValue, SizeOf(Integer));
end;

procedure StreamInsertInt64(const AStream: TStream; const AValue: Int64);
begin
  StreamInsertInt64(AStream, AValue, AStream.Position);
end;

procedure StreamInsertInt64(const AStream: TStream; const AValue, APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Int64));
  AStream.Write(AValue, SizeOf(Int64));
end;

procedure StreamInsertLongInt(const AStream: TStream; const AValue: Integer);
begin
  StreamInsertLongInt(AStream, AValue, AStream.Position);
end;

procedure StreamInsertLongInt(const AStream: TStream; const AValue: Integer; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(LongInt));
  AStream.Write(AValue, SizeOf(LongInt));
end;

procedure StreamInsertSingle(const AStream: TStream; const AValue: Single);
begin
  StreamInsertSingle(AStream, AValue, AStream.Position);
end;

procedure StreamInsertSingle(const AStream: TStream; const AValue: Single; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Single));
  AStream.Write(AValue, SizeOf(Single));
end;

procedure StreamInsertShortInt(const AStream: TStream; const AValue: ShortInt);
begin
  StreamInsertShortInt(AStream, AValue, AStream.Position);
end;

procedure StreamInsertShortInt(const AStream: TStream; const AValue: ShortInt; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(ShortInt));
  AStream.Write(AValue, SizeOf(ShortInt));
end;

{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamInsertShortString(const AStream: TStream; const AValue: ShortString);
  begin
    StreamInsertShortString(AStream, AValue, AStream.Position);
  end;

  procedure StreamInsertShortString(const AStream: TStream; const AValue: ShortString; const APosition: Int64);
  var
    LStringLength: Int64;
  begin
    LStringLength := Length(AValue) * SizeOf(AnsiChar);
    StreamMakeSpace(AStream, APosition, LStringLength);
    AStream.Write(LStringLength, SizeOf(Int64));
    AStream.Write(AValue[1], LStringLength);
  end;
{$ENDIF}

procedure StreamInsertSmallInt(const AStream: TStream; const AValue: SmallInt);
begin
  StreamInsertSmallInt(AStream, AValue, AStream.Position);
end;

procedure StreamInsertSmallInt(const AStream: TStream; const AValue: SmallInt; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(SmallInt));
  AStream.Write(AValue, SizeOf(SmallInt));
end;

procedure StreamInsertString(const AStream: TStream; const AValue: String);
begin
  StreamInsertString(AStream, AValue, AStream.Position);
end;

procedure StreamInsertString(const AStream: TStream; const AValue: String; const APosition: Int64);
var
  LStringLength: Int64;
begin
  LStringLength := ByteLength(AValue);
  StreamMakeSpace(AStream, APosition, LStringLength);
  AStream.Write(LStringLength, SizeOf(Int64));
  AStream.Write(AValue[1], LStringLength);
end;

procedure StreamInsertStream(const AStream: TStream; const AValue: TStream);
begin
  StreamInsertStream(AStream, AValue, AStream.Position);
end;

procedure StreamInsertStream(const AStream: TStream; const AValue: TStream; const APosition: Int64);
begin
  StreamInsertInt64(AStream, AValue.Size); // We need to know the size of the stream...
  StreamMakeSpace(AStream, AStream.Position, AValue.Size);
  AStream.CopyFrom(AValue, AValue.Size);
end;

procedure StreamInsertTime(const AStream: TStream; const AValue: TTime);
begin
  StreamInsertTime(AStream, AValue, AStream.Position);
end;

procedure StreamInsertTime(const AStream: TStream; const AValue: TTime; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(TTime));
  AStream.Write(AValue, SizeOf(TTime));
end;

{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamInsertWideString(const AStream: TStream; const AValue: WideString);
  begin
    StreamInsertWideString(AStream, AValue, AStream.Position);
  end;

  procedure StreamInsertWideString(const AStream: TStream; const AValue: WideString; const APosition: Int64);
  var
    LStringLength: Int64;
  begin
    LStringLength := ByteLength(AValue);
    StreamMakeSpace(AStream, APosition, LStringLength);
    AStream.Write(LStringLength, SizeOf(Int64));
    AStream.Write(AValue[1], ByteLength(AValue));
  end;
{$ENDIF}

procedure StreamInsertWord(const AStream: TStream; const AValue: Word);
begin
  StreamInsertWord(AStream, AValue, AStream.Position);
end;

procedure StreamInsertWord(const AStream: TStream; const AValue: Word; const APosition: Int64);
begin
  StreamMakeSpace(AStream, APosition, SizeOf(Word));
  AStream.Write(AValue, SizeOf(Word));
end;

// Read Methods

{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  function StreamReadAnsiChar(const AStream: TStream): AnsiChar;
  begin
    Result := StreamReadAnsiChar(AStream, AStream.Position);
  end;

  function StreamReadAnsiChar(const AStream: TStream; const APosition: Int64): AnsiChar;
  begin
    AStream.Position := APosition;
    AStream.Read(Result, SizeOf(AnsiChar));
  end;

  function StreamReadAnsiString(const AStream: TStream): AnsiString;
  begin
    Result := StreamReadAnsiString(AStream, AStream.Position);
  end;

  function StreamReadAnsiString(const AStream: TStream; const APosition: Int64): AnsiString;
  var
    LStringLength: Int64;
  begin
    AStream.Position := APosition;
    AStream.Read(LStringLength, SizeOf(Int64));
    SetLength(Result, LStringLength);
    AStream.Read(Result[1], LStringLength);
  end;
{$ENDIF}

function StreamReadBoolean(const AStream: TStream): Boolean;
begin
  Result := StreamReadBoolean(AStream, AStream.Position);
end;

function StreamReadBoolean(const AStream: TStream; const APosition: Int64): Boolean;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(Boolean));
end;

function StreamReadByte(const AStream: TStream): Byte;
begin
  Result := StreamReadByte(AStream, AStream.Position);
end;

function StreamReadByte(const AStream: TStream; const APosition: Int64): Byte;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(Byte));
end;

function StreamReadCardinal(const AStream: TStream): Cardinal;
begin
  Result := StreamReadCardinal(AStream, AStream.Position);
end;

function StreamReadCardinal(const AStream: TStream; const APosition: Int64): Cardinal;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(Result));
end;

function StreamReadChar(const AStream: TStream): Char;
begin
  Result := StreamReadChar(AStream, AStream.Position);
end;

function StreamReadChar(const AStream: TStream; const APosition: Int64): Char;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(Char));
end;

function StreamReadCurrency(const AStream: TStream): Currency;
begin
  Result := StreamReadCurrency(AStream, AStream.Position);
end;

function StreamReadCurrency(const AStream: TStream; const APosition: Int64): Currency;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(Currency));
end;

function StreamReadDate(const AStream: TStream): TDate;
begin
  Result := StreamReadDate(AStream, AStream.Position);
end;

function StreamReadDate(const AStream: TStream; const APosition: Int64): TDate;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(TDate));
end;

function StreamReadDateTime(const AStream: TStream): TDateTime;
begin
  Result := StreamReadDateTime(AStream, AStream.Position);
end;

function StreamReadDateTime(const AStream: TStream; const APosition: Int64): TDateTime;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(TDateTime));
end;

function StreamReadDouble(const AStream: TStream): Double;
begin
  Result := StreamReadDouble(AStream, AStream.Position);
end;

function StreamReadDouble(const AStream: TStream; const APosition: Int64): Double;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(Double));
end;

function StreamReadExtended(const AStream: TStream): Extended;
begin
  Result := StreamReadExtended(AStream, AStream.Position);
end;

function StreamReadExtended(const AStream: TStream; const APosition: Int64): Extended;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(Extended));
end;

function StreamReadGUID(const AStream: TStream): TGUID;
begin
  Result := StreamReadGUID(AStream, AStream.Position);
end;

function StreamReadGUID(const AStream: TStream; const APosition: Int64): TGUID;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(Result));
end;

function StreamReadInteger(const AStream: TStream): Integer;
begin
  Result := StreamReadInteger(AStream, AStream.Position);
end;

function StreamReadInteger(const AStream: TStream; const APosition: Int64): Integer;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(Integer));
end;

function StreamReadInt64(const AStream: TStream): Int64;
begin
  Result := StreamReadInt64(AStream, AStream.Position);
end;

function StreamReadInt64(const AStream: TStream; const APosition: Int64): Int64;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(Int64));
end;

function StreamReadLongInt(const AStream: TStream): LongInt;
begin
  Result := StreamReadLongInt(AStream, AStream.Position);
end;

function StreamReadLongInt(const AStream: TStream; const APosition: Int64): LongInt;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(LongInt));
end;

function StreamReadSingle(const AStream: TStream): Single;
begin
  Result := StreamReadSingle(AStream, AStream.Position);
end;

function StreamReadSingle(const AStream: TStream; const APosition: Int64): Single;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(Single));
end;

function StreamReadShortInt(const AStream: TStream): ShortInt;
begin
  Result := StreamReadShortInt(AStream, AStream.Position);
end;

function StreamReadShortInt(const AStream: TStream; const APosition: Int64): ShortInt;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(ShortInt));
end;

{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  function StreamReadShortString(const AStream: TStream): ShortString;
  begin
    Result := StreamReadShortString(AStream, AStream.Position);
  end;

  function StreamReadShortString(const AStream: TStream; const APosition: Int64): ShortString;
  var
    LStringLength: Int64;
  begin
    AStream.Position := APosition;
    AStream.Read(LStringLength, SizeOf(Int64));
    SetLength(Result, LStringLength);
    AStream.Read(Result[1], LStringLength);
  end;
{$ENDIF}

function StreamReadSmallInt(const AStream: TStream): SmallInt;
begin
  Result := StreamReadSmallInt(AStream, AStream.Position);
end;

function StreamReadSmallInt(const AStream: TStream; const APosition: Int64): SmallInt;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(SmallInt));
end;

function StreamReadString(const AStream: TStream): String;
begin
  Result := StreamReadString(AStream, AStream.Position);
end;

function StreamReadString(const AStream: TStream; const APosition: Int64): String;
var
  LStringLength: Int64;
  LValue: TBytes;
begin
  AStream.Position := APosition;
  AStream.Read(LStringLength, SizeOf(Int64));
  SetLength(LValue, LStringLength);
  AStream.Read(LValue[0], LStringLength);
  {$IFDEF FPC}
    Result := UTF8CStringToUTF8String(PChar(LValue), Length(LValue));
  {$ELSE}
    Result := TEncoding.Unicode.GetString(LValue);
  {$ENDIF FPC}
end;

procedure StreamReadStream(const AStream: TStream; const AOutStream: TStream);
begin
  StreamReadStream(AStream, AOutStream, AStream.Position);
end;

procedure StreamReadStream(const AStream: TStream; const AOutStream: TStream; const APosition: Int64);
var
  LSize: Int64;
begin
  LSize := StreamReadInt64(AStream, APosition);
  AOutStream.CopyFrom(AStream, LSize);
  AOutStream.Position := 0;
end;

function StreamReadTime(const AStream: TStream): TTime;
begin
  Result := StreamReadTime(AStream, AStream.Position);
end;

function StreamReadTime(const AStream: TStream; const APosition: Int64): TTime;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(TTime));
end;

{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  function StreamReadWideString(const AStream: TStream): WideString;
  begin
    Result := StreamReadWideString(AStream, AStream.Position);
  end;

  function StreamReadWideString(const AStream: TStream; const APosition: Int64): WideString;
  var
    LStringLength: Int64;
    LValue: TBytes;
  begin
    AStream.Position := APosition;
    AStream.Read(LStringLength, SizeOf(Int64));
    SetLength(LValue, LStringLength);
    AStream.Read(LValue[0], LStringLength);
    {$IFDEF FPC}
      Result := UTF8CStringToUTF8String(PChar(LValue), Length(LValue));
    {$ELSE}
      Result := TEncoding.Unicode.GetString(LValue);
    {$ENDIF FPC}
  end;
{$ENDIF}

function StreamReadWord(const AStream: TStream): Word;
begin
  Result := StreamReadWord(AStream, AStream.Position);
end;

function StreamReadWord(const AStream: TStream; const APosition: Int64): Word;
begin
  AStream.Position := APosition;
  AStream.Read(Result, SizeOf(Word));
end;

// Write Methods

{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamWriteAnsiChar(const AStream: TStream; const AValue: AnsiChar);
  begin
    StreamWriteAnsiChar(AStream, AValue, AStream.Size);
  end;

  procedure StreamWriteAnsiChar(const AStream: TStream; const AValue: AnsiChar; const APosition: Int64);
  begin
    AStream.Position := APosition;
    AStream.Write(AValue, SizeOf(AnsiChar));
  end;

  procedure StreamWriteAnsiString(const AStream: TStream; const AValue: AnsiString);
  begin
    StreamWriteAnsiString(AStream, AValue, AStream.Size);
  end;

  procedure StreamWriteAnsiString(const AStream: TStream; const AValue: AnsiString; const APosition: Int64);
  var
    LStringLength: Int64;
  begin
    AStream.Position := APosition;
    LStringLength := Length(AValue) * SizeOf(AnsiChar);
    AStream.Write(LStringLength, SizeOf(Int64));
    AStream.Write(AValue[1], LStringLength);
  end;
{$ENDIF}

procedure StreamWriteBoolean(const AStream: TStream; const AValue: Boolean);
begin
  StreamWriteBoolean(AStream, AValue, AStream.Size);
end;

procedure StreamWriteBoolean(const AStream: TStream; const AValue: Boolean; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(Boolean));
end;

procedure StreamWriteByte(const AStream: TStream; const AValue: Byte);
begin
  StreamWriteByte(AStream, AValue, AStream.Size);
end;

procedure StreamWriteByte(const AStream: TStream; const AValue: Byte; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(Byte));
end;

procedure StreamWriteCardinal(const AStream: TStream; const AValue: Cardinal);
begin
  StreamWriteCardinal(AStream, AValue, AStream.Size);
end;

procedure StreamWriteCardinal(const AStream: TStream; const AValue: Cardinal; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteChar(const AStream: TStream; const AValue: Char);
begin
  StreamWriteChar(AStream, AValue, AStream.Size);
end;

procedure StreamWriteChar(const AStream: TStream; const AValue: Char; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(Char));
end;

procedure StreamWriteCurrency(const AStream: TStream; const AValue: Currency);
begin
  StreamWriteCurrency(AStream, AValue, AStream.Size);
end;

procedure StreamWriteCurrency(const AStream: TStream; const AValue: Currency; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(Currency));
end;

procedure StreamWriteDate(const AStream: TStream; const AValue: TDate);
begin
  StreamWriteDate(AStream, AValue, AStream.Size);
end;

procedure StreamWriteDate(const AStream: TStream; const AValue: TDate; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(TDate));
end;

procedure StreamWriteDateTime(const AStream: TStream; const AValue: TDateTime);
begin
  StreamWriteDateTime(AStream, AValue, AStream.Size);
end;

procedure StreamWriteDateTime(const AStream: TStream; const AValue: TDateTime; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(TDateTime));
end;

procedure StreamWriteDouble(const AStream: TStream; const AValue: Double);
begin
  StreamWriteDouble(AStream, AValue, AStream.Size);
end;

procedure StreamWriteDouble(const AStream: TStream; const AValue: Double; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(Double));
end;

procedure StreamWriteExtended(const AStream: TStream; const AValue: Extended);
begin
  StreamWriteExtended(AStream, AValue, AStream.Size);
end;

procedure StreamWriteExtended(const AStream: TStream; const AValue: Extended; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(Extended));
end;

procedure StreamWriteGUID(const AStream: TStream; const AValue: TGUID);
begin
  StreamWriteGUID(AStream, AValue, AStream.Size);
end;

procedure StreamWriteGUID(const AStream: TStream; const AValue: TGUID; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteInteger(const AStream: TStream; const AValue: Integer);
begin
  StreamWriteInteger(AStream, AValue, AStream.Size);
end;

procedure StreamWriteInteger(const AStream: TStream; const AValue: Integer; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(Integer));
end;

procedure StreamWriteInt64(const AStream: TStream; const AValue: Int64);
begin
  StreamWriteInt64(AStream, AValue, AStream.Size);
end;

procedure StreamWriteInt64(const AStream: TStream; const AValue, APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(Int64));
end;

procedure StreamWriteLongInt(const AStream: TStream; const AValue: Integer);
begin
  StreamWriteLongInt(AStream, AValue, AStream.Size);
end;

procedure StreamWriteLongInt(const AStream: TStream; const AValue: Integer; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(LongInt));
end;

procedure StreamWriteSingle(const AStream: TStream; const AValue: Single);
begin
  StreamWriteSingle(AStream, AValue, AStream.Size);
end;

procedure StreamWriteSingle(const AStream: TStream; const AValue: Single; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(Single));
end;

procedure StreamWriteShortInt(const AStream: TStream; const AValue: ShortInt);
begin
  StreamWriteShortInt(AStream, AValue, AStream.Size);
end;

procedure StreamWriteShortInt(const AStream: TStream; const AValue: ShortInt; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(ShortInt));
end;

{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamWriteShortString(const AStream: TStream; const AValue: ShortString);
  begin
    StreamWriteShortString(AStream, AValue, AStream.Size);
  end;

  procedure StreamWriteShortString(const AStream: TStream; const AValue: ShortString; const APosition: Int64);
  var
    LStringLength: Int64;
  begin
    LStringLength := Length(AValue) * SizeOf(AnsiChar);
    AStream.Position := APosition;
    AStream.Write(LStringLength, SizeOf(Int64));
    AStream.Write(AValue[1], LStringLength);
  end;
{$ENDIF}

procedure StreamWriteSmallInt(const AStream: TStream; const AValue: SmallInt);
begin
  StreamWriteSmallInt(AStream, AValue, AStream.Size);
end;

procedure StreamWriteSmallInt(const AStream: TStream; const AValue: SmallInt; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(SmallInt));
end;

procedure StreamWriteString(const AStream: TStream; const AValue: String);
begin
  StreamWriteString(AStream, AValue, AStream.Size);
end;

procedure StreamWriteString(const AStream: TStream; const AValue: String; const APosition: Int64);
var
  LStringLength: Int64;
begin
  AStream.Position := APosition;
  LStringLength := ByteLength(AValue);
  AStream.Write(LStringLength, SizeOf(Int64));
  AStream.Write(AValue[1], LStringLength);
end;

procedure StreamWriteStream(const AStream: TStream; const AValue: TStream);
begin
  StreamWriteStream(AStream, AValue, AStream.Position);
end;

procedure StreamWriteStream(const AStream: TStream; const AValue: TStream; const APosition: Int64);
begin
  AValue.Position := 0;
  StreamWriteInt64(AStream, AValue.Size, APosition); // We need to know the size first...
  AStream.CopyFrom(AValue, AValue.Size);
end;

procedure StreamWriteTime(const AStream: TStream; const AValue: TTime);
begin
  StreamWriteTime(AStream, AValue, AStream.Size);
end;

procedure StreamWriteTime(const AStream: TStream; const AValue: TTime; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(TTime));
end;

{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamWriteWideString(const AStream: TStream; const AValue: WideString);
  begin
    StreamWriteWideString(AStream, AValue, AStream.Size);
  end;

  procedure StreamWriteWideString(const AStream: TStream; const AValue: WideString; const APosition: Int64);
  var
    LStringLength: Int64;
  begin
    LStringLength := ByteLength(AValue);
    AStream.Position := APosition;
    AStream.Write(LStringLength, SizeOf(Int64));
    AStream.Write(AValue[1], LStringLength);
  end;
{$ENDIF}

procedure StreamWriteWord(const AStream: TStream; const AValue: Word);
begin
  StreamWriteWord(AStream, AValue, AStream.Size);
end;

procedure StreamWriteWord(const AStream: TStream; const AValue: Word; const APosition: Int64);
begin
  AStream.Position := APosition;
  AStream.Write(AValue, SizeOf(Word));
end;

// Delete Methods

{$IF Defined(MSWINDOWS) OR Defined(FPC)}
  procedure StreamDeleteAnsiChar(const ACaret: ILKStreamCaret);
  begin
    ACaret.Delete(SizeOf(AnsiChar));
  end;

  procedure StreamDeleteAnsiChar(const ACaret: ILKStreamCaret; const APosition: Int64);
  begin
    ACaret.Position := APosition;
    StreamDeleteAnsiChar(ACaret);
  end;

  procedure StreamDeleteAnsiString(const ACaret: ILKStreamCaret);
  var
    LStringLength: Int64;
  begin
    ACaret.Read(LStringLength, SizeOf(Int64));
    ACaret.Position := ACaret.Position - SizeOf(Int64);
    ACaret.Delete(LStringLength + SizeOf(Int64));
  end;

  procedure StreamDeleteAnsiString(const ACaret: ILKStreamCaret; const APosition: Int64);
  begin
    ACaret.Position := APosition;
    StreamDeleteAnsiString(ACaret);
  end;
{$ENDIF}

procedure StreamDeleteBoolean(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(Boolean));
end;

procedure StreamDeleteBoolean(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteBoolean(ACaret);
end;

procedure StreamDeleteByte(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(Byte));
end;

procedure StreamDeleteByte(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteByte(ACaret);
end;

procedure StreamDeleteCardinal(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(Cardinal));
end;

procedure StreamDeleteCardinal(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteCardinal(ACaret);
end;

procedure StreamDeleteChar(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(Char));
end;

procedure StreamDeleteChar(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteChar(ACaret);
end;

procedure StreamDeleteCurrency(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(Currency));
end;

procedure StreamDeleteCurrency(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteCurrency(ACaret);
end;

procedure StreamDeleteDate(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(TDate));
end;

procedure StreamDeleteDate(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteDate(ACaret);
end;

procedure StreamDeleteDateTime(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(TDateTime));
end;

procedure StreamDeleteDateTime(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteDateTime(ACaret);
end;

procedure StreamDeleteDouble(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(Double));
end;

procedure StreamDeleteDouble(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteDouble(ACaret);
end;

procedure StreamDeleteExtended(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(Extended));
end;

procedure StreamDeleteExtended(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteExtended(ACaret);
end;

procedure StreamDeleteGUID(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(TGUID));
end;

procedure StreamDeleteGUID(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteGUID(ACaret);
end;

procedure StreamDeleteInt64(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(Int64));
end;

procedure StreamDeleteInt64(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteInt64(ACaret);
end;

procedure StreamDeleteInteger(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(Integer));
end;

procedure StreamDeleteInteger(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteInteger(ACaret);
end;

procedure StreamDeleteLongInt(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(LongInt));
end;

procedure StreamDeleteLongInt(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteLongInt(ACaret);
end;

procedure StreamDeleteShortInt(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(ShortInt));
end;

procedure StreamDeleteShortInt(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteShortInt(ACaret);
end;

procedure StreamDeleteShortString(const ACaret: ILKStreamCaret);
var
  LStringLength: Int64;
begin
  ACaret.Read(LStringLength, SizeOf(Int64));
  ACaret.Position := ACaret.Position - SizeOf(Int64);
  ACaret.Delete(LStringLength + SizeOf(Int64));
end;

procedure StreamDeleteShortString(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteShortString(ACaret);
end;

procedure StreamDeleteSingle(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(Single));
end;

procedure StreamDeleteSingle(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteSingle(ACaret);
end;

procedure StreamDeleteSmallInt(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(SmallInt));
end;

procedure StreamDeleteSmallInt(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteSmallInt(ACaret);
end;

procedure StreamDeleteString(const ACaret: ILKStreamCaret);
var
  LStringLength: Int64;
begin
  ACaret.Read(LStringLength, SizeOf(Int64));
  ACaret.Position := ACaret.Position - SizeOf(Int64);
  ACaret.Delete(LStringLength - SizeOf(Int64));
end;

procedure StreamDeleteString(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteString(ACaret);
end;

procedure StreamDeleteStream(const ACaret: ILKStreamCaret);
var
  LStreamLength: Int64;
begin
  ACaret.Read(LStreamLength, SizeOf(Int64));
  ACaret.Position := ACaret.Position - SizeOf(Int64);
  ACaret.Delete(LStreamLength + SizeOf(Int64));
end;

procedure StreamDeleteStream(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteStream(ACaret);
end;

procedure StreamDeleteTime(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(TTime));
end;

procedure StreamDeleteTime(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteTime(ACaret);
end;

procedure StreamDeleteWideString(const ACaret: ILKStreamCaret);
var
  LStringLength: Int64;
begin
  ACaret.Read(LStringLength, SizeOf(Int64));
  ACaret.Position := ACaret.Position - SizeOf(Int64);
  ACaret.Delete(LStringLength + SizeOf(Int64));
end;

procedure StreamDeleteWideString(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteWideString(ACaret);
end;

procedure StreamDeleteWord(const ACaret: ILKStreamCaret);
begin
  ACaret.Delete(SizeOf(Word));
end;

procedure StreamDeleteWord(const ACaret: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamDeleteWord(ACaret);
end;

// Insert Methods
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamInsertAnsiChar(const ACaret: ILKStreamCaret; const AValue: AnsiChar);
  begin
    ACaret.Insert(AValue, SizeOf(AnsiChar));
  end;

  procedure StreamInsertAnsiChar(const ACaret: ILKStreamCaret; const AValue: AnsiChar; const APosition: Int64);
  begin
    ACaret.Position := APosition;
    StreamInsertAnsiChar(ACaret, AValue);
  end;

  procedure StreamInsertAnsiString(const ACaret: ILKStreamCaret; const AValue: AnsiString);
  var
    LStringLength: Int64;
  begin
    LStringLength := Length(AValue) * SizeOf(AnsiChar);
    ACaret.Insert(LStringLength, SizeOf(Int64));
    ACaret.Insert(AValue[1], LStringLength);
  end;

  procedure StreamInsertAnsiString(const ACaret: ILKStreamCaret; const AValue: AnsiString; const APosition: Int64);
  begin
    ACaret.Position := APosition;
    StreamInsertAnsiString(ACaret, AValue);
  end;
{$ENDIF}
procedure StreamInsertBoolean(const ACaret: ILKStreamCaret; const AValue: Boolean);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertBoolean(const ACaret: ILKStreamCaret; const AValue: Boolean; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertBoolean(ACaret, AValue);
end;

procedure StreamInsertByte(const ACaret: ILKStreamCaret; const AValue: Byte);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertByte(const ACaret: ILKStreamCaret; const AValue: Byte; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertByte(ACaret, AValue);
end;

procedure StreamInsertCardinal(const ACaret: ILKStreamCaret; const AValue: Cardinal);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertCardinal(const ACaret: ILKStreamCaret; const AValue: Cardinal; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertCardinal(ACaret, AValue);
end;

procedure StreamInsertChar(const ACaret: ILKStreamCaret; const AValue: Char);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertChar(const ACaret: ILKStreamCaret; const AValue: Char; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertChar(ACaret, AValue);
end;

procedure StreamInsertCurrency(const ACaret: ILKStreamCaret; const AValue: Currency);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertCurrency(const ACaret: ILKStreamCaret; const AValue: Currency; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertCurrency(ACaret, AValue);
end;

procedure StreamInsertDate(const ACaret: ILKStreamCaret; const AValue: TDate);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertDate(const ACaret: ILKStreamCaret; const AValue: TDate; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertDate(ACaret, AValue);
end;

procedure StreamInsertDateTime(const ACaret: ILKStreamCaret; const AValue: TDateTime);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertDateTime(const ACaret: ILKStreamCaret; const AValue: TDateTime; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertDateTime(ACaret, AValue);
end;

procedure StreamInsertDouble(const ACaret: ILKStreamCaret; const AValue: Double);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertDouble(const ACaret: ILKStreamCaret; const AValue: Double; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertDouble(ACaret, AValue);
end;

procedure StreamInsertExtended(const ACaret: ILKStreamCaret; const AValue: Extended);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertExtended(const ACaret: ILKStreamCaret; const AValue: Extended; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertExtended(ACaret, AValue);
end;

procedure StreamInsertGUID(const ACaret: ILKStreamCaret; const AValue: TGUID);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertGUID(const ACaret: ILKStreamCaret; const AValue: TGUID; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertGUID(ACaret, AValue);
end;

procedure StreamInsertInteger(const ACaret: ILKStreamCaret; const AValue: Integer);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertInteger(const ACaret: ILKStreamCaret; const AValue: Integer; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertInteger(ACaret, AValue);
end;

procedure StreamInsertInt64(const ACaret: ILKStreamCaret; const AValue: Int64);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertInt64(const ACaret: ILKStreamCaret; const AValue, APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertInt64(ACaret, AValue);
end;

procedure StreamInsertLongInt(const ACaret: ILKStreamCaret; const AValue: Integer);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertLongInt(const ACaret: ILKStreamCaret; const AValue: Integer; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertLongInt(ACaret, AValue);
end;

procedure StreamInsertSingle(const ACaret: ILKStreamCaret; const AValue: Single);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertSingle(const ACaret: ILKStreamCaret; const AValue: Single; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertSingle(ACaret, AValue);
end;

procedure StreamInsertShortInt(const ACaret: ILKStreamCaret; const AValue: ShortInt);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertShortInt(const ACaret: ILKStreamCaret; const AValue: ShortInt; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertShortInt(ACaret, AValue);
end;

{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamInsertShortString(const ACaret: ILKStreamCaret; const AValue: ShortString);
  var
    LStringLength: Int64;
  begin
    LStringLength := Length(AValue) * SizeOf(AnsiChar);
    ACaret.Insert(LStringLength, SizeOf(Int64));
    ACaret.Insert(AValue[1], LStringLength);
  end;

  procedure StreamInsertShortString(const ACaret: ILKStreamCaret; const AValue: ShortString; const APosition: Int64);
  begin
    ACaret.Position := APosition;
    StreamInsertShortString(ACaret, AValue);
  end;
{$ENDIF}

procedure StreamInsertSmallInt(const ACaret: ILKStreamCaret; const AValue: SmallInt);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertSmallInt(const ACaret: ILKStreamCaret; const AValue: SmallInt; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertSmallInt(ACaret, AValue);
end;

procedure StreamInsertString(const ACaret: ILKStreamCaret; const AValue: String);
var
  LStringLength: Int64;
begin
  LStringLength := ByteLength(AValue);
  ACaret.Insert(LStringLength, SizeOf(Int64));
  ACaret.Insert(AValue[1], LStringLength);
end;

procedure StreamInsertString(const ACaret: ILKStreamCaret; const AValue: String; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertString(ACaret, AValue);
end;

procedure StreamInsertStream(const ACaret: ILKStreamCaret; const AValue: TStream);
var
  I: Int64;
  LByte: Byte;
begin
  ACaret.AcquireWriteLock;
  try
    StreamInsertInt64(ACaret, AValue.Size); // We need to know the size of the stream...
    I := 0;
    repeat
      AValue.Position := I;
      AValue.Read(LByte, 1);
      ACaret.Insert(LByte, 1);
      Inc(I);
    until I > AValue.Size;
  finally
    ACaret.ReleaseWriteLock;
  end;
end;

procedure StreamInsertStream(const ACaret: ILKStreamCaret; const AValue: TStream; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertStream(ACaret, AValue);
end;

procedure StreamInsertTime(const ACaret: ILKStreamCaret; const AValue: TTime);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertTime(const ACaret: ILKStreamCaret; const AValue: TTime; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertTime(ACaret, AValue);
end;

{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamInsertWideString(const ACaret: ILKStreamCaret; const AValue: WideString);
  var
    LStringLength: Int64;
  begin
    LStringLength := ByteLength(AValue);
    ACaret.Insert(LStringLength, SizeOf(Int64));
    ACaret.Insert(AValue[1], ByteLength(AValue));
  end;

  procedure StreamInsertWideString(const ACaret: ILKStreamCaret; const AValue: WideString; const APosition: Int64);
  begin
    ACaret.Position := APosition;
    StreamInsertWideString(ACaret, AValue);
  end;
{$ENDIF}

procedure StreamInsertWord(const ACaret: ILKStreamCaret; const AValue: Word);
begin
  ACaret.Insert(AValue, SizeOf(AValue));
end;

procedure StreamInsertWord(const ACaret: ILKStreamCaret; const AValue: Word; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamInsertWord(ACaret, AValue);
end;

// Read Methods

{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  function StreamReadAnsiChar(const ACaret: ILKStreamCaret): AnsiChar;
  begin
    ACaret.Read(Result, SizeOf(AnsiChar));
  end;

  function StreamReadAnsiChar(const ACaret: ILKStreamCaret; const APosition: Int64): AnsiChar;
  begin
    ACaret.Position := APosition;
    Result := StreamReadAnsiChar(ACaret);
  end;

  function StreamReadAnsiString(const ACaret: ILKStreamCaret): AnsiString;
  var
    LStringLength: Int64;
  begin
    ACaret.Read(LStringLength, SizeOf(Int64));
    SetLength(Result, LStringLength);
    ACaret.Read(Result[1], LStringLength);
  end;

  function StreamReadAnsiString(const ACaret: ILKStreamCaret; const APosition: Int64): AnsiString;
  begin
    ACaret.Position := APosition;
    Result := StreamReadAnsiString(ACaret);
  end;
{$ENDIF}

function StreamReadBoolean(const ACaret: ILKStreamCaret): Boolean;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadBoolean(const ACaret: ILKStreamCaret; const APosition: Int64): Boolean;
begin
  ACaret.Position := APosition;
  Result := StreamReadBoolean(ACaret);
end;

function StreamReadByte(const ACaret: ILKStreamCaret): Byte;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadByte(const ACaret: ILKStreamCaret; const APosition: Int64): Byte;
begin
  ACaret.Position := APosition;
  Result := StreamReadByte(ACaret);
end;

function StreamReadCardinal(const ACaret: ILKStreamCaret): Cardinal;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadCardinal(const ACaret: ILKStreamCaret; const APosition: Int64): Cardinal;
begin
  ACaret.Position := APosition;
  Result := StreamReadCardinal(ACaret);
end;

function StreamReadChar(const ACaret: ILKStreamCaret): Char;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadChar(const ACaret: ILKStreamCaret; const APosition: Int64): Char;
begin
  ACaret.Position := APosition;
  Result := StreamReadChar(ACaret);
end;

function StreamReadCurrency(const ACaret: ILKStreamCaret): Currency;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadCurrency(const ACaret: ILKStreamCaret; const APosition: Int64): Currency;
begin
  ACaret.Position := APosition;
  Result := StreamReadCurrency(ACaret);
end;

function StreamReadDate(const ACaret: ILKStreamCaret): TDate;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadDate(const ACaret: ILKStreamCaret; const APosition: Int64): TDate;
begin
  ACaret.Position := APosition;
  Result := StreamReadDate(ACaret);
end;

function StreamReadDateTime(const ACaret: ILKStreamCaret): TDateTime;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadDateTime(const ACaret: ILKStreamCaret; const APosition: Int64): TDateTime;
begin
  ACaret.Position := APosition;
  Result := StreamReadDateTime(ACaret);
end;

function StreamReadDouble(const ACaret: ILKStreamCaret): Double;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadDouble(const ACaret: ILKStreamCaret; const APosition: Int64): Double;
begin
  ACaret.Position := APosition;
  Result := StreamReadDouble(ACaret);
end;

function StreamReadExtended(const ACaret: ILKStreamCaret): Extended;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadExtended(const ACaret: ILKStreamCaret; const APosition: Int64): Extended;
begin
  ACaret.Position := APosition;
  Result := StreamReadExtended(ACaret);
end;

function StreamReadGUID(const ACaret: ILKStreamCaret): TGUID;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadGUID(const ACaret: ILKStreamCaret; const APosition: Int64): TGUID;
begin
  ACaret.Position := APosition;
  Result := StreamReadGUID(ACaret);
end;

function StreamReadInteger(const ACaret: ILKStreamCaret): Integer;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadInteger(const ACaret: ILKStreamCaret; const APosition: Int64): Integer;
begin
  ACaret.Position := APosition;
  Result := StreamReadInteger(ACaret);
end;

function StreamReadInt64(const ACaret: ILKStreamCaret): Int64;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadInt64(const ACaret: ILKStreamCaret; const APosition: Int64): Int64;
begin
  ACaret.Position := APosition;
  Result := StreamReadInt64(ACaret);
end;

function StreamReadLongInt(const ACaret: ILKStreamCaret): LongInt;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadLongInt(const ACaret: ILKStreamCaret; const APosition: Int64): LongInt;
begin
  ACaret.Position := APosition;
  Result := StreamReadLongInt(ACaret);
end;

function StreamReadSingle(const ACaret: ILKStreamCaret): Single;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadSingle(const ACaret: ILKStreamCaret; const APosition: Int64): Single;
begin
  ACaret.Position := APosition;
  Result := StreamReadSingle(ACaret);
end;

function StreamReadShortInt(const ACaret: ILKStreamCaret): ShortInt;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadShortInt(const ACaret: ILKStreamCaret; const APosition: Int64): ShortInt;
begin
  ACaret.Position := APosition;
  Result := StreamReadShortInt(ACaret);
end;

{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  function StreamReadShortString(const ACaret: ILKStreamCaret): ShortString;
  var
    LStringLength: Int64;
  begin
    ACaret.Read(LStringLength, SizeOf(Int64));
    SetLength(Result, LStringLength);
    ACaret.Read(Result[1], LStringLength);
  end;

  function StreamReadShortString(const ACaret: ILKStreamCaret; const APosition: Int64): ShortString;
  begin
    ACaret.Position := APosition;
    Result := StreamReadShortString(ACaret);
  end;
{$ENDIF}

function StreamReadSmallInt(const ACaret: ILKStreamCaret): SmallInt;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadSmallInt(const ACaret: ILKStreamCaret; const APosition: Int64): SmallInt;
begin
  ACaret.Position := APosition;
  Result := StreamReadSmallInt(ACaret);
end;

function StreamReadString(const ACaret: ILKStreamCaret): String;
var
  LStringLength: Int64;
  LValue: TBytes;
begin
  ACaret.Read(LStringLength, SizeOf(Int64));
  SetLength(LValue, LStringLength);
  ACaret.Read(LValue[0], LStringLength);
  {$IFDEF FPC}
    Result := UTF8CStringToUTF8String(PChar(LValue), Length(LValue));
  {$ELSE}
    Result := TEncoding.Unicode.GetString(LValue);
  {$ENDIF FPC}
end;

function StreamReadString(const ACaret: ILKStreamCaret; const APosition: Int64): String;
begin
  ACaret.Position := APosition;
  Result := StreamReadString(ACaret);
end;

procedure StreamReadStream(const ACaret: ILKStreamCaret; const AOutStream: ILKStreamCaret);
var
  LSize: Int64;
  LValue: Array of Byte;
begin
  LSize := StreamReadInt64(ACaret);
  SetLength(LValue, LSize);
  ACaret.Read(LValue[0], LSize);
  AOutStream.Insert(LValue, LSize);
end;

procedure StreamReadStream(const ACaret: ILKStreamCaret; const AOutStream: ILKStreamCaret; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamReadStream(ACaret, AOutStream);
end;

function StreamReadTime(const ACaret: ILKStreamCaret): TTime;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadTime(const ACaret: ILKStreamCaret; const APosition: Int64): TTime;
begin
  ACaret.Position := APosition;
  Result := StreamReadTime(ACaret);
end;

{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  function StreamReadWideString(const ACaret: ILKStreamCaret): WideString;
  var
    LStringLength: Int64;
    LValue: TBytes;
  begin
    ACaret.Read(LStringLength, SizeOf(Int64));
    SetLength(LValue, LStringLength);
    ACaret.Read(LValue[0], LStringLength);
    {$IFDEF FPC}
      Result := UTF8CStringToUTF8String(PChar(LValue), Length(LValue));
    {$ELSE}
      Result := TEncoding.Unicode.GetString(LValue);
    {$ENDIF FPC}
  end;

  function StreamReadWideString(const ACaret: ILKStreamCaret; const APosition: Int64): WideString;
  begin
    ACaret.Position := APosition;
    Result := StreamReadWideString(ACaret);
  end;
{$ENDIF}

function StreamReadWord(const ACaret: ILKStreamCaret): Word;
begin
  ACaret.Read(Result, SizeOf(Result));
end;

function StreamReadWord(const ACaret: ILKStreamCaret; const APosition: Int64): Word;
begin
  ACaret.Position := APosition;
  Result := StreamReadWord(ACaret);
end;

// Write Methods
{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamWriteAnsiChar(const ACaret: ILKStreamCaret; const AValue: AnsiChar);
  begin
    ACaret.Write(AValue, SizeOf(AnsiChar));
  end;

  procedure StreamWriteAnsiChar(const ACaret: ILKStreamCaret; const AValue: AnsiChar; const APosition: Int64);
  begin
    ACaret.Position := APosition;
    StreamWriteAnsiChar(ACaret, AValue);
  end;

  procedure StreamWriteAnsiString(const ACaret: ILKStreamCaret; const AValue: AnsiString);
  var
    LStringLength: Int64;
  begin
    LStringLength := Length(AValue) * SizeOf(AnsiChar);
    ACaret.Write(LStringLength, SizeOf(Int64));
    ACaret.Write(AValue[1], LStringLength);
  end;

  procedure StreamWriteAnsiString(const ACaret: ILKStreamCaret; const AValue: AnsiString; const APosition: Int64);
  begin
    ACaret.Position := APosition;
    StreamWriteAnsiString(ACaret, AValue);
  end;
{$ENDIF}
procedure StreamWriteBoolean(const ACaret: ILKStreamCaret; const AValue: Boolean);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteBoolean(const ACaret: ILKStreamCaret; const AValue: Boolean; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteBoolean(ACaret, AValue);
end;

procedure StreamWriteByte(const ACaret: ILKStreamCaret; const AValue: Byte);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteByte(const ACaret: ILKStreamCaret; const AValue: Byte; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteByte(ACaret, AValue);
end;

procedure StreamWriteCardinal(const ACaret: ILKStreamCaret; const AValue: Cardinal);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteCardinal(const ACaret: ILKStreamCaret; const AValue: Cardinal; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteCardinal(ACaret, AValue);
end;

procedure StreamWriteChar(const ACaret: ILKStreamCaret; const AValue: Char);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteChar(const ACaret: ILKStreamCaret; const AValue: Char; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteChar(ACaret, AValue);
end;

procedure StreamWriteCurrency(const ACaret: ILKStreamCaret; const AValue: Currency);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteCurrency(const ACaret: ILKStreamCaret; const AValue: Currency; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteCurrency(ACaret, AValue);
end;

procedure StreamWriteDate(const ACaret: ILKStreamCaret; const AValue: TDate);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteDate(const ACaret: ILKStreamCaret; const AValue: TDate; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteDate(ACaret, AValue);
end;

procedure StreamWriteDateTime(const ACaret: ILKStreamCaret; const AValue: TDateTime);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteDateTime(const ACaret: ILKStreamCaret; const AValue: TDateTime; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteDateTime(ACaret, AValue);
end;

procedure StreamWriteDouble(const ACaret: ILKStreamCaret; const AValue: Double);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteDouble(const ACaret: ILKStreamCaret; const AValue: Double; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteDouble(ACaret, AValue);
end;

procedure StreamWriteExtended(const ACaret: ILKStreamCaret; const AValue: Extended);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteExtended(const ACaret: ILKStreamCaret; const AValue: Extended; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteExtended(ACaret, AValue);
end;

procedure StreamWriteGUID(const ACaret: ILKStreamCaret; const AValue: TGUID);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteGUID(const ACaret: ILKStreamCaret; const AValue: TGUID; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteGUID(ACaret, AValue);
end;

procedure StreamWriteInteger(const ACaret: ILKStreamCaret; const AValue: Integer);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteInteger(const ACaret: ILKStreamCaret; const AValue: Integer; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteInteger(ACaret, AValue);
end;

procedure StreamWriteInt64(const ACaret: ILKStreamCaret; const AValue: Int64);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteInt64(const ACaret: ILKStreamCaret; const AValue, APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteInt64(ACaret, AValue);
end;

procedure StreamWriteLongInt(const ACaret: ILKStreamCaret; const AValue: Integer);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteLongInt(const ACaret: ILKStreamCaret; const AValue: Integer; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteLongInt(ACaret, AValue);
end;

procedure StreamWriteSingle(const ACaret: ILKStreamCaret; const AValue: Single);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteSingle(const ACaret: ILKStreamCaret; const AValue: Single; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteSingle(ACaret, AValue);
end;

procedure StreamWriteShortInt(const ACaret: ILKStreamCaret; const AValue: ShortInt);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteShortInt(const ACaret: ILKStreamCaret; const AValue: ShortInt; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteShortInt(ACaret, AValue);
end;

{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamWriteShortString(const ACaret: ILKStreamCaret; const AValue: ShortString);
  var
    LStringLength: Int64;
  begin
    LStringLength := Length(AValue) * SizeOf(AnsiChar);
    ACaret.Write(LStringLength, SizeOf(Int64));
    ACaret.Write(AValue[1], LStringLength);
  end;

  procedure StreamWriteShortString(const ACaret: ILKStreamCaret; const AValue: ShortString; const APosition: Int64);
  begin
    ACaret.Position := APosition;
    StreamWriteShortString(ACaret, AValue);
  end;
{$ENDIF}

procedure StreamWriteSmallInt(const ACaret: ILKStreamCaret; const AValue: SmallInt);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteSmallInt(const ACaret: ILKStreamCaret; const AValue: SmallInt; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteSmallInt(ACaret, AValue);
end;

procedure StreamWriteString(const ACaret: ILKStreamCaret; const AValue: String);
var
  LStringLength: Int64;
begin
  LStringLength := ByteLength(AValue);
  ACaret.Write(LStringLength, SizeOf(Int64));
  ACaret.Write(AValue[1], LStringLength);
end;

procedure StreamWriteString(const ACaret: ILKStreamCaret; const AValue: String; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteString(ACaret, AValue);
end;

procedure StreamWriteStream(const ACaret: ILKStreamCaret; const AValue: TStream);
var
  I: Int64;
  LByte: Byte;
begin
  ACaret.AcquireWriteLock;
  try
    StreamWriteInt64(ACaret, AValue.Size); // We need to know the size of the stream...
    I := 0;
    repeat
      AValue.Position := I;
      AValue.Read(LByte, 1);
      ACaret.Write(LByte, 1);
      Inc(I);
    until I > AValue.Size;
  finally
    ACaret.ReleaseWriteLock;
  end;
end;

procedure StreamWriteStream(const ACaret: ILKStreamCaret; const AValue: TStream; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteStream(ACaret, AValue);
end;

procedure StreamWriteTime(const ACaret: ILKStreamCaret; const AValue: TTime);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteTime(const ACaret: ILKStreamCaret; const AValue: TTime; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteTime(ACaret, AValue);
end;

{$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
  procedure StreamWriteWideString(const ACaret: ILKStreamCaret; const AValue: WideString);
  var
    LStringLength: Int64;
  begin
    LStringLength := ByteLength(AValue);
    ACaret.Write(LStringLength, SizeOf(Int64));
    ACaret.Write(AValue[1], ByteLength(AValue));
  end;

  procedure StreamWriteWideString(const ACaret: ILKStreamCaret; const AValue: WideString; const APosition: Int64);
  begin
    ACaret.Position := APosition;
    StreamWriteWideString(ACaret, AValue);
  end;
{$ENDIF}

procedure StreamWriteWord(const ACaret: ILKStreamCaret; const AValue: Word);
begin
  ACaret.Write(AValue, SizeOf(AValue));
end;

procedure StreamWriteWord(const ACaret: ILKStreamCaret; const AValue: Word; const APosition: Int64);
begin
  ACaret.Position := APosition;
  StreamWriteWord(ACaret, AValue);
end;

end.

