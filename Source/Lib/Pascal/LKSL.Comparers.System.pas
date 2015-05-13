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
unit LKSL.Comparers.System;

interface

{$I LKSL.inc}

{
  About this unit:
    - This unit provides Comparers for the standard data types in System.pas.
    - It basically saves implementing developers from having to constantly define Comparers for these types.
}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.SysUtils,
  {$ELSE}
    SysUtils,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types;

  {$I LKSL_RTTI.inc}

type
  ILKByteComparer = ILKComparer<Byte>;
  ILKCardinalComparer = ILKComparer<Cardinal>;
  ILKCharComparer = ILKComparer<Char>;
  ILKCurrencyComparer = ILKComparer<Currency>;
  ILKDateComparer = ILKComparer<TDate>;
  ILKDateTimeComparer = ILKComparer<TDateTime>;
  ILKDoubleComparer = ILKComparer<Double>;
  ILKExtendedComparer = ILKComparer<Extended>;
  ILKGUIDComparer = ILKComparer<TGUID>;
  ILKInt64Comparer = ILKComparer<Int64>;
  ILKIntegerComparer = ILKComparer<Integer>;
  ILKLongIntComparer = ILKComparer<LongInt>;
  ILKShortIntComparer = ILKComparer<ShortInt>;
  ILKShortStringComparer = ILKComparer<ShortString>;
  ILKSingleComparer = ILKComparer<Single>;
  ILKSmallIntComparer = ILKComparer<SmallInt>;
  ILKStringComparer = ILKComparer<String>;
  ILKTimeComparer = ILKComparer<TTime>;
  ILKWideStringComparer = ILKComparer<WideString>;
  ILKWordComparer = ILKComparer<Word>;

  TLKByteComparer = class(TLKComparer<Byte>, ILKByteComparer)
    function AEqualToB(const A, B: Byte): Boolean; override;
    function AGreaterThanB(const A, B: Byte): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: Byte): Boolean; override;
    function ALessThanB(const A, B: Byte): Boolean; override;
    function ALessThanOrEqualB(const A, B: Byte): Boolean; override;
  end;

  TLKCardinalComparer = class(TLKComparer<Cardinal>, ILKCardinalComparer)
    function AEqualToB(const A, B: Cardinal): Boolean; override;
    function AGreaterThanB(const A, B: Cardinal): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: Cardinal): Boolean; override;
    function ALessThanB(const A, B: Cardinal): Boolean; override;
    function ALessThanOrEqualB(const A, B: Cardinal): Boolean; override;
  end;

  TLKCharComparer = class(TLKComparer<Char>, ILKCharComparer)
    function AEqualToB(const A, B: Char): Boolean; override;
    function AGreaterThanB(const A, B: Char): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: Char): Boolean; override;
    function ALessThanB(const A, B: Char): Boolean; override;
    function ALessThanOrEqualB(const A, B: Char): Boolean; override;
  end;

  TLKCurrencyComparer = class(TLKComparer<Currency>, ILKCurrencyComparer)
    function AEqualToB(const A, B: Currency): Boolean; override;
    function AGreaterThanB(const A, B: Currency): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: Currency): Boolean; override;
    function ALessThanB(const A, B: Currency): Boolean; override;
    function ALessThanOrEqualB(const A, B: Currency): Boolean; override;
  end;

  TLKDateComparer = class(TLKComparer<TDate>, ILKDateComparer)
    function AEqualToB(const A, B: TDate): Boolean; override;
    function AGreaterThanB(const A, B: TDate): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: TDate): Boolean; override;
    function ALessThanB(const A, B: TDate): Boolean; override;
    function ALessThanOrEqualB(const A, B: TDate): Boolean; override;
  end;

  TLKDateTimeComparer = class(TLKComparer<TDateTime>, ILKDateTimeComparer)
    function AEqualToB(const A, B: TDateTime): Boolean; override;
    function AGreaterThanB(const A, B: TDateTime): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: TDateTime): Boolean; override;
    function ALessThanB(const A, B: TDateTime): Boolean; override;
    function ALessThanOrEqualB(const A, B: TDateTime): Boolean; override;
  end;

  TLKDoubleComparer = class(TLKComparer<Double>, ILKDoubleComparer)
    function AEqualToB(const A, B: Double): Boolean; override;
    function AGreaterThanB(const A, B: Double): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: Double): Boolean; override;
    function ALessThanB(const A, B: Double): Boolean; override;
    function ALessThanOrEqualB(const A, B: Double): Boolean; override;
  end;

  TLKExtendedComparer = class(TLKComparer<Extended>, ILKExtendedComparer)
    function AEqualToB(const A, B: Extended): Boolean; override;
    function AGreaterThanB(const A, B: Extended): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: Extended): Boolean; override;
    function ALessThanB(const A, B: Extended): Boolean; override;
    function ALessThanOrEqualB(const A, B: Extended): Boolean; override;
  end;

  TLKGUIDComparer = class(TLKComparer<TGUID>, ILKGUIDComparer)
    function AEqualToB(const A, B: TGUID): Boolean; override;
    function AGreaterThanB(const A, B: TGUID): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: TGUID): Boolean; override;
    function ALessThanB(const A, B: TGUID): Boolean; override;
    function ALessThanOrEqualB(const A, B: TGUID): Boolean; override;
  end;

  TLKInt64Comparer = class(TLKComparer<Int64>, ILKInt64Comparer)
    function AEqualToB(const A, B: Int64): Boolean; override;
    function AGreaterThanB(const A, B: Int64): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: Int64): Boolean; override;
    function ALessThanB(const A, B: Int64): Boolean; override;
    function ALessThanOrEqualB(const A, B: Int64): Boolean; override;
  end;

  TLKIntegerComparer = class(TLKComparer<Integer>, ILKIntegerComparer)
    function AEqualToB(const A, B: Integer): Boolean; override;
    function AGreaterThanB(const A, B: Integer): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: Integer): Boolean; override;
    function ALessThanB(const A, B: Integer): Boolean; override;
    function ALessThanOrEqualB(const A, B: Integer): Boolean; override;
  end;

  TLKLongIntComparer = class(TLKComparer<LongInt>, ILKLongIntComparer)
    function AEqualToB(const A, B: LongInt): Boolean; override;
    function AGreaterThanB(const A, B: LongInt): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: LongInt): Boolean; override;
    function ALessThanB(const A, B: LongInt): Boolean; override;
    function ALessThanOrEqualB(const A, B: LongInt): Boolean; override;
  end;

  TLKShortIntComparer = class(TLKComparer<ShortInt>, ILKShortIntComparer)
    function AEqualToB(const A, B: ShortInt): Boolean; override;
    function AGreaterThanB(const A, B: ShortInt): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: ShortInt): Boolean; override;
    function ALessThanB(const A, B: ShortInt): Boolean; override;
    function ALessThanOrEqualB(const A, B: ShortInt): Boolean; override;
  end;

  TLKShortStringComparer = class(TLKComparer<ShortString>, ILKShortStringComparer)
    function AEqualToB(const A, B: ShortString): Boolean; override;
    function AGreaterThanB(const A, B: ShortString): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: ShortString): Boolean; override;
    function ALessThanB(const A, B: ShortString): Boolean; override;
    function ALessThanOrEqualB(const A, B: ShortString): Boolean; override;
  end;

  TLKSingleComparer = class(TLKComparer<Single>, ILKSingleComparer)
    function AEqualToB(const A, B: Single): Boolean; override;
    function AGreaterThanB(const A, B: Single): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: Single): Boolean; override;
    function ALessThanB(const A, B: Single): Boolean; override;
    function ALessThanOrEqualB(const A, B: Single): Boolean; override;
  end;

  TLKSmallIntComparer = class(TLKComparer<SmallInt>, ILKSmallIntComparer)
    function AEqualToB(const A, B: SmallInt): Boolean; override;
    function AGreaterThanB(const A, B: SmallInt): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: SmallInt): Boolean; override;
    function ALessThanB(const A, B: SmallInt): Boolean; override;
    function ALessThanOrEqualB(const A, B: SmallInt): Boolean; override;
  end;

  TLKStringComparer = class(TLKComparer<String>, ILKStringComparer)
    function AEqualToB(const A, B: String): Boolean; override;
    function AGreaterThanB(const A, B: String): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: String): Boolean; override;
    function ALessThanB(const A, B: String): Boolean; override;
    function ALessThanOrEqualB(const A, B: String): Boolean; override;
  end;

  TLKTimeComparer = class(TLKComparer<TTime>, ILKTimeComparer)
    function AEqualToB(const A, B: TTime): Boolean; override;
    function AGreaterThanB(const A, B: TTime): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: TTime): Boolean; override;
    function ALessThanB(const A, B: TTime): Boolean; override;
    function ALessThanOrEqualB(const A, B: TTime): Boolean; override;
  end;

  TLKWideStringComparer = class(TLKComparer<WideString>, ILKWideStringComparer)
    function AEqualToB(const A, B: WideString): Boolean; override;
    function AGreaterThanB(const A, B: WideString): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: WideString): Boolean; override;
    function ALessThanB(const A, B: WideString): Boolean; override;
    function ALessThanOrEqualB(const A, B: WideString): Boolean; override;
  end;

  TLKWordComparer = class(TLKComparer<Word>, ILKWordComparer)
    function AEqualToB(const A, B: Word): Boolean; override;
    function AGreaterThanB(const A, B: Word): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: Word): Boolean; override;
    function ALessThanB(const A, B: Word): Boolean; override;
    function ALessThanOrEqualB(const A, B: Word): Boolean; override;
  end;

implementation

{ TLKByteComparer }

function TLKByteComparer.AEqualToB(const A, B: Byte): Boolean;
begin
  Result := (A = B);
end;

function TLKByteComparer.AGreaterThanB(const A, B: Byte): Boolean;
begin
  Result := (A > B);
end;

function TLKByteComparer.AGreaterThanOrEqualB(const A, B: Byte): Boolean;
begin
  Result := (A >= B);
end;

function TLKByteComparer.ALessThanB(const A, B: Byte): Boolean;
begin
  Result := (A < B);
end;

function TLKByteComparer.ALessThanOrEqualB(const A, B: Byte): Boolean;
begin
  Result := (A <= B);
end;

{ TLKCardinalComparer }

function TLKCardinalComparer.AEqualToB(const A, B: Cardinal): Boolean;
begin
  Result := (A = B);
end;

function TLKCardinalComparer.AGreaterThanB(const A, B: Cardinal): Boolean;
begin
  Result := (A > B);
end;

function TLKCardinalComparer.AGreaterThanOrEqualB(const A, B: Cardinal): Boolean;
begin
  Result := (A >= B);
end;

function TLKCardinalComparer.ALessThanB(const A, B: Cardinal): Boolean;
begin
  Result := (A < B);
end;

function TLKCardinalComparer.ALessThanOrEqualB(const A, B: Cardinal): Boolean;
begin
  Result := (A <= B);
end;

{ TLKCharComparer }

function TLKCharComparer.AEqualToB(const A, B: Char): Boolean;
begin
  Result := (A = B);
end;

function TLKCharComparer.AGreaterThanB(const A, B: Char): Boolean;
begin
  Result := (A > B);
end;

function TLKCharComparer.AGreaterThanOrEqualB(const A, B: Char): Boolean;
begin
  Result := (A >= B);
end;

function TLKCharComparer.ALessThanB(const A, B: Char): Boolean;
begin
  Result := (A < B);
end;

function TLKCharComparer.ALessThanOrEqualB(const A, B: Char): Boolean;
begin
  Result := (A <= B);
end;

{ TLKCurrencyComparer }

function TLKCurrencyComparer.AEqualToB(const A, B: Currency): Boolean;
begin
  Result := (A = B);
end;

function TLKCurrencyComparer.AGreaterThanB(const A, B: Currency): Boolean;
begin
  Result := (A > B);
end;

function TLKCurrencyComparer.AGreaterThanOrEqualB(const A, B: Currency): Boolean;
begin
  Result := (A >= B);
end;

function TLKCurrencyComparer.ALessThanB(const A, B: Currency): Boolean;
begin
  Result := (A < B);
end;

function TLKCurrencyComparer.ALessThanOrEqualB(const A, B: Currency): Boolean;
begin
  Result := (A <= B);
end;

{ TLKDateComparer }

function TLKDateComparer.AEqualToB(const A, B: TDate): Boolean;
begin
  Result := (A = B);
end;

function TLKDateComparer.AGreaterThanB(const A, B: TDate): Boolean;
begin
  Result := (A > B);
end;

function TLKDateComparer.AGreaterThanOrEqualB(const A, B: TDate): Boolean;
begin
  Result := (A >= B);
end;

function TLKDateComparer.ALessThanB(const A, B: TDate): Boolean;
begin
  Result := (A < B);
end;

function TLKDateComparer.ALessThanOrEqualB(const A, B: TDate): Boolean;
begin
  Result := (A <= B);
end;

{ TLKDateTimeComparer }

function TLKDateTimeComparer.AEqualToB(const A, B: TDateTime): Boolean;
begin
  Result := (A = B);
end;

function TLKDateTimeComparer.AGreaterThanB(const A, B: TDateTime): Boolean;
begin
  Result := (A > B);
end;

function TLKDateTimeComparer.AGreaterThanOrEqualB(const A, B: TDateTime): Boolean;
begin
  Result := (A >= B);
end;

function TLKDateTimeComparer.ALessThanB(const A, B: TDateTime): Boolean;
begin
  Result := (A < B);
end;

function TLKDateTimeComparer.ALessThanOrEqualB(const A, B: TDateTime): Boolean;
begin
  Result := (A <= B);
end;

{ TLKDoubleComparer }

function TLKDoubleComparer.AEqualToB(const A, B: Double): Boolean;
begin
  Result := (A = B);
end;

function TLKDoubleComparer.AGreaterThanB(const A, B: Double): Boolean;
begin
  Result := (A > B);
end;

function TLKDoubleComparer.AGreaterThanOrEqualB(const A, B: Double): Boolean;
begin
  Result := (A >= B);
end;

function TLKDoubleComparer.ALessThanB(const A, B: Double): Boolean;
begin
  Result := (A < B);
end;

function TLKDoubleComparer.ALessThanOrEqualB(const A, B: Double): Boolean;
begin
  Result := (A <= B);
end;

{ TLKExtendedComparer }

function TLKExtendedComparer.AEqualToB(const A, B: Extended): Boolean;
begin
  Result := (A = B);
end;

function TLKExtendedComparer.AGreaterThanB(const A, B: Extended): Boolean;
begin
  Result := (A > B);
end;

function TLKExtendedComparer.AGreaterThanOrEqualB(const A, B: Extended): Boolean;
begin
  Result := (A >= B);
end;

function TLKExtendedComparer.ALessThanB(const A, B: Extended): Boolean;
begin
  Result := (A < B);
end;

function TLKExtendedComparer.ALessThanOrEqualB(const A, B: Extended): Boolean;
begin
  Result := (A <= B);
end;

{ TLKGUIDComparer }

function TLKGUIDComparer.AEqualToB(const A, B: TGUID): Boolean;
begin
  Result := (A = B);
end;

function TLKGUIDComparer.AGreaterThanB(const A, B: TGUID): Boolean;
begin
  {$IFDEF FPC}
    Result := (A > B);
  {$ELSE}
    Result := GUIDToString(A) > GUIDToString(B);
  {$ENDIF FPC}
end;

function TLKGUIDComparer.AGreaterThanOrEqualB(const A, B: TGUID): Boolean;
begin
  {$IFDEF FPC}
    Result := (A >= B);
  {$ELSE}
    Result := GUIDToString(A) >= GUIDToString(B);
  {$ENDIF FPC}
end;

function TLKGUIDComparer.ALessThanB(const A, B: TGUID): Boolean;
begin
  {$IFDEF FPC}
    Result := (A < B);
  {$ELSE}
    Result := GUIDToString(A) < GUIDToString(B);
  {$ENDIF FPC}
end;

function TLKGUIDComparer.ALessThanOrEqualB(const A, B: TGUID): Boolean;
begin
  {$IFDEF FPC}
    Result := (A <= B);
  {$ELSE}
    Result := GUIDToString(A) <= GUIDToString(B);
  {$ENDIF FPC}
end;

{ TLKInt64Comparer }

function TLKInt64Comparer.AEqualToB(const A, B: Int64): Boolean;
begin
  Result := (A = B);
end;

function TLKInt64Comparer.AGreaterThanB(const A, B: Int64): Boolean;
begin
  Result := (A > B);
end;

function TLKInt64Comparer.AGreaterThanOrEqualB(const A, B: Int64): Boolean;
begin
  Result := (A >= B);
end;

function TLKInt64Comparer.ALessThanB(const A, B: Int64): Boolean;
begin
  Result := (A < B);
end;

function TLKInt64Comparer.ALessThanOrEqualB(const A, B: Int64): Boolean;
begin
  Result := (A <= B);
end;

{ TLKIntegerComparer }

function TLKIntegerComparer.AEqualToB(const A, B: Integer): Boolean;
begin
  Result := (A = B);
end;

function TLKIntegerComparer.AGreaterThanB(const A, B: Integer): Boolean;
begin
  Result := (A > B);
end;

function TLKIntegerComparer.AGreaterThanOrEqualB(const A, B: Integer): Boolean;
begin
  Result := (A >= B);
end;

function TLKIntegerComparer.ALessThanB(const A, B: Integer): Boolean;
begin
  Result := (A < B);
end;

function TLKIntegerComparer.ALessThanOrEqualB(const A, B: Integer): Boolean;
begin
  Result := (A <= B);
end;

{ TLKLongIntComparer }

function TLKLongIntComparer.AEqualToB(const A, B: LongInt): Boolean;
begin
  Result := (A = B);
end;

function TLKLongIntComparer.AGreaterThanB(const A, B: LongInt): Boolean;
begin
  Result := (A > B);
end;

function TLKLongIntComparer.AGreaterThanOrEqualB(const A, B: LongInt): Boolean;
begin
  Result := (A >= B);
end;

function TLKLongIntComparer.ALessThanB(const A, B: LongInt): Boolean;
begin
  Result := (A < B);
end;

function TLKLongIntComparer.ALessThanOrEqualB(const A, B: LongInt): Boolean;
begin
  Result := (A <= B);
end;

{ TLKShortIntComparer }

function TLKShortIntComparer.AEqualToB(const A, B: ShortInt): Boolean;
begin
  Result := (A = B);
end;

function TLKShortIntComparer.AGreaterThanB(const A, B: ShortInt): Boolean;
begin
  Result := (A > B);
end;

function TLKShortIntComparer.AGreaterThanOrEqualB(const A, B: ShortInt): Boolean;
begin
  Result := (A >= B);
end;

function TLKShortIntComparer.ALessThanB(const A, B: ShortInt): Boolean;
begin
  Result := (A < B);
end;

function TLKShortIntComparer.ALessThanOrEqualB(const A, B: ShortInt): Boolean;
begin
  Result := (A <= B);
end;

{ TLKShortStringComparer }

function TLKShortStringComparer.AEqualToB(const A, B: ShortString): Boolean;
begin
  Result := (A = B);
end;

function TLKShortStringComparer.AGreaterThanB(const A, B: ShortString): Boolean;
begin
  Result := (A > B);
end;

function TLKShortStringComparer.AGreaterThanOrEqualB(const A, B: ShortString): Boolean;
begin
  Result := (A >= B);
end;

function TLKShortStringComparer.ALessThanB(const A, B: ShortString): Boolean;
begin
  Result := (A < B);
end;

function TLKShortStringComparer.ALessThanOrEqualB(const A, B: ShortString): Boolean;
begin
  Result := (A <= B);
end;

{ TLKSingleComparer }

function TLKSingleComparer.AEqualToB(const A, B: Single): Boolean;
begin
  Result := (A = B);
end;

function TLKSingleComparer.AGreaterThanB(const A, B: Single): Boolean;
begin
  Result := (A > B);
end;

function TLKSingleComparer.AGreaterThanOrEqualB(const A, B: Single): Boolean;
begin
  Result := (A >= B);
end;

function TLKSingleComparer.ALessThanB(const A, B: Single): Boolean;
begin
  Result := (A < B);
end;

function TLKSingleComparer.ALessThanOrEqualB(const A, B: Single): Boolean;
begin
  Result := (A <= B);
end;

{ TLKSmallIntComparer }

function TLKSmallIntComparer.AEqualToB(const A, B: SmallInt): Boolean;
begin
  Result := (A = B);
end;

function TLKSmallIntComparer.AGreaterThanB(const A, B: SmallInt): Boolean;
begin
  Result := (A > B);
end;

function TLKSmallIntComparer.AGreaterThanOrEqualB(const A, B: SmallInt): Boolean;
begin
  Result := (A >= B);
end;

function TLKSmallIntComparer.ALessThanB(const A, B: SmallInt): Boolean;
begin
  Result := (A < B);
end;

function TLKSmallIntComparer.ALessThanOrEqualB(const A, B: SmallInt): Boolean;
begin
  Result := (A <= B);
end;

{ TLKStringComparer }

function TLKStringComparer.AEqualToB(const A, B: String): Boolean;
begin
  Result := (A = B);
end;

function TLKStringComparer.AGreaterThanB(const A, B: String): Boolean;
begin
  Result := (A > B);
end;

function TLKStringComparer.AGreaterThanOrEqualB(const A, B: String): Boolean;
begin
  Result := (A >= B);
end;

function TLKStringComparer.ALessThanB(const A, B: String): Boolean;
begin
  Result := (A < B);
end;

function TLKStringComparer.ALessThanOrEqualB(const A, B: String): Boolean;
begin
  Result := (A <= B);
end;

{ TLKTimeComparer }

function TLKTimeComparer.AEqualToB(const A, B: TTime): Boolean;
begin
  Result := (A = B);
end;

function TLKTimeComparer.AGreaterThanB(const A, B: TTime): Boolean;
begin
  Result := (A > B);
end;

function TLKTimeComparer.AGreaterThanOrEqualB(const A, B: TTime): Boolean;
begin
  Result := (A >= B);
end;

function TLKTimeComparer.ALessThanB(const A, B: TTime): Boolean;
begin
  Result := (A < B);
end;

function TLKTimeComparer.ALessThanOrEqualB(const A, B: TTime): Boolean;
begin
  Result := (A <= B);
end;

{ TLKWideStringComparer }

function TLKWideStringComparer.AEqualToB(const A, B: WideString): Boolean;
begin
  Result := (A = B);
end;

function TLKWideStringComparer.AGreaterThanB(const A, B: WideString): Boolean;
begin
  Result := (A > B);
end;

function TLKWideStringComparer.AGreaterThanOrEqualB(const A, B: WideString): Boolean;
begin
  Result := (A >= B);
end;

function TLKWideStringComparer.ALessThanB(const A, B: WideString): Boolean;
begin
  Result := (A < B);
end;

function TLKWideStringComparer.ALessThanOrEqualB(const A, B: WideString): Boolean;
begin
  Result := (A <= B);
end;

{ TLKWordComparer }

function TLKWordComparer.AEqualToB(const A, B: Word): Boolean;
begin
  Result := (A = B);
end;

function TLKWordComparer.AGreaterThanB(const A, B: Word): Boolean;
begin
  Result := (A > B);
end;

function TLKWordComparer.AGreaterThanOrEqualB(const A, B: Word): Boolean;
begin
  Result := (A >= B);
end;

function TLKWordComparer.ALessThanB(const A, B: Word): Boolean;
begin
  Result := (A < B);
end;

function TLKWordComparer.ALessThanOrEqualB(const A, B: Word): Boolean;
begin
  Result := (A <= B);
end;

end.
