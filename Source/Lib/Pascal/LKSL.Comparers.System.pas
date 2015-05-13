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
    - This unit provides useful enhancements for Generics types used in the LKSL.
}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes,
  {$ELSE}
    Classes,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types;

  {$I LKSL_RTTI.inc}

type
  ILKByteComparer = ILKComparer<Byte>;
  ILKCardinalComparer = ILKComparer<Cardinal>;
  ILKCharComparer = ILKComparer<Char>;
  ILKCurrencyComparer = ILKComparer<Currency>;
  ILKIntegerComparer = ILKComparer<Integer>;
  ILKStringComparer = ILKComparer<String>;

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

  TLKIntegerComparer = class(TLKComparer<Integer>, ILKIntegerComparer)
    function AEqualToB(const A, B: Integer): Boolean; override;
    function AGreaterThanB(const A, B: Integer): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: Integer): Boolean; override;
    function ALessThanB(const A, B: Integer): Boolean; override;
    function ALessThanOrEqualB(const A, B: Integer): Boolean; override;
  end;

  TLKStringComparer = class(TLKComparer<String>, ILKStringComparer)
    function AEqualToB(const A, B: String): Boolean; override;
    function AGreaterThanB(const A, B: String): Boolean; override;
    function AGreaterThanOrEqualB(const A, B: String): Boolean; override;
    function ALessThanB(const A, B: String): Boolean; override;
    function ALessThanOrEqualB(const A, B: String): Boolean; override;
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

end.
