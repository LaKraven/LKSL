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
unit LKSL.Math.Base;

interface

{$I LKSL.inc}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.Math,
  {$ELSE}
    Classes, SysUtils, Math,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types,
  LKSL.Math.Common;

  {$I LKSL_RTTI.inc}

type

  { Enum Types }
  TLKUnitSI = (siYocto, siZepto, siAtto, siFemto, siPico, siNano, siMicro, siMilli, siCenti, siDeci, siOne,
               siDeca, siHecto, siKilo, siMega, siGiga, siTera, siPeta, siExa, siZetta, siYotta);

const
  LK_CONVERSION_TABLE_SI: Array[TLKUnitSI, TLKUnitSI] of LKFloat = (
                                                                   //  Yocto   Zepto   Atto    Femto   Pico    Nano    Micro   Milli   Centi   Deci    One     Deca    Hecto   Kilo    Mega    Giga    Tera    Peta    Exa     Zetta   Yotta
                                                             {Yocto}  (1,      1e1,    1e2,    1e3,    1e6,    1e9,    1e12,   1e15,   1e18,   1e21,   1e24,   1e27,   1e30,   1e33,   1e36,   1e39,   1e42,   1e45,   1e48,   1e51,   1e55),
                                                             {Zepto}  (1e-1,   1,      1e1,    1e2,    1e3,    1e6,    1e9,    1e12,   1e15,   1e18,   1e21,   1e24,   1e27,   1e30,   1e33,   1e36,   1e39,   1e42,   1e45,   1e48,   1e51),
                                                              {Atto}  (1e-2,   1e-1,   1,      1e1,    1e2,    1e3,    1e6,    1e9,    1e12,   1e15,   1e18,   1e21,   1e24,   1e27,   1e30,   1e33,   1e36,   1e39,   1e42,   1e45,   1e48),
                                                             {Femto}  (1e-3,   1e-2,   1e-1,   1,      1e1,    1e2,    1e3,    1e6,    1e9,    1e12,   1e15,   1e18,   1e21,   1e24,   1e27,   1e30,   1e33,   1e36,   1e39,   1e42,   1e45),
                                                              {Pico}  (1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1,    1e2,    1e3,    1e6,    1e9,    1e12,   1e15,   1e18,   1e21,   1e24,   1e27,   1e30,   1e33,   1e36,   1e39,   1e52),
                                                              {Nano}  (1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1,    1e2,    1e3,    1e6,    1e9,    1e12,   1e15,   1e18,   1e21,   1e24,   1e27,   1e30,   1e33,   1e36,   1e39),
                                                             {Micro}  (1e-12,  1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1,    1e2,    1e3,    1e6,    1e9,    1e12,   1e15,   1e18,   1e21,   1e24,   1e27,   1e30,   1e33,   1e36),
                                                             {Milli}  (1e-15,  1e-12,  1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1,    1e2,    1e3,    1e6,    1e9,    1e12,   1e15,   1e18,   1e21,   1e24,   1e27,   1e30,   1e33),
                                                             {Centi}  (1e-18,  1e-15,  1e-12,  1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1,    1e2,    1e3,    1e6,    1e9,    1e12,   1e15,   1e18,   1e21,   1e24,   1e27,   1e30),
                                                              {Deci}  (1e-21,  1e-18,  1e-15,  1e-12,  1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1,    1e2,    1e3,    1e6,    1e9,    1e12,   1e15,   1e18,   1e21,   1e24,   1e27),
                                                               {One}  (1e-24,  1e-21,  1e-18,  1e-15,  1e-12,  1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1,    1e2,    1e3,    1e6,    1e9,    1e12,   1e15,   1e18,   1e21,   1e24),
                                                              {Deca}  (1e-27,  1e-24,  1e-21,  1e-18,  1e-15,  1e-12,  1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1,    1e2,    1e3,    1e6,    1e9,    1e12,   1e15,   1e18,   1e21),
                                                             {Hecto}  (1e-30,  1e-27,  1e-24,  1e-21,  1e-18,  1e-15,  1e-12,  1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1,    1e2,    1e3,    1e6,    1e9,    1e12,   1e15,   1e18),
                                                              {Kilo}  (1e-33,  1e-30,  1e-27,  1e-24,  1e-21,  1e-18,  1e-15,  1e-12,  1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1,    1e2,    1e3,    1e6,    1e9,    1e12,   1e15),
                                                              {Mega}  (1e-36,  1e-33,  1e-30,  1e-27,  1e-24,  1e-21,  1e-18,  1e-15,  1e-12,  1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1,    1e2,    1e3,    1e6,    1e9,    1e12),
                                                              {Giga}  (1e-39,  1e-36,  1e-33,  1e-30,  1e-27,  1e-24,  1e-21,  1e-18,  1e-15,  1e-12,  1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1,    1e2,    1e3,    1e6,    1e9),
                                                              {Tera}  (1e-42,  1e-39,  1e-36,  1e-33,  1e-30,  1e-27,  1e-24,  1e-21,  1e-18,  1e-15,  1e-12,  1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1,    1e2,    1e3,    1e6),
                                                              {Peta}  (1e-45,  1e-42,  1e-39,  1e-36,  1e-33,  1e-30,  1e-27,  1e-24,  1e-21,  1e-18,  1e-15,  1e-12,  1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1,    1e2,    1e3),
                                                               {Exa}  (1e-48,  1e-45,  1e-42,  1e-39,  1e-36,  1e-33,  1e-30,  1e-27,  1e-24,  1e-21,  1e-18,  1e-15,  1e-12,  1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1,    1e2),
                                                             {Zetta}  (1e-51,  1e-48,  1e-45,  1e-42,  1e-39,  1e-36,  1e-33,  1e-30,  1e-27,  1e-24,  1e-21,  1e-18,  1e-15,  1e-12,  1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1,      1e1),
                                                             {Yotta}  (1e-54,  1e-51,  1e-48,  1e-45,  1e-42,  1e-39,  1e-36,  1e-33,  1e-30,  1e-27,  1e-24,  1e-21,  1e-18,  1e-15,  1e-12,  1e-9,   1e-6,   1e-3,   1e-2,   1e-1,   1)
                                                                   );

// ? To ?
function SIUnitConvert(const ASourceValue: LKFloat; const AInputUnit, AOutputUnit: TLKUnitSI): LKFloat; inline;

implementation

// ? to ?

function SIUnitConvert(const ASourceValue: LKFloat; const AInputUnit, AOutputUnit: TLKUnitSI): LKFloat;
begin
  Result := ASourceValue * LK_CONVERSION_TABLE_SI[AInputUnit, AOutputUnit];
end;

end.
