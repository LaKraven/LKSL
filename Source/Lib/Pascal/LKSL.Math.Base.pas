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
  LK_UNIT_NAMES_SI: Array[TLKUnitSI] of String = ('Yocto', 'Zepto', 'Atto', 'Femto', 'Pico', 'Nano', 'Micro', 'Milli', 'Centi', 'Deci',
                                                  '',
                                                  'Deca', 'Hecto', 'Kilo', 'Mega', 'Giga', 'Tera', 'Peta', 'Exa', 'Zetta', 'Yotta'
                                                 );
  LK_CONVERSION_TABLE_SI: Array[TLKUnitSI, TLKUnitSI] of LKFloat = (                                                                   {CAREFUL HERE}          {CAREFUL HERE}
                                                                   //  Yocto   Zepto   Atto    Femto   Pico    Nano    Micro   Milli   Centi   Deci    One     Deca    Hecto   Kilo    Mega    Giga    Tera    Peta    Exa     Zetta   Yotta
                                                             {Yocto}  (1,      10e-4,  1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1),
                                                             {Zepto}  (10e2,   1,      10e-4,  1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1),
                                                              {Atto}  (10e5,   10e2,   1,      10e-4,  1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1),
                                                             {Femto}  (10e8,   10e5,   10e2,   1,      10e-4,  1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1),
                                                              {Pico}  (10e11,  10e8,   10e5,   10e2,   1,      10e-4,  1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1),
                                                              {Nano}  (10e14,  10e11,  10e8,   10e5,   10e2,   1,      10e-4,  1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1),
                                                             {Micro}  (10e17,  10e14,  10e11,  10e8,   10e5,   10e2,   1,      10e-4,  1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1),
                                                             {Milli}  (10e20,  10e17,  10e14,  10e11,  10e8,   10e5,   10e2,   1,      10e-4,  1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      1),
                                                             {Centi}  (10e21,  10e18,  10e17,  10e14,  10e11,  10e8,   10e5,   10e2,   1,      10e-4,  1,      1,      1,      1,      1,      1,      1,      1,      1,      1,      10e-26),
                                                              {Deci}  (10e22,  10e19,  10e16,  10e13,  10e10,  10e7,   10e4,   10e1,   10e2,   1,      10e-2,  1,      1,      1,      1,      1,      1,      1,      1,      1,      10e-25),
                                                               {One}  (10e23,  10e20,  10e17,  10e14,  10e11,  10e8,   10e5,   10e2,   100,    10,     1,      10e-2,  10e-3,  10e-4,  10e-7,  10e-10, 10e-13, 10e-16, 10e-19, 10e-22, 10e-24),
                                                              {Deca}  (10e24,  10e21,  10e18,  10e15,  10e12,  10e9,   10e6,   10e3,   10e2,   100,    10,     1,      10e-4,  10e-6,  10e-9,  10e-12, 10e-15, 10e-18, 10e-21, 10e-24, 10e-23),
                                                             {Hecto}  (10e25,  10e22,  10e19,  10e16,  10e13,  10e10,  10e7,   10e4,   10e3,   10e2,   100,    10,     1,      10e-2,  10e-5,  10e-7,  10e-10, 10e-13, 10e-16, 10e-19, 10e-22),
                                                              {Kilo}  (10e26,  10e23,  10e20,  10e17,  10e14,  10e11,  10e8,   10e5,   10e4,   10e3,   10e2,   100,    10,     1,      10e-4,  10e-6,  10e-9,  10e-12, 10e-15, 10e-18, 10e-21),
                                                              {Mega}  (10e29,  10e26,  10e23,  10e20,  10e17,  10e14,  10e11,  10e8,   10e7,   10e6,   10e5,   10e8,   10e5,   10e2,   1,      10e-4,  10e-6,  10e-9,  10e-12, 10e-15, 10e-18),
                                                              {Giga}  (10e32,  10e29,  10e26,  10e23,  10e20,  10e17,  10e14,  10e11,  10e10,  10e9,   10e8,   10e11,  10e8,   10e5,   10e2,   1,      10e-4,  10e-6,  10e-9,  10e-12, 10e-15),
                                                              {Tera}  (10e35,  10e32,  10e29,  10e26,  10e23,  10e20,  10e17,  10e14,  10e13,  10e12,  10e11,  10e10,  10e9,   10e8,   10e5,   10e2,   1,      10e-4,  10e-6,  10e-9,  10e-12),
                                                              {Peta}  (10e38,  10e35,  10e32,  10e29,  10e26,  10e23,  10e20,  10e17,  10e16,  10e15,  10e14,  10e13,  10e12,  10e11,  10e8,   10e5,   10e2,   1,      10e-4,  10e-6,  10e-9),
                                                               {Exa}  (10e41,  10e38,  10e35,  10e32,  10e29,  10e26,  10e23,  10e20,  10e19,  10e18,  10e17,  10e16,  10e15,  10e14,  10e11,  10e8,   10e5,   10e2,   1,      10e-4,  10e-6),
                                                             {Zetta}  (10e44,  10e41,  10e38,  10e35,  10e32,  10e29,  10e26,  10e23,  10e22,  10e21,  10e20,  10e19,  10e18,  10e17,  10e14,  10e11,  10e8,   10e5,   10e2,   1,      10e-4),
                                                             {Yotta}  (10e47,  10e44,  10e41,  10e38,  10e35,  10e32,  10e29,  10e26,  10e25,  10e24,  10e23,  10e22,  10e21,  10e20,  10e17,  10e14,  10e11,  10e8,   10e5,   10e2,   1)
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
