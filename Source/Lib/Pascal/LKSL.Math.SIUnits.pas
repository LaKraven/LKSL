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
unit LKSL.Math.SIUnits;

interface

{$I LKSL.inc}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.Math,
  {$ELSE}
    Classes, SysUtils, Math,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types;

  {$I LKSL_RTTI.inc}

type
  { Enum Types }
  TLKSIUnitNotation = (unShort, unLong);
  TLKSIMagnitude = (simYocto, simZepto, simAtto, simFemto, simPico, simNano, simMicro, simMilli, simCenti, simDeci,
                     simOne,
                     simDeca, simHecto, simKilo, simMega, simGiga, simTera, simPeta, simExa, simZetta, simYotta);
  TLKSIBaseUnitType = (sbtLength, sbtMass, sbtTime, sbtCurrent, sbtTemperature, sbtSubstance, sbtLuminousIntensity);
  TLKSIBaseUnits = (sbuMetre, sbuKilogram, sbuSecond, sbuAmpere, sbuKelvin, sbuMole, sbuCandela);

  { Array Types }
  TLKSIUnitNotations = Array[TLKSIUnitNotation] of String;

  ///  <summary><c>Describes the "properties" of a Unit from the International System of Units.</c></summary>
  TLKSIUnit = record
    Name: String;
    Symbol: String;
    BaseMagnitude: TLKSIMagnitude;
    Notations: TLKSIUnitNotations;
  end;

const
  LK_UNIT_METRE: TLKSIUnit =   (
                                  Name: 'metre';
                                  Symbol: 'm';
                                  BaseMagnitude: simOne;
                                  Notations: ('m', 'metre');
                               );

  LK_UNIT_GRAM: TLKSIUnit =    (
                                  Name: 'gram';
                                  Symbol: 'g';
                                  BaseMagnitude: simKilo;
                                  Notations: ('g', 'gram');
                               );

  LK_UNIT_SECOND: TLKSIUnit =  (
                                  Name: 'second';
                                  Symbol: 's';
                                  BaseMagnitude: simOne;
                                  Notations: ('s', 'second');
                               );

  LK_UNIT_AMPERE: TLKSIUnit =  (
                                  Name: 'ampere';
                                  Symbol: 'A';
                                  BaseMagnitude: simOne;
                                  Notations: ('A', 'ampere');
                               );

  LK_UNIT_KELVIN: TLKSIUnit =  (
                                  Name: 'kelvin';
                                  Symbol: 'K';
                                  BaseMagnitude: simOne;
                                  Notations: ('K', 'kelvin');
                               );

  LK_UNIT_MOLE: TLKSIUnit =    (
                                  Name: 'mole';
                                  Symbol: 'mol';
                                  BaseMagnitude: simOne;
                                  Notations: ('mol', 'mole');
                               );

  LK_UNIT_CANDELA: TLKSIUnit = (
                                  Name: 'candela';
                                  Symbol: 'cd';
                                  BaseMagnitude: simOne;
                                  Notations: ('cd', 'candela');
                               );

  LK_UNIT_MAGNITUDE_NAMES_SI: Array[TLKSIMagnitude, TLKSIUnitNotation] of String = (
                                                                                     ('y', 'Yocto'), ('z', 'Zepto'), ('a', 'Atto'), ('f', 'Femto'), ('p', 'Pico'), ('n', 'Nano'), ('µ', 'Micro'), ('m', 'Milli'), ('c', 'Centi'), ('d', 'Deci'),
                                                                                     ('', ''),
                                                                                     ('da', 'Deca'), ('h', 'Hecto'), ('k', 'Kilo'), ('M', 'Mega'), ('G', 'Giga'), ('T', 'Tera'), ('P', 'Peta'), ('E', 'Exa'), ('Z', 'Zetta'), ('Y', 'Yotta')
                                                                                   );

  {$REGION 'Magnitude Conversion Table'}
    LK_UNIT_MAGNITUDE_CONVERSIONTABLE_SI: Array[TLKSIMagnitude, TLKSIMagnitude] of LKFloat = (
                                                                               //  Yocto   Zepto   Atto    Femto   Pico    Nano    Micro   Milli   Centi   Deci    One     Deca    Hecto   Kilo    Mega    Giga    Tera    Peta    Exa     Zetta   Yotta
                                                                         {Yocto}  (1,      1e-3,   1e-6,   1e-9,   1e-12,  1e-15,  1e-18,  1e-21,  1e-22,  1e-23,  1e-24,  1e-25,  1e-26,  1e-27,  1e-30,  1e-33,  1e-36,  1e-39,  1e-42,  1e-45,  1e-48),
                                                                         {Zepto}  (1e3,    1,      1e-3,   1e-6,   1e-9,   1e-12,  1e-15,  1e-18,  1e-19,  1e-20,  1e-21,  1e-22,  1e-23,  1e-24,  1e-27,  1e-30,  1e-33,  1e-36,  1e-39,  1e-42,  1e-45),
                                                                          {Atto}  (1e6,    1e3,    1,      1e-3,   1e-6,   1e-9,   1e-12,  1e-15,  1e-16,  1e-17,  1e-18,  1e-19,  1e-20,  1e-21,  1e-24,  1e-27,  1e-30,  1e-33,  1e-36,  1e-39,  1e-42),
                                                                         {Femto}  (1e9,    1e6,    1e3,    1,      1e-3,   1e-6,   1e-9,   1e-12,  1e-13,  1e-14,  1e-15,  1e-16,  1e-17,  1e-18,  1e-21,  1e-24,  1e-27,  1e-30,  1e-33,  1e-36,  1e-39),
                                                                          {Pico}  (1e12,   1e9,    1e6,    1e3,    1,      1e-3,   1e-6,   1e-9,   1e-10,  1e-11,  1e-12,  1e-13,  1e-14,  1e-15,  1e-18,  1e-21,  1e-24,  1e-27,  1e-30,  1e-33,  1e-36),
                                                                          {Nano}  (1e15,   1e12,   1e9,    1e6,    1e3,    1,      1e-3,   1e-6,   1e-7,   1e-8,   1e-9,   1e-10,  1e-11,  1e-12,  1e-15,  1e-18,  1e-21,  1e-24,  1e-27,  1e-30,  1e-33),
                                                                         {Micro}  (1e18,   1e15,   1e12,   1e9,    1e6,    1e3,    1,      1e-3,   1e-4,   1e-5,   1e-6,   1e-7,   1e-8,   1e-9,   1e-12,  1e-15,  1e-18,  1e-21,  1e-24,  1e-27,  1e-30),
                                                                         {Milli}  (1e21,   1e18,   1e15,   1e12,   1e9,    1e6,    1e1,    1,      1e-1,   1e-2,   1e-3,   1e-4,   1e-5,   1e-6,   1e-9,   1e-12,  1e-15,  1e-18,  1e-21,  1e-24,  1e-27),
                                                                         {Centi}  (1e22,   1e19,   1e16,   1e13,   1e10,   1e7,    1e4,    1e1,    1,      1e-1,   1e-2,   1e-3,   1e-4,   1e-5,   1e-8,   1e-11,  1e-14,  1e-17,  1e-20,  1e-23,  1e-26),
                                                                          {Deci}  (1e23,   1e20,   1e17,   1e14,   1e11,   1e8,    1e5,    1e2,    1e1,    1,      1e-1,   1e-2,   1e-3,   1e-4,   1e-7,   1e-10,  1e-13,  1e-16,  1e-19,  1e-22,  1e-25),
                                                                           {One}  (1e24,   1e21,   1e18,   1e15,   1e12,   1e9,    1e6,    1e3,    1e2,    1e1,    1,      1e-1,   1e-2,   1e-3,   1e-6,   1e-9,   1e-12,  1e-15,  1e-18,  1e-21,  1e-24),
                                                                          {Deca}  (1e25,   1e22,   1e19,   1e16,   1e13,   1e10,   1e7,    1e4,    1e3,    1e2,    1e1,    1,      1e-1,   1e-2,   1e-5,   1e-8,   1e-11,  1e-14,  1e-17,  1e-20,  1e-23),
                                                                         {Hecto}  (1e26,   1e23,   1e20,   1e17,   1e14,   1e11,   1e8,    1e5,    1e4,    1e3,    1e2,    1e1,    1,      1e-1,   1e-4,   1e-7,   1e-10,  1e-13,  1e-16,  1e-19,  1e-22),
                                                                          {Kilo}  (1e27,   1e24,   1e21,   1e18,   1e15,   1e12,   1e9,    1e6,    1e5,    1e4,    1e3,    1e2,    1e1,    1,      1e-3,   1e-6,   1e-9,   1e-12,  1e-15,  1e-18,  1e-21),
                                                                          {Mega}  (1e30,   1e27,   1e24,   1e21,   1e18,   1e15,   1e12,   1e9,    1e8,    1e7,    1e6,    1e5,    1e4,    1e3,    1,      1e-3,   1e-6,   1e-9,   1e-12,  1e-15,  1e-18),
                                                                          {Giga}  (1e33,   1e30,   1e27,   1e24,   1e21,   1e18,   1e15,   1e12,   1e11,   1e10,   1e9,    1e8,    1e7,    1e6,    1e3,    1,      1e-3,   1e-6,   1e-9,   1e-12,  1e-15),
                                                                          {Tera}  (1e36,   1e33,   1e30,   1e27,   1e24,   1e21,   1e18,   1e15,   1e14,   1e13,   1e12,   1e11,   1e10,   1e9,    1e6,    1e3,    1,      1e-3,   1e-6,   1e-9,   1e-12),
                                                                          {Peta}  (1e39,   1e36,   1e33,   1e30,   1e27,   1e24,   1e21,   1e18,   1e17,   1e16,   1e15,   1e14,   1e13,   1e12,   1e9,    1e6,    1e3,    1,      1e-3,   1e-6,   1e-9),
                                                                           {Exa}  (1e42,   1e39,   1e36,   1e33,   1e30,   1e27,   1e24,   1e21,   1e20,   1e19,   1e18,   1e17,   1e16,   1e15,   1e12,   1e9,    1e6,    1e3,    1,      1e-3,   1e-6),
                                                                         {Zetta}  (1e45,   1e42,   1e39,   1e36,   1e33,   1e30,   1e27,   1e24,   1e23,   1e22,   1e21,   1e20,   1e19,   1e18,   1e15,   1e12,   1e9,    1e6,    1e3,    1,      1e-3),
                                                                         {Yotta}  (1e48,   1e45,   1e42,   1e39,   1e36,   1e33,   1e30,   1e27,   1e26,   1e25,   1e24,   1e23,   1e22,   1e21,   1e18,   1e15,   1e12,   1e9,    1e6,    1e3,    1)
                                                                               );
  {$ENDREGION}

function SIMagnitudeConvert(const ASourceValue: LKFloat; const AFromMagnitude, AToMagnitude: TLKSIMagnitude): LKFloat; inline;
function SIMagnitudeGetNotationText(const AMagnitude: TLKSIMagnitude; const ANotation: TLKSIUnitNotation): String; inline;
procedure SIMagnitudeToBest(const AInValue: LKFloat; const AInMagnitude: TLKSIMagnitude; var AOutValue: LKFloat; var AOutMagnitude: TLKSIMagnitude); overload; inline;
procedure SIMagnitudeToBest(var AValue: LKFloat; var AOutMagnitude: TLKSIMagnitude); overload; inline;

implementation

{ LKUnitConversionSI }

function SIMagnitudeConvert(const ASourceValue: LKFloat; const AFromMagnitude, AToMagnitude: TLKSIMagnitude): LKFloat;
begin
  Result := ASourceValue * LK_UNIT_MAGNITUDE_CONVERSIONTABLE_SI[AFromMagnitude, AToMagnitude];
end;

function SIMagnitudeGetNotationText(const AMagnitude: TLKSIMagnitude; const ANotation: TLKSIUnitNotation): String;
begin
  Result := LK_UNIT_MAGNITUDE_NAMES_SI[AMagnitude, ANotation];
end;

procedure SIMagnitudeToBest(var AValue: LKFloat; var AOutMagnitude: TLKSIMagnitude);
begin
  { TODO -oSJS-cSI Units : Implement method to determine most appropriate Order of Magnitude to represent given value }
end;

procedure SIMagnitudeToBest(const AInValue: LKFloat; const AInMagnitude: TLKSIMagnitude; var AOutValue: LKFloat; var AOutMagnitude: TLKSIMagnitude);
begin
  // Presume that no conversion is required.
  AOutValue := AInValue;
  AOutMagnitude := AInMagnitude;
  SIMagnitudeToBest(AOutValue, AOutMagnitude);
end;

end.
