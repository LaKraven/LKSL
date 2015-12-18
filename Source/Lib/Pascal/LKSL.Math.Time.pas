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
unit LKSL.Math.Time;

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
  TLKTimeUnit = (tuYoctosecond, tuZeptosecond, tuAttosecond, tuFemtosecond, tuPicosecond, tuNanosecond, tuMicrosecond, tuMillisecond,
                 tuSecond, tuKilosecond, tuMegasecond, tuGigasecond, tuTerasecond, tuPetasecond, tuExasecond, tuZettasecond, tuYottasecond);

  TLKTimeUnitNotation = (tunShort, tunLong);

  {
    TLKTime
      - Contains a Time Value and its Time Unit enumerable Type
  }
  TLKTime = record
    TimeUnit: TLKTimeUnit;
    TimeValue: LKFloat;
  end;

const
  LKTimeUnits: Array[TLKTimeUnit] of Array[TLKTimeUnitNotation] of String = (
                                                                               ('ys', 'Yoctosecond'), // Smallest unit
                                                                               ('zs', 'Zeptosecond'),
                                                                               ('as', 'Attosecond'),
                                                                               ('fs', 'Femtosecond'),
                                                                               ('ps', 'Picosecond'),
                                                                               ('ns', 'Nanosecond'),
                                                                               ('µs', 'Microsecond'),
                                                                               ('ms', 'Millisecond'),
                                                                               ('s', 'Second'),       // Standard Unit
                                                                               ('ks', 'Kilosecond'),
                                                                               ('Ms', 'Megasecond'),
                                                                               ('Gs', 'Gigasecond'),
                                                                               ('Ts', 'Terasecond'),
                                                                               ('Ps', 'Petasecond'),
                                                                               ('Es', 'Exasecond'),
                                                                               ('Zs', 'Zettasecond'),
                                                                               ('Ys', 'Yottasecond')  // Largest Unit
                                                                            );
  LKTimeUnitLongNotation: Array[Boolean] of TLKTimeUnitNotation = (tunShort, tunLong);
// ? To ?
function TimeUnitConvert(const ASourceValue: LKFloat; const AInputUnit, AOutputUnit: TLKTimeUnit): LKFloat;
// Yoctoseconds To ?
procedure YoctosecondsToBest(const AYoctoseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function YoctosecondsToBest(const AYoctoseconds: LKFloat): TLKTime; overload; inline;
function YoctosecondsToZeptoseconds(const AYoctoseconds: LKFloat): LKFloat; inline;
function YoctosecondsToAttoseconds(const AYoctoseconds: LKFloat): LKFloat; inline;
function YoctosecondsToFemtoseconds(const AYoctoseconds: LKFloat): LKFloat; inline;
function YoctosecondsToPicoseconds(const AYoctoseconds: LKFloat): LKFloat; inline;
function YoctosecondsToNanoseconds(const AYoctoseconds: LKFloat): LKFloat; inline;
function YoctosecondsToMicroseconds(const AYoctoseconds: LKFloat): LKFloat; inline;
function YoctosecondsToMilliseconds(const AYoctoseconds: LKFloat): LKFloat; inline;
function YoctosecondsToSeconds(const AYoctoseconds: LKFloat): LKFloat; inline;
function YoctosecondsToKiloseconds(const AYoctoseconds: LKFloat): LKFloat; inline;
function YoctosecondsToMegaseconds(const AYoctoseconds: LKFloat): LKFloat; inline;
function YoctosecondsToGigaseconds(const AYoctoseconds: LKFloat): LKFloat; inline;
function YoctosecondsToTeraseconds(const AYoctoseconds: LKFloat): LKFloat; inline;
function YoctosecondsToPetaseconds(const AYoctoseconds: LKFloat): LKFloat; inline;
function YoctosecondsToExaseconds(const AYoctoseconds: LKFloat): LKFloat; inline;
function YoctosecondsToZettaseconds(const AYoctoseconds: LKFloat): LKFloat; inline;
function YoctosecondsToYottaseconds(const AYoctoseconds: LKFloat): LKFloat; inline;
// Zeptoseconds To ?
procedure ZeptosecondsToBest(const AZeptoseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function ZeptosecondsToBest(const AZeptoseconds: LKFloat): TLKTime; overload; inline;
function ZeptosecondsToYoctoseconds(const AZeptoseconds: LKFloat): LKFloat; inline; // Down
function ZeptosecondsToAttoseconds(const AZeptoseconds: LKFloat): LKFloat; inline;
function ZeptosecondsToFemtoseconds(const AZeptoseconds: LKFloat): LKFloat; inline;
function ZeptosecondsToPicoseconds(const AZeptoseconds: LKFloat): LKFloat; inline;
function ZeptosecondsToNanoseconds(const AZeptoseconds: LKFloat): LKFloat; inline;
function ZeptosecondsToMicroseconds(const AZeptoseconds: LKFloat): LKFloat; inline;
function ZeptosecondsToMilliseconds(const AZeptoseconds: LKFloat): LKFloat; inline;
function ZeptosecondsToSeconds(const AZeptoseconds: LKFloat): LKFloat; inline;
function ZeptosecondsToKiloseconds(const AZeptoseconds: LKFloat): LKFloat; inline;
function ZeptosecondsToMegaseconds(const AZeptoseconds: LKFloat): LKFloat; inline;
function ZeptosecondsToGigaseconds(const AZeptoseconds: LKFloat): LKFloat; inline;
function ZeptosecondsToTeraseconds(const AZeptoseconds: LKFloat): LKFloat; inline;
function ZeptosecondsToPetaseconds(const AZeptoseconds: LKFloat): LKFloat; inline;
function ZeptosecondsToExaseconds(const AZeptoseconds: LKFloat): LKFloat; inline;
function ZeptosecondsToZettaseconds(const AZeptoseconds: LKFloat): LKFloat; inline;
function ZeptosecondsToYottaseconds(const AZeptoseconds: LKFloat): LKFloat; inline;
// Attoseconds To ?
procedure AttosecondsToBest(const AAttoseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function AttosecondsToBest(const AAttoseconds: LKFloat): TLKTime; overload; inline;
function AttosecondsToYoctoseconds(const AAttoseconds: LKFloat): LKFloat; inline;
function AttosecondsToZeptoseconds(const AAttoseconds: LKFloat): LKFloat; inline; // Down
function AttosecondsToFemtoseconds(const AAttoseconds: LKFloat): LKFloat; inline;
function AttosecondsToPicoseconds(const AAttoseconds: LKFloat): LKFloat; inline;
function AttosecondsToNanoseconds(const AAttoseconds: LKFloat): LKFloat; inline;
function AttosecondsToMicroseconds(const AAttoseconds: LKFloat): LKFloat; inline;
function AttosecondsToMilliseconds(const AAttoseconds: LKFloat): LKFloat; inline;
function AttosecondsToSeconds(const AAttoseconds: LKFloat): LKFloat; inline;
function AttosecondsToKiloseconds(const AAttoseconds: LKFloat): LKFloat; inline;
function AttosecondsToMegaseconds(const AAttoseconds: LKFloat): LKFloat; inline;
function AttosecondsToGigaseconds(const AAttoseconds: LKFloat): LKFloat; inline;
function AttosecondsToTeraseconds(const AAttoseconds: LKFloat): LKFloat; inline;
function AttosecondsToPetaseconds(const AAttoseconds: LKFloat): LKFloat; inline;
function AttosecondsToExaseconds(const AAttoseconds: LKFloat): LKFloat; inline;
function AttosecondsToZettaseconds(const AAttoseconds: LKFloat): LKFloat; inline;
function AttosecondsToYottaseconds(const AAttoseconds: LKFloat): LKFloat; inline;
// Femtoseconds To ?
procedure FemtosecondsToBest(const AFemtoseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function FemtosecondsToBest(const AFemtoseconds: LKFloat): TLKTime; overload; inline;
function FemtosecondsToYoctoseconds(const AFemtoseconds: LKFloat): LKFloat; inline;
function FemtosecondsToZeptoseconds(const AFemtoseconds: LKFloat): LKFloat; inline;
function FemtosecondsToAttoseconds(const AFemtoseconds: LKFloat): LKFloat; inline; // Down
function FemtosecondsToPicoseconds(const AFemtoseconds: LKFloat): LKFloat; inline;
function FemtosecondsToNanoseconds(const AFemtoseconds: LKFloat): LKFloat; inline;
function FemtosecondsToMicroseconds(const AFemtoseconds: LKFloat): LKFloat; inline;
function FemtosecondsToMilliseconds(const AFemtoseconds: LKFloat): LKFloat; inline;
function FemtosecondsToSeconds(const AFemtoseconds: LKFloat): LKFloat; inline;
function FemtosecondsToKiloseconds(const AFemtoseconds: LKFloat): LKFloat; inline;
function FemtosecondsToMegaseconds(const AFemtoseconds: LKFloat): LKFloat; inline;
function FemtosecondsToGigaseconds(const AFemtoseconds: LKFloat): LKFloat; inline;
function FemtosecondsToTeraseconds(const AFemtoseconds: LKFloat): LKFloat; inline;
function FemtosecondsToPetaseconds(const AFemtoseconds: LKFloat): LKFloat; inline;
function FemtosecondsToExaseconds(const AFemtoseconds: LKFloat): LKFloat; inline;
function FemtosecondsToZettaseconds(const AFemtoseconds: LKFloat): LKFloat; inline;
function FemtosecondsToYottaseconds(const AFemtoseconds: LKFloat): LKFloat; inline;
// Picoseconds To ?
procedure PicosecondsToBest(const APicoseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function PicosecondsToBest(const APicoseconds: LKFloat): TLKTime; overload; inline;
function PicosecondsToYoctoseconds(const APicoseconds: LKFloat): LKFloat; inline;
function PicosecondsToZeptoseconds(const APicoseconds: LKFloat): LKFloat; inline;
function PicosecondsToAttoseconds(const APicoseconds: LKFloat): LKFloat; inline;
function PicosecondsToFemtoseconds(const APicoseconds: LKFloat): LKFloat; inline; // Down
function PicosecondsToNanoseconds(const APicoseconds: LKFloat): LKFloat; inline;
function PicosecondsToMicroseconds(const APicoseconds: LKFloat): LKFloat; inline;
function PicosecondsToMilliseconds(const APicoseconds: LKFloat): LKFloat; inline;
function PicosecondsToSeconds(const APicoseconds: LKFloat): LKFloat; inline;
function PicosecondsToKiloseconds(const APicoseconds: LKFloat): LKFloat; inline;
function PicosecondsToMegaseconds(const APicoseconds: LKFloat): LKFloat; inline;
function PicosecondsToGigaseconds(const APicoseconds: LKFloat): LKFloat; inline;
function PicosecondsToTeraseconds(const APicoseconds: LKFloat): LKFloat; inline;
function PicosecondsToPetaseconds(const APicoseconds: LKFloat): LKFloat; inline;
function PicosecondsToExaseconds(const APicoseconds: LKFloat): LKFloat; inline;
function PicosecondsToZettaseconds(const APicoseconds: LKFloat): LKFloat; inline;
function PicosecondsToYottaseconds(const APicoseconds: LKFloat): LKFloat; inline;
// Nanoseconds To ?
procedure NanosecondsToBest(const ANanoseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function NanosecondsToBest(const ANanoseconds: LKFloat): TLKTime; overload; inline;
function NanosecondsToYoctoseconds(const ANanoseconds: LKFloat): LKFloat; inline;
function NanosecondsToZeptoseconds(const ANanoseconds: LKFloat): LKFloat; inline;
function NanosecondsToAttoseconds(const ANanoseconds: LKFloat): LKFloat; inline;
function NanosecondsToFemtoseconds(const ANanoseconds: LKFloat): LKFloat; inline;
function NanosecondsToPicoseconds(const ANanoseconds: LKFloat): LKFloat; inline; // Down
function NanosecondsToMicroseconds(const ANanoseconds: LKFloat): LKFloat; inline;
function NanosecondsToMilliseconds(const ANanoseconds: LKFloat): LKFloat; inline;
function NanosecondsToSeconds(const ANanoseconds: LKFloat): LKFloat; inline;
function NanosecondsToKiloseconds(const ANanoseconds: LKFloat): LKFloat; inline;
function NanosecondsToMegaseconds(const ANanoseconds: LKFloat): LKFloat; inline;
function NanosecondsToGigaseconds(const ANanoseconds: LKFloat): LKFloat; inline;
function NanosecondsToTeraseconds(const ANanoseconds: LKFloat): LKFloat; inline;
function NanosecondsToPetaseconds(const ANanoseconds: LKFloat): LKFloat; inline;
function NanosecondsToExaseconds(const ANanoseconds: LKFloat): LKFloat; inline;
function NanosecondsToZettaseconds(const ANanoseconds: LKFloat): LKFloat; inline;
function NanosecondsToYottaseconds(const ANanoseconds: LKFloat): LKFloat; inline;
// Microseconds To ?
procedure MicrosecondsToBest(const AMicroseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function MicrosecondsToBest(const AMicroseconds: LKFloat): TLKTime; overload; inline;
function MicrosecondsToYoctoseconds(const AMicroseconds: LKFloat): LKFloat; inline;
function MicrosecondsToZeptoseconds(const AMicroseconds: LKFloat): LKFloat; inline;
function MicrosecondsToAttoseconds(const AMicroseconds: LKFloat): LKFloat; inline;
function MicrosecondsToFemtoseconds(const AMicroseconds: LKFloat): LKFloat; inline;
function MicrosecondsToPicoseconds(const AMicroseconds: LKFloat): LKFloat; inline;
function MicrosecondsToNanoseconds(const AMicroseconds: LKFloat): LKFloat; inline; // Down
function MicrosecondsToMilliseconds(const AMicroseconds: LKFloat): LKFloat; inline;
function MicrosecondsToSeconds(const AMicroseconds: LKFloat): LKFloat; inline;
function MicrosecondsToKiloseconds(const AMicroseconds: LKFloat): LKFloat; inline;
function MicrosecondsToMegaseconds(const AMicroseconds: LKFloat): LKFloat; inline;
function MicrosecondsToGigaseconds(const AMicroseconds: LKFloat): LKFloat; inline;
function MicrosecondsToTeraseconds(const AMicroseconds: LKFloat): LKFloat; inline;
function MicrosecondsToPetaseconds(const AMicroseconds: LKFloat): LKFloat; inline;
function MicrosecondsToExaseconds(const AMicroseconds: LKFloat): LKFloat; inline;
function MicrosecondsToZettaseconds(const AMicroseconds: LKFloat): LKFloat; inline;
function MicrosecondsToYottaseconds(const AMicroseconds: LKFloat): LKFloat; inline;
// Milliseconds To ?
procedure MillisecondsToBest(const AMilliseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function MillisecondsToBest(const AMilliseconds: LKFloat): TLKTime; overload; inline;
function MillisecondsToYoctoseconds(const AMilliseconds: LKFloat): LKFloat; inline;
function MillisecondsToZeptoseconds(const AMilliseconds: LKFloat): LKFloat; inline;
function MillisecondsToAttoseconds(const AMilliseconds: LKFloat): LKFloat; inline;
function MillisecondsToFemtoseconds(const AMilliseconds: LKFloat): LKFloat; inline;
function MillisecondsToPicoseconds(const AMilliseconds: LKFloat): LKFloat; inline;
function MillisecondsToNanoseconds(const AMilliseconds: LKFloat): LKFloat; inline;
function MillisecondsToMicroseconds(const AMilliseconds: LKFloat): LKFloat; inline; // Down
function MillisecondsToSeconds(const AMilliseconds: LKFloat): LKFloat; inline;
function MillisecondsToKiloseconds(const AMilliseconds: LKFloat): LKFloat; inline;
function MillisecondsToMegaseconds(const AMilliseconds: LKFloat): LKFloat; inline;
function MillisecondsToGigaseconds(const AMilliseconds: LKFloat): LKFloat; inline;
function MillisecondsToTeraseconds(const AMilliseconds: LKFloat): LKFloat; inline;
function MillisecondsToPetaseconds(const AMilliseconds: LKFloat): LKFloat; inline;
function MillisecondsToExaseconds(const AMilliseconds: LKFloat): LKFloat; inline;
function MillisecondsToZettaseconds(const AMilliseconds: LKFloat): LKFloat; inline;
function MillisecondsToYottaseconds(const AMilliseconds: LKFloat): LKFloat; inline;
// Seconds To ?
procedure SecondsToBest(const ASeconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function SecondsToBest(const ASeconds: LKFloat): TLKTime; overload; inline;
function SecondsToYoctoseconds(const ASeconds: LKFloat): LKFloat; inline;
function SecondsToZeptoseconds(const ASeconds: LKFloat): LKFloat; inline;
function SecondsToAttoseconds(const ASeconds: LKFloat): LKFloat; inline;
function SecondsToFemtoseconds(const ASeconds: LKFloat): LKFloat; inline;
function SecondsToPicoseconds(const ASeconds: LKFloat): LKFloat; inline;
function SecondsToNanoseconds(const ASeconds: LKFloat): LKFloat; inline;
function SecondsToMicroseconds(const ASeconds: LKFloat): LKFloat; inline;
function SecondsToMilliseconds(const ASeconds: LKFloat): LKFloat; inline;
function SecondsToKiloseconds(const ASeconds: LKFloat): LKFloat; inline;
function SecondsToMegaseconds(const ASeconds: LKFloat): LKFloat; inline;
function SecondsToGigaseconds(const ASeconds: LKFloat): LKFloat; inline;
function SecondsToTeraseconds(const ASeconds: LKFloat): LKFloat; inline;
function SecondsToPetaseconds(const ASeconds: LKFloat): LKFloat; inline;
function SecondsToExaseconds(const ASeconds: LKFloat): LKFloat; inline;
function SecondsToZettaseconds(const ASeconds: LKFloat): LKFloat; inline;
function SecondsToYottaseconds(const ASeconds: LKFloat): LKFloat; inline;
// Kiloseconds To ?
procedure KilosecondsToBest(const AKiloseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function KilosecondsToBest(const AKiloseconds: LKFloat): TLKTime; overload; inline;
function KilosecondsToYoctoseconds(const AKiloseconds: LKFloat): LKFloat; inline;
function KilosecondsToZeptoseconds(const AKiloseconds: LKFloat): LKFloat; inline;
function KilosecondsToAttoseconds(const AKiloseconds: LKFloat): LKFloat; inline;
function KilosecondsToFemtoseconds(const AKiloseconds: LKFloat): LKFloat; inline;
function KilosecondsToPicoseconds(const AKiloseconds: LKFloat): LKFloat; inline;
function KilosecondsToNanoseconds(const AKiloseconds: LKFloat): LKFloat; inline;
function KilosecondsToMicroseconds(const AKiloseconds: LKFloat): LKFloat; inline;
function KilosecondsToMilliseconds(const AKiloseconds: LKFloat): LKFloat; inline;
function KilosecondsToSeconds(const AKiloseconds: LKFloat): LKFloat; inline; // Down
function KilosecondsToMegaseconds(const AKiloseconds: LKFloat): LKFloat; inline;
function KilosecondsToGigaseconds(const AKiloseconds: LKFloat): LKFloat; inline;
function KilosecondsToTeraseconds(const AKiloseconds: LKFloat): LKFloat; inline;
function KilosecondsToPetaseconds(const AKiloseconds: LKFloat): LKFloat; inline;
function KilosecondsToExaseconds(const AKiloseconds: LKFloat): LKFloat; inline;
function KilosecondsToZettaseconds(const AKiloseconds: LKFloat): LKFloat; inline;
function KilosecondsToYottaseconds(const AKiloseconds: LKFloat): LKFloat; inline;
// Megaseconds To ?
procedure MegasecondsToBest(const AMegaseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function MegasecondsToBest(const AMegaseconds: LKFloat): TLKTime; overload; inline;
function MegasecondsToYoctoseconds(const AMegaseconds: LKFloat): LKFloat; inline;
function MegasecondsToZeptoseconds(const AMegaseconds: LKFloat): LKFloat; inline;
function MegasecondsToAttoseconds(const AMegaseconds: LKFloat): LKFloat; inline;
function MegasecondsToFemtoseconds(const AMegaseconds: LKFloat): LKFloat; inline;
function MegasecondsToPicoseconds(const AMegaseconds: LKFloat): LKFloat; inline;
function MegasecondsToNanoseconds(const AMegaseconds: LKFloat): LKFloat; inline;
function MegasecondsToMicroseconds(const AMegaseconds: LKFloat): LKFloat; inline;
function MegasecondsToMilliseconds(const AMegaseconds: LKFloat): LKFloat; inline;
function MegasecondsToSeconds(const AMegaseconds: LKFloat): LKFloat; inline;
function MegasecondsToKiloseconds(const AMegaseconds: LKFloat): LKFloat; inline; // Down
function MegasecondsToGigaseconds(const AMegaseconds: LKFloat): LKFloat; inline;
function MegasecondsToTeraseconds(const AMegaseconds: LKFloat): LKFloat; inline;
function MegasecondsToPetaseconds(const AMegaseconds: LKFloat): LKFloat; inline;
function MegasecondsToExaseconds(const AMegaseconds: LKFloat): LKFloat; inline;
function MegasecondsToZettaseconds(const AMegaseconds: LKFloat): LKFloat; inline;
function MegasecondsToYottaseconds(const AMegaseconds: LKFloat): LKFloat; inline;
// Gigaseconds To ?
procedure GigasecondsToBest(const AGigaseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function GigasecondsToBest(const AGigaseconds: LKFloat): TLKTime; overload; inline;
function GigasecondsToYoctoseconds(const AGigaseconds: LKFloat): LKFloat; inline;
function GigasecondsToZeptoseconds(const AGigaseconds: LKFloat): LKFloat; inline;
function GigasecondsToAttoseconds(const AGigaseconds: LKFloat): LKFloat; inline;
function GigasecondsToFemtoseconds(const AGigaseconds: LKFloat): LKFloat; inline;
function GigasecondsToPicoseconds(const AGigaseconds: LKFloat): LKFloat; inline;
function GigasecondsToNanoseconds(const AGigaseconds: LKFloat): LKFloat; inline;
function GigasecondsToMicroseconds(const AGigaseconds: LKFloat): LKFloat; inline;
function GigasecondsToMilliseconds(const AGigaseconds: LKFloat): LKFloat; inline;
function GigasecondsToSeconds(const AGigaseconds: LKFloat): LKFloat; inline;
function GigasecondsToKiloseconds(const AGigaseconds: LKFloat): LKFloat; inline;
function GigasecondsToMegaseconds(const AGigaseconds: LKFloat): LKFloat; inline; // Down
function GigasecondsToTeraseconds(const AGigaseconds: LKFloat): LKFloat; inline;
function GigasecondsToPetaseconds(const AGigaseconds: LKFloat): LKFloat; inline;
function GigasecondsToExaseconds(const AGigaseconds: LKFloat): LKFloat; inline;
function GigasecondsToZettaseconds(const AGigaseconds: LKFloat): LKFloat; inline;
function GigasecondsToYottaseconds(const AGigaseconds: LKFloat): LKFloat; inline;
// Teraseconds To ?
procedure TerasecondsToBest(const ATeraseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function TerasecondsToBest(const ATeraseconds: LKFloat): TLKTime; overload; inline;
function TerasecondsToYoctoseconds(const ATeraseconds: LKFloat): LKFloat; inline;
function TerasecondsToZeptoseconds(const ATeraseconds: LKFloat): LKFloat; inline;
function TerasecondsToAttoseconds(const ATeraseconds: LKFloat): LKFloat; inline;
function TerasecondsToFemtoseconds(const ATeraseconds: LKFloat): LKFloat; inline;
function TerasecondsToPicoseconds(const ATeraseconds: LKFloat): LKFloat; inline;
function TerasecondsToNanoseconds(const ATeraseconds: LKFloat): LKFloat; inline;
function TerasecondsToMicroseconds(const ATeraseconds: LKFloat): LKFloat; inline;
function TerasecondsToMilliseconds(const ATeraseconds: LKFloat): LKFloat; inline;
function TerasecondsToSeconds(const ATeraseconds: LKFloat): LKFloat; inline;
function TerasecondsToKiloseconds(const ATeraseconds: LKFloat): LKFloat; inline;
function TerasecondsToMegaseconds(const ATeraseconds: LKFloat): LKFloat; inline;
function TerasecondsToGigaseconds(const ATeraseconds: LKFloat): LKFloat; inline; // Down
function TerasecondsToPetaseconds(const ATeraseconds: LKFloat): LKFloat; inline;
function TerasecondsToExaseconds(const ATeraseconds: LKFloat): LKFloat; inline;
function TerasecondsToZettaseconds(const ATeraseconds: LKFloat): LKFloat; inline;
function TerasecondsToYottaseconds(const ATeraseconds: LKFloat): LKFloat; inline;
// Petaseconds To ?
procedure PetasecondsToBest(const APetaseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function PetasecondsToBest(const APetaseconds: LKFloat): TLKTime; overload; inline;
function PetasecondsToYoctoseconds(const APetaseconds: LKFloat): LKFloat; inline;
function PetasecondsToZeptoseconds(const APetaseconds: LKFloat): LKFloat; inline;
function PetasecondsToAttoseconds(const APetaseconds: LKFloat): LKFloat; inline;
function PetasecondsToFemtoseconds(const APetaseconds: LKFloat): LKFloat; inline;
function PetasecondsToPicoseconds(const APetaseconds: LKFloat): LKFloat; inline;
function PetasecondsToNanoseconds(const APetaseconds: LKFloat): LKFloat; inline;
function PetasecondsToMicroseconds(const APetaseconds: LKFloat): LKFloat; inline;
function PetasecondsToMilliseconds(const APetaseconds: LKFloat): LKFloat; inline;
function PetasecondsToSeconds(const APetaseconds: LKFloat): LKFloat; inline;
function PetasecondsToKiloseconds(const APetaseconds: LKFloat): LKFloat; inline;
function PetasecondsToMegaseconds(const APetaseconds: LKFloat): LKFloat; inline;
function PetasecondsToGigaseconds(const APetaseconds: LKFloat): LKFloat; inline;
function PetasecondsToTeraseconds(const APetaseconds: LKFloat): LKFloat; inline; // Down
function PetasecondsToExaseconds(const APetaseconds: LKFloat): LKFloat; inline;
function PetasecondsToZettaseconds(const APetaseconds: LKFloat): LKFloat; inline;
function PetasecondsToYottaseconds(const APetaseconds: LKFloat): LKFloat; inline;
// Exaseconds To ?
procedure ExasecondsToBest(const AExaseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function ExasecondsToBest(const AExaseconds: LKFloat): TLKTime; overload; inline;
function ExasecondsToYoctoseconds(const AExaseconds: LKFloat): LKFloat; inline;
function ExasecondsToZeptoseconds(const AExaseconds: LKFloat): LKFloat; inline;
function ExasecondsToAttoseconds(const AExaseconds: LKFloat): LKFloat; inline;
function ExasecondsToFemtoseconds(const AExaseconds: LKFloat): LKFloat; inline;
function ExasecondsToPicoseconds(const AExaseconds: LKFloat): LKFloat; inline;
function ExasecondsToNanoseconds(const AExaseconds: LKFloat): LKFloat; inline;
function ExasecondsToMicroseconds(const AExaseconds: LKFloat): LKFloat; inline;
function ExasecondsToMilliseconds(const AExaseconds: LKFloat): LKFloat; inline;
function ExasecondsToSeconds(const AExaseconds: LKFloat): LKFloat; inline;
function ExasecondsToKiloseconds(const AExaseconds: LKFloat): LKFloat; inline;
function ExasecondsToMegaseconds(const AExaseconds: LKFloat): LKFloat; inline;
function ExasecondsToGigaseconds(const AExaseconds: LKFloat): LKFloat; inline;
function ExasecondsToTeraseconds(const AExaseconds: LKFloat): LKFloat; inline;
function ExasecondsToPetaseconds(const AExaseconds: LKFloat): LKFloat; inline; // Down
function ExasecondsToZettaseconds(const AExaseconds: LKFloat): LKFloat; inline;
function ExasecondsToYottaseconds(const AExaseconds: LKFloat): LKFloat; inline;
// Zettaseconds To ?
procedure ZettasecondsToBest(const AZettaseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function ZettasecondsToBest(const AZettaseconds: LKFloat): TLKTime; overload; inline;
function ZettasecondsToYoctoseconds(const AZettaseconds: LKFloat): LKFloat; inline;
function ZettasecondsToZeptoseconds(const AZettaseconds: LKFloat): LKFloat; inline;
function ZettasecondsToAttoseconds(const AZettaseconds: LKFloat): LKFloat; inline;
function ZettasecondsToFemtoseconds(const AZettaseconds: LKFloat): LKFloat; inline;
function ZettasecondsToPicoseconds(const AZettaseconds: LKFloat): LKFloat; inline;
function ZettasecondsToNanoseconds(const AZettaseconds: LKFloat): LKFloat; inline;
function ZettasecondsToMicroseconds(const AZettaseconds: LKFloat): LKFloat; inline;
function ZettasecondsToMilliseconds(const AZettaseconds: LKFloat): LKFloat; inline;
function ZettasecondsToSeconds(const AZettaseconds: LKFloat): LKFloat; inline;
function ZettasecondsToKiloseconds(const AZettaseconds: LKFloat): LKFloat; inline;
function ZettasecondsToMegaseconds(const AZettaseconds: LKFloat): LKFloat; inline;
function ZettasecondsToGigaseconds(const AZettaseconds: LKFloat): LKFloat; inline;
function ZettasecondsToTeraseconds(const AZettaseconds: LKFloat): LKFloat; inline;
function ZettasecondsToPetaseconds(const AZettaseconds: LKFloat): LKFloat; inline;
function ZettasecondsToExaseconds(const AZettaseconds: LKFloat): LKFloat; inline; // Down
function ZettasecondsToYottaseconds(const AZettaseconds: LKFloat): LKFloat; inline;
// Yottaseconds To ?
procedure YottasecondsToBest(const AYottaseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
function YottasecondsToBest(const AYottaseconds: LKFloat): TLKTime; overload; inline;
function YottasecondsToYoctoseconds(const AYottaseconds: LKFloat): LKFloat; inline;
function YottasecondsToZeptoseconds(const AYottaseconds: LKFloat): LKFloat; inline;
function YottasecondsToAttoseconds(const AYottaseconds: LKFloat): LKFloat; inline;
function YottasecondsToFemtoseconds(const AYottaseconds: LKFloat): LKFloat; inline;
function YottasecondsToPicoseconds(const AYottaseconds: LKFloat): LKFloat; inline;
function YottasecondsToNanoseconds(const AYottaseconds: LKFloat): LKFloat; inline;
function YottasecondsToMicroseconds(const AYottaseconds: LKFloat): LKFloat; inline;
function YottasecondsToMilliseconds(const AYottaseconds: LKFloat): LKFloat; inline;
function YottasecondsToSeconds(const AYottaseconds: LKFloat): LKFloat; inline;
function YottasecondsToKiloseconds(const AYottaseconds: LKFloat): LKFloat; inline;
function YottasecondsToMegaseconds(const AYottaseconds: LKFloat): LKFloat; inline;
function YottasecondsToGigaseconds(const AYottaseconds: LKFloat): LKFloat; inline;
function YottasecondsToTeraseconds(const AYottaseconds: LKFloat): LKFloat; inline;
function YottasecondsToPetaseconds(const AYottaseconds: LKFloat): LKFloat; inline;
function YottasecondsToExaseconds(const AYottaseconds: LKFloat): LKFloat; inline;
function YottasecondsToZettaseconds(const AYottaseconds: LKFloat): LKFloat; inline; // Down

implementation

type
  TLKTimeConversionMethod = function(const ASourceValue: LKFloat): LKFloat;

// ? to ?
function NoConversion(const AInput: LKFloat): LKFloat; inline;
begin
  // "NoConversion" is a bit of a cheat for the TimeUnitConvert method.
  // It's called if the Source and Desitnation time units are the same.
  Result := AInput;
end;

function TimeUnitConvert(const ASourceValue: LKFloat; const AInputUnit, AOutputUnit: TLKTimeUnit): LKFloat;
const
  CONVERSION_METHODS: Array[TLKTimeUnit, TLKTimeUnit] of TLKTimeConversionMethod =
                                                                                    (
                                                                                      // Yoctoseconds
                                                                                      (
                                                                                        NoConversion, YoctosecondsToZeptoseconds, YoctosecondsToAttoseconds, YoctosecondsToFemtoseconds,
                                                                                        YoctosecondsToPicoseconds, YoctosecondsToNanoseconds, YoctosecondsToMicroseconds, YoctosecondsToMilliseconds,
                                                                                        YoctosecondsToSeconds, YoctosecondsToKiloseconds, YoctosecondsToMegaseconds, YoctosecondsToGigaseconds,
                                                                                        YoctosecondsToTeraseconds, YoctosecondsToPetaseconds, YoctosecondsToExaseconds, YoctosecondsToZettaseconds,
                                                                                        YoctosecondsToYottaseconds
                                                                                      ),
                                                                                      // Zeptoseconds
                                                                                      (
                                                                                        ZeptosecondsToYoctoseconds, NoConversion, ZeptosecondsToAttoseconds, ZeptosecondsToFemtoseconds,
                                                                                        ZeptosecondsToPicoseconds, ZeptosecondsToNanoseconds, ZeptosecondsToMicroseconds, ZeptosecondsToMilliseconds,
                                                                                        ZeptosecondsToSeconds, ZeptosecondsToKiloseconds, ZeptosecondsToMegaseconds, ZeptosecondsToGigaseconds,
                                                                                        ZeptosecondsToTeraseconds, ZeptosecondsToPetaseconds, ZeptosecondsToExaseconds, ZeptosecondsToZettaseconds,
                                                                                        ZeptosecondsToYottaseconds
                                                                                      ),
                                                                                      // Attoseconds
                                                                                      (
                                                                                        AttosecondsToYoctoseconds, AttosecondsToZeptoseconds, NoConversion, AttosecondsToFemtoseconds,
                                                                                        AttosecondsToPicoseconds, AttosecondsToNanoseconds, AttosecondsToMicroseconds, AttosecondsToMilliseconds,
                                                                                        AttosecondsToSeconds, AttosecondsToKiloseconds, AttosecondsToMegaseconds, AttosecondsToGigaseconds,
                                                                                        AttosecondsToTeraseconds, AttosecondsToPetaseconds, AttosecondsToExaseconds, AttosecondsToZettaseconds,
                                                                                        AttosecondsToYottaseconds
                                                                                      ),
                                                                                      // Femtoseconds
                                                                                      (
                                                                                        FemtosecondsToYoctoseconds, FemtosecondsToZeptoseconds, FemtosecondsToAttoseconds, NoConversion,
                                                                                        FemtosecondsToPicoseconds, FemtosecondsToNanoseconds, FemtosecondsToMicroseconds, FemtosecondsToMilliseconds,
                                                                                        FemtosecondsToSeconds, FemtosecondsToKiloseconds, FemtosecondsToMegaseconds, FemtosecondsToGigaseconds,
                                                                                        FemtosecondsToTeraseconds, FemtosecondsToPetaseconds, FemtosecondsToExaseconds, FemtosecondsToZettaseconds,
                                                                                        FemtosecondsToYottaseconds
                                                                                      ),
                                                                                      // Picoseconds
                                                                                      (
                                                                                        PicosecondsToYoctoseconds, PicosecondsToZeptoseconds, PicosecondsToAttoseconds, PicosecondsToFemtoseconds,
                                                                                        NoConversion, PicosecondsToNanoseconds, PicosecondsToMicroseconds, PicosecondsToMilliseconds,
                                                                                        PicosecondsToSeconds, PicosecondsToKiloseconds, PicosecondsToMegaseconds, PicosecondsToGigaseconds,
                                                                                        PicosecondsToTeraseconds, PicosecondsToPetaseconds, PicosecondsToExaseconds, PicosecondsToZettaseconds,
                                                                                        PicosecondsToYottaseconds
                                                                                      ),
                                                                                      // Nanoseconds
                                                                                      (
                                                                                        NanosecondsToYoctoseconds, NanosecondsToZeptoseconds, NanosecondsToAttoseconds, NanosecondsToFemtoseconds,
                                                                                        NanosecondsToPicoseconds, NoConversion, NanosecondsToMicroseconds, NanosecondsToMilliseconds,
                                                                                        NanosecondsToSeconds, NanosecondsToKiloseconds, NanosecondsToMegaseconds, NanosecondsToGigaseconds,
                                                                                        NanosecondsToTeraseconds, NanosecondsToPetaseconds, NanosecondsToExaseconds, NanosecondsToZettaseconds,
                                                                                        NanosecondsToYottaseconds
                                                                                      ),
                                                                                      // Microseconds
                                                                                      (
                                                                                        MicrosecondsToYoctoseconds, MicrosecondsToZeptoseconds, MicrosecondsToAttoseconds, MicrosecondsToFemtoseconds,
                                                                                        MicrosecondsToPicoseconds, MicrosecondsToNanoseconds, NoConversion, MicrosecondsToMilliseconds,
                                                                                        MicrosecondsToSeconds, MicrosecondsToKiloseconds, MicrosecondsToMegaseconds, MicrosecondsToGigaseconds,
                                                                                        MicrosecondsToTeraseconds, MicrosecondsToPetaseconds, MicrosecondsToExaseconds, MicrosecondsToZettaseconds,
                                                                                        MicrosecondsToYottaseconds
                                                                                      ),
                                                                                      // Milliseconds
                                                                                      (
                                                                                        MillisecondsToYoctoseconds, MillisecondsToZeptoseconds, MillisecondsToAttoseconds, MillisecondsToFemtoseconds,
                                                                                        MillisecondsToPicoseconds, MillisecondsToNanoseconds, MillisecondsToMicroseconds, NoConversion,
                                                                                        MillisecondsToSeconds, MillisecondsToKiloseconds, MillisecondsToMegaseconds, MillisecondsToGigaseconds,
                                                                                        MillisecondsToTeraseconds, MillisecondsToPetaseconds, MillisecondsToExaseconds, MillisecondsToZettaseconds,
                                                                                        MillisecondsToYottaseconds
                                                                                      ),
                                                                                      // Seconds
                                                                                      (
                                                                                        SecondsToYoctoseconds, SecondsToZeptoseconds, SecondsToAttoseconds, SecondsToFemtoseconds, SecondsToPicoseconds,
                                                                                        SecondsToNanoseconds, SecondsToMicroseconds, SecondsToMilliseconds, NoConversion, SecondsToKiloseconds,
                                                                                        SecondsToMegaseconds, SecondsToGigaseconds, SecondsToTeraseconds, SecondsToPetaseconds, SecondsToExaseconds,
                                                                                        SecondsToZettaseconds, SecondsToYottaseconds
                                                                                      ),
                                                                                      // Kiloseconds
                                                                                      (
                                                                                        KilosecondsToYoctoseconds, KilosecondsToZeptoseconds, KilosecondsToAttoseconds, KilosecondsToFemtoseconds,
                                                                                        KilosecondsToPicoseconds, KilosecondsToNanoseconds, KilosecondsToMicroseconds, KilosecondsToMilliseconds,
                                                                                        KilosecondsToSeconds, NoConversion, KilosecondsToMegaseconds, KilosecondsToGigaseconds, KilosecondsToTeraseconds,
                                                                                        KilosecondsToPetaseconds, KilosecondsToExaseconds, KilosecondsToZettaseconds, KilosecondsToYottaseconds
                                                                                      ),
                                                                                      // Megaseconds
                                                                                      (
                                                                                        MegasecondsToYoctoseconds, MegasecondsToZeptoseconds, MegasecondsToAttoseconds, MegasecondsToFemtoseconds,
                                                                                        MegasecondsToPicoseconds, MegasecondsToNanoseconds, MegasecondsToMicroseconds, MegasecondsToMilliseconds,
                                                                                        MegasecondsToSeconds, MegasecondsToKiloseconds, NoConversion, MegasecondsToGigaseconds, MegasecondsToTeraseconds,
                                                                                        MegasecondsToPetaseconds, MegasecondsToExaseconds, MegasecondsToZettaseconds, MegasecondsToYottaseconds
                                                                                      ),
                                                                                      // Gigaseconds
                                                                                      (
                                                                                        GigasecondsToYoctoseconds, GigasecondsToZeptoseconds, GigasecondsToAttoseconds, GigasecondsToFemtoseconds,
                                                                                        GigasecondsToPicoseconds, GigasecondsToNanoseconds, GigasecondsToMicroseconds, GigasecondsToMilliseconds,
                                                                                        GigasecondsToSeconds, GigasecondsToKiloseconds, GigasecondsToMegaseconds, NoConversion, GigasecondsToTeraseconds,
                                                                                        GigasecondsToPetaseconds, GigasecondsToExaseconds, GigasecondsToZettaseconds, GigasecondsToYottaseconds
                                                                                      ),
                                                                                      // Teraseconds
                                                                                      (
                                                                                        TerasecondsToYoctoseconds, TerasecondsToZeptoseconds, TerasecondsToAttoseconds, TerasecondsToFemtoseconds,
                                                                                        TerasecondsToPicoseconds, TerasecondsToNanoseconds, TerasecondsToMicroseconds, TerasecondsToMilliseconds,
                                                                                        TerasecondsToSeconds, TerasecondsToKiloseconds, TerasecondsToMegaseconds, TerasecondsToGigaseconds,
                                                                                        NoConversion, TerasecondsToPetaseconds, TerasecondsToExaseconds, TerasecondsToZettaseconds, TerasecondsToYottaseconds
                                                                                      ),
                                                                                      // Petaseconds
                                                                                      (
                                                                                        PetasecondsToYoctoseconds, PetasecondsToZeptoseconds, PetasecondsToAttoseconds, PetasecondsToFemtoseconds,
                                                                                        PetasecondsToPicoseconds, PetasecondsToNanoseconds, PetasecondsToMicroseconds, PetasecondsToMilliseconds,
                                                                                        PetasecondsToSeconds, PetasecondsToKiloseconds, PetasecondsToMegaseconds, PetasecondsToGigaseconds,
                                                                                        PetasecondsToTeraseconds, NoConversion, PetasecondsToExaseconds, PetasecondsToZettaseconds, PetasecondsToYottaseconds
                                                                                      ),
                                                                                      // Exaseconds
                                                                                      (
                                                                                        ExasecondsToYoctoseconds, ExasecondsToZeptoseconds, ExasecondsToAttoseconds, ExasecondsToFemtoseconds,
                                                                                        ExasecondsToPicoseconds, ExasecondsToNanoseconds, ExasecondsToMicroseconds, ExasecondsToMilliseconds,
                                                                                        ExasecondsToSeconds, ExasecondsToKiloseconds, ExasecondsToMegaseconds, ExasecondsToGigaseconds,
                                                                                        ExasecondsToTeraseconds, ExasecondsToPetaseconds, NoConversion, ExasecondsToZettaseconds, ExasecondsToYottaseconds
                                                                                      ),
                                                                                      // Zettaseconds
                                                                                      (
                                                                                        ZettasecondsToYoctoseconds, ZettasecondsToZeptoseconds, ZettasecondsToAttoseconds, ZettasecondsToFemtoseconds,
                                                                                        ZettasecondsToPicoseconds, ZettasecondsToNanoseconds, ZettasecondsToMicroseconds, ZettasecondsToMilliseconds,
                                                                                        ZettasecondsToSeconds, ZettasecondsToKiloseconds, ZettasecondsToMegaseconds, ZettasecondsToGigaseconds,
                                                                                        ZettasecondsToTeraseconds, ZettasecondsToPetaseconds, ZettasecondsToExaseconds, NoConversion, ZettasecondsToYottaseconds
                                                                                      ),
                                                                                      // Yottaseconds
                                                                                      (
                                                                                        YottasecondsToYoctoseconds, YottasecondsToZeptoseconds, YottasecondsToAttoseconds, YottasecondsToFemtoseconds,
                                                                                        YottasecondsToPicoseconds, YottasecondsToNanoseconds, YottasecondsToMicroseconds, YottasecondsToMilliseconds,
                                                                                        YottasecondsToSeconds, YottasecondsToKiloseconds, YottasecondsToMegaseconds, YottasecondsToGigaseconds,
                                                                                        YottasecondsToTeraseconds, YottasecondsToPetaseconds, YottasecondsToExaseconds, YottasecondsToZettaseconds, NoConversion
                                                                                      )
                                                                                    );
begin
  Result := CONVERSION_METHODS[AInputUnit, AOutputUnit](ASourceValue);
end;

// Yoctoseconds To ?
procedure YoctosecondsToBest(const AYoctoseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS: Array[0..15] of TLKTimeConversionMethod = (
                                                                  YoctosecondsToZeptoseconds,
                                                                  YoctosecondsToAttoseconds,
                                                                  YoctosecondsToFemtoseconds,
                                                                  YoctosecondsToPicoseconds,
                                                                  YoctosecondsToNanoseconds,
                                                                  YoctosecondsToMicroseconds,
                                                                  YoctosecondsToMilliseconds,
                                                                  YoctosecondsToSeconds,
                                                                  YoctosecondsToKiloseconds,
                                                                  YoctosecondsToMegaseconds,
                                                                  YoctosecondsToGigaseconds,
                                                                  YoctosecondsToTeraseconds,
                                                                  YoctosecondsToPetaseconds,
                                                                  YoctosecondsToExaseconds,
                                                                  YoctosecondsToZettaseconds,
                                                                  YoctosecondsToYottaseconds
                                                                );
  CONVERSION_UNITS: Array[0..15] of TLKTimeUnit = (
                                                    tuZeptosecond, tuAttosecond, tuFemtosecond,
                                                    tuPicosecond, tuNanosecond, tuMicrosecond,
                                                    tuMillisecond, tuSecond, tuKilosecond, tuMegasecond,
                                                    tuGigasecond, tuTerasecond, tuPetasecond, tuExasecond,
                                                    tuZettasecond, tuYottasecond
                                                  );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AYoctoseconds;
  AOutUnit := tuYoctosecond;
  while (I < High(CONVERSION_METHODS)) and
          (
            (((AOutValue < -999) and (AOutValue < 0)) or
            ((AOutValue > 999) and (AOutValue > 0)))
          ) do
  begin
    AOutValue := CONVERSION_METHODS[I](AYoctoseconds);
    AOutUnit := CONVERSION_UNITS[I];
    Inc(I);
  end;
end;

function YoctosecondsToBest(const AYoctoseconds: LKFloat): TLKTime; overload;
begin
  YoctosecondsToBest(AYoctoseconds, Result.TimeValue, Result.TimeUnit);
end;

function YoctosecondsToZeptoseconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / 1000;
end;

function YoctosecondsToAttoseconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / 1000000;
end;

function YoctosecondsToFemtoseconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / 1000000000;
end;

function YoctosecondsToPicoseconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / 1000000000000;
end;

function YoctosecondsToNanoseconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / 1000000000000000;
end;

function YoctosecondsToMicroseconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / 1000000000000000000;
end;

function YoctosecondsToMilliseconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / IntPower(1000, 7);
end;

function YoctosecondsToSeconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / IntPower(1000, 8);
end;

function YoctosecondsToKiloseconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / IntPower(1000, 9);
end;

function YoctosecondsToMegaseconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / IntPower(1000, 10);
end;

function YoctosecondsToGigaseconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / IntPower(1000, 11);
end;

function YoctosecondsToTeraseconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / IntPower(1000, 12);
end;

function YoctosecondsToPetaseconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / IntPower(1000, 13);
end;

function YoctosecondsToExaseconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / IntPower(1000, 14);
end;

function YoctosecondsToZettaseconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / IntPower(1000, 15);
end;

function YoctosecondsToYottaseconds(const AYoctoseconds: LKFloat): LKFloat;
begin
  Result := AYoctoseconds / IntPower(1000, 16);
end;

// Zeptoseconds To ?
procedure ZeptosecondsToBest(const AZeptoseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS: Array[0..14] of TLKTimeConversionMethod = (
                                                                  ZeptosecondsToAttoseconds,
                                                                  ZeptosecondsToFemtoseconds,
                                                                  ZeptosecondsToPicoseconds,
                                                                  ZeptosecondsToNanoseconds,
                                                                  ZeptosecondsToMicroseconds,
                                                                  ZeptosecondsToMilliseconds,
                                                                  ZeptosecondsToSeconds,
                                                                  ZeptosecondsToKiloseconds,
                                                                  ZeptosecondsToMegaseconds,
                                                                  ZeptosecondsToGigaseconds,
                                                                  ZeptosecondsToTeraseconds,
                                                                  ZeptosecondsToPetaseconds,
                                                                  ZeptosecondsToExaseconds,
                                                                  ZeptosecondsToZettaseconds,
                                                                  ZeptosecondsToYottaseconds
                                                                );
  CONVERSION_UNITS: Array[0..14] of TLKTimeUnit = (
                                                    tuAttosecond, tuFemtosecond,
                                                    tuPicosecond, tuNanosecond, tuMicrosecond,
                                                    tuMillisecond, tuSecond, tuKilosecond, tuMegasecond,
                                                    tuGigasecond, tuTerasecond, tuPetasecond, tuExasecond,
                                                    tuZettasecond, tuYottasecond
                                                  );
var
  I: Integer;
begin
  AOutValue := AZeptoseconds;
  AOutUnit := tuZeptosecond;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    AOutValue := ZeptosecondsToYoctoseconds(AZeptoseconds);
    AOutUnit := tuYoctosecond;
  end else
  begin
    I := 0;
    while (I < High(CONVERSION_METHODS)) and
            (
              (((AOutValue < -999) and (AOutValue < 0)) or
              ((AOutValue > 999) and (AOutValue > 0)))
            ) do
    begin
      AOutValue := CONVERSION_METHODS[I](AZeptoseconds);
      AOutUnit := CONVERSION_UNITS[I];
      Inc(I);
    end;
  end;
end;

function ZeptosecondsToBest(const AZeptoseconds: LKFloat): TLKTime; overload;
begin
  ZeptosecondsToBest(AZeptoseconds, Result.TimeValue, Result.TimeUnit);
end;

function ZeptosecondsToYoctoseconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds * 1000;
end;

function ZeptosecondsToAttoseconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds / 1000;
end;

function ZeptosecondsToFemtoseconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds / 1000000;
end;

function ZeptosecondsToPicoseconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds / 1000000000;
end;

function ZeptosecondsToNanoseconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds / 1000000000000;
end;

function ZeptosecondsToMicroseconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds / 1000000000000000;
end;

function ZeptosecondsToMilliseconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds / 1000000000000000000;
end;

function ZeptosecondsToSeconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds / IntPower(1000, 7);
end;

function ZeptosecondsToKiloseconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds / IntPower(1000, 8);
end;

function ZeptosecondsToMegaseconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds / IntPower(1000, 9);
end;

function ZeptosecondsToGigaseconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds / IntPower(1000, 10);
end;

function ZeptosecondsToTeraseconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds / IntPower(1000, 11);
end;

function ZeptosecondsToPetaseconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds / IntPower(1000, 12);
end;

function ZeptosecondsToExaseconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds / IntPower(1000, 13);
end;

function ZeptosecondsToZettaseconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds / IntPower(1000, 14);
end;

function ZeptosecondsToYottaseconds(const AZeptoseconds: LKFloat): LKFloat;
begin
  Result := AZeptoseconds / IntPower(1000, 15);
end;

// Attoseconds To ?
procedure AttosecondsToBest(const AAttoseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..1] of TLKTimeConversionMethod = (
                                                                      AttosecondsToZeptoseconds,
                                                                      AttosecondstoYoctoseconds
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..1] of TLKTimeUnit = (tuZeptosecond, tuYoctosecond);
  CONVERSION_METHODS_UP: Array[0..13] of TLKTimeConversionMethod = (
                                                                     AttosecondsToFemtoseconds,
                                                                     AttosecondsToPicoseconds,
                                                                     AttosecondsToNanoseconds,
                                                                     AttosecondsToMicroseconds,
                                                                     AttosecondsToMilliseconds,
                                                                     AttosecondsToSeconds,
                                                                     AttosecondsToKiloseconds,
                                                                     AttosecondsToMegaseconds,
                                                                     AttosecondsToGigaseconds,
                                                                     AttosecondsToTeraseconds,
                                                                     AttosecondsToPetaseconds,
                                                                     AttosecondsToExaseconds,
                                                                     AttosecondsToZettaseconds,
                                                                     AttosecondsToYottaseconds
                                                                   );
  CONVERSION_UNITS_UP: Array[0..13] of TLKTimeUnit = (
                                                       tuFemtosecond,
                                                       tuPicosecond, tuNanosecond, tuMicrosecond,
                                                       tuMillisecond, tuSecond, tuKilosecond, tuMegasecond,
                                                       tuGigasecond, tuTerasecond, tuPetasecond, tuExasecond,
                                                       tuZettasecond, tuYottasecond
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AAttoseconds;
  AOutUnit := tuAttosecond;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AAttoseconds);
      AOutUnit := CONVERSION_UNITS_DOWN[I];
      Inc(I);
    end;
  end else
  begin
    while (I < High(CONVERSION_METHODS_UP)) and
            (
              (((AOutValue < -999) and (AOutValue < 0)) or
              ((AOutValue > 999) and (AOutValue > 0)))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_UP[I](AAttoseconds);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function AttosecondsToBest(const AAttoseconds: LKFloat): TLKTime; overload;
begin
  AttosecondsToBest(AAttoseconds, Result.TimeValue, Result.TimeUnit);
end;

function AttosecondsToYoctoseconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds * 1000000;
end;

function AttosecondsToZeptoseconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds * 1000;
end;

function AttosecondsToFemtoseconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds / 1000;
end;

function AttosecondsToPicoseconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds / 1000000;
end;

function AttosecondsToNanoseconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds / 1000000000;
end;

function AttosecondsToMicroseconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds / 1000000000000;
end;

function AttosecondsToMilliseconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds / 1000000000000000;
end;

function AttosecondsToSeconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds / 1000000000000000000;
end;

function AttosecondsToKiloseconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds / IntPower(1000, 7);
end;

function AttosecondsToMegaseconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds / IntPower(1000, 8);
end;

function AttosecondsToGigaseconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds / IntPower(1000, 9);
end;

function AttosecondsToTeraseconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds / IntPower(1000, 10);
end;

function AttosecondsToPetaseconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds / IntPower(1000, 11);
end;

function AttosecondsToExaseconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds / IntPower(1000, 12);
end;

function AttosecondsToZettaseconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds / IntPower(1000, 13);
end;

function AttosecondsToYottaseconds(const AAttoseconds: LKFloat): LKFloat;
begin
  Result := AAttoseconds / IntPower(1000, 14);
end;

// Femtoseconds To ?
procedure FemtosecondsToBest(const AFemtoseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..2] of TLKTimeConversionMethod = (
                                                                      FemtosecondsToAttoseconds,
                                                                      FemtosecondsToZeptoseconds,
                                                                      FemtosecondsToYoctoseconds
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..2] of TLKTimeUnit = (tuAttosecond, tuZeptosecond, tuYoctosecond);
  CONVERSION_METHODS_UP: Array[0..12] of TLKTimeConversionMethod = (
                                                                     FemtosecondsToPicoseconds,
                                                                     FemtosecondsToNanoseconds,
                                                                     FemtosecondsToMicroseconds,
                                                                     FemtosecondsToMilliseconds,
                                                                     FemtosecondsToSeconds,
                                                                     FemtosecondsToKiloseconds,
                                                                     FemtosecondsToMegaseconds,
                                                                     FemtosecondsToGigaseconds,
                                                                     FemtosecondsToTeraseconds,
                                                                     FemtosecondsToPetaseconds,
                                                                     FemtosecondsToExaseconds,
                                                                     FemtosecondsToZettaseconds,
                                                                     FemtosecondsToYottaseconds
                                                                   );
  CONVERSION_UNITS_UP: Array[0..12] of TLKTimeUnit = (
                                                       tuPicosecond, tuNanosecond, tuMicrosecond,
                                                       tuMillisecond, tuSecond, tuKilosecond, tuMegasecond,
                                                       tuGigasecond, tuTerasecond, tuPetasecond, tuExasecond,
                                                       tuZettasecond, tuYottasecond
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AFemtoseconds;
  AOutUnit := tuFemtosecond;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AFemtoseconds);
      AOutUnit := CONVERSION_UNITS_DOWN[I];
      Inc(I);
    end;
  end else
  begin
    while (I < High(CONVERSION_METHODS_UP)) and
            (
              (((AOutValue < -999) and (AOutValue < 0)) or
              ((AOutValue > 999) and (AOutValue > 0)))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_UP[I](AFemtoseconds);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function FemtosecondsToBest(const AFemtoseconds: LKFloat): TLKTime; overload;
begin
  FemtosecondsToBest(AFemtoseconds, Result.TimeValue, Result.TimeUnit);
end;

function FemtosecondsToYoctoseconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds * 1000000000;
end;

function FemtosecondsToZeptoseconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds * 1000000;
end;

function FemtosecondsToAttoseconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds * 1000;
end;

function FemtosecondsToPicoseconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds / 1000;
end;

function FemtosecondsToNanoseconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds / 1000000;
end;

function FemtosecondsToMicroseconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds / 1000000000;
end;

function FemtosecondsToMilliseconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds / 1000000000000;
end;

function FemtosecondsToSeconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds / 1000000000000000;
end;

function FemtosecondsToKiloseconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds / 1000000000000000000;
end;

function FemtosecondsToMegaseconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds / IntPower(1000, 7);
end;

function FemtosecondsToGigaseconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds / IntPower(1000, 8);
end;

function FemtosecondsToTeraseconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds / IntPower(1000, 9);
end;

function FemtosecondsToPetaseconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds / IntPower(1000, 10);
end;

function FemtosecondsToExaseconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds / IntPower(1000, 11);
end;

function FemtosecondsToZettaseconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds / IntPower(1000, 12);
end;

function FemtosecondsToYottaseconds(const AFemtoseconds: LKFloat): LKFloat;
begin
  Result := AFemtoseconds / IntPower(1000, 13);
end;

// Picoseconds To ?
procedure PicosecondsToBest(const APicoseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..3] of TLKTimeConversionMethod = (
                                                                      PicosecondsToFemtoseconds,
                                                                      PicosecondsToAttoseconds,
                                                                      PicosecondsToZeptoseconds,
                                                                      PicosecondsToYoctoseconds
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..3] of TLKTimeUnit = (tuFemtosecond, tuAttosecond, tuZeptosecond, tuYoctosecond);
  CONVERSION_METHODS_UP: Array[0..11] of TLKTimeConversionMethod = (
                                                                     PicosecondsToNanoseconds,
                                                                     PicosecondsToMicroseconds,
                                                                     PicosecondsToMilliseconds,
                                                                     PicosecondsToSeconds,
                                                                     PicosecondsToKiloseconds,
                                                                     PicosecondsToMegaseconds,
                                                                     PicosecondsToGigaseconds,
                                                                     PicosecondsToTeraseconds,
                                                                     PicosecondsToPetaseconds,
                                                                     PicosecondsToExaseconds,
                                                                     PicosecondsToZettaseconds,
                                                                     PicosecondsToYottaseconds
                                                                   );
  CONVERSION_UNITS_UP: Array[0..11] of TLKTimeUnit = (
                                                       tuNanosecond, tuMicrosecond, tuMillisecond,
                                                       tuSecond, tuKilosecond, tuMegasecond, tuGigasecond,
                                                       tuTerasecond, tuPetasecond, tuExasecond,
                                                       tuZettasecond, tuYottasecond
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := APicoseconds;
  AOutUnit := tuPicosecond;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](APicoseconds);
      AOutUnit := CONVERSION_UNITS_DOWN[I];
      Inc(I);
    end;
  end else
  begin
    while (I < High(CONVERSION_METHODS_UP)) and
            (
              (((AOutValue < -999) and (AOutValue < 0)) or
              ((AOutValue > 999) and (AOutValue > 0)))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_UP[I](APicoseconds);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function PicosecondsToBest(const APicoseconds: LKFloat): TLKTime; overload;
begin
  PicosecondsToBest(APicoseconds, Result.TimeValue, Result.TimeUnit);
end;

function PicosecondsToYoctoseconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds * 1000000000000;
end;

function PicosecondsToZeptoseconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds * 1000000000;
end;

function PicosecondsToAttoseconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds * 1000000;
end;

function PicosecondsToFemtoseconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds * 1000;
end;

function PicosecondsToNanoseconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds / 1000;
end;

function PicosecondsToMicroseconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds / 1000000;
end;

function PicosecondsToMilliseconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds / 1000000000;
end;

function PicosecondsToSeconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds / 1000000000000;
end;

function PicosecondsToKiloseconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds / 1000000000000000;
end;

function PicosecondsToMegaseconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds / 1000000000000000000;
end;

function PicosecondsToGigaseconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds / IntPower(1000, 7);
end;

function PicosecondsToTeraseconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds / IntPower(1000, 8);
end;

function PicosecondsToPetaseconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds / IntPower(1000, 9);
end;

function PicosecondsToExaseconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds / IntPower(1000, 10);
end;

function PicosecondsToZettaseconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds / IntPower(1000, 11);
end;

function PicosecondsToYottaseconds(const APicoseconds: LKFloat): LKFloat;
begin
  Result := APicoseconds / IntPower(1000, 12);
end;

// Nanoseconds To ?
procedure NanosecondsToBest(const ANanoseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..4] of TLKTimeConversionMethod = (
                                                                      NanosecondsToPicoseconds,
                                                                      NanosecondsToFemtoseconds,
                                                                      NanosecondsToAttoseconds,
                                                                      NanosecondsToZeptoseconds,
                                                                      NanosecondsToYoctoseconds
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..4] of TLKTimeUnit = (tuPicosecond, tuFemtosecond, tuAttosecond,
                                                       tuZeptosecond, tuYoctosecond);
  CONVERSION_METHODS_UP: Array[0..10] of TLKTimeConversionMethod = (
                                                                     NanosecondsToMicroseconds,
                                                                     NanosecondsToMilliseconds,
                                                                     NanosecondsToSeconds,
                                                                     NanosecondsToKiloseconds,
                                                                     NanosecondsToMegaseconds,
                                                                     NanosecondsToGigaseconds,
                                                                     NanosecondsToTeraseconds,
                                                                     NanosecondsToPetaseconds,
                                                                     NanosecondsToExaseconds,
                                                                     NanosecondsToZettaseconds,
                                                                     NanosecondsToYottaseconds
                                                                   );
  CONVERSION_UNITS_UP: Array[0..10] of TLKTimeUnit = (
                                                       tuMicrosecond, tuMillisecond,
                                                       tuSecond, tuKilosecond, tuMegasecond, tuGigasecond,
                                                       tuTerasecond, tuPetasecond, tuExasecond,
                                                       tuZettasecond, tuYottasecond
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := ANanoseconds;
  AOutUnit := tuNanosecond;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](ANanoseconds);
      AOutUnit := CONVERSION_UNITS_DOWN[I];
      Inc(I);
    end;
  end else
  begin
    while (I < High(CONVERSION_METHODS_UP)) and
            (
              (((AOutValue < -999) and (AOutValue < 0)) or
              ((AOutValue > 999) and (AOutValue > 0)))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_UP[I](ANanoseconds);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function NanosecondsToBest(const ANanoseconds: LKFloat): TLKTime; overload;
begin
  NanosecondsToBest(ANanoseconds, Result.TimeValue, Result.TimeUnit);
end;

function NanosecondsToYoctoseconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds * 1000000000000000;
end;

function NanosecondsToZeptoseconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds * 1000000000000;
end;

function NanosecondsToAttoseconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds * 1000000000;
end;

function NanosecondsToFemtoseconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds * 1000000;
end;

function NanosecondsToPicoseconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds * 1000;
end;

function NanosecondsToMicroseconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds / 1000;
end;

function NanosecondsToMilliseconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds / 1000000;
end;

function NanosecondsToSeconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds / 1000000000;
end;

function NanosecondsToKiloseconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds / 1000000000000;
end;

function NanosecondsToMegaseconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds / 1000000000000000;
end;

function NanosecondsToGigaseconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds / 1000000000000000000;
end;

function NanosecondsToTeraseconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds / IntPower(1000, 7);
end;

function NanosecondsToPetaseconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds / IntPower(1000, 8);
end;

function NanosecondsToExaseconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds / IntPower(1000, 9);
end;

function NanosecondsToZettaseconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds / IntPower(1000, 10);
end;

function NanosecondsToYottaseconds(const ANanoseconds: LKFloat): LKFloat;
begin
  Result := ANanoseconds / IntPower(1000, 11);
end;

// Microseconds To ?
procedure MicrosecondsToBest(const AMicroseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..5] of TLKTimeConversionMethod = (
                                                                      MicrosecondsToNanoseconds,
                                                                      MicrosecondsToPicoseconds,
                                                                      MicrosecondsToFemtoseconds,
                                                                      MicrosecondsToAttoseconds,
                                                                      MicrosecondsToZeptoseconds,
                                                                      MicrosecondsToYoctoseconds
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..5] of TLKTimeUnit = (tuNanosecond, tuPicosecond, tuFemtosecond,
                                                       tuAttosecond, tuZeptosecond, tuYoctosecond);
  CONVERSION_METHODS_UP: Array[0..9] of TLKTimeConversionMethod = (
                                                                     MicrosecondsToMilliseconds,
                                                                     MicrosecondsToSeconds,
                                                                     MicrosecondsToKiloseconds,
                                                                     MicrosecondsToMegaseconds,
                                                                     MicrosecondsToGigaseconds,
                                                                     MicrosecondsToTeraseconds,
                                                                     MicrosecondsToPetaseconds,
                                                                     MicrosecondsToExaseconds,
                                                                     MicrosecondsToZettaseconds,
                                                                     MicrosecondsToYottaseconds
                                                                   );
  CONVERSION_UNITS_UP: Array[0..9] of TLKTimeUnit = (
                                                       tuMillisecond,
                                                       tuSecond, tuKilosecond, tuMegasecond, tuGigasecond,
                                                       tuTerasecond, tuPetasecond, tuExasecond,
                                                       tuZettasecond, tuYottasecond
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AMicroseconds;
  AOutUnit := tuMicrosecond;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AMicroseconds);
      AOutUnit := CONVERSION_UNITS_DOWN[I];
      Inc(I);
    end;
  end else
  begin
    while (I < High(CONVERSION_METHODS_UP)) and
            (
              (((AOutValue < -999) and (AOutValue < 0)) or
              ((AOutValue > 999) and (AOutValue > 0)))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_UP[I](AMicroseconds);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function MicrosecondsToBest(const AMicroseconds: LKFloat): TLKTime; overload;
begin
  MicrosecondsToBest(AMicroseconds, Result.TimeValue, Result.TimeUnit);
end;

function MicrosecondsToYoctoseconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds * 1000000000000000000;
end;

function MicrosecondsToZeptoseconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds * 1000000000000000;
end;

function MicrosecondsToAttoseconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds * 1000000000000;
end;

function MicrosecondsToFemtoseconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds * 1000000000;
end;

function MicrosecondsToPicoseconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds * 1000000;
end;

function MicrosecondsToNanoseconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds * 1000;
end;

function MicrosecondsToMilliseconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds / 1000;
end;

function MicrosecondsToSeconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds / 1000000;
end;

function MicrosecondsToKiloseconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds / 1000000000;
end;

function MicrosecondsToMegaseconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds / 1000000000000;
end;

function MicrosecondsToGigaseconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds / 1000000000000000;
end;

function MicrosecondsToTeraseconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds / 1000000000000000000;
end;

function MicrosecondsToPetaseconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds / IntPower(1000, 7);
end;

function MicrosecondsToExaseconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds / IntPower(1000, 8);
end;

function MicrosecondsToZettaseconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds / IntPower(1000, 9);
end;

function MicrosecondsToYottaseconds(const AMicroseconds: LKFloat): LKFloat;
begin
  Result := AMicroseconds / IntPower(1000, 10);
end;

// Milliseconds To ?
procedure MillisecondsToBest(const AMilliseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..6] of TLKTimeConversionMethod = (
                                                                      MillisecondsToMicroseconds,
                                                                      MillisecondsToNanoseconds,
                                                                      MillisecondsToPicoseconds,
                                                                      MillisecondsToFemtoseconds,
                                                                      MillisecondsToAttoseconds,
                                                                      MillisecondsToZeptoseconds,
                                                                      MillisecondsToYoctoseconds
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..6] of TLKTimeUnit = (tuMicrosecond, tuNanosecond, tuPicosecond,
                                                       tuFemtosecond, tuAttosecond, tuZeptosecond,
                                                       tuYoctosecond);
  CONVERSION_METHODS_UP: Array[0..8] of TLKTimeConversionMethod = (
                                                                     MillisecondsToSeconds,
                                                                     MillisecondsToKiloseconds,
                                                                     MillisecondsToMegaseconds,
                                                                     MillisecondsToGigaseconds,
                                                                     MillisecondsToTeraseconds,
                                                                     MillisecondsToPetaseconds,
                                                                     MillisecondsToExaseconds,
                                                                     MillisecondsToZettaseconds,
                                                                     MillisecondsToYottaseconds
                                                                   );
  CONVERSION_UNITS_UP: Array[0..8] of TLKTimeUnit = (
                                                       tuSecond, tuKilosecond, tuMegasecond, tuGigasecond,
                                                       tuTerasecond, tuPetasecond, tuExasecond,
                                                       tuZettasecond, tuYottasecond
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AMilliseconds;
  AOutUnit := tuMillisecond;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AMilliseconds);
      AOutUnit := CONVERSION_UNITS_DOWN[I];
      Inc(I);
    end;
  end else
  begin
    while (I < High(CONVERSION_METHODS_UP)) and
            (
              (((AOutValue < -999) and (AOutValue < 0)) or
              ((AOutValue > 999) and (AOutValue > 0)))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_UP[I](AMilliseconds);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function MillisecondsToBest(const AMilliseconds: LKFloat): TLKTime; overload;
begin
  MillisecondsToBest(AMilliseconds, Result.TimeValue, Result.TimeUnit);
end;

function MillisecondsToYoctoseconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds * IntPower(1000, 7);
end;

function MillisecondsToZeptoseconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds * 1000000000000000000;
end;

function MillisecondsToAttoseconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds * 1000000000000000;
end;

function MillisecondsToFemtoseconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds * 1000000000000;
end;

function MillisecondsToPicoseconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds * 1000000000;
end;

function MillisecondsToNanoseconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds * 1000000;
end;

function MillisecondsToMicroseconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds * 1000;
end;

function MillisecondsToSeconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds / 1000;
end;

function MillisecondsToKiloseconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds / 1000000;
end;

function MillisecondsToMegaseconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds / 1000000000;
end;

function MillisecondsToGigaseconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds / 1000000000000;
end;

function MillisecondsToTeraseconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds / 1000000000000000;
end;

function MillisecondsToPetaseconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds / 1000000000000000000;
end;

function MillisecondsToExaseconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds / IntPower(1000, 7);
end;

function MillisecondsToZettaseconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds / IntPower(1000, 8);
end;

function MillisecondsToYottaseconds(const AMilliseconds: LKFloat): LKFloat;
begin
  Result := AMilliseconds / IntPower(1000, 9);
end;

// Seconds To ?
procedure SecondsToBest(const ASeconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..7] of TLKTimeConversionMethod = (
                                                                      SecondsToMilliseconds,
                                                                      SecondsToMicroseconds,
                                                                      SecondsToNanoseconds,
                                                                      SecondsToPicoseconds,
                                                                      SecondsToFemtoseconds,
                                                                      SecondsToAttoseconds,
                                                                      SecondsToZeptoseconds,
                                                                      SecondsToYoctoseconds
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..7] of TLKTimeUnit = (tuMillisecond, tuMicrosecond, tuNanosecond, tuPicosecond,
                                                       tuFemtosecond, tuAttosecond, tuZeptosecond,
                                                       tuYoctosecond);
  CONVERSION_METHODS_UP: Array[0..7] of TLKTimeConversionMethod = (
                                                                     SecondsToKiloseconds,
                                                                     SecondsToMegaseconds,
                                                                     SecondsToGigaseconds,
                                                                     SecondsToTeraseconds,
                                                                     SecondsToPetaseconds,
                                                                     SecondsToExaseconds,
                                                                     SecondsToZettaseconds,
                                                                     SecondsToYottaseconds
                                                                   );
  CONVERSION_UNITS_UP: Array[0..7] of TLKTimeUnit = (
                                                       tuKilosecond, tuMegasecond, tuGigasecond,
                                                       tuTerasecond, tuPetasecond, tuExasecond,
                                                       tuZettasecond, tuYottasecond
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := ASeconds;
  AOutUnit := tuSecond;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](ASeconds);
      AOutUnit := CONVERSION_UNITS_DOWN[I];
      Inc(I);
    end;
  end else
  begin
    while (I < High(CONVERSION_METHODS_UP)) and
            (
              (((AOutValue < -999) and (AOutValue < 0)) or
              ((AOutValue > 999) and (AOutValue > 0)))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_UP[I](ASeconds);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function SecondsToBest(const ASeconds: LKFloat): TLKTime; overload;
begin
  SecondsToBest(ASeconds, Result.TimeValue, Result.TimeUnit);
end;

function SecondsToYoctoseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds * IntPower(1000, 8);
end;

function SecondsToZeptoseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds * IntPower(1000, 7);
end;

function SecondsToAttoseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds * 1000000000000000000;
end;

function SecondsToFemtoseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds * 1000000000000000;
end;

function SecondsToPicoseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds * 1000000000000;
end;

function SecondsToNanoseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds * 1000000000;
end;

function SecondsToMicroseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds * 1000000;
end;

function SecondsToMilliseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds * 1000;
end;

function SecondsToKiloseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds / 1000;
end;

function SecondsToMegaseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds / 1000000;
end;

function SecondsToGigaseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds / 1000000000;
end;

function SecondsToTeraseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds / 1000000000000;
end;

function SecondsToPetaseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds / 1000000000000000;
end;

function SecondsToExaseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds / 1000000000000000000;
end;

function SecondsToZettaseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds / IntPower(1000, 7);
end;

function SecondsToYottaseconds(const ASeconds: LKFloat): LKFloat;
begin
  Result := ASeconds / IntPower(1000, 8);
end;

// Kiloseconds To ?
procedure KilosecondsToBest(const AKiloseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..8] of TLKTimeConversionMethod = (
                                                                      KilosecondsToSeconds,
                                                                      KiloSecondsToMilliseconds,
                                                                      KilosecondsToMicroseconds,
                                                                      KilosecondsToNanoseconds,
                                                                      KilosecondsToPicoseconds,
                                                                      KilosecondsToFemtoseconds,
                                                                      KilosecondsToAttoseconds,
                                                                      KilosecondsToZeptoseconds,
                                                                      KilosecondsToYoctoseconds
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..8] of TLKTimeUnit = (tuSecond, tuMillisecond, tuMicrosecond,
                                                       tuNanosecond, tuPicosecond, tuFemtosecond,
                                                       tuAttosecond, tuZeptosecond, tuYoctosecond);
  CONVERSION_METHODS_UP: Array[0..6] of TLKTimeConversionMethod = (
                                                                     KilosecondsToMegaseconds,
                                                                     KilosecondsToGigaseconds,
                                                                     KilosecondsToTeraseconds,
                                                                     KilosecondsToPetaseconds,
                                                                     KilosecondsToExaseconds,
                                                                     KilosecondsToZettaseconds,
                                                                     KilosecondsToYottaseconds
                                                                   );
  CONVERSION_UNITS_UP: Array[0..6] of TLKTimeUnit = (
                                                       tuMegasecond, tuGigasecond,
                                                       tuTerasecond, tuPetasecond, tuExasecond,
                                                       tuZettasecond, tuYottasecond
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AKiloseconds;
  AOutUnit := tuKilosecond;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AKiloseconds);
      AOutUnit := CONVERSION_UNITS_DOWN[I];
      Inc(I);
    end;
  end else
  begin
    while (I < High(CONVERSION_METHODS_UP)) and
            (
              (((AOutValue < -999) and (AOutValue < 0)) or
              ((AOutValue > 999) and (AOutValue > 0)))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_UP[I](AKiloseconds);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function KilosecondsToBest(const AKiloseconds: LKFloat): TLKTime; overload;
begin
  KilosecondsToBest(AKiloseconds, Result.TimeValue, Result.TimeUnit);
end;

function KilosecondsToYoctoseconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds * IntPower(1000, 9);
end;

function KilosecondsToZeptoseconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds * IntPower(1000, 8);
end;

function KilosecondsToAttoseconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds * IntPower(1000, 7);
end;

function KilosecondsToFemtoseconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds * 1000000000000000000;
end;

function KilosecondsToPicoseconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds * 1000000000000000;
end;

function KilosecondsToNanoseconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds * 1000000000000;
end;

function KilosecondsToMicroseconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds * 1000000000;
end;

function KilosecondsToMilliseconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds * 1000000;
end;

function KilosecondsToSeconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds * 1000;
end;

function KilosecondsToMegaseconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds / 1000;
end;

function KilosecondsToGigaseconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds / 1000000;
end;

function KilosecondsToTeraseconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds / 1000000000;
end;

function KilosecondsToPetaseconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds / 1000000000000;
end;

function KilosecondsToExaseconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds / 1000000000000000;
end;

function KilosecondsToZettaseconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds / 1000000000000000000;
end;

function KilosecondsToYottaseconds(const AKiloseconds: LKFloat): LKFloat;
begin
  Result := AKiloseconds / IntPower(1000, 7);
end;

// Megaseconds To ?
procedure MegasecondsToBest(const AMegaseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..9] of TLKTimeConversionMethod = (
                                                                      MegasecondsToKiloseconds,
                                                                      MegasecondsToSeconds,
                                                                      MegasecondsToMilliseconds,
                                                                      MegasecondsToMicroseconds,
                                                                      MegasecondsToNanoseconds,
                                                                      MegasecondsToPicoseconds,
                                                                      MegasecondsToFemtoseconds,
                                                                      MegasecondsToAttoseconds,
                                                                      MegasecondsToZeptoseconds,
                                                                      MegasecondsToYoctoseconds
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..9] of TLKTimeUnit = (tuKilosecond, tuMillisecond, tuSecond,
                                                       tuMicrosecond, tuNanosecond, tuPicosecond,
                                                       tuFemtosecond, tuAttosecond, tuZeptosecond, tuYoctosecond);
  CONVERSION_METHODS_UP: Array[0..5] of TLKTimeConversionMethod = (
                                                                     MegasecondsToGigaseconds,
                                                                     MegasecondsToTeraseconds,
                                                                     MegasecondsToPetaseconds,
                                                                     MegasecondsToExaseconds,
                                                                     MegasecondsToZettaseconds,
                                                                     MegasecondsToYottaseconds
                                                                   );
  CONVERSION_UNITS_UP: Array[0..5] of TLKTimeUnit = (
                                                       tuGigasecond, tuTerasecond, tuPetasecond,
                                                       tuExasecond, tuZettasecond, tuYottasecond
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AMegaseconds;
  AOutUnit := tuMegasecond;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AMegaseconds);
      AOutUnit := CONVERSION_UNITS_DOWN[I];
      Inc(I);
    end;
  end else
  begin
    while (I < High(CONVERSION_METHODS_UP)) and
            (
              (((AOutValue < -999) and (AOutValue < 0)) or
              ((AOutValue > 999) and (AOutValue > 0)))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_UP[I](AMegaseconds);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function MegasecondsToBest(const AMegaseconds: LKFloat): TLKTime; overload;
begin
  MegasecondsToBest(AMegaseconds, Result.TimeValue, Result.TimeUnit);
end;

function MegasecondsToYoctoseconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds * IntPower(1000, 10);
end;

function MegasecondsToZeptoseconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds * IntPower(1000, 9);
end;

function MegasecondsToAttoseconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds * IntPower(1000, 8);
end;

function MegasecondsToFemtoseconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds * IntPower(1000, 7);
end;

function MegasecondsToPicoseconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds * 1000000000000000000;
end;

function MegasecondsToNanoseconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds * 1000000000000000;
end;

function MegasecondsToMicroseconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds * 1000000000000;
end;

function MegasecondsToMilliseconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds * 1000000000;
end;

function MegasecondsToSeconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds * 1000000;
end;

function MegasecondsToKiloseconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds * 1000;
end;

function MegasecondsToGigaseconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds / 1000;
end;

function MegasecondsToTeraseconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds / 1000000;
end;

function MegasecondsToPetaseconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds / 1000000000;
end;

function MegasecondsToExaseconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds / 1000000000000;
end;

function MegasecondsToZettaseconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds / 1000000000000000;
end;

function MegasecondsToYottaseconds(const AMegaseconds: LKFloat): LKFloat;
begin
  Result := AMegaseconds / 1000000000000000000;
end;

// Gigaseconds To ?
procedure GigasecondsToBest(const AGigaseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..10] of TLKTimeConversionMethod = (
                                                                      GigasecondsToMegaseconds,
                                                                      GigasecondsToKiloseconds,
                                                                      GigasecondsToSeconds,
                                                                      GigasecondsToMilliseconds,
                                                                      GigasecondsToMicroseconds,
                                                                      GigasecondsToNanoseconds,
                                                                      GigasecondsToPicoseconds,
                                                                      GigasecondsToFemtoseconds,
                                                                      GigasecondsToAttoseconds,
                                                                      GigasecondsToZeptoseconds,
                                                                      GigasecondsToYoctoseconds
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..10] of TLKTimeUnit = (tuMegasecond, tuKilosecond, tuMillisecond, tuSecond,
                                                       tuMicrosecond, tuNanosecond, tuPicosecond,
                                                       tuFemtosecond, tuAttosecond, tuZeptosecond, tuYoctosecond);
  CONVERSION_METHODS_UP: Array[0..4] of TLKTimeConversionMethod = (
                                                                     GigasecondsToTeraseconds,
                                                                     GigasecondsToPetaseconds,
                                                                     GigasecondsToExaseconds,
                                                                     GigasecondsToZettaseconds,
                                                                     GigasecondsToYottaseconds
                                                                   );
  CONVERSION_UNITS_UP: Array[0..4] of TLKTimeUnit = (
                                                       tuTerasecond, tuPetasecond,
                                                       tuExasecond, tuZettasecond, tuYottasecond
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AGigaseconds;
  AOutUnit := tuGigasecond;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AGigaseconds);
      AOutUnit := CONVERSION_UNITS_DOWN[I];
      Inc(I);
    end;
  end else
  begin
    while (I < High(CONVERSION_METHODS_UP)) and
            (
              (((AOutValue < -999) and (AOutValue < 0)) or
              ((AOutValue > 999) and (AOutValue > 0)))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_UP[I](AGigaseconds);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function GigasecondsToBest(const AGigaseconds: LKFloat): TLKTime; overload;
begin
  GigasecondsToBest(AGigaseconds, Result.TimeValue, Result.TimeUnit);
end;

function GigasecondsToYoctoseconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds * IntPower(1000, 11);
end;

function GigasecondsToZeptoseconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds * IntPower(1000, 10);
end;

function GigasecondsToAttoseconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds * IntPower(1000, 9);
end;

function GigasecondsToFemtoseconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds * IntPower(1000, 8);
end;

function GigasecondsToPicoseconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds * IntPower(1000, 7);
end;

function GigasecondsToNanoseconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds * 1000000000000000000;
end;

function GigasecondsToMicroseconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds * 1000000000000000;
end;

function GigasecondsToMilliseconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds * 1000000000000;
end;

function GigasecondsToSeconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds * 1000000000;
end;

function GigasecondsToKiloseconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds * 1000000;
end;

function GigasecondsToMegaseconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds * 1000;
end;

function GigasecondsToTeraseconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds / 1000;
end;

function GigasecondsToPetaseconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds / 1000000;
end;

function GigasecondsToExaseconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds / 1000000000;
end;

function GigasecondsToZettaseconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds / 1000000000000;
end;

function GigasecondsToYottaseconds(const AGigaseconds: LKFloat): LKFloat;
begin
  Result := AGigaseconds / 1000000000000000;
end;

// Teraseconds To ?
procedure TerasecondsToBest(const ATeraseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..11] of TLKTimeConversionMethod = (
                                                                      TerasecondsToGigaseconds,
                                                                      TerasecondsToMegaseconds,
                                                                      TerasecondsToKiloseconds,
                                                                      TerasecondsToSeconds,
                                                                      TerasecondsToMilliseconds,
                                                                      TerasecondsToMicroseconds,
                                                                      TerasecondsToNanoseconds,
                                                                      TerasecondsToPicoseconds,
                                                                      TerasecondsToFemtoseconds,
                                                                      TerasecondsToAttoseconds,
                                                                      TerasecondsToZeptoseconds,
                                                                      TerasecondsToYoctoseconds
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..11] of TLKTimeUnit = (tuGigasecond, tuMegasecond, tuKilosecond, tuMillisecond, tuSecond,
                                                       tuMicrosecond, tuNanosecond, tuPicosecond,
                                                       tuFemtosecond, tuAttosecond, tuZeptosecond, tuYoctosecond);
  CONVERSION_METHODS_UP: Array[0..3] of TLKTimeConversionMethod = (
                                                                     TerasecondsToPetaseconds,
                                                                     TerasecondsToExaseconds,
                                                                     TerasecondsToZettaseconds,
                                                                     TerasecondsToYottaseconds
                                                                   );
  CONVERSION_UNITS_UP: Array[0..3] of TLKTimeUnit = (
                                                       tuPetasecond, tuExasecond, tuZettasecond, tuYottasecond
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := ATeraseconds;
  AOutUnit := tuTerasecond;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](ATeraseconds);
      AOutUnit := CONVERSION_UNITS_DOWN[I];
      Inc(I);
    end;
  end else
  begin
    while (I < High(CONVERSION_METHODS_UP)) and
            (
              (((AOutValue < -999) and (AOutValue < 0)) or
              ((AOutValue > 999) and (AOutValue > 0)))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_UP[I](ATeraseconds);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function TerasecondsToBest(const ATeraseconds: LKFloat): TLKTime; overload;
begin
  TerasecondsToBest(ATeraseconds, Result.TimeValue, Result.TimeUnit);
end;

function TerasecondsToYoctoseconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds * IntPower(1000, 12);
end;

function TerasecondsToZeptoseconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds * IntPower(1000, 11);
end;

function TerasecondsToAttoseconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds * IntPower(1000, 10);
end;

function TerasecondsToFemtoseconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds * IntPower(1000, 9);
end;

function TerasecondsToPicoseconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds * IntPower(1000, 8);
end;

function TerasecondsToNanoseconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds * IntPower(1000, 7);
end;

function TerasecondsToMicroseconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds * 1000000000000000000;
end;

function TerasecondsToMilliseconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds * 1000000000000000;
end;

function TerasecondsToSeconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds * 1000000000000;
end;

function TerasecondsToKiloseconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds * 1000000000;
end;

function TerasecondsToMegaseconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds * 1000000;
end;

function TerasecondsToGigaseconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds * 1000;
end;

function TerasecondsToPetaseconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds / 1000;
end;

function TerasecondsToExaseconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds / 1000000;
end;

function TerasecondsToZettaseconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds / 1000000000;
end;

function TerasecondsToYottaseconds(const ATeraseconds: LKFloat): LKFloat;
begin
  Result := ATeraseconds / 1000000000000;
end;

// Petaseconds To ?
procedure PetasecondsToBest(const APetaseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..12] of TLKTimeConversionMethod = (
                                                                      PetasecondsToTeraseconds,
                                                                      PetasecondsToGigaseconds,
                                                                      PetasecondsToMegaseconds,
                                                                      PetasecondsToKiloseconds,
                                                                      PetasecondsToSeconds,
                                                                      PetasecondsToMilliseconds,
                                                                      PetasecondsToMicroseconds,
                                                                      PetasecondsToNanoseconds,
                                                                      PetasecondsToPicoseconds,
                                                                      PetasecondsToFemtoseconds,
                                                                      PetasecondsToAttoseconds,
                                                                      PetasecondsToZeptoseconds,
                                                                      PetasecondsToYoctoseconds
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..12] of TLKTimeUnit = (tuTerasecond, tuGigasecond, tuMegasecond, tuKilosecond, tuMillisecond, tuSecond,
                                                       tuMicrosecond, tuNanosecond, tuPicosecond,
                                                       tuFemtosecond, tuAttosecond, tuZeptosecond, tuYoctosecond);
  CONVERSION_METHODS_UP: Array[0..2] of TLKTimeConversionMethod = (
                                                                     PetasecondsToExaseconds,
                                                                     PetasecondsToZettaseconds,
                                                                     PetasecondsToYottaseconds
                                                                   );
  CONVERSION_UNITS_UP: Array[0..2] of TLKTimeUnit = (
                                                       tuExasecond, tuZettasecond, tuYottasecond
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := APetaseconds;
  AOutUnit := tuPetasecond;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](APetaseconds);
      AOutUnit := CONVERSION_UNITS_DOWN[I];
      Inc(I);
    end;
  end else
  begin
    while (I < High(CONVERSION_METHODS_UP)) and
            (
              (((AOutValue < -999) and (AOutValue < 0)) or
              ((AOutValue > 999) and (AOutValue > 0)))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_UP[I](APetaseconds);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function PetasecondsToBest(const APetaseconds: LKFloat): TLKTime; overload;
begin
  PetasecondsToBest(APetaseconds, Result.TimeValue, Result.TimeUnit);
end;

function PetasecondsToYoctoseconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds * IntPower(1000, 13);
end;

function PetasecondsToZeptoseconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds * IntPower(1000, 12);
end;

function PetasecondsToAttoseconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds * IntPower(1000, 11);
end;

function PetasecondsToFemtoseconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds * IntPower(1000, 10);
end;

function PetasecondsToPicoseconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds * IntPower(1000, 9);
end;

function PetasecondsToNanoseconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds * IntPower(1000, 8);
end;

function PetasecondsToMicroseconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds * IntPower(1000, 7);
end;

function PetasecondsToMilliseconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds * 1000000000000000000;
end;

function PetasecondsToSeconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds * 1000000000000000;
end;

function PetasecondsToKiloseconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds * 1000000000000;
end;

function PetasecondsToMegaseconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds * 1000000000;
end;

function PetasecondsToGigaseconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds * 1000000;
end;

function PetasecondsToTeraseconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds * 1000;
end;

function PetasecondsToExaseconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds / 1000;
end;

function PetasecondsToZettaseconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds / 1000000;
end;

function PetasecondsToYottaseconds(const APetaseconds: LKFloat): LKFloat;
begin
  Result := APetaseconds / 1000000000;
end;

// Exaseconds To ?
procedure ExasecondsToBest(const AExaseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..13] of TLKTimeConversionMethod = (
                                                                      ExasecondsToPetaseconds,
                                                                      ExasecondsToTeraseconds,
                                                                      ExasecondsToGigaseconds,
                                                                      ExasecondsToMegaseconds,
                                                                      ExasecondsToKiloseconds,
                                                                      ExasecondsToSeconds,
                                                                      ExasecondsToMilliseconds,
                                                                      ExasecondsToMicroseconds,
                                                                      ExasecondsToNanoseconds,
                                                                      ExasecondsToPicoseconds,
                                                                      ExasecondsToFemtoseconds,
                                                                      ExasecondsToAttoseconds,
                                                                      ExasecondsToZeptoseconds,
                                                                      ExasecondsToYoctoseconds
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..13] of TLKTimeUnit = (tuPetasecond, tuTerasecond, tuGigasecond, tuMegasecond,
                                                       tuKilosecond, tuMillisecond, tuSecond, tuMicrosecond,
                                                       tuNanosecond, tuPicosecond, tuFemtosecond, tuAttosecond,
                                                       tuZeptosecond, tuYoctosecond);
  CONVERSION_METHODS_UP: Array[0..1] of TLKTimeConversionMethod = (
                                                                     ExasecondsToZettaseconds,
                                                                     ExasecondsToYottaseconds
                                                                   );
  CONVERSION_UNITS_UP: Array[0..1] of TLKTimeUnit = (
                                                       tuZettasecond, tuYottasecond
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AExaseconds;
  AOutUnit := tuExasecond;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AExaseconds);
      AOutUnit := CONVERSION_UNITS_DOWN[I];
      Inc(I);
    end;
  end else
  begin
    while (I < High(CONVERSION_METHODS_UP)) and
            (
              (((AOutValue < -999) and (AOutValue < 0)) or
              ((AOutValue > 999) and (AOutValue > 0)))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_UP[I](AExaseconds);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function ExasecondsToBest(const AExaseconds: LKFloat): TLKTime; overload;
begin
  ExasecondsToBest(AExaseconds, Result.TimeValue, Result.TimeUnit);
end;

function ExasecondsToYoctoseconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds * IntPower(1000, 14);
end;

function ExasecondsToZeptoseconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds * IntPower(1000, 13);
end;

function ExasecondsToAttoseconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds * IntPower(1000, 12);
end;

function ExasecondsToFemtoseconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds * IntPower(1000, 11);
end;

function ExasecondsToPicoseconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds * IntPower(1000, 10);
end;

function ExasecondsToNanoseconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds * IntPower(1000, 9);
end;

function ExasecondsToMicroseconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds * IntPower(1000, 8);
end;

function ExasecondsToMilliseconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds * IntPower(1000, 7);
end;

function ExasecondsToSeconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds * 1000000000000000000;
end;

function ExasecondsToKiloseconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds * 1000000000000000;
end;

function ExasecondsToMegaseconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds * 1000000000000;
end;

function ExasecondsToGigaseconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds * 1000000000;
end;

function ExasecondsToTeraseconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds * 1000000;
end;

function ExasecondsToPetaseconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds * 1000;
end;

function ExasecondsToZettaseconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds / 1000;
end;

function ExasecondsToYottaseconds(const AExaseconds: LKFloat): LKFloat;
begin
  Result := AExaseconds / 1000000;
end;

// Zettaseconds To ?
procedure ZettasecondsToBest(const AZettaseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..14] of TLKTimeConversionMethod = (
                                                                      ZettasecondsToExaseconds,
                                                                      ZettasecondsToPetaseconds,
                                                                      ZettasecondsToTeraseconds,
                                                                      ZettasecondsToGigaseconds,
                                                                      ZettasecondsToMegaseconds,
                                                                      ZettasecondsToKiloseconds,
                                                                      ZettasecondsToSeconds,
                                                                      ZettasecondsToMilliseconds,
                                                                      ZettasecondsToMicroseconds,
                                                                      ZettasecondsToNanoseconds,
                                                                      ZettasecondsToPicoseconds,
                                                                      ZettasecondsToFemtoseconds,
                                                                      ZettasecondsToAttoseconds,
                                                                      ZettasecondsToZeptoseconds,
                                                                      ZettasecondsToYoctoseconds
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..14] of TLKTimeUnit = (tuExasecond, tuPetasecond, tuTerasecond, tuGigasecond, tuMegasecond,
                                                       tuKilosecond, tuMillisecond, tuSecond, tuMicrosecond,
                                                       tuNanosecond, tuPicosecond, tuFemtosecond, tuAttosecond,
                                                       tuZeptosecond, tuYoctosecond);
var
  I: Integer;
begin
  I := 0;
  AOutValue := AZettaseconds;
  AOutUnit := tuZettasecond;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AZettaseconds);
      AOutUnit := CONVERSION_UNITS_DOWN[I];
      Inc(I);
    end;
  end else
  begin
    AOutValue := ZettasecondsToYottaseconds(AZettaseconds);
    AOutUnit := tuYottasecond;
  end;
end;

function ZettasecondsToBest(const AZettaseconds: LKFloat): TLKTime; overload;
begin
  ZettasecondsToBest(AZettaseconds, Result.TimeValue, Result.TimeUnit);
end;

function ZettasecondsToYoctoseconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds * IntPower(1000, 15);
end;

function ZettasecondsToZeptoseconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds * IntPower(1000, 14);
end;

function ZettasecondsToAttoseconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds * IntPower(1000, 13);
end;

function ZettasecondsToFemtoseconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds * IntPower(1000, 12);
end;

function ZettasecondsToPicoseconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds * IntPower(1000, 11);
end;

function ZettasecondsToNanoseconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds * IntPower(1000, 10);
end;

function ZettasecondsToMicroseconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds * IntPower(1000, 9);
end;

function ZettasecondsToMilliseconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds * IntPower(1000, 8);
end;

function ZettasecondsToSeconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds * IntPower(1000, 7);
end;

function ZettasecondsToKiloseconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds * 1000000000000000000;
end;

function ZettasecondsToMegaseconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds * 1000000000000000;
end;

function ZettasecondsToGigaseconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds * 1000000000000;
end;

function ZettasecondsToTeraseconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds * 1000000000;
end;

function ZettasecondsToPetaseconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds * 1000000;
end;

function ZettasecondsToExaseconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds * 1000;
end;

function ZettasecondsToYottaseconds(const AZettaseconds: LKFloat): LKFloat;
begin
  Result := AZettaseconds / 1000;
end;

// Yottaseconds To ?
procedure YottasecondsToBest(const AYottaseconds: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKTimeUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..15] of TLKTimeConversionMethod = (
                                                                      YottasecondsToZettaseconds,
                                                                      YottasecondsToExaseconds,
                                                                      YottasecondsToPetaseconds,
                                                                      YottasecondsToTeraseconds,
                                                                      YottasecondsToGigaseconds,
                                                                      YottasecondsToMegaseconds,
                                                                      YottasecondsToKiloseconds,
                                                                      YottasecondsToSeconds,
                                                                      YottasecondsToMilliseconds,
                                                                      YottasecondsToMicroseconds,
                                                                      YottasecondsToNanoseconds,
                                                                      YottasecondsToPicoseconds,
                                                                      YottasecondsToFemtoseconds,
                                                                      YottasecondsToAttoseconds,
                                                                      YottasecondsToZeptoseconds,
                                                                      YottasecondsToYoctoseconds
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..15] of TLKTimeUnit = (tuZettasecond, tuExasecond, tuPetasecond, tuTerasecond, tuGigasecond, tuMegasecond,
                                                       tuKilosecond, tuMillisecond, tuSecond, tuMicrosecond,
                                                       tuNanosecond, tuPicosecond, tuFemtosecond, tuAttosecond,
                                                       tuZeptosecond, tuYoctosecond);
var
  I: Integer;
begin
  I := 0;
  AOutValue := AYottaseconds;
  AOutUnit := tuYottasecond;
  while (I < High(CONVERSION_METHODS_DOWN)) and
          (
            ((AOutValue < 1) and (AOutValue > 0)) or
            ((AOutValue > -1) and (AOutValue < 0))
          ) do
  begin
    AOutValue := CONVERSION_METHODS_DOWN[I](AYottaseconds);
    AOutUnit := CONVERSION_UNITS_DOWN[I];
    Inc(I);
  end;
end;

function YottasecondsToBest(const AYottaseconds: LKFloat): TLKTime; overload;
begin
  YottasecondsToBest(AYottaseconds, Result.TimeValue, Result.TimeUnit);
end;

function YottasecondsToYoctoseconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * IntPower(1000, 16);
end;

function YottasecondsToZeptoseconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * IntPower(1000, 15);
end;

function YottasecondsToAttoseconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * IntPower(1000, 14);
end;

function YottasecondsToFemtoseconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * IntPower(1000, 13);
end;

function YottasecondsToPicoseconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * IntPower(1000, 12);
end;

function YottasecondsToNanoseconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * IntPower(1000, 11);
end;

function YottasecondsToMicroseconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * IntPower(1000, 10);
end;

function YottasecondsToMilliseconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * IntPower(1000, 9);
end;

function YottasecondsToSeconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * IntPower(1000, 8);
end;

function YottasecondsToKiloseconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * IntPower(1000, 7);
end;

function YottasecondsToMegaseconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * 1000000000000000000;
end;

function YottasecondsToGigaseconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * 1000000000000000;
end;

function YottasecondsToTeraseconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * 1000000000000;
end;

function YottasecondsToPetaseconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * 1000000000;
end;

function YottasecondsToExaseconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * 1000000;
end;

function YottasecondsToZettaseconds(const AYottaseconds: LKFloat): LKFloat;
begin
  Result := AYottaseconds * 1000;
end;

end.
