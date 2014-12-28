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

// Yoctoseconds To ?
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

// Yoctoseconds To ?
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
