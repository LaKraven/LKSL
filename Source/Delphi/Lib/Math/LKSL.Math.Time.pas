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
function YoctosecondsToZeptoseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
function YoctosecondsToAttoseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
function YoctosecondsToFemtoseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
function YoctosecondsToPicoseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
function YoctosecondsToNanoseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
function YoctosecondsToMicroseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
function YoctosecondsToMilliseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
function YoctosecondsToSeconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
function YoctosecondsToKiloseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
function YoctosecondsToMegaseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
function YoctosecondsToGigaseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
function YoctosecondsToTeraseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
function YoctosecondsToPetaseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
function YoctosecondsToExaseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
function YoctosecondsToZettaseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
function YoctosecondsToYottaseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat; inline;
// Zeptoseconds To ?
function ZeptosecondsToYoctoseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline; // Down
function ZeptosecondsToAttoseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline;
function ZeptosecondsToFemtoseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline;
function ZeptosecondsToPicoseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline;
function ZeptosecondsToNanoseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline;
function ZeptosecondsToMicroseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline;
function ZeptosecondsToMilliseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline;
function ZeptosecondsToSeconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline;
function ZeptosecondsToKiloseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline;
function ZeptosecondsToMegaseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline;
function ZeptosecondsToGigaseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline;
function ZeptosecondsToTeraseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline;
function ZeptosecondsToPetaseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline;
function ZeptosecondsToExaseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline;
function ZeptosecondsToZettaseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline;
function ZeptosecondsToYottaseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat; inline;
// Attoseconds To ?
function AttosecondsToYoctoseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline;
function AttosecondsToZeptoseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline; // Down
function AttosecondsToFemtoseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline;
function AttosecondsToPicoseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline;
function AttosecondsToNanoseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline;
function AttosecondsToMicroseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline;
function AttosecondsToMilliseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline;
function AttosecondsToSeconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline;
function AttosecondsToKiloseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline;
function AttosecondsToMegaseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline;
function AttosecondsToGigaseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline;
function AttosecondsToTeraseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline;
function AttosecondsToPetaseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline;
function AttosecondsToExaseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline;
function AttosecondsToZettaseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline;
function AttosecondsToYottaseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat; inline;
// Femtoseconds To ?
function FemtosecondsToYoctoseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline;
function FemtosecondsToZeptoseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline;
function FemtosecondsToAttoseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline; // Down
function FemtosecondsToPicoseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline;
function FemtosecondsToNanoseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline;
function FemtosecondsToMicroseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline;
function FemtosecondsToMilliseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline;
function FemtosecondsToSeconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline;
function FemtosecondsToKiloseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline;
function FemtosecondsToMegaseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline;
function FemtosecondsToGigaseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline;
function FemtosecondsToTeraseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline;
function FemtosecondsToPetaseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline;
function FemtosecondsToExaseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline;
function FemtosecondsToZettaseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline;
function FemtosecondsToYottaseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat; inline;
// Picoseconds To ?
function PicosecondsToYoctoseconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline;
function PicosecondsToZeptoseconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline;
function PicosecondsToAttoseconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline;
function PicosecondsToFemtoseconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline; // Down
function PicosecondsToNanoseconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline;
function PicosecondsToMicroseconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline;
function PicosecondsToMilliseconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline;
function PicosecondsToSeconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline;
function PicosecondsToKiloseconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline;
function PicosecondsToMegaseconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline;
function PicosecondsToGigaseconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline;
function PicosecondsToTeraseconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline;
function PicosecondsToPetaseconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline;
function PicosecondsToExaseconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline;
function PicosecondsToZettaseconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline;
function PicosecondsToYottaseconds(const APicoseconds: LKTimeFloat): LKTimeFloat; inline;
// Nanoseconds To ?
function NanosecondsToYoctoseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline;
function NanosecondsToZeptoseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline;
function NanosecondsToAttoseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline;
function NanosecondsToFemtoseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline;
function NanosecondsToPicoseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline; // Down
function NanosecondsToMicroseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline;
function NanosecondsToMilliseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline;
function NanosecondsToSeconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline;
function NanosecondsToKiloseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline;
function NanosecondsToMegaseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline;
function NanosecondsToGigaseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline;
function NanosecondsToTeraseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline;
function NanosecondsToPetaseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline;
function NanosecondsToExaseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline;
function NanosecondsToZettaseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline;
function NanosecondsToYottaseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat; inline;
// Microseconds To ?
function MicrosecondsToYoctoseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline;
function MicrosecondsToZeptoseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline;
function MicrosecondsToAttoseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline;
function MicrosecondsToFemtoseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline;
function MicrosecondsToPicoseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline;
function MicrosecondsToNanoseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline; // Down
function MicrosecondsToMilliseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline;
function MicrosecondsToSeconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline;
function MicrosecondsToKiloseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline;
function MicrosecondsToMegaseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline;
function MicrosecondsToGigaseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline;
function MicrosecondsToTeraseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline;
function MicrosecondsToPetaseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline;
function MicrosecondsToExaseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline;
function MicrosecondsToZettaseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline;
function MicrosecondsToYottaseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat; inline;
// Milliseconds To ?
function MillisecondsToYoctoseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline;
function MillisecondsToZeptoseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline;
function MillisecondsToAttoseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline;
function MillisecondsToFemtoseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline;
function MillisecondsToPicoseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline;
function MillisecondsToNanoseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline;
function MillisecondsToMicroseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline; // Down
function MillisecondsToSeconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline;
function MillisecondsToKiloseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline;
function MillisecondsToMegaseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline;
function MillisecondsToGigaseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline;
function MillisecondsToTeraseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline;
function MillisecondsToPetaseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline;
function MillisecondsToExaseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline;
function MillisecondsToZettaseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline;
function MillisecondsToYottaseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat; inline;
// Seconds To ?
function SecondsToYoctoseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
function SecondsToZeptoseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
function SecondsToAttoseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
function SecondsToFemtoseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
function SecondsToPicoseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
function SecondsToNanoseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
function SecondsToMicroseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
function SecondsToMilliseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
function SecondsToKiloseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
function SecondsToMegaseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
function SecondsToGigaseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
function SecondsToTeraseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
function SecondsToPetaseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
function SecondsToExaseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
function SecondsToZettaseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
function SecondsToYottaseconds(const ASeconds: LKTimeFloat): LKTimeFloat; inline;
     // Kiloseconds To ?
function KilosecondsToYoctoseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline;
function KilosecondsToZeptoseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline;
function KilosecondsToAttoseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline;
function KilosecondsToFemtoseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline;
function KilosecondsToPicoseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline;
function KilosecondsToNanoseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline;
function KilosecondsToMicroseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline;
function KilosecondsToMilliseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline;
function KilosecondsToSeconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline; // Down
function KilosecondsToMegaseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline;
function KilosecondsToGigaseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline;
function KilosecondsToTeraseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline;
function KilosecondsToPetaseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline;
function KilosecondsToExaseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline;
function KilosecondsToZettaseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline;
function KilosecondsToYottaseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat; inline;
// Megaseconds To ?
function MegasecondsToYoctoseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline;
function MegasecondsToZeptoseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline;
function MegasecondsToAttoseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline;
function MegasecondsToFemtoseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline;
function MegasecondsToPicoseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline;
function MegasecondsToNanoseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline;
function MegasecondsToMicroseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline;
function MegasecondsToMilliseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline;
function MegasecondsToSeconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline;
function MegasecondsToKiloseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline; // Down
function MegasecondsToGigaseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline;
function MegasecondsToTeraseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline;
function MegasecondsToPetaseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline;
function MegasecondsToExaseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline;
function MegasecondsToZettaseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline;
function MegasecondsToYottaseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat; inline;
// Gigaseconds To ?
function GigasecondsToYoctoseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline;
function GigasecondsToZeptoseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline;
function GigasecondsToAttoseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline;
function GigasecondsToFemtoseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline;
function GigasecondsToPicoseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline;
function GigasecondsToNanoseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline;
function GigasecondsToMicroseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline;
function GigasecondsToMilliseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline;
function GigasecondsToSeconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline;
function GigasecondsToKiloseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline;
function GigasecondsToMegaseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline; // Down
function GigasecondsToTeraseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline;
function GigasecondsToPetaseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline;
function GigasecondsToExaseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline;
function GigasecondsToZettaseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline;
function GigasecondsToYottaseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat; inline;
// Teraseconds To ?
function TerasecondsToYoctoseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline;
function TerasecondsToZeptoseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline;
function TerasecondsToAttoseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline;
function TerasecondsToFemtoseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline;
function TerasecondsToPicoseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline;
function TerasecondsToNanoseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline;
function TerasecondsToMicroseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline;
function TerasecondsToMilliseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline;
function TerasecondsToSeconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline;
function TerasecondsToKiloseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline;
function TerasecondsToMegaseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline;
function TerasecondsToGigaseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline; // Down
function TerasecondsToPetaseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline;
function TerasecondsToExaseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline;
function TerasecondsToZettaseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline;
function TerasecondsToYottaseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat; inline;
// Petaseconds To ?
function PetasecondsToYoctoseconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline;
function PetasecondsToZeptoseconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline;
function PetasecondsToAttoseconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline;
function PetasecondsToFemtoseconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline;
function PetasecondsToPicoseconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline;
function PetasecondsToNanoseconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline;
function PetasecondsToMicroseconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline;
function PetasecondsToMilliseconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline;
function PetasecondsToSeconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline;
function PetasecondsToKiloseconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline;
function PetasecondsToMegaseconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline;
function PetasecondsToGigaseconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline;
function PetasecondsToTeraseconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline; // Down
function PetasecondsToExaseconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline;
function PetasecondsToZettaseconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline;
function PetasecondsToYottaseconds(const APetaseconds: LKTimeFloat): LKTimeFloat; inline;
// Exaseconds To ?
function ExasecondsToYoctoseconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline;
function ExasecondsToZeptoseconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline;
function ExasecondsToAttoseconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline;
function ExasecondsToFemtoseconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline;
function ExasecondsToPicoseconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline;
function ExasecondsToNanoseconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline;
function ExasecondsToMicroseconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline;
function ExasecondsToMilliseconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline;
function ExasecondsToSeconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline;
function ExasecondsToKiloseconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline;
function ExasecondsToMegaseconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline;
function ExasecondsToGigaseconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline;
function ExasecondsToTeraseconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline;
function ExasecondsToPetaseconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline; // Down
function ExasecondsToZettaseconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline;
function ExasecondsToYottaseconds(const AExaseconds: LKTimeFloat): LKTimeFloat; inline;
// Zettaseconds To ?
function ZettasecondsToYoctoseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline;
function ZettasecondsToZeptoseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline;
function ZettasecondsToAttoseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline;
function ZettasecondsToFemtoseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline;
function ZettasecondsToPicoseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline;
function ZettasecondsToNanoseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline;
function ZettasecondsToMicroseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline;
function ZettasecondsToMilliseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline;
function ZettasecondsToSeconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline;
function ZettasecondsToKiloseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline;
function ZettasecondsToMegaseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline;
function ZettasecondsToGigaseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline;
function ZettasecondsToTeraseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline;
function ZettasecondsToPetaseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline;
function ZettasecondsToExaseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline; // Down
function ZettasecondsToYottaseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat; inline;
// Yottaseconds To ?
function YottasecondsToYoctoseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline;
function YottasecondsToZeptoseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline;
function YottasecondsToAttoseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline;
function YottasecondsToFemtoseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline;
function YottasecondsToPicoseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline;
function YottasecondsToNanoseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline;
function YottasecondsToMicroseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline;
function YottasecondsToMilliseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline;
function YottasecondsToSeconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline;
function YottasecondsToKiloseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline;
function YottasecondsToMegaseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline;
function YottasecondsToGigaseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline;
function YottasecondsToTeraseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline;
function YottasecondsToPetaseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline;
function YottasecondsToExaseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline;
function YottasecondsToZettaseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat; inline; // Down

implementation

// Yoctoseconds To ?
function YoctosecondsToZeptoseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / 1000;
end;

function YoctosecondsToAttoseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / 1000000;
end;

function YoctosecondsToFemtoseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / 1000000000;
end;

function YoctosecondsToPicoseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / 1000000000000;
end;

function YoctosecondsToNanoseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / 1000000000000000;
end;

function YoctosecondsToMicroseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / 1000000000000000000;
end;

function YoctosecondsToMilliseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / Power(1000, 7);
end;

function YoctosecondsToSeconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / Power(1000, 8);
end;

function YoctosecondsToKiloseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / Power(1000, 9);
end;

function YoctosecondsToMegaseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / Power(1000, 10);
end;

function YoctosecondsToGigaseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / Power(1000, 11);
end;

function YoctosecondsToTeraseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / Power(1000, 12);
end;

function YoctosecondsToPetaseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / Power(1000, 13);
end;

function YoctosecondsToExaseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / Power(1000, 14);
end;

function YoctosecondsToZettaseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / Power(1000, 15);
end;

function YoctosecondsToYottaseconds(const AYoctoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYoctoseconds / Power(1000, 16);
end;

// Zeptoseconds To ?
function ZeptosecondsToYoctoseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds * 1000;
end;

function ZeptosecondsToAttoseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds / 1000;
end;

function ZeptosecondsToFemtoseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds / 1000000;
end;

function ZeptosecondsToPicoseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds / 1000000000;
end;

function ZeptosecondsToNanoseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds / 1000000000000;
end;

function ZeptosecondsToMicroseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds / 1000000000000000;
end;

function ZeptosecondsToMilliseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds / 1000000000000000000;
end;

function ZeptosecondsToSeconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds / Power(1000, 7);
end;

function ZeptosecondsToKiloseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds / Power(1000, 8);
end;

function ZeptosecondsToMegaseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds / Power(1000, 9);
end;

function ZeptosecondsToGigaseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds / Power(1000, 10);
end;

function ZeptosecondsToTeraseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds / Power(1000, 11);
end;

function ZeptosecondsToPetaseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds / Power(1000, 12);
end;

function ZeptosecondsToExaseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds / Power(1000, 13);
end;

function ZeptosecondsToZettaseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds / Power(1000, 14);
end;

function ZeptosecondsToYottaseconds(const AZeptoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZeptoseconds / Power(1000, 15);
end;

// Attoseconds To ?
function AttosecondsToYoctoseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds * 1000000;
end;

function AttosecondsToZeptoseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds * 1000;
end;

function AttosecondsToFemtoseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds / 1000;
end;

function AttosecondsToPicoseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds / 1000000;
end;

function AttosecondsToNanoseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds / 1000000000;
end;

function AttosecondsToMicroseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds / 1000000000000;
end;

function AttosecondsToMilliseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds / 1000000000000000;
end;

function AttosecondsToSeconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds / 1000000000000000000;
end;

function AttosecondsToKiloseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds / Power(1000, 7);
end;

function AttosecondsToMegaseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds / Power(1000, 8);
end;

function AttosecondsToGigaseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds / Power(1000, 9);
end;

function AttosecondsToTeraseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds / Power(1000, 10);
end;

function AttosecondsToPetaseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds / Power(1000, 11);
end;

function AttosecondsToExaseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds / Power(1000, 12);
end;

function AttosecondsToZettaseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds / Power(1000, 13);
end;

function AttosecondsToYottaseconds(const AAttoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AAttoseconds / Power(1000, 14);
end;

// Femtoseconds To ?
function FemtosecondsToYoctoseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds * 1000000000;
end;

function FemtosecondsToZeptoseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds * 1000000;
end;

function FemtosecondsToAttoseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds * 1000;
end;

function FemtosecondsToPicoseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds / 1000;
end;

function FemtosecondsToNanoseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds / 1000000;
end;

function FemtosecondsToMicroseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds / 1000000000;
end;

function FemtosecondsToMilliseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds / 1000000000000;
end;

function FemtosecondsToSeconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds / 1000000000000000;
end;

function FemtosecondsToKiloseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds / 1000000000000000000;
end;

function FemtosecondsToMegaseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds / Power(1000, 7);
end;

function FemtosecondsToGigaseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds / Power(1000, 8);
end;

function FemtosecondsToTeraseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds / Power(1000, 9);
end;

function FemtosecondsToPetaseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds / Power(1000, 10);
end;

function FemtosecondsToExaseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds / Power(1000, 11);
end;

function FemtosecondsToZettaseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds / Power(1000, 12);
end;

function FemtosecondsToYottaseconds(const AFemtoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AFemtoseconds / Power(1000, 13);
end;

// Picoseconds To ?
function PicosecondsToYoctoseconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds * 1000000000000;
end;

function PicosecondsToZeptoseconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds * 1000000000;
end;

function PicosecondsToAttoseconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds * 1000000;
end;

function PicosecondsToFemtoseconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds * 1000;
end;

function PicosecondsToNanoseconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds / 1000;
end;

function PicosecondsToMicroseconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds / 1000000;
end;

function PicosecondsToMilliseconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds / 1000000000;
end;

function PicosecondsToSeconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds / 1000000000000;
end;

function PicosecondsToKiloseconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds / 1000000000000000;
end;

function PicosecondsToMegaseconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds / 1000000000000000000;
end;

function PicosecondsToGigaseconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds / Power(1000, 7);
end;

function PicosecondsToTeraseconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds / Power(1000, 8);
end;

function PicosecondsToPetaseconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds / Power(1000, 9);
end;

function PicosecondsToExaseconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds / Power(1000, 10);
end;

function PicosecondsToZettaseconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds / Power(1000, 11);
end;

function PicosecondsToYottaseconds(const APicoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APicoseconds / Power(1000, 12);
end;

// Nanoseconds To ?
function NanosecondsToYoctoseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds * 1000000000000000;
end;

function NanosecondsToZeptoseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds * 1000000000000;
end;

function NanosecondsToAttoseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds * 1000000000;
end;

function NanosecondsToFemtoseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds * 1000000;
end;

function NanosecondsToPicoseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds * 1000;
end;

function NanosecondsToMicroseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds / 1000;
end;

function NanosecondsToMilliseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds / 1000000;
end;

function NanosecondsToSeconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds / 1000000000;
end;

function NanosecondsToKiloseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds / 1000000000000;
end;

function NanosecondsToMegaseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds / 1000000000000000;
end;

function NanosecondsToGigaseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds / 1000000000000000000;
end;

function NanosecondsToTeraseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds / Power(1000, 7);
end;

function NanosecondsToPetaseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds / Power(1000, 8);
end;

function NanosecondsToExaseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds / Power(1000, 9);
end;

function NanosecondsToZettaseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds / Power(1000, 10);
end;

function NanosecondsToYottaseconds(const ANanoseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ANanoseconds / Power(1000, 11);
end;

// Microseconds To ?
function MicrosecondsToYoctoseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds * 1000000000000000000;
end;

function MicrosecondsToZeptoseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds * 1000000000000000;
end;

function MicrosecondsToAttoseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds * 1000000000000;
end;

function MicrosecondsToFemtoseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds * 1000000000;
end;

function MicrosecondsToPicoseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds * 1000000;
end;

function MicrosecondsToNanoseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds * 1000;
end;

function MicrosecondsToMilliseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds / 1000;
end;

function MicrosecondsToSeconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds / 1000000;
end;

function MicrosecondsToKiloseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds / 1000000000;
end;

function MicrosecondsToMegaseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds / 1000000000000;
end;

function MicrosecondsToGigaseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds / 1000000000000000;
end;

function MicrosecondsToTeraseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds / 1000000000000000000;
end;

function MicrosecondsToPetaseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds / Power(1000, 7);
end;

function MicrosecondsToExaseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds / Power(1000, 8);
end;

function MicrosecondsToZettaseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds / Power(1000, 9);
end;

function MicrosecondsToYottaseconds(const AMicroseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMicroseconds / Power(1000, 10);
end;

// Milliseconds To ?
function MillisecondsToYoctoseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds * Power(1000, 7);
end;

function MillisecondsToZeptoseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds * 1000000000000000000;
end;

function MillisecondsToAttoseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds * 1000000000000000;
end;

function MillisecondsToFemtoseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds * 1000000000000;
end;

function MillisecondsToPicoseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds * 1000000000;
end;

function MillisecondsToNanoseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds * 1000000;
end;

function MillisecondsToMicroseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds * 1000;
end;

function MillisecondsToSeconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds / 1000;
end;

function MillisecondsToKiloseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds / 1000000;
end;

function MillisecondsToMegaseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds / 1000000000;
end;

function MillisecondsToGigaseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds / 1000000000000;
end;

function MillisecondsToTeraseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds / 1000000000000000;
end;

function MillisecondsToPetaseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds / 1000000000000000000;
end;

function MillisecondsToExaseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds / Power(1000, 7);
end;

function MillisecondsToZettaseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds / Power(1000, 8);
end;

function MillisecondsToYottaseconds(const AMilliseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMilliseconds / Power(1000, 9);
end;

// Seconds To ?
function SecondsToYoctoseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds * Power(1000, 8);
end;

function SecondsToZeptoseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds * Power(1000, 7);
end;

function SecondsToAttoseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds * 1000000000000000000;
end;

function SecondsToFemtoseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds * 1000000000000000;
end;

function SecondsToPicoseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds * 1000000000000;
end;

function SecondsToNanoseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds * 1000000000;
end;

function SecondsToMicroseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds * 1000000;
end;

function SecondsToMilliseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds * 1000;
end;

function SecondsToKiloseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds / 1000;
end;

function SecondsToMegaseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds / 1000000;
end;

function SecondsToGigaseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds / 1000000000;
end;

function SecondsToTeraseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds / 1000000000000;
end;

function SecondsToPetaseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds / 1000000000000000;
end;

function SecondsToExaseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds / 1000000000000000000;
end;

function SecondsToZettaseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds / Power(1000, 7);
end;

function SecondsToYottaseconds(const ASeconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ASeconds / Power(1000, 8);
end;

     // Kiloseconds To ?
function KilosecondsToYoctoseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds * Power(1000, 9);
end;

function KilosecondsToZeptoseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds * Power(1000, 8);
end;

function KilosecondsToAttoseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds * Power(1000, 7);
end;

function KilosecondsToFemtoseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds * 1000000000000000000;
end;

function KilosecondsToPicoseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds * 1000000000000000;
end;

function KilosecondsToNanoseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds * 1000000000000;
end;

function KilosecondsToMicroseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds * 1000000000;
end;

function KilosecondsToMilliseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds * 1000000;
end;

function KilosecondsToSeconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds * 1000;
end;

function KilosecondsToMegaseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds / 1000;
end;

function KilosecondsToGigaseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds / 1000000;
end;

function KilosecondsToTeraseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds / 1000000000;
end;

function KilosecondsToPetaseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds / 1000000000000;
end;

function KilosecondsToExaseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds / 1000000000000000;
end;

function KilosecondsToZettaseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds / 1000000000000000000;
end;

function KilosecondsToYottaseconds(const AKiloseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AKiloseconds / Power(1000, 7);
end;

// Megaseconds To ?
function MegasecondsToYoctoseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds * Power(1000, 10);
end;

function MegasecondsToZeptoseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds * Power(1000, 9);
end;

function MegasecondsToAttoseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds * Power(1000, 8);
end;

function MegasecondsToFemtoseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds * Power(1000, 7);
end;

function MegasecondsToPicoseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds * 1000000000000000000;
end;

function MegasecondsToNanoseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds * 1000000000000000;
end;

function MegasecondsToMicroseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds * 1000000000000;
end;

function MegasecondsToMilliseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds * 1000000000;
end;

function MegasecondsToSeconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds * 1000000;
end;

function MegasecondsToKiloseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds * 1000;
end;

function MegasecondsToGigaseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds / 1000;
end;

function MegasecondsToTeraseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds / 1000000;
end;

function MegasecondsToPetaseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds / 1000000000;
end;

function MegasecondsToExaseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds / 1000000000000;
end;

function MegasecondsToZettaseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds / 1000000000000000;
end;

function MegasecondsToYottaseconds(const AMegaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AMegaseconds / 1000000000000000000;
end;

// Gigaseconds To ?
function GigasecondsToYoctoseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds * Power(1000, 11);
end;

function GigasecondsToZeptoseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds * Power(1000, 10);
end;

function GigasecondsToAttoseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds * Power(1000, 9);
end;

function GigasecondsToFemtoseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds * Power(1000, 8);
end;

function GigasecondsToPicoseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds * Power(1000, 7);
end;

function GigasecondsToNanoseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds * 1000000000000000000;
end;

function GigasecondsToMicroseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds * 1000000000000000;
end;

function GigasecondsToMilliseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds * 1000000000000;
end;

function GigasecondsToSeconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds * 1000000000;
end;

function GigasecondsToKiloseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds * 1000000;
end;

function GigasecondsToMegaseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds * 1000;
end;

function GigasecondsToTeraseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds / 1000;
end;

function GigasecondsToPetaseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds / 1000000;
end;

function GigasecondsToExaseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds / 1000000000;
end;

function GigasecondsToZettaseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds / 1000000000000;
end;

function GigasecondsToYottaseconds(const AGigaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AGigaseconds / 1000000000000000;
end;

// Teraseconds To ?
function TerasecondsToYoctoseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds * Power(1000, 12);
end;

function TerasecondsToZeptoseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds * Power(1000, 11);
end;

function TerasecondsToAttoseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds * Power(1000, 10);
end;

function TerasecondsToFemtoseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds * Power(1000, 9);
end;

function TerasecondsToPicoseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds * Power(1000, 8);
end;

function TerasecondsToNanoseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds * Power(1000, 7);
end;

function TerasecondsToMicroseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds * 1000000000000000000;
end;

function TerasecondsToMilliseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds * 1000000000000000;
end;

function TerasecondsToSeconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds * 1000000000000;
end;

function TerasecondsToKiloseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds * 1000000000;
end;

function TerasecondsToMegaseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds * 1000000;
end;

function TerasecondsToGigaseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds * 1000;
end;

function TerasecondsToPetaseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds / 1000;
end;

function TerasecondsToExaseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds / 1000000;
end;

function TerasecondsToZettaseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds / 1000000000;
end;

function TerasecondsToYottaseconds(const ATeraseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := ATeraseconds / 1000000000000;
end;

// Petaseconds To ?
function PetasecondsToYoctoseconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds * Power(1000, 13);
end;

function PetasecondsToZeptoseconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds * Power(1000, 12);
end;

function PetasecondsToAttoseconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds * Power(1000, 11);
end;

function PetasecondsToFemtoseconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds * Power(1000, 10);
end;

function PetasecondsToPicoseconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds * Power(1000, 9);
end;

function PetasecondsToNanoseconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds * Power(1000, 8);
end;

function PetasecondsToMicroseconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds * Power(1000, 7);
end;

function PetasecondsToMilliseconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds * 1000000000000000000;
end;

function PetasecondsToSeconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds * 1000000000000000;
end;

function PetasecondsToKiloseconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds * 1000000000000;
end;

function PetasecondsToMegaseconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds * 1000000000;
end;

function PetasecondsToGigaseconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds * 1000000;
end;

function PetasecondsToTeraseconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds * 1000;
end;

function PetasecondsToExaseconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds / 1000;
end;

function PetasecondsToZettaseconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds / 1000000;
end;

function PetasecondsToYottaseconds(const APetaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := APetaseconds / 1000000000;
end;

// Exaseconds To ?
function ExasecondsToYoctoseconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds * Power(1000, 14);
end;

function ExasecondsToZeptoseconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds * Power(1000, 13);
end;

function ExasecondsToAttoseconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds * Power(1000, 12);
end;

function ExasecondsToFemtoseconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds * Power(1000, 11);
end;

function ExasecondsToPicoseconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds * Power(1000, 10);
end;

function ExasecondsToNanoseconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds * Power(1000, 9);
end;

function ExasecondsToMicroseconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds * Power(1000, 8);
end;

function ExasecondsToMilliseconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds * Power(1000, 7);
end;

function ExasecondsToSeconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds * 1000000000000000000;
end;

function ExasecondsToKiloseconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds * 1000000000000000;
end;

function ExasecondsToMegaseconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds * 1000000000000;
end;

function ExasecondsToGigaseconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds * 1000000000;
end;

function ExasecondsToTeraseconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds * 1000000;
end;

function ExasecondsToPetaseconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds * 1000;
end;

function ExasecondsToZettaseconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds / 1000;
end;

function ExasecondsToYottaseconds(const AExaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AExaseconds / 1000000;
end;

// Zettaseconds To ?
function ZettasecondsToYoctoseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds * Power(1000, 15);
end;

function ZettasecondsToZeptoseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds * Power(1000, 14);
end;

function ZettasecondsToAttoseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds * Power(1000, 13);
end;

function ZettasecondsToFemtoseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds * Power(1000, 12);
end;

function ZettasecondsToPicoseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds * Power(1000, 11);
end;

function ZettasecondsToNanoseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds * Power(1000, 10);
end;

function ZettasecondsToMicroseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds * Power(1000, 9);
end;

function ZettasecondsToMilliseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds * Power(1000, 8);
end;

function ZettasecondsToSeconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds * Power(1000, 7);
end;

function ZettasecondsToKiloseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds * 1000000000000000000;
end;

function ZettasecondsToMegaseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds * 1000000000000000;
end;

function ZettasecondsToGigaseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds * 1000000000000;
end;

function ZettasecondsToTeraseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds * 1000000000;
end;

function ZettasecondsToPetaseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds * 1000000;
end;

function ZettasecondsToExaseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds * 1000;
end;

function ZettasecondsToYottaseconds(const AZettaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AZettaseconds / 1000;
end;

// Yottaseconds To ?
function YottasecondsToYoctoseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * Power(1000, 16);
end;

function YottasecondsToZeptoseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * Power(1000, 15);
end;

function YottasecondsToAttoseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * Power(1000, 14);
end;

function YottasecondsToFemtoseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * Power(1000, 13);
end;

function YottasecondsToPicoseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * Power(1000, 12);
end;

function YottasecondsToNanoseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * Power(1000, 11);
end;

function YottasecondsToMicroseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * Power(1000, 10);
end;

function YottasecondsToMilliseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * Power(1000, 9);
end;

function YottasecondsToSeconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * Power(1000, 8);
end;

function YottasecondsToKiloseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * Power(1000, 7);
end;

function YottasecondsToMegaseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * 1000000000000000000;
end;

function YottasecondsToGigaseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * 1000000000000000;
end;

function YottasecondsToTeraseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * 1000000000000;
end;

function YottasecondsToPetaseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * 1000000000;
end;

function YottasecondsToExaseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * 1000000;
end;

function YottasecondsToZettaseconds(const AYottaseconds: LKTimeFloat): LKTimeFloat;
begin
  Result := AYottaseconds * 1000;
end;

end.
