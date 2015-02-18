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

{$I ..\Common\LKSL.inc}

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.Math,
  {$ELSE}
    Classes, SysUtils, Math,
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}
  LKSL.Common.Types,
  LKSL.Math.Common;

  {$I ..\Common\LKSL_RTTI.inc}

type

  { Enum Types }
  TLKUnit = (tuYoctounit, tuZeptounit, tuAttounit, tuFemtounit, tuPicounit, tuNanounit,
             tuMicrounit, tuMilliunit, tuUnit, tuKilounit, tuMegaunit, tuGigaunit,
             tuTeraunit, tuPetaunit, tuExaunit, tuZettaunit, tuYottaunit);

// ? To ?
function TimeUnitConvert(const ASourceValue: LKFloat; const AInputUnit, AOutputUnit: TLKUnit): LKFloat;
// Yoctounits To ?
procedure YoctounitsToBest(const AYoctounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function YoctounitsToZeptounits(const AYoctounits: LKFloat): LKFloat; inline;
function YoctounitsToAttounits(const AYoctounits: LKFloat): LKFloat; inline;
function YoctounitsToFemtounits(const AYoctounits: LKFloat): LKFloat; inline;
function YoctounitsToPicounits(const AYoctounits: LKFloat): LKFloat; inline;
function YoctounitsToNanounits(const AYoctounits: LKFloat): LKFloat; inline;
function YoctounitsToMicrounits(const AYoctounits: LKFloat): LKFloat; inline;
function YoctounitsToMilliunits(const AYoctounits: LKFloat): LKFloat; inline;
function YoctounitsToUnits(const AYoctounits: LKFloat): LKFloat; inline;
function YoctounitsToKilounits(const AYoctounits: LKFloat): LKFloat; inline;
function YoctounitsToMegaunits(const AYoctounits: LKFloat): LKFloat; inline;
function YoctounitsToGigaunits(const AYoctounits: LKFloat): LKFloat; inline;
function YoctounitsToTeraunits(const AYoctounits: LKFloat): LKFloat; inline;
function YoctounitsToPetaunits(const AYoctounits: LKFloat): LKFloat; inline;
function YoctounitsToExaunits(const AYoctounits: LKFloat): LKFloat; inline;
function YoctounitsToZettaunits(const AYoctounits: LKFloat): LKFloat; inline;
function YoctounitsToYottaunits(const AYoctounits: LKFloat): LKFloat; inline;
// Zeptounits To ?
procedure ZeptounitsToBest(const AZeptounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function ZeptounitsToYoctounits(const AZeptounits: LKFloat): LKFloat; inline; // Down
function ZeptounitsToAttounits(const AZeptounits: LKFloat): LKFloat; inline;
function ZeptounitsToFemtounits(const AZeptounits: LKFloat): LKFloat; inline;
function ZeptounitsToPicounits(const AZeptounits: LKFloat): LKFloat; inline;
function ZeptounitsToNanounits(const AZeptounits: LKFloat): LKFloat; inline;
function ZeptounitsToMicrounits(const AZeptounits: LKFloat): LKFloat; inline;
function ZeptounitsToMilliunits(const AZeptounits: LKFloat): LKFloat; inline;
function ZeptounitsToUnits(const AZeptounits: LKFloat): LKFloat; inline;
function ZeptounitsToKilounits(const AZeptounits: LKFloat): LKFloat; inline;
function ZeptounitsToMegaunits(const AZeptounits: LKFloat): LKFloat; inline;
function ZeptounitsToGigaunits(const AZeptounits: LKFloat): LKFloat; inline;
function ZeptounitsToTeraunits(const AZeptounits: LKFloat): LKFloat; inline;
function ZeptounitsToPetaunits(const AZeptounits: LKFloat): LKFloat; inline;
function ZeptounitsToExaunits(const AZeptounits: LKFloat): LKFloat; inline;
function ZeptounitsToZettaunits(const AZeptounits: LKFloat): LKFloat; inline;
function ZeptounitsToYottaunits(const AZeptounits: LKFloat): LKFloat; inline;
// Attounits To ?
procedure AttounitsToBest(const AAttounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function AttounitsToYoctounits(const AAttounits: LKFloat): LKFloat; inline;
function AttounitsToZeptounits(const AAttounits: LKFloat): LKFloat; inline; // Down
function AttounitsToFemtounits(const AAttounits: LKFloat): LKFloat; inline;
function AttounitsToPicounits(const AAttounits: LKFloat): LKFloat; inline;
function AttounitsToNanounits(const AAttounits: LKFloat): LKFloat; inline;
function AttounitsToMicrounits(const AAttounits: LKFloat): LKFloat; inline;
function AttounitsToMilliunits(const AAttounits: LKFloat): LKFloat; inline;
function AttounitsToUnits(const AAttounits: LKFloat): LKFloat; inline;
function AttounitsToKilounits(const AAttounits: LKFloat): LKFloat; inline;
function AttounitsToMegaunits(const AAttounits: LKFloat): LKFloat; inline;
function AttounitsToGigaunits(const AAttounits: LKFloat): LKFloat; inline;
function AttounitsToTeraunits(const AAttounits: LKFloat): LKFloat; inline;
function AttounitsToPetaunits(const AAttounits: LKFloat): LKFloat; inline;
function AttounitsToExaunits(const AAttounits: LKFloat): LKFloat; inline;
function AttounitsToZettaunits(const AAttounits: LKFloat): LKFloat; inline;
function AttounitsToYottaunits(const AAttounits: LKFloat): LKFloat; inline;
// Femtounits To ?
procedure FemtounitsToBest(const AFemtounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function FemtounitsToYoctounits(const AFemtounits: LKFloat): LKFloat; inline;
function FemtounitsToZeptounits(const AFemtounits: LKFloat): LKFloat; inline;
function FemtounitsToAttounits(const AFemtounits: LKFloat): LKFloat; inline; // Down
function FemtounitsToPicounits(const AFemtounits: LKFloat): LKFloat; inline;
function FemtounitsToNanounits(const AFemtounits: LKFloat): LKFloat; inline;
function FemtounitsToMicrounits(const AFemtounits: LKFloat): LKFloat; inline;
function FemtounitsToMilliunits(const AFemtounits: LKFloat): LKFloat; inline;
function FemtounitsToUnits(const AFemtounits: LKFloat): LKFloat; inline;
function FemtounitsToKilounits(const AFemtounits: LKFloat): LKFloat; inline;
function FemtounitsToMegaunits(const AFemtounits: LKFloat): LKFloat; inline;
function FemtounitsToGigaunits(const AFemtounits: LKFloat): LKFloat; inline;
function FemtounitsToTeraunits(const AFemtounits: LKFloat): LKFloat; inline;
function FemtounitsToPetaunits(const AFemtounits: LKFloat): LKFloat; inline;
function FemtounitsToExaunits(const AFemtounits: LKFloat): LKFloat; inline;
function FemtounitsToZettaunits(const AFemtounits: LKFloat): LKFloat; inline;
function FemtounitsToYottaunits(const AFemtounits: LKFloat): LKFloat; inline;
// Picounits To ?
procedure PicounitsToBest(const APicounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function PicounitsToYoctounits(const APicounits: LKFloat): LKFloat; inline;
function PicounitsToZeptounits(const APicounits: LKFloat): LKFloat; inline;
function PicounitsToAttounits(const APicounits: LKFloat): LKFloat; inline;
function PicounitsToFemtounits(const APicounits: LKFloat): LKFloat; inline; // Down
function PicounitsToNanounits(const APicounits: LKFloat): LKFloat; inline;
function PicounitsToMicrounits(const APicounits: LKFloat): LKFloat; inline;
function PicounitsToMilliunits(const APicounits: LKFloat): LKFloat; inline;
function PicounitsToUnits(const APicounits: LKFloat): LKFloat; inline;
function PicounitsToKilounits(const APicounits: LKFloat): LKFloat; inline;
function PicounitsToMegaunits(const APicounits: LKFloat): LKFloat; inline;
function PicounitsToGigaunits(const APicounits: LKFloat): LKFloat; inline;
function PicounitsToTeraunits(const APicounits: LKFloat): LKFloat; inline;
function PicounitsToPetaunits(const APicounits: LKFloat): LKFloat; inline;
function PicounitsToExaunits(const APicounits: LKFloat): LKFloat; inline;
function PicounitsToZettaunits(const APicounits: LKFloat): LKFloat; inline;
function PicounitsToYottaunits(const APicounits: LKFloat): LKFloat; inline;
// Nanounits To ?
procedure NanounitsToBest(const ANanounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function NanounitsToYoctounits(const ANanounits: LKFloat): LKFloat; inline;
function NanounitsToZeptounits(const ANanounits: LKFloat): LKFloat; inline;
function NanounitsToAttounits(const ANanounits: LKFloat): LKFloat; inline;
function NanounitsToFemtounits(const ANanounits: LKFloat): LKFloat; inline;
function NanounitsToPicounits(const ANanounits: LKFloat): LKFloat; inline; // Down
function NanounitsToMicrounits(const ANanounits: LKFloat): LKFloat; inline;
function NanounitsToMilliunits(const ANanounits: LKFloat): LKFloat; inline;
function NanounitsToUnits(const ANanounits: LKFloat): LKFloat; inline;
function NanounitsToKilounits(const ANanounits: LKFloat): LKFloat; inline;
function NanounitsToMegaunits(const ANanounits: LKFloat): LKFloat; inline;
function NanounitsToGigaunits(const ANanounits: LKFloat): LKFloat; inline;
function NanounitsToTeraunits(const ANanounits: LKFloat): LKFloat; inline;
function NanounitsToPetaunits(const ANanounits: LKFloat): LKFloat; inline;
function NanounitsToExaunits(const ANanounits: LKFloat): LKFloat; inline;
function NanounitsToZettaunits(const ANanounits: LKFloat): LKFloat; inline;
function NanounitsToYottaunits(const ANanounits: LKFloat): LKFloat; inline;
// Microunits To ?
procedure MicrounitsToBest(const AMicrounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function MicrounitsToYoctounits(const AMicrounits: LKFloat): LKFloat; inline;
function MicrounitsToZeptounits(const AMicrounits: LKFloat): LKFloat; inline;
function MicrounitsToAttounits(const AMicrounits: LKFloat): LKFloat; inline;
function MicrounitsToFemtounits(const AMicrounits: LKFloat): LKFloat; inline;
function MicrounitsToPicounits(const AMicrounits: LKFloat): LKFloat; inline;
function MicrounitsToNanounits(const AMicrounits: LKFloat): LKFloat; inline; // Down
function MicrounitsToMilliunits(const AMicrounits: LKFloat): LKFloat; inline;
function MicrounitsToUnits(const AMicrounits: LKFloat): LKFloat; inline;
function MicrounitsToKilounits(const AMicrounits: LKFloat): LKFloat; inline;
function MicrounitsToMegaunits(const AMicrounits: LKFloat): LKFloat; inline;
function MicrounitsToGigaunits(const AMicrounits: LKFloat): LKFloat; inline;
function MicrounitsToTeraunits(const AMicrounits: LKFloat): LKFloat; inline;
function MicrounitsToPetaunits(const AMicrounits: LKFloat): LKFloat; inline;
function MicrounitsToExaunits(const AMicrounits: LKFloat): LKFloat; inline;
function MicrounitsToZettaunits(const AMicrounits: LKFloat): LKFloat; inline;
function MicrounitsToYottaunits(const AMicrounits: LKFloat): LKFloat; inline;
// Milliunits To ?
procedure MilliunitsToBest(const AMilliunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function MilliunitsToYoctounits(const AMilliunits: LKFloat): LKFloat; inline;
function MilliunitsToZeptounits(const AMilliunits: LKFloat): LKFloat; inline;
function MilliunitsToAttounits(const AMilliunits: LKFloat): LKFloat; inline;
function MilliunitsToFemtounits(const AMilliunits: LKFloat): LKFloat; inline;
function MilliunitsToPicounits(const AMilliunits: LKFloat): LKFloat; inline;
function MilliunitsToNanounits(const AMilliunits: LKFloat): LKFloat; inline;
function MilliunitsToMicrounits(const AMilliunits: LKFloat): LKFloat; inline; // Down
function MilliunitsToUnits(const AMilliunits: LKFloat): LKFloat; inline;
function MilliunitsToKilounits(const AMilliunits: LKFloat): LKFloat; inline;
function MilliunitsToMegaunits(const AMilliunits: LKFloat): LKFloat; inline;
function MilliunitsToGigaunits(const AMilliunits: LKFloat): LKFloat; inline;
function MilliunitsToTeraunits(const AMilliunits: LKFloat): LKFloat; inline;
function MilliunitsToPetaunits(const AMilliunits: LKFloat): LKFloat; inline;
function MilliunitsToExaunits(const AMilliunits: LKFloat): LKFloat; inline;
function MilliunitsToZettaunits(const AMilliunits: LKFloat): LKFloat; inline;
function MilliunitsToYottaunits(const AMilliunits: LKFloat): LKFloat; inline;
// Units To ?
procedure UnitsToBest(const AUnits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function UnitsToYoctounits(const AUnits: LKFloat): LKFloat; inline;
function UnitsToZeptounits(const AUnits: LKFloat): LKFloat; inline;
function UnitsToAttounits(const AUnits: LKFloat): LKFloat; inline;
function UnitsToFemtounits(const AUnits: LKFloat): LKFloat; inline;
function UnitsToPicounits(const AUnits: LKFloat): LKFloat; inline;
function UnitsToNanounits(const AUnits: LKFloat): LKFloat; inline;
function UnitsToMicrounits(const AUnits: LKFloat): LKFloat; inline;
function UnitsToMilliunits(const AUnits: LKFloat): LKFloat; inline;
function UnitsToKilounits(const AUnits: LKFloat): LKFloat; inline;
function UnitsToMegaunits(const AUnits: LKFloat): LKFloat; inline;
function UnitsToGigaunits(const AUnits: LKFloat): LKFloat; inline;
function UnitsToTeraunits(const AUnits: LKFloat): LKFloat; inline;
function UnitsToPetaunits(const AUnits: LKFloat): LKFloat; inline;
function UnitsToExaunits(const AUnits: LKFloat): LKFloat; inline;
function UnitsToZettaunits(const AUnits: LKFloat): LKFloat; inline;
function UnitsToYottaunits(const AUnits: LKFloat): LKFloat; inline;
// Kilounits To ?
procedure KilounitsToBest(const AKilounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function KilounitsToYoctounits(const AKilounits: LKFloat): LKFloat; inline;
function KilounitsToZeptounits(const AKilounits: LKFloat): LKFloat; inline;
function KilounitsToAttounits(const AKilounits: LKFloat): LKFloat; inline;
function KilounitsToFemtounits(const AKilounits: LKFloat): LKFloat; inline;
function KilounitsToPicounits(const AKilounits: LKFloat): LKFloat; inline;
function KilounitsToNanounits(const AKilounits: LKFloat): LKFloat; inline;
function KilounitsToMicrounits(const AKilounits: LKFloat): LKFloat; inline;
function KilounitsToMilliunits(const AKilounits: LKFloat): LKFloat; inline;
function KilounitsToUnits(const AKilounits: LKFloat): LKFloat; inline; // Down
function KilounitsToMegaunits(const AKilounits: LKFloat): LKFloat; inline;
function KilounitsToGigaunits(const AKilounits: LKFloat): LKFloat; inline;
function KilounitsToTeraunits(const AKilounits: LKFloat): LKFloat; inline;
function KilounitsToPetaunits(const AKilounits: LKFloat): LKFloat; inline;
function KilounitsToExaunits(const AKilounits: LKFloat): LKFloat; inline;
function KilounitsToZettaunits(const AKilounits: LKFloat): LKFloat; inline;
function KilounitsToYottaunits(const AKilounits: LKFloat): LKFloat; inline;
// Megaunits To ?
procedure MegaunitsToBest(const AMegaunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function MegaunitsToYoctounits(const AMegaunits: LKFloat): LKFloat; inline;
function MegaunitsToZeptounits(const AMegaunits: LKFloat): LKFloat; inline;
function MegaunitsToAttounits(const AMegaunits: LKFloat): LKFloat; inline;
function MegaunitsToFemtounits(const AMegaunits: LKFloat): LKFloat; inline;
function MegaunitsToPicounits(const AMegaunits: LKFloat): LKFloat; inline;
function MegaunitsToNanounits(const AMegaunits: LKFloat): LKFloat; inline;
function MegaunitsToMicrounits(const AMegaunits: LKFloat): LKFloat; inline;
function MegaunitsToMilliunits(const AMegaunits: LKFloat): LKFloat; inline;
function MegaunitsToUnits(const AMegaunits: LKFloat): LKFloat; inline;
function MegaunitsToKilounits(const AMegaunits: LKFloat): LKFloat; inline; // Down
function MegaunitsToGigaunits(const AMegaunits: LKFloat): LKFloat; inline;
function MegaunitsToTeraunits(const AMegaunits: LKFloat): LKFloat; inline;
function MegaunitsToPetaunits(const AMegaunits: LKFloat): LKFloat; inline;
function MegaunitsToExaunits(const AMegaunits: LKFloat): LKFloat; inline;
function MegaunitsToZettaunits(const AMegaunits: LKFloat): LKFloat; inline;
function MegaunitsToYottaunits(const AMegaunits: LKFloat): LKFloat; inline;
// Gigaunits To ?
procedure GigaunitsToBest(const AGigaunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function GigaunitsToYoctounits(const AGigaunits: LKFloat): LKFloat; inline;
function GigaunitsToZeptounits(const AGigaunits: LKFloat): LKFloat; inline;
function GigaunitsToAttounits(const AGigaunits: LKFloat): LKFloat; inline;
function GigaunitsToFemtounits(const AGigaunits: LKFloat): LKFloat; inline;
function GigaunitsToPicounits(const AGigaunits: LKFloat): LKFloat; inline;
function GigaunitsToNanounits(const AGigaunits: LKFloat): LKFloat; inline;
function GigaunitsToMicrounits(const AGigaunits: LKFloat): LKFloat; inline;
function GigaunitsToMilliunits(const AGigaunits: LKFloat): LKFloat; inline;
function GigaunitsToUnits(const AGigaunits: LKFloat): LKFloat; inline;
function GigaunitsToKilounits(const AGigaunits: LKFloat): LKFloat; inline;
function GigaunitsToMegaunits(const AGigaunits: LKFloat): LKFloat; inline; // Down
function GigaunitsToTeraunits(const AGigaunits: LKFloat): LKFloat; inline;
function GigaunitsToPetaunits(const AGigaunits: LKFloat): LKFloat; inline;
function GigaunitsToExaunits(const AGigaunits: LKFloat): LKFloat; inline;
function GigaunitsToZettaunits(const AGigaunits: LKFloat): LKFloat; inline;
function GigaunitsToYottaunits(const AGigaunits: LKFloat): LKFloat; inline;
// Teraunits To ?
procedure TeraunitsToBest(const ATeraunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function TeraunitsToYoctounits(const ATeraunits: LKFloat): LKFloat; inline;
function TeraunitsToZeptounits(const ATeraunits: LKFloat): LKFloat; inline;
function TeraunitsToAttounits(const ATeraunits: LKFloat): LKFloat; inline;
function TeraunitsToFemtounits(const ATeraunits: LKFloat): LKFloat; inline;
function TeraunitsToPicounits(const ATeraunits: LKFloat): LKFloat; inline;
function TeraunitsToNanounits(const ATeraunits: LKFloat): LKFloat; inline;
function TeraunitsToMicrounits(const ATeraunits: LKFloat): LKFloat; inline;
function TeraunitsToMilliunits(const ATeraunits: LKFloat): LKFloat; inline;
function TeraunitsToUnits(const ATeraunits: LKFloat): LKFloat; inline;
function TeraunitsToKilounits(const ATeraunits: LKFloat): LKFloat; inline;
function TeraunitsToMegaunits(const ATeraunits: LKFloat): LKFloat; inline;
function TeraunitsToGigaunits(const ATeraunits: LKFloat): LKFloat; inline; // Down
function TeraunitsToPetaunits(const ATeraunits: LKFloat): LKFloat; inline;
function TeraunitsToExaunits(const ATeraunits: LKFloat): LKFloat; inline;
function TeraunitsToZettaunits(const ATeraunits: LKFloat): LKFloat; inline;
function TeraunitsToYottaunits(const ATeraunits: LKFloat): LKFloat; inline;
// Petaunits To ?
procedure PetaunitsToBest(const APetaunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function PetaunitsToYoctounits(const APetaunits: LKFloat): LKFloat; inline;
function PetaunitsToZeptounits(const APetaunits: LKFloat): LKFloat; inline;
function PetaunitsToAttounits(const APetaunits: LKFloat): LKFloat; inline;
function PetaunitsToFemtounits(const APetaunits: LKFloat): LKFloat; inline;
function PetaunitsToPicounits(const APetaunits: LKFloat): LKFloat; inline;
function PetaunitsToNanounits(const APetaunits: LKFloat): LKFloat; inline;
function PetaunitsToMicrounits(const APetaunits: LKFloat): LKFloat; inline;
function PetaunitsToMilliunits(const APetaunits: LKFloat): LKFloat; inline;
function PetaunitsToUnits(const APetaunits: LKFloat): LKFloat; inline;
function PetaunitsToKilounits(const APetaunits: LKFloat): LKFloat; inline;
function PetaunitsToMegaunits(const APetaunits: LKFloat): LKFloat; inline;
function PetaunitsToGigaunits(const APetaunits: LKFloat): LKFloat; inline;
function PetaunitsToTeraunits(const APetaunits: LKFloat): LKFloat; inline; // Down
function PetaunitsToExaunits(const APetaunits: LKFloat): LKFloat; inline;
function PetaunitsToZettaunits(const APetaunits: LKFloat): LKFloat; inline;
function PetaunitsToYottaunits(const APetaunits: LKFloat): LKFloat; inline;
// Exaunits To ?
procedure ExaunitsToBest(const AExaunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function ExaunitsToYoctounits(const AExaunits: LKFloat): LKFloat; inline;
function ExaunitsToZeptounits(const AExaunits: LKFloat): LKFloat; inline;
function ExaunitsToAttounits(const AExaunits: LKFloat): LKFloat; inline;
function ExaunitsToFemtounits(const AExaunits: LKFloat): LKFloat; inline;
function ExaunitsToPicounits(const AExaunits: LKFloat): LKFloat; inline;
function ExaunitsToNanounits(const AExaunits: LKFloat): LKFloat; inline;
function ExaunitsToMicrounits(const AExaunits: LKFloat): LKFloat; inline;
function ExaunitsToMilliunits(const AExaunits: LKFloat): LKFloat; inline;
function ExaunitsToUnits(const AExaunits: LKFloat): LKFloat; inline;
function ExaunitsToKilounits(const AExaunits: LKFloat): LKFloat; inline;
function ExaunitsToMegaunits(const AExaunits: LKFloat): LKFloat; inline;
function ExaunitsToGigaunits(const AExaunits: LKFloat): LKFloat; inline;
function ExaunitsToTeraunits(const AExaunits: LKFloat): LKFloat; inline;
function ExaunitsToPetaunits(const AExaunits: LKFloat): LKFloat; inline; // Down
function ExaunitsToZettaunits(const AExaunits: LKFloat): LKFloat; inline;
function ExaunitsToYottaunits(const AExaunits: LKFloat): LKFloat; inline;
// Zettaunits To ?
procedure ZettaunitsToBest(const AZettaunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function ZettaunitsToYoctounits(const AZettaunits: LKFloat): LKFloat; inline;
function ZettaunitsToZeptounits(const AZettaunits: LKFloat): LKFloat; inline;
function ZettaunitsToAttounits(const AZettaunits: LKFloat): LKFloat; inline;
function ZettaunitsToFemtounits(const AZettaunits: LKFloat): LKFloat; inline;
function ZettaunitsToPicounits(const AZettaunits: LKFloat): LKFloat; inline;
function ZettaunitsToNanounits(const AZettaunits: LKFloat): LKFloat; inline;
function ZettaunitsToMicrounits(const AZettaunits: LKFloat): LKFloat; inline;
function ZettaunitsToMilliunits(const AZettaunits: LKFloat): LKFloat; inline;
function ZettaunitsToUnits(const AZettaunits: LKFloat): LKFloat; inline;
function ZettaunitsToKilounits(const AZettaunits: LKFloat): LKFloat; inline;
function ZettaunitsToMegaunits(const AZettaunits: LKFloat): LKFloat; inline;
function ZettaunitsToGigaunits(const AZettaunits: LKFloat): LKFloat; inline;
function ZettaunitsToTeraunits(const AZettaunits: LKFloat): LKFloat; inline;
function ZettaunitsToPetaunits(const AZettaunits: LKFloat): LKFloat; inline;
function ZettaunitsToExaunits(const AZettaunits: LKFloat): LKFloat; inline; // Down
function ZettaunitsToYottaunits(const AZettaunits: LKFloat): LKFloat; inline;
// Yottaunits To ?
procedure YottaunitsToBest(const AYottaunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
function YottaunitsToYoctounits(const AYottaunits: LKFloat): LKFloat; inline;
function YottaunitsToZeptounits(const AYottaunits: LKFloat): LKFloat; inline;
function YottaunitsToAttounits(const AYottaunits: LKFloat): LKFloat; inline;
function YottaunitsToFemtounits(const AYottaunits: LKFloat): LKFloat; inline;
function YottaunitsToPicounits(const AYottaunits: LKFloat): LKFloat; inline;
function YottaunitsToNanounits(const AYottaunits: LKFloat): LKFloat; inline;
function YottaunitsToMicrounits(const AYottaunits: LKFloat): LKFloat; inline;
function YottaunitsToMilliunits(const AYottaunits: LKFloat): LKFloat; inline;
function YottaunitsToUnits(const AYottaunits: LKFloat): LKFloat; inline;
function YottaunitsToKilounits(const AYottaunits: LKFloat): LKFloat; inline;
function YottaunitsToMegaunits(const AYottaunits: LKFloat): LKFloat; inline;
function YottaunitsToGigaunits(const AYottaunits: LKFloat): LKFloat; inline;
function YottaunitsToTeraunits(const AYottaunits: LKFloat): LKFloat; inline;
function YottaunitsToPetaunits(const AYottaunits: LKFloat): LKFloat; inline;
function YottaunitsToExaunits(const AYottaunits: LKFloat): LKFloat; inline;
function YottaunitsToZettaunits(const AYottaunits: LKFloat): LKFloat; inline; // Down

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

function TimeUnitConvert(const ASourceValue: LKFloat; const AInputUnit, AOutputUnit: TLKUnit): LKFloat;
const
  CONVERSION_METHODS: Array[TLKUnit, TLKUnit] of TLKTimeConversionMethod =
                                                                                    (
                                                                                      // Yoctounits
                                                                                      (
                                                                                        NoConversion, YoctounitsToZeptounits, YoctounitsToAttounits, YoctounitsToFemtounits,
                                                                                        YoctounitsToPicounits, YoctounitsToNanounits, YoctounitsToMicrounits, YoctounitsToMilliunits,
                                                                                        YoctounitsToUnits, YoctounitsToKilounits, YoctounitsToMegaunits, YoctounitsToGigaunits,
                                                                                        YoctounitsToTeraunits, YoctounitsToPetaunits, YoctounitsToExaunits, YoctounitsToZettaunits,
                                                                                        YoctounitsToYottaunits
                                                                                      ),
                                                                                      // Zeptounits
                                                                                      (
                                                                                        ZeptounitsToYoctounits, NoConversion, ZeptounitsToAttounits, ZeptounitsToFemtounits,
                                                                                        ZeptounitsToPicounits, ZeptounitsToNanounits, ZeptounitsToMicrounits, ZeptounitsToMilliunits,
                                                                                        ZeptounitsToUnits, ZeptounitsToKilounits, ZeptounitsToMegaunits, ZeptounitsToGigaunits,
                                                                                        ZeptounitsToTeraunits, ZeptounitsToPetaunits, ZeptounitsToExaunits, ZeptounitsToZettaunits,
                                                                                        ZeptounitsToYottaunits
                                                                                      ),
                                                                                      // Attounits
                                                                                      (
                                                                                        AttounitsToYoctounits, AttounitsToZeptounits, NoConversion, AttounitsToFemtounits,
                                                                                        AttounitsToPicounits, AttounitsToNanounits, AttounitsToMicrounits, AttounitsToMilliunits,
                                                                                        AttounitsToUnits, AttounitsToKilounits, AttounitsToMegaunits, AttounitsToGigaunits,
                                                                                        AttounitsToTeraunits, AttounitsToPetaunits, AttounitsToExaunits, AttounitsToZettaunits,
                                                                                        AttounitsToYottaunits
                                                                                      ),
                                                                                      // Femtounits
                                                                                      (
                                                                                        FemtounitsToYoctounits, FemtounitsToZeptounits, FemtounitsToAttounits, NoConversion,
                                                                                        FemtounitsToPicounits, FemtounitsToNanounits, FemtounitsToMicrounits, FemtounitsToMilliunits,
                                                                                        FemtounitsToUnits, FemtounitsToKilounits, FemtounitsToMegaunits, FemtounitsToGigaunits,
                                                                                        FemtounitsToTeraunits, FemtounitsToPetaunits, FemtounitsToExaunits, FemtounitsToZettaunits,
                                                                                        FemtounitsToYottaunits
                                                                                      ),
                                                                                      // Picounits
                                                                                      (
                                                                                        PicounitsToYoctounits, PicounitsToZeptounits, PicounitsToAttounits, PicounitsToFemtounits,
                                                                                        NoConversion, PicounitsToNanounits, PicounitsToMicrounits, PicounitsToMilliunits,
                                                                                        PicounitsToUnits, PicounitsToKilounits, PicounitsToMegaunits, PicounitsToGigaunits,
                                                                                        PicounitsToTeraunits, PicounitsToPetaunits, PicounitsToExaunits, PicounitsToZettaunits,
                                                                                        PicounitsToYottaunits
                                                                                      ),
                                                                                      // Nanounits
                                                                                      (
                                                                                        NanounitsToYoctounits, NanounitsToZeptounits, NanounitsToAttounits, NanounitsToFemtounits,
                                                                                        NanounitsToPicounits, NoConversion, NanounitsToMicrounits, NanounitsToMilliunits,
                                                                                        NanounitsToUnits, NanounitsToKilounits, NanounitsToMegaunits, NanounitsToGigaunits,
                                                                                        NanounitsToTeraunits, NanounitsToPetaunits, NanounitsToExaunits, NanounitsToZettaunits,
                                                                                        NanounitsToYottaunits
                                                                                      ),
                                                                                      // Microunits
                                                                                      (
                                                                                        MicrounitsToYoctounits, MicrounitsToZeptounits, MicrounitsToAttounits, MicrounitsToFemtounits,
                                                                                        MicrounitsToPicounits, MicrounitsToNanounits, NoConversion, MicrounitsToMilliunits,
                                                                                        MicrounitsToUnits, MicrounitsToKilounits, MicrounitsToMegaunits, MicrounitsToGigaunits,
                                                                                        MicrounitsToTeraunits, MicrounitsToPetaunits, MicrounitsToExaunits, MicrounitsToZettaunits,
                                                                                        MicrounitsToYottaunits
                                                                                      ),
                                                                                      // Milliunits
                                                                                      (
                                                                                        MilliunitsToYoctounits, MilliunitsToZeptounits, MilliunitsToAttounits, MilliunitsToFemtounits,
                                                                                        MilliunitsToPicounits, MilliunitsToNanounits, MilliunitsToMicrounits, NoConversion,
                                                                                        MilliunitsToUnits, MilliunitsToKilounits, MilliunitsToMegaunits, MilliunitsToGigaunits,
                                                                                        MilliunitsToTeraunits, MilliunitsToPetaunits, MilliunitsToExaunits, MilliunitsToZettaunits,
                                                                                        MilliunitsToYottaunits
                                                                                      ),
                                                                                      // Units
                                                                                      (
                                                                                        UnitsToYoctounits, UnitsToZeptounits, UnitsToAttounits, UnitsToFemtounits, UnitsToPicounits,
                                                                                        UnitsToNanounits, UnitsToMicrounits, UnitsToMilliunits, NoConversion, UnitsToKilounits,
                                                                                        UnitsToMegaunits, UnitsToGigaunits, UnitsToTeraunits, UnitsToPetaunits, UnitsToExaunits,
                                                                                        UnitsToZettaunits, UnitsToYottaunits
                                                                                      ),
                                                                                      // Kilounits
                                                                                      (
                                                                                        KilounitsToYoctounits, KilounitsToZeptounits, KilounitsToAttounits, KilounitsToFemtounits,
                                                                                        KilounitsToPicounits, KilounitsToNanounits, KilounitsToMicrounits, KilounitsToMilliunits,
                                                                                        KilounitsToUnits, NoConversion, KilounitsToMegaunits, KilounitsToGigaunits, KilounitsToTeraunits,
                                                                                        KilounitsToPetaunits, KilounitsToExaunits, KilounitsToZettaunits, KilounitsToYottaunits
                                                                                      ),
                                                                                      // Megaunits
                                                                                      (
                                                                                        MegaunitsToYoctounits, MegaunitsToZeptounits, MegaunitsToAttounits, MegaunitsToFemtounits,
                                                                                        MegaunitsToPicounits, MegaunitsToNanounits, MegaunitsToMicrounits, MegaunitsToMilliunits,
                                                                                        MegaunitsToUnits, MegaunitsToKilounits, NoConversion, MegaunitsToGigaunits, MegaunitsToTeraunits,
                                                                                        MegaunitsToPetaunits, MegaunitsToExaunits, MegaunitsToZettaunits, MegaunitsToYottaunits
                                                                                      ),
                                                                                      // Gigaunits
                                                                                      (
                                                                                        GigaunitsToYoctounits, GigaunitsToZeptounits, GigaunitsToAttounits, GigaunitsToFemtounits,
                                                                                        GigaunitsToPicounits, GigaunitsToNanounits, GigaunitsToMicrounits, GigaunitsToMilliunits,
                                                                                        GigaunitsToUnits, GigaunitsToKilounits, GigaunitsToMegaunits, NoConversion, GigaunitsToTeraunits,
                                                                                        GigaunitsToPetaunits, GigaunitsToExaunits, GigaunitsToZettaunits, GigaunitsToYottaunits
                                                                                      ),
                                                                                      // Teraunits
                                                                                      (
                                                                                        TeraunitsToYoctounits, TeraunitsToZeptounits, TeraunitsToAttounits, TeraunitsToFemtounits,
                                                                                        TeraunitsToPicounits, TeraunitsToNanounits, TeraunitsToMicrounits, TeraunitsToMilliunits,
                                                                                        TeraunitsToUnits, TeraunitsToKilounits, TeraunitsToMegaunits, TeraunitsToGigaunits,
                                                                                        NoConversion, TeraunitsToPetaunits, TeraunitsToExaunits, TeraunitsToZettaunits, TeraunitsToYottaunits
                                                                                      ),
                                                                                      // Petaunits
                                                                                      (
                                                                                        PetaunitsToYoctounits, PetaunitsToZeptounits, PetaunitsToAttounits, PetaunitsToFemtounits,
                                                                                        PetaunitsToPicounits, PetaunitsToNanounits, PetaunitsToMicrounits, PetaunitsToMilliunits,
                                                                                        PetaunitsToUnits, PetaunitsToKilounits, PetaunitsToMegaunits, PetaunitsToGigaunits,
                                                                                        PetaunitsToTeraunits, NoConversion, PetaunitsToExaunits, PetaunitsToZettaunits, PetaunitsToYottaunits
                                                                                      ),
                                                                                      // Exaunits
                                                                                      (
                                                                                        ExaunitsToYoctounits, ExaunitsToZeptounits, ExaunitsToAttounits, ExaunitsToFemtounits,
                                                                                        ExaunitsToPicounits, ExaunitsToNanounits, ExaunitsToMicrounits, ExaunitsToMilliunits,
                                                                                        ExaunitsToUnits, ExaunitsToKilounits, ExaunitsToMegaunits, ExaunitsToGigaunits,
                                                                                        ExaunitsToTeraunits, ExaunitsToPetaunits, NoConversion, ExaunitsToZettaunits, ExaunitsToYottaunits
                                                                                      ),
                                                                                      // Zettaunits
                                                                                      (
                                                                                        ZettaunitsToYoctounits, ZettaunitsToZeptounits, ZettaunitsToAttounits, ZettaunitsToFemtounits,
                                                                                        ZettaunitsToPicounits, ZettaunitsToNanounits, ZettaunitsToMicrounits, ZettaunitsToMilliunits,
                                                                                        ZettaunitsToUnits, ZettaunitsToKilounits, ZettaunitsToMegaunits, ZettaunitsToGigaunits,
                                                                                        ZettaunitsToTeraunits, ZettaunitsToPetaunits, ZettaunitsToExaunits, NoConversion, ZettaunitsToYottaunits
                                                                                      ),
                                                                                      // Yottaunits
                                                                                      (
                                                                                        YottaunitsToYoctounits, YottaunitsToZeptounits, YottaunitsToAttounits, YottaunitsToFemtounits,
                                                                                        YottaunitsToPicounits, YottaunitsToNanounits, YottaunitsToMicrounits, YottaunitsToMilliunits,
                                                                                        YottaunitsToUnits, YottaunitsToKilounits, YottaunitsToMegaunits, YottaunitsToGigaunits,
                                                                                        YottaunitsToTeraunits, YottaunitsToPetaunits, YottaunitsToExaunits, YottaunitsToZettaunits, NoConversion
                                                                                      )
                                                                                    );
begin
  Result := CONVERSION_METHODS[AInputUnit, AOutputUnit](ASourceValue);
end;

// Yoctounits To ?
procedure YoctounitsToBest(const AYoctounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS: Array[0..15] of TLKTimeConversionMethod = (
                                                                  YoctounitsToZeptounits,
                                                                  YoctounitsToAttounits,
                                                                  YoctounitsToFemtounits,
                                                                  YoctounitsToPicounits,
                                                                  YoctounitsToNanounits,
                                                                  YoctounitsToMicrounits,
                                                                  YoctounitsToMilliunits,
                                                                  YoctounitsToUnits,
                                                                  YoctounitsToKilounits,
                                                                  YoctounitsToMegaunits,
                                                                  YoctounitsToGigaunits,
                                                                  YoctounitsToTeraunits,
                                                                  YoctounitsToPetaunits,
                                                                  YoctounitsToExaunits,
                                                                  YoctounitsToZettaunits,
                                                                  YoctounitsToYottaunits
                                                                );
  CONVERSION_UNITS: Array[0..15] of TLKUnit = (
                                                    tuZeptounit, tuAttounit, tuFemtounit,
                                                    tuPicounit, tuNanounit, tuMicrounit,
                                                    tuMilliunit, tuUnit, tuKilounit, tuMegaunit,
                                                    tuGigaunit, tuTeraunit, tuPetaunit, tuExaunit,
                                                    tuZettaunit, tuYottaunit
                                                  );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AYoctounits;
  AOutUnit := tuYoctounit;
  while (I < High(CONVERSION_METHODS)) and
          (
            (((AOutValue < -999) and (AOutValue < 0)) or
            ((AOutValue > 999) and (AOutValue > 0)))
          ) do
  begin
    AOutValue := CONVERSION_METHODS[I](AYoctounits);
    AOutUnit := CONVERSION_UNITS[I];
    Inc(I);
  end;
end;

function YoctounitsToZeptounits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / 1000;
end;

function YoctounitsToAttounits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / 1000000;
end;

function YoctounitsToFemtounits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / 1000000000;
end;

function YoctounitsToPicounits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / 1000000000000;
end;

function YoctounitsToNanounits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / 1000000000000000;
end;

function YoctounitsToMicrounits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / 1000000000000000000;
end;

function YoctounitsToMilliunits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / IntPower(1000, 7);
end;

function YoctounitsToUnits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / IntPower(1000, 8);
end;

function YoctounitsToKilounits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / IntPower(1000, 9);
end;

function YoctounitsToMegaunits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / IntPower(1000, 10);
end;

function YoctounitsToGigaunits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / IntPower(1000, 11);
end;

function YoctounitsToTeraunits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / IntPower(1000, 12);
end;

function YoctounitsToPetaunits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / IntPower(1000, 13);
end;

function YoctounitsToExaunits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / IntPower(1000, 14);
end;

function YoctounitsToZettaunits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / IntPower(1000, 15);
end;

function YoctounitsToYottaunits(const AYoctounits: LKFloat): LKFloat;
begin
  Result := AYoctounits / IntPower(1000, 16);
end;

// Zeptounits To ?
procedure ZeptounitsToBest(const AZeptounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS: Array[0..14] of TLKTimeConversionMethod = (
                                                                  ZeptounitsToAttounits,
                                                                  ZeptounitsToFemtounits,
                                                                  ZeptounitsToPicounits,
                                                                  ZeptounitsToNanounits,
                                                                  ZeptounitsToMicrounits,
                                                                  ZeptounitsToMilliunits,
                                                                  ZeptounitsToUnits,
                                                                  ZeptounitsToKilounits,
                                                                  ZeptounitsToMegaunits,
                                                                  ZeptounitsToGigaunits,
                                                                  ZeptounitsToTeraunits,
                                                                  ZeptounitsToPetaunits,
                                                                  ZeptounitsToExaunits,
                                                                  ZeptounitsToZettaunits,
                                                                  ZeptounitsToYottaunits
                                                                );
  CONVERSION_UNITS: Array[0..14] of TLKUnit = (
                                                    tuAttounit, tuFemtounit,
                                                    tuPicounit, tuNanounit, tuMicrounit,
                                                    tuMilliunit, tuUnit, tuKilounit, tuMegaunit,
                                                    tuGigaunit, tuTeraunit, tuPetaunit, tuExaunit,
                                                    tuZettaunit, tuYottaunit
                                                  );
var
  I: Integer;
begin
  AOutValue := AZeptounits;
  AOutUnit := tuZeptounit;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    AOutValue := ZeptounitsToYoctounits(AZeptounits);
    AOutUnit := tuYoctounit;
  end else
  begin
    I := 0;
    while (I < High(CONVERSION_METHODS)) and
            (
              (((AOutValue < -999) and (AOutValue < 0)) or
              ((AOutValue > 999) and (AOutValue > 0)))
            ) do
    begin
      AOutValue := CONVERSION_METHODS[I](AZeptounits);
      AOutUnit := CONVERSION_UNITS[I];
      Inc(I);
    end;
  end;
end;

function ZeptounitsToYoctounits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits * 1000;
end;

function ZeptounitsToAttounits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits / 1000;
end;

function ZeptounitsToFemtounits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits / 1000000;
end;

function ZeptounitsToPicounits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits / 1000000000;
end;

function ZeptounitsToNanounits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits / 1000000000000;
end;

function ZeptounitsToMicrounits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits / 1000000000000000;
end;

function ZeptounitsToMilliunits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits / 1000000000000000000;
end;

function ZeptounitsToUnits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits / IntPower(1000, 7);
end;

function ZeptounitsToKilounits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits / IntPower(1000, 8);
end;

function ZeptounitsToMegaunits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits / IntPower(1000, 9);
end;

function ZeptounitsToGigaunits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits / IntPower(1000, 10);
end;

function ZeptounitsToTeraunits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits / IntPower(1000, 11);
end;

function ZeptounitsToPetaunits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits / IntPower(1000, 12);
end;

function ZeptounitsToExaunits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits / IntPower(1000, 13);
end;

function ZeptounitsToZettaunits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits / IntPower(1000, 14);
end;

function ZeptounitsToYottaunits(const AZeptounits: LKFloat): LKFloat;
begin
  Result := AZeptounits / IntPower(1000, 15);
end;

// Attounits To ?
procedure AttounitsToBest(const AAttounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..1] of TLKTimeConversionMethod = (
                                                                      AttounitsToZeptounits,
                                                                      AttounitstoYoctounits
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..1] of TLKUnit = (tuZeptounit, tuYoctounit);
  CONVERSION_METHODS_UP: Array[0..13] of TLKTimeConversionMethod = (
                                                                     AttounitsToFemtounits,
                                                                     AttounitsToPicounits,
                                                                     AttounitsToNanounits,
                                                                     AttounitsToMicrounits,
                                                                     AttounitsToMilliunits,
                                                                     AttounitsToUnits,
                                                                     AttounitsToKilounits,
                                                                     AttounitsToMegaunits,
                                                                     AttounitsToGigaunits,
                                                                     AttounitsToTeraunits,
                                                                     AttounitsToPetaunits,
                                                                     AttounitsToExaunits,
                                                                     AttounitsToZettaunits,
                                                                     AttounitsToYottaunits
                                                                   );
  CONVERSION_UNITS_UP: Array[0..13] of TLKUnit = (
                                                       tuFemtounit,
                                                       tuPicounit, tuNanounit, tuMicrounit,
                                                       tuMilliunit, tuUnit, tuKilounit, tuMegaunit,
                                                       tuGigaunit, tuTeraunit, tuPetaunit, tuExaunit,
                                                       tuZettaunit, tuYottaunit
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AAttounits;
  AOutUnit := tuAttounit;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AAttounits);
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
      AOutValue := CONVERSION_METHODS_UP[I](AAttounits);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;
                                                     
function AttounitsToYoctounits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits * 1000000;
end;

function AttounitsToZeptounits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits * 1000;
end;

function AttounitsToFemtounits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits / 1000;
end;

function AttounitsToPicounits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits / 1000000;
end;

function AttounitsToNanounits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits / 1000000000;
end;

function AttounitsToMicrounits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits / 1000000000000;
end;

function AttounitsToMilliunits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits / 1000000000000000;
end;

function AttounitsToUnits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits / 1000000000000000000;
end;

function AttounitsToKilounits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits / IntPower(1000, 7);
end;

function AttounitsToMegaunits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits / IntPower(1000, 8);
end;

function AttounitsToGigaunits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits / IntPower(1000, 9);
end;

function AttounitsToTeraunits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits / IntPower(1000, 10);
end;

function AttounitsToPetaunits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits / IntPower(1000, 11);
end;

function AttounitsToExaunits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits / IntPower(1000, 12);
end;

function AttounitsToZettaunits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits / IntPower(1000, 13);
end;

function AttounitsToYottaunits(const AAttounits: LKFloat): LKFloat;
begin
  Result := AAttounits / IntPower(1000, 14);
end;

// Femtounits To ?
procedure FemtounitsToBest(const AFemtounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..2] of TLKTimeConversionMethod = (
                                                                      FemtounitsToAttounits,
                                                                      FemtounitsToZeptounits,
                                                                      FemtounitsToYoctounits
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..2] of TLKUnit = (tuAttounit, tuZeptounit, tuYoctounit);
  CONVERSION_METHODS_UP: Array[0..12] of TLKTimeConversionMethod = (
                                                                     FemtounitsToPicounits,
                                                                     FemtounitsToNanounits,
                                                                     FemtounitsToMicrounits,
                                                                     FemtounitsToMilliunits,
                                                                     FemtounitsToUnits,
                                                                     FemtounitsToKilounits,
                                                                     FemtounitsToMegaunits,
                                                                     FemtounitsToGigaunits,
                                                                     FemtounitsToTeraunits,
                                                                     FemtounitsToPetaunits,
                                                                     FemtounitsToExaunits,
                                                                     FemtounitsToZettaunits,
                                                                     FemtounitsToYottaunits
                                                                   );
  CONVERSION_UNITS_UP: Array[0..12] of TLKUnit = (
                                                       tuPicounit, tuNanounit, tuMicrounit,
                                                       tuMilliunit, tuUnit, tuKilounit, tuMegaunit,
                                                       tuGigaunit, tuTeraunit, tuPetaunit, tuExaunit,
                                                       tuZettaunit, tuYottaunit
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AFemtounits;
  AOutUnit := tuFemtounit;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AFemtounits);
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
      AOutValue := CONVERSION_METHODS_UP[I](AFemtounits);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function FemtounitsToYoctounits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits * 1000000000;
end;

function FemtounitsToZeptounits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits * 1000000;
end;

function FemtounitsToAttounits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits * 1000;
end;

function FemtounitsToPicounits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits / 1000;
end;

function FemtounitsToNanounits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits / 1000000;
end;

function FemtounitsToMicrounits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits / 1000000000;
end;

function FemtounitsToMilliunits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits / 1000000000000;
end;

function FemtounitsToUnits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits / 1000000000000000;
end;

function FemtounitsToKilounits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits / 1000000000000000000;
end;

function FemtounitsToMegaunits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits / IntPower(1000, 7);
end;

function FemtounitsToGigaunits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits / IntPower(1000, 8);
end;

function FemtounitsToTeraunits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits / IntPower(1000, 9);
end;

function FemtounitsToPetaunits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits / IntPower(1000, 10);
end;

function FemtounitsToExaunits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits / IntPower(1000, 11);
end;

function FemtounitsToZettaunits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits / IntPower(1000, 12);
end;

function FemtounitsToYottaunits(const AFemtounits: LKFloat): LKFloat;
begin
  Result := AFemtounits / IntPower(1000, 13);
end;

// Picounits To ?
procedure PicounitsToBest(const APicounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..3] of TLKTimeConversionMethod = (
                                                                      PicounitsToFemtounits,
                                                                      PicounitsToAttounits,
                                                                      PicounitsToZeptounits,
                                                                      PicounitsToYoctounits
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..3] of TLKUnit = (tuFemtounit, tuAttounit, tuZeptounit, tuYoctounit);
  CONVERSION_METHODS_UP: Array[0..11] of TLKTimeConversionMethod = (
                                                                     PicounitsToNanounits,
                                                                     PicounitsToMicrounits,
                                                                     PicounitsToMilliunits,
                                                                     PicounitsToUnits,
                                                                     PicounitsToKilounits,
                                                                     PicounitsToMegaunits,
                                                                     PicounitsToGigaunits,
                                                                     PicounitsToTeraunits,
                                                                     PicounitsToPetaunits,
                                                                     PicounitsToExaunits,
                                                                     PicounitsToZettaunits,
                                                                     PicounitsToYottaunits
                                                                   );
  CONVERSION_UNITS_UP: Array[0..11] of TLKUnit = (
                                                       tuNanounit, tuMicrounit, tuMilliunit,
                                                       tuUnit, tuKilounit, tuMegaunit, tuGigaunit,
                                                       tuTeraunit, tuPetaunit, tuExaunit,
                                                       tuZettaunit, tuYottaunit
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := APicounits;
  AOutUnit := tuPicounit;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](APicounits);
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
      AOutValue := CONVERSION_METHODS_UP[I](APicounits);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function PicounitsToYoctounits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits * 1000000000000;
end;

function PicounitsToZeptounits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits * 1000000000;
end;

function PicounitsToAttounits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits * 1000000;
end;

function PicounitsToFemtounits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits * 1000;
end;

function PicounitsToNanounits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits / 1000;
end;

function PicounitsToMicrounits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits / 1000000;
end;

function PicounitsToMilliunits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits / 1000000000;
end;

function PicounitsToUnits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits / 1000000000000;
end;

function PicounitsToKilounits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits / 1000000000000000;
end;

function PicounitsToMegaunits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits / 1000000000000000000;
end;

function PicounitsToGigaunits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits / IntPower(1000, 7);
end;

function PicounitsToTeraunits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits / IntPower(1000, 8);
end;

function PicounitsToPetaunits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits / IntPower(1000, 9);
end;

function PicounitsToExaunits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits / IntPower(1000, 10);
end;

function PicounitsToZettaunits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits / IntPower(1000, 11);
end;

function PicounitsToYottaunits(const APicounits: LKFloat): LKFloat;
begin
  Result := APicounits / IntPower(1000, 12);
end;

// Nanounits To ?
procedure NanounitsToBest(const ANanounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..4] of TLKTimeConversionMethod = (
                                                                      NanounitsToPicounits,
                                                                      NanounitsToFemtounits,
                                                                      NanounitsToAttounits,
                                                                      NanounitsToZeptounits,
                                                                      NanounitsToYoctounits
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..4] of TLKUnit = (tuPicounit, tuFemtounit, tuAttounit,
                                                       tuZeptounit, tuYoctounit);
  CONVERSION_METHODS_UP: Array[0..10] of TLKTimeConversionMethod = (
                                                                     NanounitsToMicrounits,
                                                                     NanounitsToMilliunits,
                                                                     NanounitsToUnits,
                                                                     NanounitsToKilounits,
                                                                     NanounitsToMegaunits,
                                                                     NanounitsToGigaunits,
                                                                     NanounitsToTeraunits,
                                                                     NanounitsToPetaunits,
                                                                     NanounitsToExaunits,
                                                                     NanounitsToZettaunits,
                                                                     NanounitsToYottaunits
                                                                   );
  CONVERSION_UNITS_UP: Array[0..10] of TLKUnit = (
                                                       tuMicrounit, tuMilliunit,
                                                       tuUnit, tuKilounit, tuMegaunit, tuGigaunit,
                                                       tuTeraunit, tuPetaunit, tuExaunit,
                                                       tuZettaunit, tuYottaunit
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := ANanounits;
  AOutUnit := tuNanounit;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](ANanounits);
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
      AOutValue := CONVERSION_METHODS_UP[I](ANanounits);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function NanounitsToYoctounits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits * 1000000000000000;
end;

function NanounitsToZeptounits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits * 1000000000000;
end;

function NanounitsToAttounits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits * 1000000000;
end;

function NanounitsToFemtounits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits * 1000000;
end;

function NanounitsToPicounits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits * 1000;
end;

function NanounitsToMicrounits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits / 1000;
end;

function NanounitsToMilliunits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits / 1000000;
end;

function NanounitsToUnits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits / 1000000000;
end;

function NanounitsToKilounits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits / 1000000000000;
end;

function NanounitsToMegaunits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits / 1000000000000000;
end;

function NanounitsToGigaunits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits / 1000000000000000000;
end;

function NanounitsToTeraunits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits / IntPower(1000, 7);
end;

function NanounitsToPetaunits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits / IntPower(1000, 8);
end;

function NanounitsToExaunits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits / IntPower(1000, 9);
end;

function NanounitsToZettaunits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits / IntPower(1000, 10);
end;

function NanounitsToYottaunits(const ANanounits: LKFloat): LKFloat;
begin
  Result := ANanounits / IntPower(1000, 11);
end;

// Microunits To ?
procedure MicrounitsToBest(const AMicrounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..5] of TLKTimeConversionMethod = (
                                                                      MicrounitsToNanounits,
                                                                      MicrounitsToPicounits,
                                                                      MicrounitsToFemtounits,
                                                                      MicrounitsToAttounits,
                                                                      MicrounitsToZeptounits,
                                                                      MicrounitsToYoctounits
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..5] of TLKUnit = (tuNanounit, tuPicounit, tuFemtounit,
                                                       tuAttounit, tuZeptounit, tuYoctounit);
  CONVERSION_METHODS_UP: Array[0..9] of TLKTimeConversionMethod = (
                                                                     MicrounitsToMilliunits,
                                                                     MicrounitsToUnits,
                                                                     MicrounitsToKilounits,
                                                                     MicrounitsToMegaunits,
                                                                     MicrounitsToGigaunits,
                                                                     MicrounitsToTeraunits,
                                                                     MicrounitsToPetaunits,
                                                                     MicrounitsToExaunits,
                                                                     MicrounitsToZettaunits,
                                                                     MicrounitsToYottaunits
                                                                   );
  CONVERSION_UNITS_UP: Array[0..9] of TLKUnit = (
                                                       tuMilliunit,
                                                       tuUnit, tuKilounit, tuMegaunit, tuGigaunit,
                                                       tuTeraunit, tuPetaunit, tuExaunit,
                                                       tuZettaunit, tuYottaunit
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AMicrounits;
  AOutUnit := tuMicrounit;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AMicrounits);
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
      AOutValue := CONVERSION_METHODS_UP[I](AMicrounits);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function MicrounitsToYoctounits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits * 1000000000000000000;
end;

function MicrounitsToZeptounits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits * 1000000000000000;
end;

function MicrounitsToAttounits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits * 1000000000000;
end;

function MicrounitsToFemtounits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits * 1000000000;
end;

function MicrounitsToPicounits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits * 1000000;
end;

function MicrounitsToNanounits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits * 1000;
end;

function MicrounitsToMilliunits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits / 1000;
end;

function MicrounitsToUnits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits / 1000000;
end;

function MicrounitsToKilounits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits / 1000000000;
end;

function MicrounitsToMegaunits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits / 1000000000000;
end;

function MicrounitsToGigaunits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits / 1000000000000000;
end;

function MicrounitsToTeraunits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits / 1000000000000000000;
end;

function MicrounitsToPetaunits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits / IntPower(1000, 7);
end;

function MicrounitsToExaunits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits / IntPower(1000, 8);
end;

function MicrounitsToZettaunits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits / IntPower(1000, 9);
end;

function MicrounitsToYottaunits(const AMicrounits: LKFloat): LKFloat;
begin
  Result := AMicrounits / IntPower(1000, 10);
end;

// Milliunits To ?
procedure MilliunitsToBest(const AMilliunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..6] of TLKTimeConversionMethod = (
                                                                      MilliunitsToMicrounits,
                                                                      MilliunitsToNanounits,
                                                                      MilliunitsToPicounits,
                                                                      MilliunitsToFemtounits,
                                                                      MilliunitsToAttounits,
                                                                      MilliunitsToZeptounits,
                                                                      MilliunitsToYoctounits
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..6] of TLKUnit = (tuMicrounit, tuNanounit, tuPicounit,
                                                       tuFemtounit, tuAttounit, tuZeptounit,
                                                       tuYoctounit);
  CONVERSION_METHODS_UP: Array[0..8] of TLKTimeConversionMethod = (
                                                                     MilliunitsToUnits,
                                                                     MilliunitsToKilounits,
                                                                     MilliunitsToMegaunits,
                                                                     MilliunitsToGigaunits,
                                                                     MilliunitsToTeraunits,
                                                                     MilliunitsToPetaunits,
                                                                     MilliunitsToExaunits,
                                                                     MilliunitsToZettaunits,
                                                                     MilliunitsToYottaunits
                                                                   );
  CONVERSION_UNITS_UP: Array[0..8] of TLKUnit = (
                                                       tuUnit, tuKilounit, tuMegaunit, tuGigaunit,
                                                       tuTeraunit, tuPetaunit, tuExaunit,
                                                       tuZettaunit, tuYottaunit
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AMilliunits;
  AOutUnit := tuMilliunit;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AMilliunits);
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
      AOutValue := CONVERSION_METHODS_UP[I](AMilliunits);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function MilliunitsToYoctounits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits * IntPower(1000, 7);
end;

function MilliunitsToZeptounits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits * 1000000000000000000;
end;

function MilliunitsToAttounits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits * 1000000000000000;
end;

function MilliunitsToFemtounits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits * 1000000000000;
end;

function MilliunitsToPicounits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits * 1000000000;
end;

function MilliunitsToNanounits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits * 1000000;
end;

function MilliunitsToMicrounits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits * 1000;
end;

function MilliunitsToUnits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits / 1000;
end;

function MilliunitsToKilounits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits / 1000000;
end;

function MilliunitsToMegaunits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits / 1000000000;
end;

function MilliunitsToGigaunits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits / 1000000000000;
end;

function MilliunitsToTeraunits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits / 1000000000000000;
end;

function MilliunitsToPetaunits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits / 1000000000000000000;
end;

function MilliunitsToExaunits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits / IntPower(1000, 7);
end;

function MilliunitsToZettaunits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits / IntPower(1000, 8);
end;

function MilliunitsToYottaunits(const AMilliunits: LKFloat): LKFloat;
begin
  Result := AMilliunits / IntPower(1000, 9);
end;

// Units To ?
procedure UnitsToBest(const AUnits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..7] of TLKTimeConversionMethod = (
                                                                      UnitsToMilliunits,
                                                                      UnitsToMicrounits,
                                                                      UnitsToNanounits,
                                                                      UnitsToPicounits,
                                                                      UnitsToFemtounits,
                                                                      UnitsToAttounits,
                                                                      UnitsToZeptounits,
                                                                      UnitsToYoctounits
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..7] of TLKUnit = (tuMilliunit, tuMicrounit, tuNanounit, tuPicounit,
                                                       tuFemtounit, tuAttounit, tuZeptounit,
                                                       tuYoctounit);
  CONVERSION_METHODS_UP: Array[0..7] of TLKTimeConversionMethod = (
                                                                     UnitsToKilounits,
                                                                     UnitsToMegaunits,
                                                                     UnitsToGigaunits,
                                                                     UnitsToTeraunits,
                                                                     UnitsToPetaunits,
                                                                     UnitsToExaunits,
                                                                     UnitsToZettaunits,
                                                                     UnitsToYottaunits
                                                                   );
  CONVERSION_UNITS_UP: Array[0..7] of TLKUnit = (
                                                       tuKilounit, tuMegaunit, tuGigaunit,
                                                       tuTeraunit, tuPetaunit, tuExaunit,
                                                       tuZettaunit, tuYottaunit
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AUnits;
  AOutUnit := tuUnit;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AUnits);
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
      AOutValue := CONVERSION_METHODS_UP[I](AUnits);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function UnitsToYoctounits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits * IntPower(1000, 8);
end;

function UnitsToZeptounits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits * IntPower(1000, 7);
end;

function UnitsToAttounits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits * 1000000000000000000;
end;

function UnitsToFemtounits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits * 1000000000000000;
end;

function UnitsToPicounits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits * 1000000000000;
end;

function UnitsToNanounits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits * 1000000000;
end;

function UnitsToMicrounits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits * 1000000;
end;

function UnitsToMilliunits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits * 1000;
end;

function UnitsToKilounits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits / 1000;
end;

function UnitsToMegaunits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits / 1000000;
end;

function UnitsToGigaunits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits / 1000000000;
end;

function UnitsToTeraunits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits / 1000000000000;
end;

function UnitsToPetaunits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits / 1000000000000000;
end;

function UnitsToExaunits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits / 1000000000000000000;
end;

function UnitsToZettaunits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits / IntPower(1000, 7);
end;

function UnitsToYottaunits(const AUnits: LKFloat): LKFloat;
begin
  Result := AUnits / IntPower(1000, 8);
end;

// Kilounits To ?
procedure KilounitsToBest(const AKilounits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..8] of TLKTimeConversionMethod = (
                                                                      KilounitsToUnits,
                                                                      KiloUnitsToMilliunits,
                                                                      KilounitsToMicrounits,
                                                                      KilounitsToNanounits,
                                                                      KilounitsToPicounits,
                                                                      KilounitsToFemtounits,
                                                                      KilounitsToAttounits,
                                                                      KilounitsToZeptounits,
                                                                      KilounitsToYoctounits
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..8] of TLKUnit = (tuUnit, tuMilliunit, tuMicrounit,
                                                       tuNanounit, tuPicounit, tuFemtounit,
                                                       tuAttounit, tuZeptounit, tuYoctounit);
  CONVERSION_METHODS_UP: Array[0..6] of TLKTimeConversionMethod = (
                                                                     KilounitsToMegaunits,
                                                                     KilounitsToGigaunits,
                                                                     KilounitsToTeraunits,
                                                                     KilounitsToPetaunits,
                                                                     KilounitsToExaunits,
                                                                     KilounitsToZettaunits,
                                                                     KilounitsToYottaunits
                                                                   );
  CONVERSION_UNITS_UP: Array[0..6] of TLKUnit = (
                                                       tuMegaunit, tuGigaunit,
                                                       tuTeraunit, tuPetaunit, tuExaunit,
                                                       tuZettaunit, tuYottaunit
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AKilounits;
  AOutUnit := tuKilounit;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AKilounits);
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
      AOutValue := CONVERSION_METHODS_UP[I](AKilounits);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function KilounitsToYoctounits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits * IntPower(1000, 9);
end;

function KilounitsToZeptounits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits * IntPower(1000, 8);
end;

function KilounitsToAttounits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits * IntPower(1000, 7);
end;

function KilounitsToFemtounits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits * 1000000000000000000;
end;

function KilounitsToPicounits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits * 1000000000000000;
end;

function KilounitsToNanounits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits * 1000000000000;
end;

function KilounitsToMicrounits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits * 1000000000;
end;

function KilounitsToMilliunits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits * 1000000;
end;

function KilounitsToUnits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits * 1000;
end;

function KilounitsToMegaunits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits / 1000;
end;

function KilounitsToGigaunits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits / 1000000;
end;

function KilounitsToTeraunits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits / 1000000000;
end;

function KilounitsToPetaunits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits / 1000000000000;
end;

function KilounitsToExaunits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits / 1000000000000000;
end;

function KilounitsToZettaunits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits / 1000000000000000000;
end;

function KilounitsToYottaunits(const AKilounits: LKFloat): LKFloat;
begin
  Result := AKilounits / IntPower(1000, 7);
end;

// Megaunits To ?
procedure MegaunitsToBest(const AMegaunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..9] of TLKTimeConversionMethod = (
                                                                      MegaunitsToKilounits,
                                                                      MegaunitsToUnits,
                                                                      MegaunitsToMilliunits,
                                                                      MegaunitsToMicrounits,
                                                                      MegaunitsToNanounits,
                                                                      MegaunitsToPicounits,
                                                                      MegaunitsToFemtounits,
                                                                      MegaunitsToAttounits,
                                                                      MegaunitsToZeptounits,
                                                                      MegaunitsToYoctounits
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..9] of TLKUnit = (tuKilounit, tuMilliunit, tuUnit,
                                                       tuMicrounit, tuNanounit, tuPicounit,
                                                       tuFemtounit, tuAttounit, tuZeptounit, tuYoctounit);
  CONVERSION_METHODS_UP: Array[0..5] of TLKTimeConversionMethod = (
                                                                     MegaunitsToGigaunits,
                                                                     MegaunitsToTeraunits,
                                                                     MegaunitsToPetaunits,
                                                                     MegaunitsToExaunits,
                                                                     MegaunitsToZettaunits,
                                                                     MegaunitsToYottaunits
                                                                   );
  CONVERSION_UNITS_UP: Array[0..5] of TLKUnit = (
                                                       tuGigaunit, tuTeraunit, tuPetaunit,
                                                       tuExaunit, tuZettaunit, tuYottaunit
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AMegaunits;
  AOutUnit := tuMegaunit;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AMegaunits);
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
      AOutValue := CONVERSION_METHODS_UP[I](AMegaunits);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function MegaunitsToYoctounits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits * IntPower(1000, 10);
end;

function MegaunitsToZeptounits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits * IntPower(1000, 9);
end;

function MegaunitsToAttounits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits * IntPower(1000, 8);
end;

function MegaunitsToFemtounits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits * IntPower(1000, 7);
end;

function MegaunitsToPicounits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits * 1000000000000000000;
end;

function MegaunitsToNanounits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits * 1000000000000000;
end;

function MegaunitsToMicrounits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits * 1000000000000;
end;

function MegaunitsToMilliunits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits * 1000000000;
end;

function MegaunitsToUnits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits * 1000000;
end;

function MegaunitsToKilounits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits * 1000;
end;

function MegaunitsToGigaunits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits / 1000;
end;

function MegaunitsToTeraunits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits / 1000000;
end;

function MegaunitsToPetaunits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits / 1000000000;
end;

function MegaunitsToExaunits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits / 1000000000000;
end;

function MegaunitsToZettaunits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits / 1000000000000000;
end;

function MegaunitsToYottaunits(const AMegaunits: LKFloat): LKFloat;
begin
  Result := AMegaunits / 1000000000000000000;
end;

// Gigaunits To ?
procedure GigaunitsToBest(const AGigaunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..10] of TLKTimeConversionMethod = (
                                                                      GigaunitsToMegaunits,
                                                                      GigaunitsToKilounits,
                                                                      GigaunitsToUnits,
                                                                      GigaunitsToMilliunits,
                                                                      GigaunitsToMicrounits,
                                                                      GigaunitsToNanounits,
                                                                      GigaunitsToPicounits,
                                                                      GigaunitsToFemtounits,
                                                                      GigaunitsToAttounits,
                                                                      GigaunitsToZeptounits,
                                                                      GigaunitsToYoctounits
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..10] of TLKUnit = (tuMegaunit, tuKilounit, tuMilliunit, tuUnit,
                                                       tuMicrounit, tuNanounit, tuPicounit,
                                                       tuFemtounit, tuAttounit, tuZeptounit, tuYoctounit);
  CONVERSION_METHODS_UP: Array[0..4] of TLKTimeConversionMethod = (
                                                                     GigaunitsToTeraunits,
                                                                     GigaunitsToPetaunits,
                                                                     GigaunitsToExaunits,
                                                                     GigaunitsToZettaunits,
                                                                     GigaunitsToYottaunits
                                                                   );
  CONVERSION_UNITS_UP: Array[0..4] of TLKUnit = (
                                                       tuTeraunit, tuPetaunit,
                                                       tuExaunit, tuZettaunit, tuYottaunit
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AGigaunits;
  AOutUnit := tuGigaunit;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AGigaunits);
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
      AOutValue := CONVERSION_METHODS_UP[I](AGigaunits);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function GigaunitsToYoctounits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits * IntPower(1000, 11);
end;

function GigaunitsToZeptounits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits * IntPower(1000, 10);
end;

function GigaunitsToAttounits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits * IntPower(1000, 9);
end;

function GigaunitsToFemtounits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits * IntPower(1000, 8);
end;

function GigaunitsToPicounits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits * IntPower(1000, 7);
end;

function GigaunitsToNanounits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits * 1000000000000000000;
end;

function GigaunitsToMicrounits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits * 1000000000000000;
end;

function GigaunitsToMilliunits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits * 1000000000000;
end;

function GigaunitsToUnits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits * 1000000000;
end;

function GigaunitsToKilounits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits * 1000000;
end;

function GigaunitsToMegaunits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits * 1000;
end;

function GigaunitsToTeraunits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits / 1000;
end;

function GigaunitsToPetaunits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits / 1000000;
end;

function GigaunitsToExaunits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits / 1000000000;
end;

function GigaunitsToZettaunits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits / 1000000000000;
end;

function GigaunitsToYottaunits(const AGigaunits: LKFloat): LKFloat;
begin
  Result := AGigaunits / 1000000000000000;
end;

// Teraunits To ?
procedure TeraunitsToBest(const ATeraunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..11] of TLKTimeConversionMethod = (
                                                                      TeraunitsToGigaunits,
                                                                      TeraunitsToMegaunits,
                                                                      TeraunitsToKilounits,
                                                                      TeraunitsToUnits,
                                                                      TeraunitsToMilliunits,
                                                                      TeraunitsToMicrounits,
                                                                      TeraunitsToNanounits,
                                                                      TeraunitsToPicounits,
                                                                      TeraunitsToFemtounits,
                                                                      TeraunitsToAttounits,
                                                                      TeraunitsToZeptounits,
                                                                      TeraunitsToYoctounits
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..11] of TLKUnit = (tuGigaunit, tuMegaunit, tuKilounit, tuMilliunit, tuUnit,
                                                       tuMicrounit, tuNanounit, tuPicounit,
                                                       tuFemtounit, tuAttounit, tuZeptounit, tuYoctounit);
  CONVERSION_METHODS_UP: Array[0..3] of TLKTimeConversionMethod = (
                                                                     TeraunitsToPetaunits,
                                                                     TeraunitsToExaunits,
                                                                     TeraunitsToZettaunits,
                                                                     TeraunitsToYottaunits
                                                                   );
  CONVERSION_UNITS_UP: Array[0..3] of TLKUnit = (
                                                       tuPetaunit, tuExaunit, tuZettaunit, tuYottaunit
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := ATeraunits;
  AOutUnit := tuTeraunit;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](ATeraunits);
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
      AOutValue := CONVERSION_METHODS_UP[I](ATeraunits);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function TeraunitsToYoctounits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits * IntPower(1000, 12);
end;

function TeraunitsToZeptounits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits * IntPower(1000, 11);
end;

function TeraunitsToAttounits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits * IntPower(1000, 10);
end;

function TeraunitsToFemtounits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits * IntPower(1000, 9);
end;

function TeraunitsToPicounits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits * IntPower(1000, 8);
end;

function TeraunitsToNanounits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits * IntPower(1000, 7);
end;

function TeraunitsToMicrounits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits * 1000000000000000000;
end;

function TeraunitsToMilliunits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits * 1000000000000000;
end;

function TeraunitsToUnits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits * 1000000000000;
end;

function TeraunitsToKilounits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits * 1000000000;
end;

function TeraunitsToMegaunits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits * 1000000;
end;

function TeraunitsToGigaunits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits * 1000;
end;

function TeraunitsToPetaunits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits / 1000;
end;

function TeraunitsToExaunits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits / 1000000;
end;

function TeraunitsToZettaunits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits / 1000000000;
end;

function TeraunitsToYottaunits(const ATeraunits: LKFloat): LKFloat;
begin
  Result := ATeraunits / 1000000000000;
end;

// Petaunits To ?
procedure PetaunitsToBest(const APetaunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..12] of TLKTimeConversionMethod = (
                                                                      PetaunitsToTeraunits,
                                                                      PetaunitsToGigaunits,
                                                                      PetaunitsToMegaunits,
                                                                      PetaunitsToKilounits,
                                                                      PetaunitsToUnits,
                                                                      PetaunitsToMilliunits,
                                                                      PetaunitsToMicrounits,
                                                                      PetaunitsToNanounits,
                                                                      PetaunitsToPicounits,
                                                                      PetaunitsToFemtounits,
                                                                      PetaunitsToAttounits,
                                                                      PetaunitsToZeptounits,
                                                                      PetaunitsToYoctounits
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..12] of TLKUnit = (tuTeraunit, tuGigaunit, tuMegaunit, tuKilounit, tuMilliunit, tuUnit,
                                                       tuMicrounit, tuNanounit, tuPicounit,
                                                       tuFemtounit, tuAttounit, tuZeptounit, tuYoctounit);
  CONVERSION_METHODS_UP: Array[0..2] of TLKTimeConversionMethod = (
                                                                     PetaunitsToExaunits,
                                                                     PetaunitsToZettaunits,
                                                                     PetaunitsToYottaunits
                                                                   );
  CONVERSION_UNITS_UP: Array[0..2] of TLKUnit = (
                                                       tuExaunit, tuZettaunit, tuYottaunit
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := APetaunits;
  AOutUnit := tuPetaunit;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](APetaunits);
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
      AOutValue := CONVERSION_METHODS_UP[I](APetaunits);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function PetaunitsToYoctounits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits * IntPower(1000, 13);
end;

function PetaunitsToZeptounits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits * IntPower(1000, 12);
end;

function PetaunitsToAttounits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits * IntPower(1000, 11);
end;

function PetaunitsToFemtounits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits * IntPower(1000, 10);
end;

function PetaunitsToPicounits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits * IntPower(1000, 9);
end;

function PetaunitsToNanounits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits * IntPower(1000, 8);
end;

function PetaunitsToMicrounits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits * IntPower(1000, 7);
end;

function PetaunitsToMilliunits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits * 1000000000000000000;
end;

function PetaunitsToUnits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits * 1000000000000000;
end;

function PetaunitsToKilounits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits * 1000000000000;
end;

function PetaunitsToMegaunits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits * 1000000000;
end;

function PetaunitsToGigaunits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits * 1000000;
end;

function PetaunitsToTeraunits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits * 1000;
end;

function PetaunitsToExaunits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits / 1000;
end;

function PetaunitsToZettaunits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits / 1000000;
end;

function PetaunitsToYottaunits(const APetaunits: LKFloat): LKFloat;
begin
  Result := APetaunits / 1000000000;
end;

// Exaunits To ?
procedure ExaunitsToBest(const AExaunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..13] of TLKTimeConversionMethod = (
                                                                      ExaunitsToPetaunits,
                                                                      ExaunitsToTeraunits,
                                                                      ExaunitsToGigaunits,
                                                                      ExaunitsToMegaunits,
                                                                      ExaunitsToKilounits,
                                                                      ExaunitsToUnits,
                                                                      ExaunitsToMilliunits,
                                                                      ExaunitsToMicrounits,
                                                                      ExaunitsToNanounits,
                                                                      ExaunitsToPicounits,
                                                                      ExaunitsToFemtounits,
                                                                      ExaunitsToAttounits,
                                                                      ExaunitsToZeptounits,
                                                                      ExaunitsToYoctounits
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..13] of TLKUnit = (tuPetaunit, tuTeraunit, tuGigaunit, tuMegaunit,
                                                       tuKilounit, tuMilliunit, tuUnit, tuMicrounit,
                                                       tuNanounit, tuPicounit, tuFemtounit, tuAttounit,
                                                       tuZeptounit, tuYoctounit);
  CONVERSION_METHODS_UP: Array[0..1] of TLKTimeConversionMethod = (
                                                                     ExaunitsToZettaunits,
                                                                     ExaunitsToYottaunits
                                                                   );
  CONVERSION_UNITS_UP: Array[0..1] of TLKUnit = (
                                                       tuZettaunit, tuYottaunit
                                                     );
var
  I: Integer;
begin
  I := 0;
  AOutValue := AExaunits;
  AOutUnit := tuExaunit;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AExaunits);
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
      AOutValue := CONVERSION_METHODS_UP[I](AExaunits);
      AOutUnit := CONVERSION_UNITS_UP[I];
      Inc(I);
    end;
  end;
end;

function ExaunitsToYoctounits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits * IntPower(1000, 14);
end;

function ExaunitsToZeptounits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits * IntPower(1000, 13);
end;

function ExaunitsToAttounits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits * IntPower(1000, 12);
end;

function ExaunitsToFemtounits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits * IntPower(1000, 11);
end;

function ExaunitsToPicounits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits * IntPower(1000, 10);
end;

function ExaunitsToNanounits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits * IntPower(1000, 9);
end;

function ExaunitsToMicrounits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits * IntPower(1000, 8);
end;

function ExaunitsToMilliunits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits * IntPower(1000, 7);
end;

function ExaunitsToUnits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits * 1000000000000000000;
end;

function ExaunitsToKilounits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits * 1000000000000000;
end;

function ExaunitsToMegaunits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits * 1000000000000;
end;

function ExaunitsToGigaunits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits * 1000000000;
end;

function ExaunitsToTeraunits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits * 1000000;
end;

function ExaunitsToPetaunits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits * 1000;
end;

function ExaunitsToZettaunits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits / 1000;
end;

function ExaunitsToYottaunits(const AExaunits: LKFloat): LKFloat;
begin
  Result := AExaunits / 1000000;
end;

// Zettaunits To ?
procedure ZettaunitsToBest(const AZettaunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..14] of TLKTimeConversionMethod = (
                                                                      ZettaunitsToExaunits,
                                                                      ZettaunitsToPetaunits,
                                                                      ZettaunitsToTeraunits,
                                                                      ZettaunitsToGigaunits,
                                                                      ZettaunitsToMegaunits,
                                                                      ZettaunitsToKilounits,
                                                                      ZettaunitsToUnits,
                                                                      ZettaunitsToMilliunits,
                                                                      ZettaunitsToMicrounits,
                                                                      ZettaunitsToNanounits,
                                                                      ZettaunitsToPicounits,
                                                                      ZettaunitsToFemtounits,
                                                                      ZettaunitsToAttounits,
                                                                      ZettaunitsToZeptounits,
                                                                      ZettaunitsToYoctounits
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..14] of TLKUnit = (tuExaunit, tuPetaunit, tuTeraunit, tuGigaunit, tuMegaunit,
                                                       tuKilounit, tuMilliunit, tuUnit, tuMicrounit,
                                                       tuNanounit, tuPicounit, tuFemtounit, tuAttounit,
                                                       tuZeptounit, tuYoctounit);
var
  I: Integer;
begin
  I := 0;
  AOutValue := AZettaunits;
  AOutUnit := tuZettaunit;
  if ((AOutValue < 1) and (AOutValue > 0)) or ((AOutValue > -1) and (AOutValue < 0)) then
  begin
    while (I < High(CONVERSION_METHODS_DOWN)) and
            (
              ((AOutValue < 1) and (AOutValue > 0)) or
              ((AOutValue > -1) and (AOutValue < 0))
            ) do
    begin
      AOutValue := CONVERSION_METHODS_DOWN[I](AZettaunits);
      AOutUnit := CONVERSION_UNITS_DOWN[I];
      Inc(I);
    end;
  end else
  begin
    AOutValue := ZettaunitsToYottaunits(AZettaunits);
    AOutUnit := tuYottaunit;
  end;
end;

function ZettaunitsToYoctounits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits * IntPower(1000, 15);
end;

function ZettaunitsToZeptounits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits * IntPower(1000, 14);
end;

function ZettaunitsToAttounits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits * IntPower(1000, 13);
end;

function ZettaunitsToFemtounits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits * IntPower(1000, 12);
end;

function ZettaunitsToPicounits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits * IntPower(1000, 11);
end;

function ZettaunitsToNanounits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits * IntPower(1000, 10);
end;

function ZettaunitsToMicrounits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits * IntPower(1000, 9);
end;

function ZettaunitsToMilliunits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits * IntPower(1000, 8);
end;

function ZettaunitsToUnits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits * IntPower(1000, 7);
end;

function ZettaunitsToKilounits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits * 1000000000000000000;
end;

function ZettaunitsToMegaunits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits * 1000000000000000;
end;

function ZettaunitsToGigaunits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits * 1000000000000;
end;

function ZettaunitsToTeraunits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits * 1000000000;
end;

function ZettaunitsToPetaunits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits * 1000000;
end;

function ZettaunitsToExaunits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits * 1000;
end;

function ZettaunitsToYottaunits(const AZettaunits: LKFloat): LKFloat;
begin
  Result := AZettaunits / 1000;
end;

// Yottaunits To ?
procedure YottaunitsToBest(const AYottaunits: LKFloat; out AOutValue: LKFloat; out AOutUnit: TLKUnit); overload;
const
  CONVERSION_METHODS_DOWN: Array[0..15] of TLKTimeConversionMethod = (
                                                                      YottaunitsToZettaunits,
                                                                      YottaunitsToExaunits,
                                                                      YottaunitsToPetaunits,
                                                                      YottaunitsToTeraunits,
                                                                      YottaunitsToGigaunits,
                                                                      YottaunitsToMegaunits,
                                                                      YottaunitsToKilounits,
                                                                      YottaunitsToUnits,
                                                                      YottaunitsToMilliunits,
                                                                      YottaunitsToMicrounits,
                                                                      YottaunitsToNanounits,
                                                                      YottaunitsToPicounits,
                                                                      YottaunitsToFemtounits,
                                                                      YottaunitsToAttounits,
                                                                      YottaunitsToZeptounits,
                                                                      YottaunitsToYoctounits
                                                                    );
  CONVERSION_UNITS_DOWN: Array[0..15] of TLKUnit = (tuZettaunit, tuExaunit, tuPetaunit, tuTeraunit, tuGigaunit, tuMegaunit,
                                                       tuKilounit, tuMilliunit, tuUnit, tuMicrounit,
                                                       tuNanounit, tuPicounit, tuFemtounit, tuAttounit,
                                                       tuZeptounit, tuYoctounit);
var
  I: Integer;
begin
  I := 0;
  AOutValue := AYottaunits;
  AOutUnit := tuYottaunit;
  while (I < High(CONVERSION_METHODS_DOWN)) and
          (
            ((AOutValue < 1) and (AOutValue > 0)) or
            ((AOutValue > -1) and (AOutValue < 0))
          ) do
  begin
    AOutValue := CONVERSION_METHODS_DOWN[I](AYottaunits);
    AOutUnit := CONVERSION_UNITS_DOWN[I];
    Inc(I);
  end;
end;

function YottaunitsToYoctounits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * IntPower(1000, 16);
end;

function YottaunitsToZeptounits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * IntPower(1000, 15);
end;

function YottaunitsToAttounits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * IntPower(1000, 14);
end;

function YottaunitsToFemtounits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * IntPower(1000, 13);
end;

function YottaunitsToPicounits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * IntPower(1000, 12);
end;

function YottaunitsToNanounits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * IntPower(1000, 11);
end;

function YottaunitsToMicrounits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * IntPower(1000, 10);
end;

function YottaunitsToMilliunits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * IntPower(1000, 9);
end;

function YottaunitsToUnits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * IntPower(1000, 8);
end;

function YottaunitsToKilounits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * IntPower(1000, 7);
end;

function YottaunitsToMegaunits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * 1000000000000000000;
end;

function YottaunitsToGigaunits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * 1000000000000000;
end;

function YottaunitsToTeraunits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * 1000000000000;
end;

function YottaunitsToPetaunits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * 1000000000;
end;

function YottaunitsToExaunits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * 1000000;
end;

function YottaunitsToZettaunits(const AYottaunits: LKFloat): LKFloat;
begin
  Result := AYottaunits * 1000;
end;

end.
