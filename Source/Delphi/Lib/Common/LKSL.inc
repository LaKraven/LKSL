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

{$I LKSL_VERSIONS.inc}

// NOTE: The LKSL will REQUIRE Generics (without option) from December 6th 2014!
{$IFDEF SUPPORTS_GENERICS}
  {$IFNDEF LKSL_NO_GENERICS}
    {$DEFINE LKSL_USE_GENERICS} // Define "LKSL_NO_GENERICS" in your project to disable Generics
  {$ENDIF LKSL_NO_GENERICS}
{$ENDIF SUPPORTS_GENERICS}

{$IFDEF SUPPORTS_RECORDHELPERS}
  {$IFNDEF LKSL_NO_HELPERS}
    {$DEFINE LKSL_USE_HELPERS} // Define "LKSL_NO_HELPERS" in your project to disable Helpers
  {$ENDIF LKSL_NO_HELPERS}
{$ENDIF SUPPORTS_RECORDHELPERS}

{$IFDEF SUPPORTS_NAMESPACES}
  {$IFNDEF LKSL_NO_EXPLICIT_UNIT_NAMES}
    {$IFDEF NAMESPACES_BY_DEFAULT}
      {$DEFINE LKSL_USE_EXPLICIT_UNIT_NAMES} // Define "LKSL_NO_EXPLICIT_UNIT_NAMES" to disable explicit unit names on XE2+
    {$ENDIF NAMESPACES_BY_DEFAULT}
  {$ENDIF LKSL_NO_EXPLICIT_UNIT_NAMES}
{$ENDIF SUPPORTS_NAMESPACES}
