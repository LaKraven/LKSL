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

{$IFDEF FPC} // FreePascal Related Settings

  {$MODE DELPHI}
  {$MESSAGE FATAL 'FreePascal is not yet supported by the LaKraven Studios Standard Library [LKSL]'}

{$ELSE} // Delphi Related Settings

// Version Definitions

  {$IFDEF WIN32 || WIN64}
    {$DEFINE WINDOWS}
  {$ENDIF}

  {$DEFINE DELPHI}

  {$IFDEF VER80}
    {$DEFINE DELPHI1}
  {$ENDIF}

  {$IFDEF VER90}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
  {$ENDIF}

  {$IFDEF VER100}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
  {$ENDIF}

  {$IFDEF VER120}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
  {$ENDIF}

  {$IFDEF VER130}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
  {$ENDIF}

  {$IFDEF VER140}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
  {$ENDIF}

  {$IFDEF VER150}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
  {$ENDIF}

  {$IFDEF VER160}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
  {$ENDIF}

  {$IFDEF VER170}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
  {$ENDIF}

  {$IFDEF VER180}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
  {$ENDIF}

  {$IFDEF VER185}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
  {$ENDIF}

  {$IFDEF VER190} // Borland Development Studio 2007 (HighLander) .NET Personality
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2007_NET}
  {$ENDIF}

  {$IFDEF VER200}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2007_NET}
    {$DEFINE DELPHI2009}
  {$ENDIF}

  {$IFDEF VER210}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2007_NET}
    {$DEFINE DELPHI2009}
    {$DEFINE DELPHI2010}
  {$ENDIF}

  {$IFDEF VER220}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2007_NET}
    {$DEFINE DELPHI2009}
    {$DEFINE DELPHI2010}
    {$DEFINE DELPHIXE}
  {$ENDIF}

  {$IFDEF VER230}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2007_NET}
    {$DEFINE DELPHI2009}
    {$DEFINE DELPHI2010}
    {$DEFINE DELPHIXE}
    {$DEFINE DELPHIXE2}
  {$ENDIF}

  {$IFDEF VER240}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2007_NET}
    {$DEFINE DELPHI2009}
    {$DEFINE DELPHI2010}
    {$DEFINE DELPHIXE}
    {$DEFINE DELPHIXE2}
    {$DEFINE DELPHIXE3}
  {$ENDIF}

  {$IFDEF VER250}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2007_NET}
    {$DEFINE DELPHI2009}
    {$DEFINE DELPHI2010}
    {$DEFINE DELPHIXE}
    {$DEFINE DELPHIXE2}
    {$DEFINE DELPHIXE3}
    {$DEFINE DELPHIXE4}
  {$ENDIF}

  {$IFDEF VER260}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2007_NET}
    {$DEFINE DELPHI2009}
    {$DEFINE DELPHI2010}
    {$DEFINE DELPHIXE}
    {$DEFINE DELPHIXE2}
    {$DEFINE DELPHIXE3}
    {$DEFINE DELPHIXE4}
    {$DEFINE DELPHIXE5}
  {$ENDIF}

  {$IFDEF VER270}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2007_NET}
    {$DEFINE DELPHI2009}
    {$DEFINE DELPHI2010}
    {$DEFINE DELPHIXE}
    {$DEFINE DELPHIXE2}
    {$DEFINE DELPHIXE3}
    {$DEFINE DELPHIXE4}
    {$DEFINE DELPHIXE5}
    {$DEFINE DELPHIXE6}
  {$ENDIF}

  {$IFDEF VER280}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2007_NET}
    {$DEFINE DELPHI2009}
    {$DEFINE DELPHI2010}
    {$DEFINE DELPHIXE}
    {$DEFINE DELPHIXE2}
    {$DEFINE DELPHIXE3}
    {$DEFINE DELPHIXE4}
    {$DEFINE DELPHIXE5}
    {$DEFINE DELPHIXE6}
    {$DEFINE DELPHIXE7}
  {$ENDIF}

  {$IFDEF VER290}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2007_NET}
    {$DEFINE DELPHI2009}
    {$DEFINE DELPHI2010}
    {$DEFINE DELPHIXE}
    {$DEFINE DELPHIXE2}
    {$DEFINE DELPHIXE3}
    {$DEFINE DELPHIXE4}
    {$DEFINE DELPHIXE5}
    {$DEFINE DELPHIXE6}
    {$DEFINE DELPHIXE7}
    {$DEFINE DELPHIXE8}
  {$ENDIF}

  {$IFDEF VER300}
    {$DEFINE DELPHI1}
    {$DEFINE DELPHI2}
    {$DEFINE DELPHI3}
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2007_NET}
    {$DEFINE DELPHI2009}
    {$DEFINE DELPHI2010}
    {$DEFINE DELPHIXE}
    {$DEFINE DELPHIXE2}
    {$DEFINE DELPHIXE3}
    {$DEFINE DELPHIXE4}
    {$DEFINE DELPHIXE5}
    {$DEFINE DELPHIXE6}
    {$DEFINE DELPHIXE7}
    {$DEFINE DELPHIXE8}
    {$DEFINE DELPHIXE9}
  {$ENDIF}

// Support Definitions

  {$IFDEF DELPHI2005}
    {$DEFINE SUPPORTS_INLINE}
  {$ENDIF DELPHI2005}

  {$IFDEF DELPHI2006}
    {$DEFINE SUPPORTS_ADVANCED_RECORDS}
  {$ENDIF DELPHI2006}

  {$IFDEF DELPHI2009}
    {$DEFINE SUPPORTS_UNICODE}
    {$DEFINE SUPPORTS_GENERICS}
    {$DEFINE SUPPORTS_ANONYMOUSMETHODS}
    {$DEFINE SUPPORTS_NAMESPACES}
  {$ENDIF DELPHI2009}

  {$IFDEF DELPHI2010}
    {$DEFINE SUPPORTS_ADVANCED_RTTI}
  {$ENDIF DELPHI2010}

  {$IFDEF DELPHIXE2}
    {$DEFINE SUPPORTS_FIREMONKEY}
    {$DEFINE FIREMONKEY_FM1}
    {$DEFINE SUPPORTS_IOS}
    {$DEFINE SUPPORTS_OSX}
    {$DEFINE SUPPORTS_WIN64}
    {$DEFINE NAMESPACES_BY_DEFAULT}
  {$ENDIF DELPHIXE2}

  {$IFDEF DELPHIXE3}
    {$UNDEF SUPPORTS_IOS}
    {$DEFINE SUPPORTS_RECORDHELPERS}
  {$ENDIF DELPHIXE3}

  {$IFDEF DELPHIXE4}
    {$DEFINE SUPPORTS_IOS}
    {$DEFINE IOS_PROPER}
    {$DEFINE SUPPORTS_AUTOREFCOUNT}
    {$UNDEF FIREMONKEY_FM1}
    {$DEFINE FIREMONKEY_FM2}
  {$ENDIF DELPHIXE4}

  {$IFDEF DELPHIXE5}
    {$DEFINE SUPPORTS_ANDROID}
    {$DEFINE SUPPORTS_CLASSOPERATORS_NEXTGEN}
  {$ENDIF DELPHIXE5}

  {$IFDEF DELPHIXE6}
    {$DEFINE SUPPORTS_APPTETHERING}
  {$ENDIF DELPHIXE6}

  {$IFDEF DELPHIXE7}
    {$DEFINE SUPPORTS_ARRAYOPERATORS}
    {$DEFINE SUPPORTS_PARALLEL}
    {$DEFINE SUPPORTS_FIREUI}
  {$ENDIF DELPHIXE7}
{$ENDIF}