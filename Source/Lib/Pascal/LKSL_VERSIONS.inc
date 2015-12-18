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

  {.$DEFINE SUPPORTS_RECORDHELPERS}

{$ELSE} // Delphi Related Settings
  {$DEFINE DELPHI}
  {$IFDEF MSWINDOWS}{$DEFINE WINDOWS}{$ENDIF MSWINDOWS}

  // Version Definitions
  {$IFDEF VER80}{$DEFINE DELPHI1}{$ENDIF VER80}
  {$IFDEF VER90}{$DEFINE DELPHI2}{$ENDIF VER90}
  {$IFDEF VER100}{$DEFINE DELPHI3}{$ENDIF VER100}
  {$IFDEF VER120}{$DEFINE DELPHI4}{$ENDIF VER120}
  {$IFDEF VER130}{$DEFINE DELPHI5}{$ENDIF VER130}
  {$IFDEF VER140}{$DEFINE DELPHI6}{$ENDIF VER140}
  {$IFDEF VER150}{$DEFINE DELPHI7}{$ENDIF VER150}
  {$IFDEF VER160}{$DEFINE DELPHI8}{$ENDIF VER160}
  {$IFDEF VER170}{$DEFINE DELPHI2005}{$ENDIF VER170}
  {$IFDEF VER180}{$DEFINE DELPHI2006}{$ENDIF VER180}
  {$IFDEF VER185}{$DEFINE DELPHI2007}{$ENDIF VER185}
  {$IFDEF VER190}{$DEFINE DELPHI2007_NET}{$ENDIF VER190}
  {$IFDEF VER200}{$DEFINE DELPHI2009}{$ENDIF VER200}
  {$IFDEF VER210}{$DEFINE DELPHI2010}{$ENDIF VER210}
  {$IFDEF VER220}{$DEFINE DELPHIXE}{$ENDIF VER220}
  {$IFDEF VER230}{$DEFINE DELPHIXE2}{$ENDIF VER230}
  {$IFDEF VER240}{$DEFINE DELPHIXE3}{$ENDIF VER240}
  {$IFDEF VER250}{$DEFINE DELPHIXE4}{$ENDIF VER250}
  {$IFDEF VER260}{$DEFINE DELPHIXE5}{$ENDIF VER260}
  {$IFDEF VER270}{$DEFINE DELPHIXE6}{$ENDIF VER270}
  {$IFDEF VER280}{$DEFINE DELPHIXE7}{$ENDIF VER280}
  {$IFDEF VER290}{$DEFINE DELPHIXE8}{$ENDIF VER290}

  // Because we now have completely insane version numbering AND naming...
  {$IFDEF VER300}
    {$DEFINE DELPHI10}
    {$DEFINE DELPHI10SEATTLE}
    {$DEFINE SEATTLE}
    {$DEFINE DELPHISEATTLE}
  {$ENDIF VER300}
  
  {$IFDEF VER310}
    {$DEFINE DELPHI10.1}
    {$DEFINE DELPHI10.1LONDON}
    {$DEFINE LONDON}
    {$DEFINE DELPHILONDON}
  {$ENDIF VER300}  
  
// This-and-Up Defines
  {$IF COMPILERVERSION > 13}{$DEFINE DELPHI6_UP}{$IFEND}
  {$IF COMPILERVERSION > 14}{$DEFINE DELPHI7_UP}{$IFEND}
  {$IF COMPILERVERSION > 15}{$DEFINE DELPHI8_UP}{$IFEND}
  {$IF COMPILERVERSION > 16}{$DEFINE DELPHI2005_UP}{$IFEND}
  {$IF COMPILERVERSION > 17}{$DEFINE DELPHI2006_UP}{$IFEND}
  {$IF COMPILERVERSION > 18}{$DEFINE DELPHI2007_UP}{$IFEND}
  {$IF COMPILERVERSION > 18.5}{$DEFINE DELPHI2007_NET_UP}{$IFEND}
  {$IF COMPILERVERSION > 19}{$DEFINE DELPHI2009_UP}{$IFEND}
  {$IF COMPILERVERSION > 20}{$DEFINE DELPHI2010_UP}{$IFEND}
  {$IF COMPILERVERSION > 21}{$DEFINE DELPHIXE_UP}{$IFEND}
  {$IF COMPILERVERSION > 22}{$DEFINE DELPHIXE2_UP}{$IFEND}
  {$IF COMPILERVERSION > 23}{$DEFINE DELPHIXE3_UP}{$IFEND}
  {$IF COMPILERVERSION > 24}{$DEFINE DELPHIXE4_UP}{$IFEND}
  {$IF COMPILERVERSION > 25}{$DEFINE DELPHIXE5_UP}{$IFEND}
  {$IF COMPILERVERSION > 26}{$DEFINE DELPHIXE6_UP}{$IFEND}
  {$IF COMPILERVERSION > 26.5}{$DEFINE APPMETHOD1_UP}{$IFEND}
  {$IF COMPILERVERSION > 27}{$DEFINE DELPHIXE7_UP}{$IFEND}
  {$IF COMPILERVERSION > 28}{$DEFINE DELPHIXE8_UP}{$IFEND}
  {$IF COMPILERVERSION > 29}
    {$DEFINE DELPHI10SEATTLE_UP}
    {$DEFINE DELPHI10_UP}
    {$DEFINE DELPHISEATTLE_UP}
  {$IFEND}
  {$IF COMPILERVERSION > 30}
    {$DEFINE DELPHI10.1LONDON_UP}
    {$DEFINE DELPHI10.1_UP}
    {$DEFINE DELPHILONDON_UP}
    {$MESSAGE WARN 'Versions newer than Delphi 10 Seattle have not yet been tested. This message will be removed once the version you are using has been officially tested.'}
  {$IFEND}
// Support Definitions

  {$IFDEF DELPHI2005_UP}
    {$DEFINE SUPPORTS_INLINE}
  {$ENDIF DELPHI2005_UP}

  {$IFDEF DELPHI2006_UP}
    {$DEFINE SUPPORTS_ADVANCED_RECORDS}
  {$ENDIF DELPHI2006_UP}

  {$IFDEF DELPHI2009_UP}
    {$DEFINE SUPPORTS_UNICODE}
    {$DEFINE SUPPORTS_GENERICS}
    {$DEFINE SUPPORTS_ANONYMOUSMETHODS}
    {$DEFINE SUPPORTS_NAMESPACES}
    {$DEFINE SUPPORTS_REFERENCETOMETHOD}
  {$ENDIF DELPHI2009_UP}

  {$IFDEF DELPHI2010_UP}
    {$DEFINE SUPPORTS_ADVANCED_RTTI}
  {$ENDIF DELPHI2010_UP}

  {$IFDEF DELPHIXE2_UP}
    {$DEFINE SUPPORTS_FIREMONKEY}
    {$DEFINE FIREMONKEY_FM1}
    {$DEFINE SUPPORTS_IOS}
    {$DEFINE SUPPORTS_OSX}
    {$DEFINE SUPPORTS_WIN64}
    {$DEFINE NAMESPACES_BY_DEFAULT}
  {$ENDIF DELPHIXE2_UP}

  {$IFDEF DELPHIXE3_UP}
    {$UNDEF SUPPORTS_IOS}
    {$DEFINE SUPPORTS_RECORDHELPERS}
  {$ENDIF DELPHIXE3_UP}

  {$IFDEF DELPHIXE4_UP}
    {$DEFINE SUPPORTS_IOS}
    {$DEFINE IOS_PROPER}
    {$DEFINE SUPPORTS_AUTOREFCOUNT}
    {$UNDEF FIREMONKEY_FM1}
    {$DEFINE FIREMONKEY_FM2}
    {$IFDEF NEXTGEN}
      {$DEFINE SUPPORTS_DISPOSEOF}
    {$ENDIF NEXTGEN}
  {$ENDIF DELPHIXE4_UP}

  {$IFDEF DELPHIXE5_UP}
    {$DEFINE SUPPORTS_ANDROID}
    {$DEFINE SUPPORTS_CLASSOPERATORS_NEXTGEN}
  {$ENDIF DELPHIXE5_UP}

  {$IFDEF DELPHIXE6_UP}
    {$DEFINE SUPPORTS_APPTETHERING}
  {$ENDIF DELPHIXE6_UP}

  {$IFDEF DELPHIXE7_UP}
    {$DEFINE SUPPORTS_ARRAYOPERATORS}
    {$DEFINE SUPPORTS_PARALLEL}
    {$DEFINE SUPPORTS_FIREUI}
  {$DEFINE SUPPORTS_MULTITOUCH}
  {$ENDIF DELPHIXE7_UP}
  
  {$IFDEF DELPHI10_UP}
    {$DEFINE SUPPORTS_DPI}
    {$DEFINE SUPPORTS_BEACONS}
  {$ENDIF DELPHI10_UP}
  
  {$IFDEF DELPHI10.1_UP}
    {$DEFINE SUPPORTS_MOBILE_UTF8STRING}
    {$DEFINE EXTENDED_16_BYTES}
    {$DEFINE SUPPORTS_VARIABLES_VOLATILE}
    {$DEFINE SUPPORTS_WINDOWS_10_NOTIFICATIONS}
    {$DEFINE SUPPORTS_WINDOWS_DIRECTX_12}
    {$DEFINE SUPPORTS_ANDROID_6}
    {$DEFINE SUPPORTS_ENHANCED_BEACONS}
  {$ENDIF DELPHI10.1_UP}
{$ENDIF FPC}
