object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 330
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object memLog: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 34
    Width = 627
    Height = 293
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 633
    ExplicitHeight = 330
  end
  object btnDispatchEvent: TButton
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 627
    Height = 25
    Align = alTop
    Caption = 'Dispatch an Event for Processing'
    TabOrder = 1
    OnClick = btnDispatchEventClick
    ExplicitLeft = 288
    ExplicitTop = 176
    ExplicitWidth = 75
  end
end
