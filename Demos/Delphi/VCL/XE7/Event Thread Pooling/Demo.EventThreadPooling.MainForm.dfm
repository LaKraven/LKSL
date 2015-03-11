object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 331
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object memLog: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 34
    Width = 627
    Height = 294
    Align = alClient
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
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
  end
end
