object LoadingScreen: TLoadingScreen
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'CMDLauncher'
  ClientHeight = 73
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblTask: TLabel
    Left = 8
    Top = 8
    Width = 32
    Height = 13
    Caption = 'lblTask'
  end
  object lblLog: TLabel
    Left = 8
    Top = 55
    Width = 27
    Height = 13
    Caption = 'lblLog'
  end
  object TaskProgress: TCMDProgressBar
    Left = 8
    Top = 24
    Width = 289
    Height = 25
    TabOrder = 0
  end
end
