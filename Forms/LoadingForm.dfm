object LoadingScreen: TLoadingScreen
  Left = 0
  Top = 0
  AlphaBlendValue = 200
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'CMDLauncher'
  ClientHeight = 151
  ClientWidth = 350
  Color = clWhite
  TransparentColorValue = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  Visible = True
  OnShow = FormShow
  DesignSize = (
    350
    151)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTask: TLabel
    Left = 8
    Top = 78
    Width = 331
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    BiDiMode = bdLeftToRight
    Caption = 'lblTask'
    ParentBiDiMode = False
    Layout = tlCenter
  end
  object lblLog: TLabel
    AlignWithMargins = True
    Left = 4
    Top = 128
    Width = 335
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'lblLog'
    Layout = tlCenter
  end
  object imgLoading: TImage
    Left = 143
    Top = 8
    Width = 64
    Height = 64
    AutoSize = True
    Transparent = True
  end
  object TaskProgress: TCMDProgressBar
    Left = 4
    Top = 97
    Width = 335
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
end
