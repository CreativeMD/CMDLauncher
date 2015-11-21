object ResourceSelect: TResourceSelect
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Resourcepack Version Select'
  ClientHeight = 199
  ClientWidth = 193
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    193
    199)
  PixelsPerInch = 96
  TextHeight = 13
  object btnSave: TButton
    Left = 110
    Top = 167
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Save'
    TabOrder = 0
    OnClick = btnSaveClick
    ExplicitLeft = 190
    ExplicitTop = 159
  end
  object lstVersions: TListBox
    Left = 8
    Top = 8
    Width = 177
    Height = 153
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
    ExplicitWidth = 257
    ExplicitHeight = 145
  end
  object btnCancel: TButton
    Left = 8
    Top = 167
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ExplicitTop = 159
  end
end
