object Importer: TImporter
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Importer'
  ClientHeight = 195
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ImportSelection: TGroupBox
    Left = 8
    Top = 8
    Width = 439
    Height = 153
    Caption = 'Import Selection'
    TabOrder = 2
    object lblFile: TLabel
      Left = 11
      Top = 97
      Width = 77
      Height = 13
      Caption = 'Not file selected'
    end
    object rbURL: TRadioButton
      Left = 11
      Top = 24
      Width = 113
      Height = 17
      Caption = 'URL (Modpack)'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object edtURL: TEdit
      Left = 11
      Top = 47
      Width = 425
      Height = 21
      TabOrder = 1
      OnChange = edtURLChange
    end
    object rbFile: TRadioButton
      Left = 11
      Top = 74
      Width = 113
      Height = 17
      Caption = 'File'
      TabOrder = 2
    end
    object btnFile: TButton
      Left = 11
      Top = 116
      Width = 75
      Height = 25
      Caption = 'Select File'
      TabOrder = 3
      OnClick = btnFileClick
    end
  end
  object btnCancel: TButton
    Left = 8
    Top = 164
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object btnSave: TButton
    Left = 372
    Top = 165
    Width = 75
    Height = 25
    Caption = 'Save'
    ModalResult = 1
    TabOrder = 1
  end
  object dlgOpenFile: TOpenDialog
    Filter = 'Instance Filenames|*.cfg'
    Left = 296
    Top = 104
  end
end
