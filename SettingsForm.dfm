object FormSettings: TFormSettings
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 240
  ClientWidth = 300
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object chkEnabled: TCheckBox
    Left = 16
    Top = 16
    Width = 200
    Height = 17
    Caption = 'Enable clipboard monitoring'
    TabOrder = 0
  end
  object chkAutoSave: TCheckBox
    Left = 16
    Top = 48
    Width = 200
    Height = 17
    Caption = 'Auto save history on exit'
    TabOrder = 1
  end
  object Label1: TLabel
    Left = 16
    Top = 88
    Width = 120
    Height = 13
    Caption = 'Maximum history items:'
  end
  object seMaxItems: TSpinEdit
    Left = 16
    Top = 107
    Width = 121
    Height = 22
    MaxValue = 1000
    MinValue = 10
    TabOrder = 2
    Value = 100
  end
  object chkAutoStart: TCheckBox
    Left = 16
    Top = 144
    Width = 200
    Height = 17
    Caption = 'Run at Windows startup'
    TabOrder = 3
  end
  object chkStartMinimized: TCheckBox
    Left = 16
    Top = 176
    Width = 200
    Height = 17
    Caption = 'Start minimized to tray'
    TabOrder = 4
  end
  object btnOK: TButton
    Left = 120
    Top = 208
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 5
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 208
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
end