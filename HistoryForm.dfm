object FormHistory: TFormHistory
  Left = 0
  Top = 0
  Caption = 'Clipboard History'
  ClientHeight = 400
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbHistory: TListBox
    Left = 8
    Top = 8
    Width = 584
    Height = 350
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = lbHistoryDblClick
  end
  object btnCopy: TButton
    Left = 432
    Top = 364
    Width = 75
    Height = 25
    Caption = 'Copy'
    TabOrder = 1
    OnClick = btnCopyClick
  end
  object btnClose: TButton
    Left = 517
    Top = 364
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 2
    OnClick = btnCloseClick
  end
  object btnSave: TButton
    Left = 8
    Top = 364
    Width = 75
    Height = 25
    Caption = 'Save...'
    TabOrder = 3
    OnClick = btnSaveClick
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Left = 280
    Top = 184
  end
end