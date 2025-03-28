object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Clipboard Manager'
  ClientHeight = 213
  ClientWidth = 455
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnShowHistory: TButton
    Left = 24
    Top = 24
    Width = 121
    Height = 41
    Caption = 'Show History'
    TabOrder = 0
    OnClick = btnShowHistoryClick
  end
  object btnClearHistory: TButton
    Left = 24
    Top = 80
    Width = 121
    Height = 41
    Caption = 'Clear History'
    TabOrder = 1
    OnClick = btnClearHistoryClick
  end
  object btnSettings: TButton
    Left = 24
    Top = 136
    Width = 121
    Height = 41
    Caption = 'Settings'
    TabOrder = 2
    OnClick = btnSettingsClick
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 208
    Top = 104
  end
  object TrayIcon1: TTrayIcon
    PopupMenu = PopupMenu1
    OnDblClick = TrayIcon1DblClick
    Left = 280
    Top = 104
  end
  object PopupMenu1: TPopupMenu
    Left = 352
    Top = 104
    object miShow: TMenuItem
      Caption = 'Show'
      OnClick = miShowClick
    end
    object miExit: TMenuItem
      Caption = 'Exit'
      OnClick = miExitClick
    end
  end
end
