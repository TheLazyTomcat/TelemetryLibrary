object frmChannelsForm: TfrmChannelsForm
  Left = 406
  Top = 277
  Width = 864
  Height = 486
  Caption = 'Channels'
  Color = clBtnFace
  Constraints.MinHeight = 486
  Constraints.MinWidth = 864
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    856
    452)
  PixelsPerInch = 96
  TextHeight = 13
  object lblUpdateInterval: TLabel
    Left = 682
    Top = 426
    Width = 102
    Height = 13
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    Caption = 'Update interval (ms):'
  end
  object sgRegisteredChannels: TStringGrid
    Left = 8
    Top = 8
    Width = 841
    Height = 409
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultColWidth = 405
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
    ParentFont = False
    TabOrder = 0
  end
  object seUpdateInterval: TSpinEdit
    Left = 792
    Top = 424
    Width = 57
    Height = 22
    Anchors = [akRight, akBottom]
    MaxValue = 10000
    MinValue = 100
    TabOrder = 1
    Value = 250
    OnChange = seUpdateIntervalChange
  end
  object tmrUpdateTimer: TTimer
    Enabled = False
    Interval = 250
    OnTimer = tmrUpdateTimerTimer
    Left = 8
    Top = 424
  end
end
