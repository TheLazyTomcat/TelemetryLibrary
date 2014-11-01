object frmRegisterEvent: TfrmRegisterEvent
  Left = 1004
  Top = 480
  BorderStyle = bsDialog
  Caption = 'Register event'
  ClientHeight = 248
  ClientWidth = 248
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblKnownEvents: TLabel
    Left = 8
    Top = 8
    Width = 72
    Height = 13
    Caption = 'Known events:'
  end
  object lblManualEntry: TLabel
    Left = 10
    Top = 178
    Width = 159
    Height = 13
    Alignment = taRightJustify
    Caption = 'Manually selected event number:'
  end
  object lbKnownEvents: TListBox
    Left = 8
    Top = 24
    Width = 233
    Height = 145
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 0
    OnDblClick = lbKnownEventsDblClick
  end
  object seManualEntry: TSpinEdit
    Left = 176
    Top = 176
    Width = 65
    Height = 22
    MaxValue = 2147483647
    MinValue = -1
    TabOrder = 1
    Value = -1
  end
  object btnOK: TButton
    Left = 88
    Top = 216
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 168
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = btnCancelClick
  end
end
