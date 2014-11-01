object frmRegisterChannel: TfrmRegisterChannel
  Left = 941
  Top = 318
  BorderStyle = bsDialog
  Caption = 'Register channel'
  ClientHeight = 406
  ClientWidth = 312
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
  object lblKnownChannels: TLabel
    Left = 8
    Top = 8
    Width = 81
    Height = 13
    Caption = 'Known channels:'
  end
  object lblIndex: TLabel
    Left = 8
    Top = 320
    Width = 32
    Height = 13
    Caption = 'Index:'
  end
  object lblValueType: TLabel
    Left = 88
    Top = 320
    Width = 55
    Height = 13
    Caption = 'Value type:'
  end
  object lbKnownChannels: TListBox
    Left = 8
    Top = 24
    Width = 297
    Height = 242
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 0
    OnClick = lbKnownChannelsClick
    OnDblClick = lbKnownChannelsDblClick
  end
  object seIndex: TSpinEdit
    Left = 8
    Top = 336
    Width = 73
    Height = 22
    MaxValue = 100
    MinValue = -1
    TabOrder = 1
    Value = -1
  end
  object cmbValueType: TComboBox
    Left = 88
    Top = 336
    Width = 129
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 152
    Top = 376
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 232
    Top = 376
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = btnCancelClick
  end
  object cbEachFrame: TCheckBox
    Left = 224
    Top = 320
    Width = 81
    Height = 17
    Caption = 'Each frame'
    TabOrder = 5
  end
  object cbNoValue: TCheckBox
    Left = 224
    Top = 342
    Width = 73
    Height = 17
    Caption = 'No value'
    TabOrder = 6
  end
  object eChannelName: TEdit
    Left = 8
    Top = 292
    Width = 297
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
  end
  object cbManualEntry: TCheckBox
    Left = 8
    Top = 272
    Width = 113
    Height = 17
    Caption = 'Manually set name:'
    TabOrder = 8
    OnClick = cbManualEntryClick
  end
end
