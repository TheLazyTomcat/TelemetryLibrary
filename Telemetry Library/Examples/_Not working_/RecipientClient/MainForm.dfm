object frmMainForm: TfrmMainForm
  Left = 477
  Top = 84
  Width = 800
  Height = 651
  Anchors = [akLeft, akTop, akRight]
  Caption = 'Recipient Client'
  Color = clBtnFace
  Constraints.MinHeight = 651
  Constraints.MinWidth = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    792
    617)
  PixelsPerInch = 96
  TextHeight = 13
  object bvlConnectSeparator: TBevel
    Left = 0
    Top = 56
    Width = 793
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object lblPort: TLabel
    Left = 152
    Top = 8
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object stbStatusBar: TStatusBar
    Left = 0
    Top = 598
    Width = 792
    Height = 19
    Panels = <
      item
        Text = 'Not connected'
        Width = 400
      end
      item
        Width = 50
      end>
  end
  object leAddress: TLabeledEdit
    Left = 8
    Top = 24
    Width = 137
    Height = 21
    EditLabel.Width = 43
    EditLabel.Height = 13
    EditLabel.Caption = 'Address:'
    TabOrder = 0
    Text = '0.0.0.0'
  end
  object sePort: TSpinEdit
    Left = 152
    Top = 24
    Width = 81
    Height = 22
    MaxValue = 65535
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object btnConnect: TButton
    Left = 656
    Top = 16
    Width = 129
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Connect to server'
    TabOrder = 2
    OnClick = btnConnectClick
  end
  object grbRegisteredEvents: TGroupBox
    Left = 8
    Top = 136
    Width = 249
    Height = 209
    Caption = 'Registered events'
    TabOrder = 5
    object lbRegisteredEvents: TListBox
      Left = 8
      Top = 16
      Width = 233
      Height = 121
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ItemHeight = 14
      ParentFont = False
      TabOrder = 0
    end
    object btnRegisterEvent: TButton
      Left = 8
      Top = 144
      Width = 113
      Height = 25
      Caption = 'Register event...'
      TabOrder = 1
      OnClick = btnRegisterEventClick
    end
    object btnUnregisterEvent: TButton
      Left = 128
      Top = 144
      Width = 113
      Height = 25
      Caption = 'Unregister event'
      TabOrder = 2
      OnClick = btnUnregisterEventClick
    end
    object btnRegisterAllEvents: TButton
      Left = 8
      Top = 176
      Width = 113
      Height = 25
      Caption = 'Register all events'
      TabOrder = 3
      OnClick = btnRegisterAllEventsClick
    end
    object btnUnregisterAllEvents: TButton
      Left = 128
      Top = 176
      Width = 113
      Height = 25
      Caption = 'Unregister all events'
      TabOrder = 4
      OnClick = btnUnregisterAllEventsClick
    end
  end
  object grbRegisteredChannels: TGroupBox
    Left = 264
    Top = 136
    Width = 521
    Height = 209
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Registered channels'
    TabOrder = 6
    DesignSize = (
      521
      209)
    object btnRegisterChannel: TButton
      Left = 8
      Top = 144
      Width = 121
      Height = 25
      Caption = 'Register channel...'
      TabOrder = 1
      OnClick = btnRegisterChannelClick
    end
    object btnUnregisterChannel: TButton
      Left = 136
      Top = 144
      Width = 121
      Height = 25
      Caption = 'Unregister channel'
      TabOrder = 2
      OnClick = btnUnregisterChannelClick
    end
    object btnRegisterAllChannels: TButton
      Left = 8
      Top = 176
      Width = 121
      Height = 25
      Caption = 'Register all channels'
      TabOrder = 3
      OnClick = btnRegisterAllChannelsClick
    end
    object btnUnregisterAllChannels: TButton
      Left = 136
      Top = 176
      Width = 121
      Height = 25
      Caption = 'Unregister all channels'
      TabOrder = 4
      OnClick = btnUnregisterAllChannelsClick
    end
    object btnOpenChannelsWnd: TButton
      Left = 384
      Top = 144
      Width = 129
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Open channels window'
      TabOrder = 5
      OnClick = btnOpenChannelsWndClick
    end
    object lbRegisteredChannels: TListBox
      Left = 8
      Top = 16
      Width = 505
      Height = 121
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ItemHeight = 14
      ParentFont = False
      TabOrder = 0
    end
  end
  object grbEventsLog: TGroupBox
    Left = 264
    Top = 352
    Width = 521
    Height = 241
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Events log'
    TabOrder = 8
    DesignSize = (
      521
      241)
    object meEventsLog: TMemo
      Left = 8
      Top = 16
      Width = 505
      Height = 193
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      WordWrap = False
    end
    object cbLogEventsData: TCheckBox
      Left = 8
      Top = 216
      Width = 153
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Write event data to the log'
      TabOrder = 1
    end
  end
  object grbPacketsLog: TGroupBox
    Left = 8
    Top = 352
    Width = 249
    Height = 241
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Packets log'
    TabOrder = 7
    DesignSize = (
      249
      241)
    object mePacketsLog: TMemo
      Left = 8
      Top = 16
      Width = 233
      Height = 193
      Anchors = [akLeft, akTop, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      WordWrap = False
    end
    object cbShowChannelsPackets: TCheckBox
      Left = 8
      Top = 216
      Width = 137
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Show channels packets'
      TabOrder = 1
    end
  end
  object grbGameLog: TGroupBox
    Left = 264
    Top = 64
    Width = 521
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Game log'
    TabOrder = 4
    DesignSize = (
      521
      65)
    object lblLogType: TLabel
      Left = 8
      Top = 16
      Width = 46
      Height = 13
      Caption = 'Log type:'
    end
    object leLogText: TLabeledEdit
      Left = 96
      Top = 32
      Width = 353
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 78
      EditLabel.Height = 13
      EditLabel.Caption = 'Text to be sent:'
      TabOrder = 1
    end
    object btnSendLogText: TBitBtn
      Left = 456
      Top = 30
      Width = 59
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Send'
      TabOrder = 2
      OnClick = btnSendLogTextClick
    end
    object cmbLogType: TComboBox
      Left = 8
      Top = 32
      Width = 81
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 13
      ItemIndex = 0
      ParentFont = False
      TabOrder = 0
      Text = 'message'
      Items.Strings = (
        'message'
        'warning'
        'error')
    end
  end
  object grbTelemetryInformation: TGroupBox
    Left = 8
    Top = 64
    Width = 249
    Height = 65
    Caption = 'Telemetry information'
    TabOrder = 3
    object lblTelemetryC: TLabel
      Left = 7
      Top = 40
      Width = 90
      Height = 13
      Alignment = taRightJustify
      Caption = 'Telemetry version:'
    end
    object lblServerC: TLabel
      Left = 135
      Top = 40
      Width = 74
      Height = 13
      Alignment = taRightJustify
      Caption = 'Server version:'
    end
    object lblTelemetry: TLabel
      Left = 104
      Top = 40
      Width = 3
      Height = 13
    end
    object lblGame: TLabel
      Left = 8
      Top = 20
      Width = 233
      Height = 13
      Alignment = taCenter
      AutoSize = False
    end
    object lblServer: TLabel
      Left = 216
      Top = 40
      Width = 3
      Height = 13
    end
  end
  object tmrRegChannelsUpdate: TTimer
    Interval = 250
    OnTimer = tmrRegChannelsUpdateTimer
    Left = 752
    Top = 312
  end
end
