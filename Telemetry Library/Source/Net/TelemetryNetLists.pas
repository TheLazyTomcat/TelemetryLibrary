{*******************************************************************************
@abstract(List classes used in Net part of Telemetry library.)
@author(František Milt <fmilt@seznam.cz>)
@created(2013-10-09)
@lastmod(2013-10-09)

  TelemetryNetLists

  ©František Milt, all rights reserved.

  Classes in this unit (for details, refer to declaration of individual class):
@preformatted(
  TAddressList
  TClientsList)

  Last change:  2013-10-09

  Change List:@unorderedList(
    @item(2013-10-09  - First stable version.))

*******************************************************************************}
unit TelemetryNetLists;

interface

uses
  Classes,
  TelemetryLists;

type
{==============================================================================}
{------------------------------------------------------------------------------}
{                                 TAddressList                                 }
{------------------------------------------------------------------------------}
{==============================================================================}

  // Used in TAddressList to set how the list will distinguish whether client
  // can connect or not.
  // @value lmBlackList When client is listed, it cannot connect.
  // @value lmWhiteList Client can connect only when listed.
  // @value lmAllowAll  All clients can connect.
  // @value lmDenyAll   No client can connect (use with caution).
  TAddressListMode = (lmBlackList, lmWhiteList, lmAllowAll, lmDenyAll);

{==============================================================================}
{    TAddressList // Class declaration                                         }
{==============================================================================}
{
  @abstract(List used to store addresses that can or cannot, depending on list
            mode, connect to the server.)
  Addresses are stored as strings. All comparisons are case insensitive.

@member(fListMode See ListMode property.)

@member(fFileName See FileName property.)



@member(Create Object constructor.)

@member(ClientCanConnect
    Use this function to check, whether client on given address can or cannot
    connect.@br
    Result of this function depends on ListMode property value. When ListMode is
    set to invalid/unknown value, this function returns @false.

    @param Address Address of connecting client as string.

    @returns @True when client is allowed to connect, otherwise @false.)

@member(ClientIsInList
    Indicates whether given address is listed (case insensitive).

    @param Address Address to be checked in string format.

    @returns @True when address if found in list, otherwise @false.)

@member(SaveToFile
    Saves entire list to text file (one address per line).@br
    When @noAutoLink("FileName") parameter is set to an empty string, then the
    name of file to which the list is saved is taken from FileName property.

    @param(FileName Name of file (fully qualified path) to which the list will
                    be saved.))

@member(LoadFromFile
    Clears the list and loads new content from text file.@br
    When @noAutoLink("FileName") parameter is set to an empty string, then the
    name of file from which the list is loaded is taken from FileName property.

    @param(FileName Name of file (fully qualified path) from which the list will
                    be loaded.))



@member(ListMode
    Property determining how the list will evaluate whether client can ot cannot
    connect (see ClientCanConnect method for details). For available values and
    behaviour for each of them, refer to TAddressListMode enumerated type.@br
    Initialized to lmBlackList.)

@member(FileName
    File name used in SaveToFile and LoadFromFile methods. Refer to them for
    details.@br
    Initialized in following way:
    @longcode(
    ExtractFilePath(GetModuleName(hInstance)) + def_AddressListFileName; )
    ...where @code(def_AddressListFileName) is string constant of value
    "address.list.txt" (without quotes). It means the file would be located in
    the same folder as executable or library compiled with this unit.)
}
  TAddressList = class(TStringList)
  private
    fListMode:      TAddressListMode;
    fFileName:  String;
  public
    constructor Create;
    Function ClientCanConnect(Address: String): Boolean; virtual;
    Function ClientIsInList(Address: String): Boolean; virtual;
    procedure SaveToFile(const FileName: String = ''); override;
    procedure LoadFromFile(const FileName: String = ''); override;
  published
    property ListMode: TAddressListMode read fListMode write fListMode;
    property FileName: String read fFileName write fFileName;
  end;

{==============================================================================}
{------------------------------------------------------------------------------}
{                                 TClientsList                                 }
{------------------------------------------------------------------------------}
{==============================================================================}

  // Structure used as item in TClientsList class to store informations about
  // clients.
  TClientInfo = Record
    Identificator:        TGUID;
    Address:              String;
    Port:                 Word;
    WaitingForPassword:   Boolean;
    ReadyToWork:          Boolean;
    AdminRights:          Boolean;
    SuperAdminRights:     Boolean;
  end;
  PClientInfo = ^TClientInfo;

{==============================================================================}
{    TClientsList // Class declaration                                         }
{==============================================================================}
{
  @abstract(List used in client with admin rights to store informations about
            connected @noAutoLink(clients).)

@member(GetClientInfoPointer
    Getter for Pointers[] property.@br
    When index falls out of allowed boundary (<0,Count - 1>), an exception is
    raised.

    @param Index Index of requested item.

    @returns Pointer to requested item.)

@member(GetClientInfo
    Getter for Clients[] property.@br
    When index falls out of allowed boundary (<0,Count - 1>), an exception is
    raised.

    @param Index Index of requested item.

    @returns Requested item.)



@member(Clear
    Deletes all items in the list.@br
    OnChange event is called after items deletion.)

@member(IndexOf
    Searches through list for client with appropriate identificator. When the
    client is not found, -1 is returned.

    @param Identificator Requested client identificator.

    @returns Index of requested event, -1 when not found.)

@member(Add
    Adds new client record into the list.@br
    OnChange event is called after successful addition.

    @param Identificator      Client identificator.
    @param Address            Remote address.
    @param Port               Remote port.
    @param(WaitingForPassword Indicates whether server is waiting for client to
                              login.)
    @param(ReadyToWork        Indicates whether client is ready for full
                              communication exchange.)
    @param(AdminRights        Indicates whether client has administrative
                              rights.)
    @param(SuperAdminRights   Indicates whether client has superadministrative
                              rights.)

    @returns Index at which the new item was added, -1 when addition failed.)

@member(Remove
    Removes client with given identificator from the list.@br
    OnChange event is called after successful removal.

    @param Identificator Indentificator of client that has to be removed.

    @returns(Index of client that was removed, -1 when requested client was not
             found.))

@member(Delete
    Deletes client at position given by "Index" parameter. When index falls out
    of allowed boundary (<0,Count - 1>), an exception is raised.@br
    OnChange event is called after successful deletion.

    @param Index Index of client that has to be deleted.)

@member(Pointers
    Array property mapped directly to internal list. Use it for direct access to
    individual stored items.@br
    Unlike Clients[] property, you can use returned pointer to change values of
    stored items.)

@member(Clients
    Array property mapped directly to internal list. Use it to obtain values of
    individual stored items.)
}
  TClientsList = class(TCustomTelemetryList)
  private
    Function GetClientInfoPointer(Index: Integer): PClientInfo;
    Function GetClientInfo(Index: Integer): TClientInfo;
  public
    procedure Clear; override;
    Function IndexOf(Identificator: TGUID): Integer; virtual;
    Function Add(Identificator: TGUID; Address: String; Port: Word; WaitingForPassword,ReadyToWork,AdminRights,SuperAdminRights: Boolean): Integer; virtual;
    Function Remove(Identificator: TGUID): Integer; virtual;
    procedure Delete(Index: Integer); virtual;
    property Pointers[Index: Integer]: PClientInfo read GetClientInfoPointer;
    property Clients[Index: Integer]: TClientInfo read GetClientInfo; default;
  end;

implementation

uses
  SysUtils;

{==============================================================================}
{    TAddressList // Implementation                                            }
{==============================================================================}

{------------------------------------------------------------------------------}
{    TAddressList // Constants, types, variables, etc...                       }
{------------------------------------------------------------------------------}

const
  def_AddressListFileName = 'address.list.txt';

{------------------------------------------------------------------------------}
{    TAddressList // Public methods                                            }
{------------------------------------------------------------------------------}

constructor TAddressList.Create;
begin
inherited Create;
CaseSensitive := False;
ListMode := lmBlackList;
FileName := ExtractFilePath(GetModuleName(hInstance)) + def_AddressListFileName;
end;

//------------------------------------------------------------------------------

Function TAddressList.ClientCanConnect(Address: String): Boolean;
begin
case ListMode of
  lmBlackList:  Result := not ClientIsInList(Address);
  lmWhiteList:  Result := ClientIsInList(Address);
  lmAllowAll:   Result := True;
  lmDenyAll:    Result := False;
else
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TAddressList.ClientIsInList(Address: String): Boolean;
begin
Result := IndexOf(Address) >= 0;
end;

//------------------------------------------------------------------------------

procedure TAddressList.SaveToFile(const FileName: String = '');
begin
If FileName <> '' then
  inherited SaveToFile(FileName)
else
  inherited SaveToFile(Self.FileName)
end;

//------------------------------------------------------------------------------

procedure TAddressList.LoadFromFile(const FileName: String = '');
begin
Clear;
If FileName <> '' then
  begin
    If FileExists(FileName) then inherited LoadFromFile(FileName);
  end
else
  begin
    If FileExists(Self.FileName) then inherited LoadFromFile(Self.FileName);
  end
end;


{==============================================================================}
{    TClientsList // Implementation                                            }
{==============================================================================}

{------------------------------------------------------------------------------}
{    TClientsList // Private methods                                           }
{------------------------------------------------------------------------------}

Function TClientsList.GetClientInfoPointer(Index: Integer): PClientInfo;
begin
If (Index >= 0) and (Index < Count) then
  Result := PClientInfo(PtrGetItem(Index))
else
  raise Exception.Create('TClientsList.GetClientInfoPointer(Index): Index (' +
                         IntToStr(Index) + ') out of bounds.');
end;

//------------------------------------------------------------------------------

Function TClientsList.GetClientInfo(Index: Integer): TClientInfo;
begin
If (Index >= 0) and (Index < Count) then
  Result := GetClientInfoPointer(Index)^
else
  raise Exception.Create('TClientsList.GetClientInfo(Index): Index (' +
                         IntToStr(Index) + ') out of bounds.');
end;

{------------------------------------------------------------------------------}
{    TClientsList // Public methods                                            }
{------------------------------------------------------------------------------}

procedure TClientsList.Clear;
var
  i:  Integer;
begin
For i := (Count - 1) downto 0 do
  Dispose(PClientInfo(PtrGetItem(i)));
end;

//------------------------------------------------------------------------------

Function TClientsList.IndexOf(Identificator: TGUID): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := 0 to (Count - 1) do
  If IsEqualGUID(PClientInfo(PtrGetItem(i))^.Identificator,Identificator) then
    begin
      Result := i;
      Break;
    end;
end;

//------------------------------------------------------------------------------

Function TClientsList.Add(Identificator: TGUID; Address: String; Port: Word; WaitingForPassword,ReadyToWork,AdminRights,SuperAdminRights: Boolean): Integer;
var
  NewClient:  PClientInfo;
begin
New(NewClient);
NewClient^.Identificator := Identificator;
NewClient^.Address := Address;
NewClient^.Port := Port;
NewClient^.WaitingForPassword := WaitingforPassword;
NewClient^.ReadyToWork := ReadyToWork;
NewClient^.AdminRights := AdminRights;
NewClient^.SuperAdminRights := SuperAdminRights;
Result := PtrAdd(NewClient);
If Result < 0 then Dispose(NewClient);
end;

//------------------------------------------------------------------------------

Function TClientsList.Remove(Identificator: TGUID): Integer;
begin
Result := IndexOf(Identificator);
If Result >= 0 then Delete(Result);
end;

//------------------------------------------------------------------------------

procedure TClientsList.Delete(Index: Integer);
begin
If (Index >= 0) and (Index < Count) then
  begin
    Dispose(PClientInfo(PtrGetItem(Index)));
    PtrDelete(Index);
  end
else
  raise Exception.Create('TClientsList.Delete(Index): Index (' +
                         IntToStr(Index) + ') out of bounds.');
end;

end.
