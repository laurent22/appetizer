unit User;

interface

uses XMLDoc, Logger, ComObj, MSXML2_TLB, SysUtils, ExtCtrls, Graphics, Classes,
	FileSystemUtils, Windows, Contnrs, StringUtils, Forms, VersionInformation;



type

  TFolderItem = class

  	private

    	pSmallIcon: TIcon;
    	class var pUniqueID : Integer;
      function GetSmallIcon(): TIcon;
      function GetResolvedFilePath(): String;

    public

      FilePath: String;
      ID: Integer;
      Name: String;
      IsSeparator: Boolean;
      property SmallIcon:TIcon read GetSmallIcon;
      property ResolvedFilePath:String read GetResolvedFilePath;
      
      procedure ClearCachedIcons();
      class function ResolveFilePath(const filePath: String):String;
    	constructor Create();

  end;


  TSpecialFolderNames = record
  	Music: String;
    Pictures: String;
    Videos: String;
    Documents: String;
  end; 


	TUser = class(TObject)

  	private

    	pFilePath: String;
    	pXML: IXMLDomDocument;
      pSaveTimer: TTimer;
      pFolderItems: TObjectList;
      pSpecialFolderNames: TSpecialFolderNames;
      pSaveFolderItemsFlag: Boolean;
      pEditFolderItemForm: TObject;

      procedure ScheduleSave();
      procedure ScheduleSave_Timer(Sender: TObject);
      procedure SetExclusions(const value: TStringList);
      function GetExclusions(): TStringList;

  	public

    	procedure Save();
    	function GetUserSetting(const name: String): String;
      procedure SetUserSetting(const name: String; const value: String);
      procedure RefreshFolderItems();
      function GetFolderItemAt(const iIndex: Word): TFolderItem;
      function GetFolderItemByID(const iFolderItemID: Integer): TFolderItem;
      function FolderItemCount(): Word;
      procedure ReorderAndDeleteFolderItems(const folderItemIDs: Array of Integer);
      constructor Create(const filePath: String);
      function EditFolderItem(const folderItem: TFolderItem): Boolean;
      procedure InvalidateFolderItems();
      procedure AddExclusion(const filePath: String);
      property Exclusions: TStringList read GetExclusions write SetExclusions;


  end;


const

	DEFAULT_SETTINGS: Array[0..4] of Array[0..1] of String = (
		('PortableAppsPath', 'd:\temp\Clef USB\PortableApps SMALL'),
    ('DocumentsPath', 'd:\temp\test portableapp\Documents'),
    ('Locale', 'en'),
    ('IsFirstFolderItemRefresh', 'true'),
    ('FolderItemExclusions', '')
  );


implementation


uses Main, EditFolderItemUnit;



// *****************************************************************************
//
// TFolderItem Class
//
// *****************************************************************************

constructor TFolderItem.Create();
begin
	IsSeparator := false;
	pUniqueID := pUniqueID + 1;
	ID := pUniqueID;
end;


procedure TFolderItem.ClearCachedIcons();
begin
	if pSmallIcon = nil then Exit;
  FreeAndNil(pSmallIcon);
end;


function TFolderItem.GetSmallIcon(): TIcon;
begin
	result := nil;

	if IsSeparator then Exit;

	if pSmallIcon <> nil then begin
  	result := pSmallIcon;
    Exit;
  end;

  result := GetFolderItemIcon(FilePath, true);
end;


class function TFolderItem.ResolveFilePath(const filePath: String):String;
begin
	// TODO: Convert special variables
	result := filePath;
end;


function TFolderItem.GetResolvedFilePath():String;
begin
	result := ResolveFilePath(FilePath);
end;






// *****************************************************************************
//
// TUser Class
//
// *****************************************************************************

constructor TUser.Create(const filePath: String);
var eRoot: IXMLDOMElement;
  success: Boolean;
  eFolderItems, eFolderItem: IXMLDOMElement;
  i, j: Integer;
  folderItem: TFolderItem;
begin
	pFolderItems := TObjectList.Create(false);

  pSaveFolderItemsFlag := false;

  pSpecialFolderNames.Music := 'Music';
  pSpecialFolderNames.Videos := 'Videos';
  pSpecialFolderNames.Pictures := 'Pictures';
  pSpecialFolderNames.Documents := 'Documents';

	pFilePath := filePath;
  success := false;

 	pXML := CreateOleObject('Microsoft.XMLDOM') as IXMLDomDocument;
  if FileExists(filePath) then begin
  	success := pXML.Load(filePath);
    if success then begin
    	ilog('Settings loaded successfully from: ' + pFilePath);
      ilog('Creating folder items...');

      for i := 0 to pXML.documentElement.childNodes.length - 1 do begin
        eFolderItems := pXML.documentElement.childNodes.item[i] as IXMLDOMElement;
        if eFolderItems.nodeName <> 'FolderItems' then continue;

        for j := 0 to eFolderItems.childNodes.length - 1 do begin
          eFolderItem := eFolderItems.childNodes.item[j] as IXMLDOMElement;
          if eFolderItem.nodeName <> 'FolderItem' then continue;

          folderItem := TFolderItem.Create();
          folderItem.Name := eFolderItem.getAttribute('name');
          folderItem.FilePath := eFolderItem.getAttribute('filePath');
          folderItem.IsSeparator := eFolderItem.getAttribute('isSeparator') = 'true';

          pFolderItems.Add(TObject(folderItem));
        end;

        break;
      end;

    end else begin
    	elog('Coulnd''t load settings from: ' + pFilePath);
    end;
  end;

  if not success then begin
  	ilog('Creating new setting file...');
  	eRoot := pXML.createElement('MiniLaunchBar');
    eRoot.setAttribute('version', '1.0');
    pXML.appendChild(eRoot);
  end;
end;


procedure TUser.InvalidateFolderItems();
begin
  pSaveFolderItemsFlag := true;
  ScheduleSave();
end;


procedure TUser.SetExclusions(const value: TStringList);
var s: String;
	i: Integer;
begin
	s := '';
	for i := 0 to value.Count - 1 do begin
  	if s <> '' then s := s + ',';
    s := s + value[i];
  end;
  
  SetUserSetting('FolderItemExclusions', s);
end;


function TUser.GetExclusions(): TStringList;
begin
  result := SplitString(',', GetUserSetting('FolderItemExclusions'));
end;


procedure TUser.AddExclusion(const filePath: String);
var exclusions: TStringList;
	i: Integer;
begin
	exclusions := GetExclusions();

  for i := 0 to exclusions.Count - 1 do begin
  	if TFolderItem.ResolveFilePath(exclusions[i]) = TFolderItem.ResolveFilePath(filePath) then begin
    	ilog('This file path is already in the exclusion list: ' + filePath);
      Exit;
    end;
  end;

  exclusions.Add(filePath);

  SetExclusions(exclusions);  
end;


procedure TUser.ReorderAndDeleteFolderItems(const folderItemIDs: Array of Integer);
var newFolderItems: TObjectList;
	i, j: Integer;
	folderItem: TFolderItem;
  doIt, foundIt: Boolean;
begin
	doIt := false;

  // ---------------------------------------------------------------------------
  // Check if something needs to be updated
  // ---------------------------------------------------------------------------
	if Length(folderItemIDs) <> pFolderItems.Count then begin
  	// If the number of folder items is different from the number of
    // provided IDs - do the update
  	doIt := true;
  end else begin
  	// If the order of the folder items is different from the order
    // of the provided folder IDs - do the update
    for i := 0 to Length(folderItemIDs) - 1 do begin
      if folderItemIDs[i] <> TFolderItem(pFolderItems[i]).ID then begin
        doIt := true;
        break;
      end;
    end;
  end;

  // Quick exit if there's nothing to do
  if not doIt then Exit;

  // Create the list that is going to hold the new folder items
	newFolderItems := TObjectList.Create(false);

  // ---------------------------------------------------------------------------
  // Check if we need to delete some of the folder items (i.e. those that
  // are missing from the list of IDs)
  // ---------------------------------------------------------------------------
  for i := pFolderItems.Count - 1 downto 0 do begin
  	folderItem := TFolderItem(pFolderItems[i]);
		foundIt := false;
  	for j := 0 to Length(folderItemIDs) - 1 do begin
    	if folderItem.ID = folderItemIDs[j] then begin
      	foundIt := true;
        break;
      end;
    end;

    if not foundIt then begin
    	// If we couldn't find it, remove the folder item from the list
    	folderItem.Free();
      pFolderItems.Remove(TObject(folderItem));
    end;
  end;

  // ---------------------------------------------------------------------------
  // Update the order of the folder items
  // ---------------------------------------------------------------------------
	for i := 0 to Length(folderItemIDs) - 1 do begin
  	folderItem := GetFolderItemByID(folderItemIDs[i]);
    if folderItem = nil then begin
    	wlog('TUser.SetFolderItemsOrder: Unknown folder item ID: ' + IntToStr(folderItemIDs[i]));
      continue;
    end;

    newFolderItems.Add(TObject(folderItem));
  end;

  pFolderItems.Clear();
  pFolderItems := newFolderItems;

  pSaveFolderItemsFlag := true;
  ScheduleSave();
end;


function TUser.EditFolderItem(const folderItem: TFolderItem): Boolean;
var form: TEditFolderItemForm;
begin
	if pEditFolderItemForm = nil then begin
  	pEditFolderItemForm := TObject(TEditFolderItemForm.Create(TMain.Instance.mainForm));
  end;

  form := TEditFolderItemForm(pEditFolderItemForm);

  form.Left := Round(Screen.Width / 2 - form.Width / 2);
  form.Top := Round(Screen.Height / 2 - form.Height / 2);
  form.LoadFolderItem(folderItem);
  form.ShowModal();

  if form.SaveButtonClicked then begin
  	folderItem.ClearCachedIcons();
    InvalidateFolderItems();
  end;

  result := form.SaveButtonClicked;
end;


function TUser.GetFolderItemAt(const iIndex: Word): TFolderItem;
begin
	result := TFolderItem(pFolderItems[iIndex]);
end;


function TUser.GetFolderItemByID(const iFolderItemID: Integer): TFolderItem;
var i: Word;
	folderItem: TFolderItem;
begin
	result := nil;
	for i := 0 to pFolderItems.Count - 1 do begin
  	folderItem := TFolderItem(pFolderItems[i]);
    if folderItem.ID = iFolderItemID then begin
      result := folderItem;
      break;
    end;
  end;
end;


function TUser.FolderItemCount(): Word;
begin
	result := pFolderItems.Count;
end;


procedure TUser.RefreshFolderItems();
var
	directoryContents: TStringList;
  i, j: Integer;
  filePath: String;
	fileExtension: String;
  folderItem: TFolderItem;
  exclusions: TStringList;
  skipIt: Boolean;
  changeFlag: Boolean;
  documentFolderContent: TStringList;
  isFirstFolderItemRefresh: Boolean;
  documentPath: String;
  versionInfo: TVersionInfo;
begin
	ilog('Refreshing folder items...');

  // Set this flag to true whenever the FolderItems list is modified
  changeFlag := false;

  // ---------------------------------------------------------------------------
  // Get the list of excluded folder items
  // ---------------------------------------------------------------------------

  //SetUserSetting('FolderItemExclusions', '');
  exclusions := SplitString(',', GetUserSetting('FolderItemExclusions'));

  // ---------------------------------------------------------------------------
  // Check if it's the first time that the application is launched
  // ---------------------------------------------------------------------------

  isFirstFolderItemRefresh := GetUserSetting('IsFirstFolderItemRefresh') = 'true';
  if isFirstFolderItemRefresh then SetUserSetting('IsFirstFolderItemRefresh', 'false');

  // ---------------------------------------------------------------------------
  // Remove files that no longer exist
  // ---------------------------------------------------------------------------

//	for i := pFolderItems.Count - 1 downto 0 do begin
//  	folderItem := TFolderItem(pFolderItems[i]);
//    if folderItem.IsSeparator then continue;
//    if (not FileExists(folderItem.FilePath)) and (not DirectoryExists(folderItem.FilePath)) then begin
//      wlog('Folder item doesn''t exist - removing it from the list: ' + folderItem.FilePath);
//      pFolderItems.Remove(TObject(folderItem));
//      changeFlag := true;
//    end;
//  end;

	// ---------------------------------------------------------------------------
  // Get content of PortableApps folder. We only lookup for executable files
  // in the subfolders of the PortableApps folder
  // ---------------------------------------------------------------------------

	directoryContents := GetDirectoryContents(GetUserSetting('PortableAppsPath'), 1, 'exe');

	// ---------------------------------------------------------------------------
  // Check if the document folder exists and if so, gets its subfolders
  // and add them to the list of folder items to process
  // ---------------------------------------------------------------------------

  documentPath := GetUserSetting('DocumentsPath');

  if DirectoryExists(documentPath) then begin
    documentFolderContent := GetDirectoryContents(GetUserSetting('DocumentsPath'), 0, '*');

    directoryContents.Add(GetUserSetting('DocumentsPath'));

    for i := 0 to documentFolderContent.Count - 1 do begin
      if not DirectoryExists(documentFolderContent[i]) then continue;
      directoryContents.Add(documentFolderContent[i]);
    end;
  end;

	// ---------------------------------------------------------------------------
  // Process the list of folders and files, creating FolderItems objects
  // as needed.
  // ---------------------------------------------------------------------------

  for i := 0 to directoryContents.Count - 1 do begin
    filePath := directoryContents[i];
    fileExtension := ExtractFileExt(filePath);

    if (isFirstFolderItemRefresh) and (filePath = GetUserSetting('DocumentsPath')) then begin
    	// The first time the application is launched, we add a separator between
      // the executables and the document folders. The separator can be removed
      // and won't be added.
    	ilog('First launch - adding document separator...');
    	folderItem := TFolderItem.Create();
      folderItem.IsSeparator := true;
      pFolderItems.Add(TObject(folderItem));
      changeFlag := true;
    end;

    // Set this to true to prevent a folder item from being created
    // for the current file or folder
    skipIt := false;

    // Check if the file or folder is already in the FolderItems list
    for j := 0 to pFolderItems.Count - 1 do begin
      folderItem := TFolderItem(pFolderItems[j]);
      if folderItem.ResolvedFilePath = TFolderItem.ResolveFilePath(filePath) then begin
        skipIt := true;
        break;
      end;
    end;

    // Check if the file or folder is in the exclusion list
    for j := 0 to exclusions.Count - 1 do begin
      if AnsiPos(exclusions[j], filePath) >= 1 then begin
        skipIt := true;
        break;
      end;
    end;

    if not skipIt then begin
    	// Creating the folder item and adding it to the list
      ilog('Adding new folder item: ' + filePath);

      folderItem := TFolderItem.Create();

      folderItem.Name := ExtractFileName(filePath);

      try
      	versionInfo := TVersionInfo.CreateFile(filePath);
        if versionInfo.FileDescription <> '' then begin
        	folderItem.Name := versionInfo.FileDescription;
        end;
      finally
      	FreeAndNil(versionInfo);
      end;    

      folderItem.filePath := filePath;
      pFolderItems.Add(TObject(folderItem));
      changeFlag := true;
    end;
    
  end;

  // ---------------------------------------------------------------------------
  // Loop through all the folder items and remove those that are in the
  // exclusion list.
  // ---------------------------------------------------------------------------

	for i := pFolderItems.Count - 1 downto 0 do begin
  	folderItem := TFolderItem(pFolderItems[i]);
    if folderItem.IsSeparator then continue;

    for j := 0 to exclusions.Count - 1 do begin
      if AnsiPos(exclusions[j], folderItem.FilePath) >= 1 then begin
      	ilog('Folder item in exclusion list, so removing it: ' + folderItem.FilePath);
        pFolderItems.Remove(TObject(folderItem));
        changeFlag := true;
      end;
    end;

  end;

  // ---------------------------------------------------------------------------
  // If the FolderItems list has been changed, schedule a save operation
  // ---------------------------------------------------------------------------
	if changeFlag then begin
    pSaveFolderItemsFlag := true;
    ScheduleSave();
  end;
end;


procedure TUser.ScheduleSave_Timer(Sender: TObject);
begin
  Save();
end;


procedure TUser.ScheduleSave();
begin
	if pSaveTimer = nil then begin
  	pSaveTimer := TTimer.Create(nil);
    pSaveTimer.Interval := 1000;
    pSaveTimer.OnTimer := ScheduleSave_Timer;
  end;

  pSaveTimer.Enabled := true;
end;


procedure TUser.Save();
var eFolderItems, eFolderItem: IXMLDOMElement;
	i: Integer;
  folderItem: TFolderItem;
begin
  ilog('Saving user settings to: ' + pFilePath);
	if pSaveTimer <> nil then pSaveTimer.Enabled := false;

  if pSaveFolderItemsFlag then begin
  	ilog('Updating XML for folder items...');

    for i := pXML.documentElement.childNodes.length - 1 downto 0 do begin
      eFolderItems := pXML.documentElement.childNodes.item[i] as IXMLDOMElement;
      if eFolderItems.nodeName = 'FolderItems' then begin
      	pXML.documentElement.removeChild(eFolderItems);
      end;
    end;

    eFolderItems := pXML.createElement('FolderItems');
    pXML.documentElement.appendChild(eFolderItems);

    for i := 0 to pFolderItems.Count - 1 do begin
    	folderItem := TFolderItem(pFolderItems[i]);

      eFolderItem := pXML.createElement('FolderItem');
      eFolderItems.appendChild(eFolderItem);

      eFolderItem.setAttribute('name', folderItem.Name);
      eFolderItem.setAttribute('filePath', folderItem.FilePath);
      eFolderItem.setAttribute('isSeparator', StringConv(folderItem.IsSeparator));
    end;

  	pSaveFolderItemsFlag := false;
  end;

  pXML.save(pFilePath);
end;


function TUser.GetUserSetting(const name: String): String;
var eSetting, eSettings: IXMLDOMElement;
	i, j: Integer;
begin
	for i := 0 to pXML.documentElement.childNodes.length - 1 do begin
  	eSettings := pXML.documentElement.childNodes.item[i] as IXMLDOMElement;
  	if eSettings.nodeName = 'Settings' then begin
      for j := 0 to eSettings.childNodes.length - 1 do begin
      	eSetting := eSettings.childNodes.item[j] as IXMLDOMElement;
        if eSetting.getAttribute('name') = name then begin
          result := eSetting.text;
          Exit;
        end;
      end;

      break;
    end;
  end;

  for i := 0 to Length(DEFAULT_SETTINGS) - 1 do begin
  	if DEFAULT_SETTINGS[i][0] = name then begin
    	result := DEFAULT_SETTINGS[i][1];
      Exit;
    end;
  end;

  result := '';
end;


procedure TUser.SetUserSetting(const name: String; const value: String);
var eSetting, eSettings, eElement: IXMLDOMElement;
	i, j: Integer;
  done: Boolean;
begin
	done := false;
  eSettings := nil;

	for i := 0 to pXML.documentElement.childNodes.length - 1 do begin
  	eElement := pXML.documentElement.childNodes.item[i] as IXMLDOMElement;

  	if eElement.nodeName = 'Settings' then begin

    	eSettings := eElement;
    
      for j := 0 to eSettings.childNodes.length - 1 do begin
      	eSetting := eSettings.childNodes.item[j] as IXMLDOMElement;
        if eSetting.getAttribute('name') = name then begin
        	if eSetting.text = value then Exit;
          eSetting.text := value;
          done := true;
          break;
        end;

      end;

      break;
    end;
  end;

  if not done then begin
  	if eSettings = nil then begin
    	eSettings := pXML.createElement('Settings');
      pXML.documentElement.appendChild(eSettings);
    end;

    eSetting := pXML.createElement('Setting');
    eSetting.setAttribute('name', name);
    eSetting.text := value;
    eSettings.appendChild(eSetting);
  end;

  ScheduleSave();
end;


end.
