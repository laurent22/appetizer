/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "FileExplorerControl.h"
#include "../utilities/IconGetter.h"
#include "../utilities/StringUtil.h"


// Required to get OnCompareItems() working
IMPLEMENT_DYNAMIC_CLASS(FileExplorerControl, wxTreeCtrl)


BEGIN_EVENT_TABLE(FileExplorerControl, wxTreeCtrl)
  EVT_TREE_ITEM_EXPANDING(wxID_ANY, FileExplorerControl::OnItemExpanding) 
END_EVENT_TABLE()





FileExplorerControlItemData::FileExplorerControlItemData(const wxString& filePath, bool isDirectory) {
  path_ = wxString(filePath);
  isDirectory_ = isDirectory;
}


void FileExplorerControl::ExpandDirectory(const wxString& directory) {
  Freeze();

  wxFileName dir(directory);

  wxArrayString splitted;
  StringUtil::Split(dir.GetFullPath(), splitted, wxFileName::GetPathSeparator());

  wxTreeItemId currentItemId = rootId_;
  wxTreeItemId child;

  for (int i = 0; i < splitted.Count(); i++) {
    wxString n = splitted[i].Lower();

    wxTreeItemIdValue cookie;
    child = GetFirstChild(currentItemId, cookie);

    while (child.IsOk()) {
      
      if (GetItemText(child).Lower() == n) {
        currentItemId = child;
        PopulateFolder(currentItemId);
        Expand(currentItemId);
        break;
      }

      child = GetNextChild(currentItemId, cookie);
    }

  }

  if (child.IsOk()) SelectItem(child, true);

  Thaw();
}


wxString FileExplorerControlItemData::GetPath() {
  return path_;
}


bool FileExplorerControlItemData::IsDirectory() {
  return isDirectory_;
}


FileExplorerControl::FileExplorerControl()
: wxTreeCtrl(NULL, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTR_HAS_BUTTONS | wxTR_HIDE_ROOT, wxDefaultValidator, _T("fileExplorerControl"))
{} // Default constuctor to make IMPLEMENT_DYNAMIC_CLASS happy


FileExplorerControl::FileExplorerControl(wxWindow* parent, wxWindowID id, const wxPoint& pos, const wxSize& size, long style, const wxValidator& validator, const wxString& name)
:wxTreeCtrl(parent, id, pos, size, style, validator, name) {
  imageList_ = NULL;
}


FileExplorerControl::~FileExplorerControl() {
  wxDELETE(imageList_);
}


void FileExplorerControl::SetRootPath(const wxString& rootPath) {
  if (rootPath_ == rootPath) return;

  rootPath_ = rootPath;

  DeleteAllItems();
  SetImageList(NULL);
  wxDELETE(imageList_);

  rootId_ = AddRoot(_T("HiddenRoot"));
  SetItemData(rootId_, new FileExplorerControlItemData(rootPath, true));

  PopulateFolder(rootId_);
}


// ----------------------------------------------------------------------------
// wxGetAvailableDrives, for WINDOWS, DOS, OS2, MAC, UNIX (returns "/")
// ----------------------------------------------------------------------------

size_t FileExplorerControl::wxGetAvailableDrives(wxArrayString &paths, wxArrayString &names, wxArrayInt &icon_ids)
{
#if defined(__WINDOWS__) || defined(__DOS__) || defined(__OS2__)

#ifdef __WXWINCE__
    // No logical drives; return ""
    paths.Add(wxT(""));
    names.Add(wxT(""));
    icon_ids.Add(wxFileIconsTable::computer);
#elif defined(__WIN32__)
    wxChar driveBuffer[256];
    size_t n = (size_t) GetLogicalDriveStrings(255, driveBuffer);
    size_t i = 0;
    while (i < n)
    {
        wxString path, name;
        path.Printf(wxT("%c:"), driveBuffer[i]);
        name.Printf(wxT("%c:"), driveBuffer[i]);

        // Do not use GetVolumeInformation to further decorate the
        // name, since it can cause severe delays on network drives.

        int imageId;
        int driveType = ::GetDriveType(path);
        switch (driveType)
        {
            case DRIVE_REMOVABLE:
                if (path == wxT("a:") || path == wxT("b:"))
                    imageId = wxFileIconsTable::floppy;
                else
                    imageId = wxFileIconsTable::removeable;
                break;
            case DRIVE_CDROM:
                imageId = wxFileIconsTable::cdrom;
                break;
            case DRIVE_REMOTE:
            case DRIVE_FIXED:
            default:
                imageId = wxFileIconsTable::drive;
                break;
        }

        paths.Add(path);
        names.Add(name);
        icon_ids.Add(imageId);

        while (driveBuffer[i] != wxT('\0'))
            i ++;
        i ++;
        if (driveBuffer[i] == wxT('\0'))
            break;
    }
#elif defined(__OS2__)
    APIRET rc;
    ULONG ulDriveNum = 0;
    ULONG ulDriveMap = 0;
    rc = ::DosQueryCurrentDisk(&ulDriveNum, &ulDriveMap);
    if ( rc == 0)
    {
        size_t i = 0;
        while (i < 26)
        {
            if (ulDriveMap & ( 1 << i ))
            {
                wxString path, name;
                path.Printf(wxT("%c:"), 'A' + i);
                name.Printf(wxT("%c:"), 'A' + i);

                // Note: If _filesys is unsupported by some compilers,
                //       we can always replace it by DosQueryFSAttach
                char filesysname[20];
#ifdef __WATCOMC__
                ULONG cbBuffer = sizeof(filesysname);
                PFSQBUFFER2 pfsqBuffer = (PFSQBUFFER2)filesysname;
                APIRET rc = ::DosQueryFSAttach(name.fn_str(),0,FSAIL_QUERYNAME,pfsqBuffer,&cbBuffer);
                if (rc != NO_ERROR)
                {
                    filesysname[0] = '\0';
                }
#else
                _filesys(name.fn_str(), filesysname, sizeof(filesysname));
#endif
                /* FAT, LAN, HPFS, CDFS, NFS */
                int imageId;
                if (path == wxT("A:") || path == wxT("B:"))
                    imageId = wxFileIconsTable::floppy;
                else if (!strcmp(filesysname, "CDFS"))
                    imageId = wxFileIconsTable::cdrom;
                else if (!strcmp(filesysname, "LAN") ||
                         !strcmp(filesysname, "NFS"))
                    imageId = wxFileIconsTable::drive;
                else
                    imageId = wxFileIconsTable::drive;
                paths.Add(path);
                names.Add(name);
                icon_ids.Add(imageId);
            }
            i ++;
        }
    }
#else // !__WIN32__, !__OS2__
    int drive;

    /* If we can switch to the drive, it exists. */
    for( drive = 1; drive <= 26; drive++ )
    {
        wxString path, name;
        path.Printf(wxT("%c:"), (char) (drive + 'a' - 1));
        name.Printf(wxT("%c:"), (char) (drive + 'A' - 1));

        if (wxIsDriveAvailable(path))
        {
            paths.Add(path);
            names.Add(name);
            icon_ids.Add((drive <= 2) ? wxFileIconsTable::floppy : wxFileIconsTable::drive);
        }
    }
#endif // __WIN32__/!__WIN32__

#elif defined(__WXMAC__)

    ItemCount volumeIndex = 1;
    OSErr err = noErr ;

    while( noErr == err )
    {
        HFSUniStr255 volumeName ;
        FSRef fsRef ;
        FSVolumeInfo volumeInfo ;
        err = FSGetVolumeInfo(0, volumeIndex, NULL, kFSVolInfoFlags , &volumeInfo , &volumeName, &fsRef);
        if( noErr == err )
        {
            wxString path = wxMacFSRefToPath( &fsRef ) ;
            wxString name = wxMacHFSUniStrToString( &volumeName ) ;

            if ( (volumeInfo.flags & kFSVolFlagSoftwareLockedMask) || (volumeInfo.flags & kFSVolFlagHardwareLockedMask) )
            {
                icon_ids.Add(wxFileIconsTable::cdrom);
            }
            else
            {
                icon_ids.Add(wxFileIconsTable::drive);
            }
            // todo other removable

            paths.Add(path);
            names.Add(name);
            volumeIndex++ ;
        }
    }

#elif defined(__UNIX__)
    paths.Add(wxT("/"));
    names.Add(wxT("/"));
    icon_ids.Add(wxFileIconsTable::computer);
#else
    #error "Unsupported platform in wxGenericDirCtrl!"
#endif
    wxASSERT_MSG( (paths.GetCount() == names.GetCount()), wxT("The number of paths and their human readable names should be equal in number."));
    wxASSERT_MSG( (paths.GetCount() == icon_ids.GetCount()), wxT("Wrong number of icons for available drives."));
    return paths.GetCount();
}


void FileExplorerControl::PopulateFolder(const wxTreeItemId& itemId) {
  if (GetChildrenCount(itemId) > 0) return; // Been done already

  FileExplorerControlItemData* itemObject = GetItemObject(itemId);
  int iconSize = 16;   

  if (!imageList_) imageList_ = new wxImageList(iconSize, iconSize);

  if (itemObject->GetPath() == _T("MyComputer")) {

    wxArrayString paths;
    wxArrayString names;
    wxArrayInt iconIds;
    wxGetAvailableDrives(paths, names, iconIds);

    wxImageList* driveImageList = wxTheFileIconsTable->GetSmallImageList();

    for (int i = 0; i < paths.Count(); i++) {
      wxString path = paths[i];
      wxTreeItemId childId;

      wxIcon icon = driveImageList->GetIcon(iconIds[i]);

      if (icon.IsOk()) {
        int iconIndex = imageList_->Add(icon);
        childId = AppendItem(itemId, path, iconIndex, -1, new FileExplorerControlItemData(path, true));
      } else {
        childId = AppendItem(itemId, path, -1, -1, new FileExplorerControlItemData(path, true));
      }

      SetItemHasChildren(childId, true);
    }

  } else {

    wxDir folder;

    if (!wxFileName::DirExists(itemObject->GetPath()) || !folder.Open(itemObject->GetPath())) return;

    wxString folderItemName;
    bool success = folder.GetFirst(&folderItemName, wxALL_FILES_PATTERN, wxDIR_FILES | wxDIR_DIRS);
    int i = 0;    

    while (success) {
      wxString filePath = itemObject->GetPath() + wxFileName::GetPathSeparator() + folderItemName;
      bool isDirectory = wxFileName::DirExists(filePath);
      wxTreeItemId childId;

      wxFileName filename(filePath);
      filename.Normalize();

      wxIcon* icon = IconGetter::GetFolderItemIcon(filename.GetFullPath(), 16, true);

      if (icon) {
        int iconIndex = imageList_->Add(*icon);
        wxDELETE(icon);
        childId = AppendItem(itemId, folderItemName, iconIndex, -1, new FileExplorerControlItemData(filePath, isDirectory));
      } else {
        childId = AppendItem(itemId, folderItemName, -1, -1, new FileExplorerControlItemData(filePath, isDirectory));
      }

      SetItemHasChildren(childId, isDirectory);

      success = folder.GetNext(&folderItemName);      
      i++;
    }

    if (i == 0) SetItemHasChildren(itemId, false);
  }

  SetImageList(imageList_);

  SortChildren(itemId);
}


FileExplorerControlItemData* FileExplorerControl::GetItemObject(const wxTreeItemId& itemId) {
  if (!itemId.IsOk()) return NULL;
  return (FileExplorerControlItemData*)(GetItemData(itemId));
}


wxString FileExplorerControl::GetSelectedPath() {
  wxTreeItemId itemId = GetSelection();
  if (!itemId.IsOk()) return wxEmptyString;
  return GetItemObject(itemId)->GetPath();
}


void FileExplorerControl::Localize() {
  
}


int FileExplorerControl::OnCompareItems(const wxTreeItemId& item1, const wxTreeItemId& item2) {
  FileExplorerControlItemData* o1 = GetItemObject(item1);
  FileExplorerControlItemData* o2 = GetItemObject(item2);

  if (!o1 || !o2) return 0;

  wxString filePath1 = o1->GetPath();
  wxString filePath2 = o2->GetPath();

  bool isDir1 = o1->IsDirectory();
  bool isDir2 = o2->IsDirectory();

  if (isDir1 && !isDir2) return -1;
  if (!isDir1 && isDir2) return +1;
  
  return filePath1.Lower() > filePath2.Lower();
}


void FileExplorerControl::OnItemExpanding(wxTreeEvent& evt) {
  PopulateFolder(evt.GetItem());  
}