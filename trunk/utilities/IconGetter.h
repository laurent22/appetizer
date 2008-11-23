/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __IconGetter_H
#define __IconGetter_H


WX_DECLARE_HASH_MAP(int, wxIcon*, wxIntegerHash, wxIntegerEqual, IconGetterIconHashMap);
WX_DECLARE_HASH_MAP(wxString, wxIcon*, wxStringHash, wxStringEqual, IconGetterTypeIconHashMap);


// library function is: HRESULT SHGetImageList(int iImageList, REFIID riid, void **ppv)
typedef HRESULT (CALLBACK* SHGetImageListType)(int, const IID&, void*);


/**
 * Some static functions to get the icons associated with a file, folder or executable.
 * Each function can return a small or large icon
 */
class IconGetter {

  public:

    /**
     * Gets the icon for the given folder item (folder, file, executable, etc.). This
     * is the only function that should be needed in this class, since it automatically calls
     * the other functions depending on the folder item type
     * @param filePath The file path
     * @param iconSize The icon size (only 16 and 32 are currently supported)
     * @return The icon or wxNullIcon in case of error
     */
    static wxIcon* GetFolderItemIcon(const wxString& filePath, int iconSize = 32, bool alwaysReturnDefault = false);

    /**
     * Gets the (first) icon within an executable, dll or ico file
     * @param filePath The file path
     * @param iconSize The icon size (only 16 and 32 are currently supported)
     * @return The icon or wxNullIcon in case of error
     */
    static wxIcon* GetExecutableIcon(const wxString& filePath, int iconSize = 32, int iconIndex = 0);
    
    /**
     * Gets the icon of a non-executable file, or a default icon if the file
     * doesn't have an icon
     * @param filePath The file path
     * @param iconSize The icon size (only 16 and 32 are currently supported)
     * @return The icon or wxNullIcon in case of error
     */
    static wxIcon* GetDocumentIcon(const wxString& filePath, int iconSize = 32);
    
    /**
     * Gets the icon of a folder. It makes use of Desktop.ini if available.
     * @param filePath The file path
     * @param iconSize The icon size (only 16 and 32 are currently supported)
     * @return The icon or wxNullIcon in case of error
     */
    static wxIcon* GetFolderIcon(const wxString& filePath, int iconSize = 32);


    static wxString GetSystem32Path();
    static wxIcon* GetDefaultFileIcon(int iconSize);
    static wxIcon* GetDefaultFolderIcon(int iconSize);
    static wxIcon* GetDefaultTypeIcon(int iconSize, const wxString& extension);

    static void Destroy();


private:

  static wxString system32Path_;
  static SHGetImageListType SHGetImageListFunction_;
  static HINSTANCE shell32Library_;
  static IconGetterIconHashMap defaultFileIcon_;
  static IconGetterIconHashMap defaultFolderIcon_;
  static IconGetterTypeIconHashMap defaultDefaultTypeIcon_;

};

#endif // __IconGetter_H