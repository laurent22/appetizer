/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __IconGetter_H
#define __IconGetter_H

#include "wx/wx.h"

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
    static wxIcon* GetFolderItemIcon(const wxString& filePath, int iconSize = 32);

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

};

#endif // __IconGetter_H