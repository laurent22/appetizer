/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <IconUtil.h>
using namespace appetizer;

QMutex getFolderItemIcon_mutex;


IconData* IconUtil::getFolderItemIcon(const QString& filePath, int iconSize) {
  QMutexLocker locker(&getFolderItemIcon_mutex);

  IconData* output = NULL;

  // Note: certain functions, like SHGetImageList don't exist in Windows 2000,
  // so we need to load them dynamically, otherwise we get this error and the app doesn't start:
  // "The ordinal 737 could not be located in the dynamic link library Shell32.dll"

  HINSTANCE shell32Library_ = LoadLibrary(_T("SHELL32.DLL"));
  SHGetImageListType SHGetImageListFunction_ = (SHGetImageListType)GetProcAddress(shell32Library_, "SHGetImageList");

  if (shell32Library_ && SHGetImageListFunction_) {

    // Get the icon index using SHGetFileInfo
    SHFILEINFOW sfi = {0};

    SHGetFileInfo(filePath.toStdWString().c_str(), -1, &sfi, sizeof(sfi), SHGFI_SYSICONINDEX);

    // If iIcon is 0, we get a weird default icon representing a hand,
    // so don't continue.
    if (sfi.iIcon > 0) {
      // Retrieve the system image list.
      // To get the 48x48 icons, use SHIL_EXTRALARGE
      // To get the 256x256 icons (Vista only), use SHIL_JUMBO
      HIMAGELIST* imageList;
      int imageType;
      if (iconSize == 16) {
        imageType = SHIL_SMALL;
      } else if (iconSize == 48) {
        imageType = SHIL_EXTRALARGE;
      } else if (iconSize == 256) {
        imageType = SHIL_JUMBO;
      } else {
        imageType = SHIL_LARGE; // 32
      }
      HRESULT hResult = SHGetImageListFunction_(imageType, IID_IImageList, (void**)&imageList);

      if (hResult == S_OK) {
        // Get the icon we need from the list. Note that the HIMAGELIST we retrieved
        // earlier needs to be casted to the IImageList interface before use.
        HICON hIcon;
        hResult = ((IImageList*)imageList)->GetIcon(sfi.iIcon, ILD_TRANSPARENT, &hIcon);

        if (hResult == S_OK) {
          output = new IconData();
          output->hIcon = hIcon;
          output->filePath = filePath;
          output->index = 0;
        }
      }

    }

  }

  if (!output) {
    QFileInfo fileInfo(filePath);
    QString extension = fileInfo.suffix().toLower();
    if (extension == "exe" || extension == "ico") {
      return IconUtil::getExecutableIcon(filePath, iconSize);
    } else {

    }
  }

  return output;
}


IconData* IconUtil::getExecutableIcon(const QString& filePath, int iconSize, int iconIndex) {  
  #ifdef __WINDOWS__

  wchar_t filePathPtr[MAX_PATH];
  filePath.toWCharArray(filePathPtr);
  filePathPtr[filePath.length()] = 0;
  //std::auto_ptr<wchar_t> filePathW(filePathPtr);

  if (iconSize <= 32) {
    HICON smallIcon;
    HICON largeIcon;
    int result;

    if (iconSize == 16) {
      result = ExtractIconEx(filePathPtr, iconIndex, NULL, &smallIcon, 1);	
    } else {
      result = ExtractIconEx(filePathPtr, iconIndex, &largeIcon, NULL, 1);	
    }

    // If the function succeeds, the return value is the handle to an icon.
    // If the file specified was not an executable file, DLL, or icon file,
    // the return value is 1. If no icons were found in the file, the return 
    // value is NULL. If the file didn't exist, the return value is < 0
    if (result > 0) {
      IconData* iconData = new IconData();
      iconData->hIcon = smallIcon ? smallIcon : largeIcon;
      iconData->filePath = filePath;
      iconData->index = iconIndex;

      return iconData;
    }

  } else {

    HINSTANCE hDll = ::LoadLibrary(filePathPtr);
    if (hDll) {
      HANDLE handle = ::LoadImage(hDll, MAKEINTRESOURCE(iconIndex), IMAGE_ICON, iconSize, iconSize, LR_LOADTRANSPARENT);
      if (handle) {
        IconData* iconData = new IconData();
        iconData->hIcon = (HICON)handle;
        iconData->filePath = filePath;
        iconData->index = iconIndex;
        // TODO: Check that the index is valid
        return iconData;
      }
    }

    return IconUtil::getExecutableIcon(filePath, 32, iconIndex);
  }

  #endif // __WINDOWS__

  return NULL;
}


QPixmap* IconUtil::iconDataToPixmap(IconData* iconData) {
  QPixmap* output = NULL;
  QPixmap tempPixmap = QPixmap::fromWinHICON(iconData->hIcon);

  if (tempPixmap.width() > 48) {
    const QImage image = tempPixmap.toImage();

    bool isBadIcon = true;
    for (int i = 0; i < 100; i++) {
      int pixelX = rand() % image.width();
      int pixelY;
      if (pixelX < 48) {
        pixelY = rand() % (image.height() - 48) + 48;
      } else {
        pixelY = rand() % image.height();
      }

      QRgb rgb = image.pixel(pixelX, pixelY);
      int alpha = qAlpha(rgb);

      if (alpha > 0) {
        isBadIcon = false;
        break;
      }
    }

    if (isBadIcon) {
      output = new QPixmap(tempPixmap.copy(0,0,48,48));
    } else {
      output = new QPixmap(tempPixmap);
    }

  } else {
    output = new QPixmap(tempPixmap);
  }

  return output;
}