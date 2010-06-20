/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <WinVer.h>

#include "VersionInfo.h"


QString VersionInfo::GetFileDescription(const QString& filePath) {
  QString outputString;

  #ifdef __WINDOWS__

  TCHAR fullpath[MAX_PATH + 10];

  int success;  

  for (int i = 0; i < (int)filePath.length(); i++) {
    fullpath[i] = filePath[i].toAscii();
  }
  fullpath[filePath.size()] = _T('\0');

  TCHAR *str = new TCHAR[_tcslen(fullpath) + 1];
	_tcscpy(str, fullpath);
	DWORD tmp = 0;
	DWORD len = GetFileVersionInfoSize(str, &tmp);
	LPVOID pBlock = new char[len];
	success = GetFileVersionInfo(str, 0, len, pBlock);

  if (success) {
  	
	  TCHAR SubBlock[50];

    UINT cbTranslate;

    // Structure used to store enumerated languages and code pages.

    HRESULT hr;

    struct LANGANDCODEPAGE {
      WORD wLanguage;
      WORD wCodePage;
    } *lpTranslate;

    // Read the list of languages and code pages.

    success = VerQueryValue(pBlock, 
                  TEXT("\\VarFileInfo\\Translation"),
                  (LPVOID*)&lpTranslate,
                  &cbTranslate);

    if (success) {

        // Read the file description for each language and code page.

        for(int i=0; i < (int)(cbTranslate/sizeof(struct LANGANDCODEPAGE)); i++ )
        {
          hr = wsprintf(SubBlock, TEXT("\\StringFileInfo\\%04x%04x\\FileDescription"),
                    lpTranslate[i].wLanguage,
                    lpTranslate[i].wCodePage);
	        if (FAILED(hr))
	        {
            continue;
	        }

          LPVOID lpBuffer;
          UINT dwBytes;
          // Retrieve file description for language and code page "i". 
          int result = VerQueryValue(pBlock, 
                        SubBlock, 
                        &lpBuffer, 
                        &dwBytes); 

          if (result <= 0) continue;

          outputString = QString::fromWCharArray((TCHAR*)lpBuffer);

          break;

        } // for

    } // if success
  
  } // if success

  if (pBlock) delete pBlock; pBlock = NULL;
  if (str) delete str; str = NULL;

  #endif // __WINDOWS__

  return outputString;
}


QString VersionInfo::GetVersionString(const QString& filePath) {
  #ifdef __WINDOWS__

	//Fill the version info
  TCHAR fullpath[MAX_PATH + 10];

  if (filePath == "") {
	  GetModuleFileName(0, fullpath, MAX_PATH + 10);
  } else {
    for (int i = 0; i < filePath.length(); i++) {
      fullpath[i] = filePath[i].toAscii();
    }
    fullpath[filePath.length()] = _T('\0');
  }

	TCHAR *str = new TCHAR[_tcslen(fullpath) + 1];
	_tcscpy(str, fullpath);
	DWORD tmp = 0;
	DWORD len = GetFileVersionInfoSize(str, &tmp);
	LPVOID pBlock = new char[len];
	GetFileVersionInfo(str, 0, len, pBlock);
	LPVOID ptr;
	UINT ptrlen;
			
	// Structure used to store enumerated languages and code pages.
	struct LANGANDCODEPAGE {
		WORD wLanguage;
		WORD wCodePage;
	} *lpTranslate;

	UINT cbTranslate;
			
	// Read the list of languages and code pages.
	if (VerQueryValue(pBlock, 
				_T("\\VarFileInfo\\Translation"),
				(LPVOID*)&lpTranslate,
				&cbTranslate))
	{
	}
	QString version;
 
	//Format the versionstring
	if (VerQueryValue(pBlock, _T("\\"), &ptr, &ptrlen)) {
		VS_FIXEDFILEINFO *fi = (VS_FIXEDFILEINFO*)ptr;
    QTextStream(&version) << HIWORD(fi->dwFileVersionMS) << "." << LOWORD(fi->dwFileVersionMS) << "." << HIWORD(fi->dwFileVersionLS) << "." << LOWORD(fi->dwFileVersionLS);
  }

	delete [] str;
	delete [] pBlock;

	return version;

  #endif // __WINDOWS__

  return "";
}