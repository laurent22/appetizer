/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "VersionInfo.h"


wxString VersionInfo::GetFileDescription(const wxString& filePath) {
  wxString outputString;

  #ifdef __WINDOWS__

  TCHAR fullpath[MAX_PATH + 10];

  int success;  

  const wxChar* filePathChars = filePath.c_str();  
  for (int i = 0; i < filePath.Len(); i++) {
    fullpath[i] = filePathChars[i];
  }
  fullpath[filePath.Len()] = _T('\0');

  TCHAR *str = new TCHAR[_tcslen(fullpath) + 1];
	_tcscpy(str, fullpath);
	DWORD tmp = 0;
	DWORD len = GetFileVersionInfoSize(str, &tmp);
	LPVOID pBlock = new char[len];
	success = GetFileVersionInfo(str, 0, len, pBlock);

  if (success) {

	  LPVOID ptr;
	  UINT ptrlen;
  	
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

        for(int i=0; i < (cbTranslate/sizeof(struct LANGANDCODEPAGE)); i++ )
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

          outputString = wxString((TCHAR*)lpBuffer, wxConvUTF8);

          break;

        } // for

    } // if success
  
  } // if success

  wxDELETE(pBlock);
  wxDELETE(str);

  #endif // __WINDOWS__

  if (outputString == wxEmptyString) {
    wxFileName filename(filePath);
    return filename.GetName(); 
  }

  return outputString;
}


wxString VersionInfo::GetVersionString(const wxString& filePath) {
  #ifdef __WINDOWS__

	//Fill the version info
  TCHAR fullpath[MAX_PATH + 10];

  if (filePath == wxEmptyString) {
	  GetModuleFileName(0, fullpath, MAX_PATH + 10);
  } else {
    const wxChar* filePathChars = filePath.c_str();  
    for (int i = 0; i < filePath.Len(); i++) {
      fullpath[i] = filePathChars[i];
    }
    fullpath[filePath.Len()] = _T('\0');
  }

	TCHAR *str = new TCHAR[_tcslen(fullpath) + 1];
	_tcscpy(str, fullpath);
	DWORD tmp = 0;
	DWORD len = GetFileVersionInfoSize(str, &tmp);
	LPVOID pBlock = new char[len];
	GetFileVersionInfo(str, 0, len, pBlock);
	LPVOID ptr;
	UINT ptrlen;
	
	TCHAR SubBlock[50];
			
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
	wxString version;
 
	//Format the versionstring
	if (VerQueryValue(pBlock, _T("\\"), &ptr, &ptrlen)) {
		VS_FIXEDFILEINFO *fi = (VS_FIXEDFILEINFO*)ptr;

    version = wxString::Format(_T("%d.%d.%d.%d"),
               HIWORD(fi->dwFileVersionMS), LOWORD(fi->dwFileVersionMS),
               HIWORD(fi->dwFileVersionLS), LOWORD(fi->dwFileVersionLS));
  }

	delete [] str;
	delete [] pBlock;

	return version;

  #endif // __WINDOWS__

  return wxEmptyString;
}