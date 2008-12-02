/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __EncryptionUtil_H
#define __EncryptionUtil_H


class EncryptionUtil {

  public:

    static bool EncryptFile(const wxString& inputPath, const wxString& outputPath, const wxString& key);
    static bool DecryptFile(const wxString& inputPath, const wxString& outputPath, const wxString& key);

};

#endif // __EncryptionUtil_H