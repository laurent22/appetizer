/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "precompiled.h"

#ifndef __TypeDefinitions_H
#define __TypeDefinitions_H


#include "imaging/NineSlicesPainter.h"
#include "bitmap_controls/NineSlicesPanel.h"


typedef boost::shared_ptr<wxIcon> wxIconSP;
typedef boost::shared_ptr<wxBitmap> wxBitmapSP;
typedef boost::shared_ptr<NineSlicesPainter> NineSlicesPainterSP;
typedef boost::shared_ptr<NineSlicesPanel> NineSlicesPanelSP;
typedef std::vector<long> LongVector;

#endif // __TypeDefinitions_H