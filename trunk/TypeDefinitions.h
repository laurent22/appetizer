#ifndef __TypeDefinitions_H
#define __TypeDefinitions_H


#include "wx/wx.h"
#include "boost/shared_ptr.hpp"
#include "imaging/NineSlicesPainter.h"
#include "bitmap_controls/NineSlicesPanel.h"

typedef boost::shared_ptr<wxIcon> wxIconSP;
typedef boost::shared_ptr<NineSlicesPainter> NineSlicesPainterSP;
typedef boost::shared_ptr<NineSlicesPanel> NineSlicesPanelSP;



#endif // __TypeDefinitions_H