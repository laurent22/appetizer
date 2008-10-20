#ifndef __ShortcutEditorFrame_H
#define __ShortcutEditorFrame_H


#include <wx/wx.h>
#include "ShortcutEditorFrameBase.h"
#include "boost/shared_ptr.hpp"


class ShortcutEditorFrame: public ShortcutEditorFrameBase {

public:

  ShortcutEditorFrame();

};


typedef boost::shared_ptr<ShortcutEditorFrame> ShortcutEditorFrameSP;


#endif // __ShortcutEditorFrame_H