#ifndef __BitmapControlInterface_H
#define __BitmapControlInterface_H


class IBitmapProvider { };


#define DECLARE_BITMAP_CONTROL_INTERFACE() \
  protected: \
    wxBitmap pControlBitmap; \
    bool pControlBitmapInvalidated; \
    wxMemoryDC pSourceDC; \
  public: \
    wxBitmap GetControlBitmap(); \
    void InvalidateControlBitmap(); \
    void UpdateControlBitmap();

#define INITIALIZE_BITMAP_CONTROL_INTERFACE(controlClass) \
  wxBitmap controlClass::GetControlBitmap() { return pControlBitmap; } \
  void controlClass::InvalidateControlBitmap() { pControlBitmapInvalidated = true; Refresh(); } \

#define UPDATE_CONTROL_BITMAP_IF_NEEDED() \
  if (pControlBitmapInvalidated) { \
    UpdateControlBitmap(); \
    pControlBitmapInvalidated = false; \
  }

#define DRAW_PARENT_BITMAP(dc) \
  IBitmapProvider *iBitmapProvider = dynamic_cast<IBitmapProvider*>(GetParent()); \
  if (iBitmapProvider) { \
    NineSlicesPanel *parent = dynamic_cast<NineSlicesPanel*>(GetParent()); \
    wxBitmap parentBitmap = parent->GetControlBitmap(); \
    wxBitmap subBitmap = parentBitmap.GetSubBitmap(this->GetRect()); \
    dc.DrawBitmap(subBitmap, 0, 0); \
  }


#endif // __BitmapControlInterface_H