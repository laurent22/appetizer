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
  void controlClass::InvalidateControlBitmap() { pControlBitmapInvalidated = true; Refresh(); }


#define UPDATE_CONTROL_BITMAP_IF_NEEDED() \
  if (pControlBitmapInvalidated) { \
    UpdateControlBitmap(); \
    pControlBitmapInvalidated = false; \
  }