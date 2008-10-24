#include "ImageButton.h"



DEFINE_EVENT_TYPE(wxeEVT_CLICK)



BEGIN_EVENT_TABLE(ImageButton, BitmapControl)
  EVT_LEFT_UP(ImageButton::OnMouseUp)
  EVT_LEFT_DOWN(ImageButton::OnMouseDown)
  EVT_MOTION(ImageButton::OnMouseOver)
  EVT_LEAVE_WINDOW(ImageButton::OnMouseLeave)
END_EVENT_TABLE()


ImageButton::ImageButton(wxWindow *owner, int id, wxPoint point, wxSize size):
BitmapControl(owner, id, point, size) {
  state_ = _T("Up");  

  pressed_ = false;  
  gridIsExplicitelySet_ = false;
  nineSlicesPainterUp_ = NULL;
  nineSlicesPainterOver_ = NULL;
  nineSlicesPainterDown_ = NULL;
  nineSlicesPainterDisabled_ = NULL;
}


void ImageButton::SetGrid(int left, int top, int width, int height) {
  gridIsExplicitelySet_ = true;
  grid_.SetLeft(left);
  grid_.SetTop(top);
  grid_.SetWidth(width);
  grid_.SetHeight(height);
  InvalidateControlBitmap();
}


void ImageButton::LoadImage(const wxString& filePathPrefix) {
  if (filePathPrefix_ == filePathPrefix) return;

  filePathPrefix_ = filePathPrefix;

  wxDELETE(nineSlicesPainterUp_);
  wxDELETE(nineSlicesPainterOver_);
  wxDELETE(nineSlicesPainterDown_);
  wxDELETE(nineSlicesPainterDisabled_);

  // FIXME: Currently the loaded image MUST have an alpha channel otherwise the
  // blit operations will fail.  If the loaded bitmap is fully opaque, the alpha
  // value of at least one pixel must be set to 254 or less.

  Refresh();
}


void ImageButton::UpdateControlBitmap() {
  BitmapControl::UpdateControlBitmap();

  if (filePathPrefix_ == wxEmptyString) return;

  NineSlicesPainter* painter;
  wxString filePath = filePathPrefix_ + state_ + _T(".png");
  
  // Lazily create the painters
  if (state_ == _T("Disabled")) {
    if (!nineSlicesPainterDisabled_) {
      nineSlicesPainterDisabled_ = new NineSlicesPainter();
      nineSlicesPainterDisabled_->LoadImage(filePath);
    }
    painter = nineSlicesPainterDisabled_;
  } else if (state_ == _T("Down")) {
    if (!nineSlicesPainterDown_) {
      nineSlicesPainterDown_ = new NineSlicesPainter();
      nineSlicesPainterDown_->LoadImage(filePath);
    }
    painter = nineSlicesPainterDown_;
  } else if (state_ == _T("Over")) {
    if (!nineSlicesPainterOver_) {
      nineSlicesPainterOver_ = new NineSlicesPainter();
      nineSlicesPainterOver_->LoadImage(filePath);
    }
    painter = nineSlicesPainterOver_;
  } else { // default to "Up" state
    if (!nineSlicesPainterUp_) {
      nineSlicesPainterUp_ = new NineSlicesPainter();
      nineSlicesPainterUp_->LoadImage(filePath);
    }
    painter = nineSlicesPainterUp_;
  }

  if (!painter) return;

  if (gridIsExplicitelySet_) painter->SetGrid(grid_.GetLeft(), grid_.GetTop(), grid_.GetWidth(), grid_.GetHeight());

  wxMemoryDC destDC;
  destDC.SelectObject(*controlBitmap_);    
  painter->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
  destDC.SelectObject(wxNullBitmap);
}


ImageButton::~ImageButton() {
  wxDELETE(nineSlicesPainterUp_);
  wxDELETE(nineSlicesPainterOver_);
  wxDELETE(nineSlicesPainterDown_);
  wxDELETE(nineSlicesPainterDisabled_);
}


void ImageButton::SetState(const wxString& state) {
  if (state_ == state) return;
  state_ = state;
  InvalidateControlBitmap();
}


wxString ImageButton::GetState() {
  return state_;
}


void ImageButton::OnMouseDown(wxMouseEvent& evt) {
  wxWindow* w = static_cast<wxWindow*>(evt.GetEventObject());
  w->CaptureMouse();

  pressed_ = true;
  SetState(_T("Down"));
  Update();
}


void ImageButton::OnMouseUp(wxMouseEvent& evt) {
  wxWindow* w = static_cast<wxWindow*>(evt.GetEventObject());
  if (w->HasCapture()) w->ReleaseMouse();

  if (GetRect().Contains(evt.GetPosition())) {
    wxCommandEvent newEvent(wxeEVT_CLICK);
    newEvent.SetEventObject(this);
    GetEventHandler()->ProcessEvent(newEvent);
  }

  pressed_ = false;
  SetState(_T("Up"));
  Update();
}


void ImageButton::OnMouseOver(wxMouseEvent& evt) {
  wxWindow* w = static_cast<wxWindow*>(evt.GetEventObject());

  if (pressed_) {
    SetState(_T("Down"));
  } else {
    SetState(_T("Over"));
  }  
}


void ImageButton::OnMouseLeave(wxMouseEvent& evt) {
  SetState(_T("Up"));
}